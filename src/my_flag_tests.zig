const std = @import("std");
const assert = std.debug.assert;

pub const Handler = extern union {
    C: *const fn () callconv(.C) void,
    Naked: *const fn () callconv(.Naked) void,
    // Interrupt is not supported on arm
};

pub const unhandled = Handler{
    .C = struct {
        fn tmp() callconv(.C) noreturn {
            @panic("unhandled interrupt");
        }
    }.tmp,
};

pub fn RegisterCreate(comptime PackedT: type) type {
    const size = @bitSizeOf(PackedT);
    if ((size % 8) != 0)
        @compileError("size must be divisible by 8!");

    if (!std.math.isPowerOfTwo(size / 8))
        @compileError("size must encode a power of two number of bytes!");

    const IntT = std.meta.Int(.unsigned, size);

    if (@sizeOf(PackedT) != (size / 8))
        @compileError(std.fmt.comptimePrint("IntT and PackedT must have the same size!, they are {} and {} bytes respectively", .{ size / 8, @sizeOf(PackedT) }));

    return extern struct {
        const Self = @This();

        raw: IntT,

        pub const underlying_type = PackedT;

        pub inline fn read(addr: *volatile Self) PackedT {
            return @bitCast(addr.raw);
        }

        pub inline fn write(addr: *volatile Self, val: PackedT) void {
            comptime {
                assert(@bitSizeOf(PackedT) == @bitSizeOf(IntT));
            }
            addr.write_raw(@bitCast(val));
        }

        pub fn write_raw(addr: *volatile Self, val: IntT) void {
            addr.raw = val;
        }

        fn reg_setf(reg: anytype, comptime acl: anytype, comptime field_tag: anytype, value: anytype) void {
            comptime var field_name = @tagName(field_tag);
            inline for (acl) |ac| {
                comptime var f_found = (ac.tag == field_tag);
                if (f_found) {
                    comptime var is_ro = (false == ac.ar.w);
                    if (is_ro == false) {
                        @field(reg, field_name) = value;
                        std.log.debug("{s} set to {d}", .{ field_name, value });
                        return;
                    } else {
                        @compileError(std.fmt.comptimePrint("{s} is RO!\n", .{ac.f}));
                    }
                }
            }
            @compileError(std.fmt.comptimePrint("{s} not found!\n", .{field_name}));
        }

        fn reg_clearf(reg: anytype, comptime acl: anytype, comptime field_tag: anytype) void {
            comptime var field_name = @tagName(field_tag);
            inline for (acl) |ac| {
                comptime var f_found = (ac.tag == field_tag);
                if (f_found) {
                    comptime var is_ro = (false == ac.ar.w);
                    if (is_ro == false) {
                        if (ac.ar.stc == false) {
                            @field(reg, field_name) = 0;
                            std.log.debug("{s} set to 0", .{field_name});
                        } else {
                            const field_type = @TypeOf(@field(reg, field_name));
                            comptime var set_mask: field_type = 0;
                            set_mask = @as(field_type, @truncate(std.math.maxInt(std.meta.Int(.unsigned, @typeInfo(field_type).Int.bits))));

                            @field(reg, field_name) = set_mask;
                            std.log.debug("{s} set to {d} to clear", .{ field_name, set_mask });
                        }
                        return;
                    } else {
                        @compileError(std.fmt.comptimePrint("{s} is RO!\n", .{ac.f}));
                    }
                }
            }
            @compileError(std.fmt.comptimePrint("{s} not found!\n", .{field_name}));
        }

        fn has_ro_fields(comptime acl: anytype) bool {
            inline for (acl) |ac| {
                if (true == ac.ar.w) {
                    return true;
                }
            }
            return false;
        }

        fn has_wo_fields(comptime acl: anytype) bool {
            inline for (acl) |ac| {
                if (true == ac.ar.w) {
                    return true;
                }
            }
            return false;
        }

        pub inline fn modify(addr: *volatile Self, fields: anytype) void {
            var val = read(addr);
            inline for (@typeInfo(@TypeOf(fields)).Struct.fields) |field| {
                @field(val, field.name) = @field(fields, field.name);
            }
            write(addr, val);
        }

        pub inline fn toggle(addr: *volatile Self, fields: anytype) void {
            var val = read(addr);
            inline for (@typeInfo(@TypeOf(fields)).Struct.fields) |field| {
                @field(val, @tagName(field.default_value.?)) = !@field(val, @tagName(field.default_value.?));
            }
            write(addr, val);
        }
    };
}

const regs = struct {
    const AccessRights = struct { r: bool = true, w: bool = true, stc: bool = false };
    pub fn print(reg: anytype) void {
        inline for (std.meta.fields(@TypeOf(reg))) |f| {
            std.log.debug("{s:>08}: {any}", .{ f.name, @as(f.type, @field(reg, f.name)) });
        }
    }
    fn reg_setf(reg: anytype, comptime acl: anytype, comptime field_tag: anytype, value: anytype) void {
        comptime var field_name = @tagName(field_tag);
        inline for (acl) |ac| {
            comptime var f_found = (ac.tag == field_tag);
            if (f_found) {
                comptime var is_ro = (false == ac.ar.w);
                if (is_ro == false) {
                    @field(reg, field_name) = value;
                    std.log.debug("{s} set to {d}", .{ field_name, value });
                    return;
                } else {
                    @compileError(std.fmt.comptimePrint("{s} is RO!\n", .{ac.f}));
                }
            }
        }
        @compileError(std.fmt.comptimePrint("{s} not found!\n", .{field_name}));
    }

    fn reg_clearf(reg: anytype, comptime acl: anytype, comptime field_tag: anytype) void {
        comptime var field_name = @tagName(field_tag);
        inline for (acl) |ac| {
            comptime var f_found = (ac.tag == field_tag);
            if (f_found) {
                comptime var is_ro = (false == ac.ar.w);
                if (is_ro == false) {
                    if (ac.ar.stc == false) {
                        @field(reg, field_name) = 0;
                        std.log.debug("{s} set to 0", .{field_name});
                    } else {
                        const field_type = @TypeOf(@field(reg, field_name));
                        comptime var set_mask: field_type = 0;
                        set_mask = @as(field_type, @truncate(std.math.maxInt(std.meta.Int(.unsigned, @typeInfo(field_type).Int.bits))));

                        @field(reg, field_name) = set_mask;
                        std.log.debug("{s} set to {d} to clear", .{ field_name, set_mask });
                    }
                    return;
                } else {
                    @compileError(std.fmt.comptimePrint("{s} is RO!\n", .{ac.f}));
                }
            }
        }
        @compileError(std.fmt.comptimePrint("{s} not found!\n", .{field_name}));
    }

    fn reg_getf(reg: anytype, comptime acl: anytype, comptime field_tag: anytype) u32 {
        comptime var field_name = @tagName(field_tag);
        inline for (acl) |ac| {
            comptime var f_found = (ac.tag == field_tag);
            if (f_found) {
                comptime var is_wo = (false == ac.ar.r);
                if (is_wo == false) {
                    var value = @field(reg, field_name);
                    std.log.debug("{s} read as {d}", .{ field_name, value });
                    return value;
                } else {
                    @compileError(std.fmt.comptimePrint("{s} is WO!\n", .{ac.fname}));
                }
            }
        }
        @compileError(std.fmt.comptimePrint("{s} not found!\n", .{field_name}));
    }

    fn has_ro_fields(comptime acl: anytype) bool {
        inline for (acl) |ac| {
            if (true == ac.ar.w) {
                return true;
            }
        }
        return false;
    }

    fn has_wo_fields(comptime acl: anytype) bool {
        inline for (acl) |ac| {
            if (true == ac.ar.w) {
                return true;
            }
        }
        return false;
    }

    const I2C = struct {
        CONSET: packed struct {
            const Register = @This();
            ///  Reserved. User software should not write ones to reserved bits. The value read from a reserved bit is not defined.
            _r1: u2 = 0,
            ///  Assert acknowledge flag.
            AA: u1 = 0,
            ///  I2C interrupt flag.
            SI: u1 = 0,
            ///  STOP flag.
            STO: u1 = 0,
            ///  START flag.
            STA: u3 = 0,
            ///  I2C interface enable.
            I2EN: u1 = 0,
            ///  Reserved. The value read from a reserved bit is not defined.
            _r2: u23 = 0,

            const Fields = enum { _r1, AA, SI, STO, STA, I2EN, _r2 };
            const fat = struct { tag: Fields, ar: AccessRights };
            const acl = [_]fat{
                fat{ .tag = .SI, .ar = .{ .w = false } },
                fat{ .tag = .STO, .ar = .{} },
                fat{ .tag = .STA, .ar = .{ .stc = true } },
            };
            pub fn copy_from(reg: *Register, src: *Register) void {
                comptime var has_ro = !has_ro_fields(acl[0..]);
                if (has_ro) {
                    reg.* = src.*;
                } else {
                    @compileError(std.fmt.comptimePrint("Can't perform RAW copy! ({s}) has RO fields!\n", .{@typeName(Register)}));
                }
            }
            pub fn copy_to(reg: *Register, dest: *Register) void {
                comptime var has_wo = !has_wo_fields(acl[0..]);
                if (has_wo) {
                    dest.* = reg.*;
                } else {
                    @compileError(std.fmt.comptimePrint("Can't perform RAW copy! ({s}) has WO fields!\n", .{@typeName(Register)}));
                }
            }
            pub fn setf(reg: *Register, comptime field_tag: Register.Fields, value: anytype) void {
                reg_setf(reg, acl[0..], field_tag, value);
            }
            pub fn getf(reg: *Register, comptime field_tag: Register.Fields) u32 {
                return reg_getf(reg, acl[0..], field_tag);
            }
            pub fn clearf(reg: *Register, comptime field_tag: Register.Fields) void {
                reg_clearf(reg, acl[0..], field_tag);
            }

            pub const uint_t = std.meta.Int(.unsigned, @bitSizeOf(Register));

            pub fn set_raw(reg: *Register, value: anytype) void {
                comptime var has_ro = has_ro_fields(acl[0..]);
                if (has_ro) {
                    if (@bitSizeOf(uint_t) >= @bitSizeOf(@TypeOf(value))) {
                        var cast = @as(uint_t, @intCast(value));
                        reg.* = @as(Register, @bitCast(cast));
                    } else {
                        var cast = @as(uint_t, @truncate(value));
                        reg.* = @as(Register, @bitCast(cast));
                    }
                } else {
                    @compileError(std.fmt.comptimePrint("Can't perform RAW copy! ({s}) has RO fields!\n", .{@typeName(Register)}));
                }
            }
            pub fn get_raw(reg: *Register) uint_t {
                comptime var has_wo = has_wo_fields(acl[0..]);
                if (has_wo) {
                    return @as(uint_t, @bitCast(reg.*));
                } else {
                    @compileError(std.fmt.comptimePrint("Can't perform RAW copy! ({s}) has WO fields!\n", .{@typeName(Register)}));
                }
            }
        } = .{},
        STAT: packed struct {
            ///  These bits are unused and are always 0.
            _r1: u3 = 0,
            ///  These bits give the actual status information about the I 2C interface.
            Status: u5 = 0,
            ///  Reserved. The value read from a reserved bit is not defined.
            _r2: u24 = 0,
        } = .{},
    };
};

pub fn main() !void {
    std.log.info("Size of regs:{}\n", .{@sizeOf(regs.I2C)});

    var my_i2c = regs.I2C{};
    my_i2c.CONSET.setf(.STO, 1);
    my_i2c.CONSET.clearf(.STA);
    regs.print(my_i2c.CONSET);
    _ = my_i2c.CONSET.getf(.STO);
    my_i2c.CONSET.clearf(.STO);
    regs.print(my_i2c.CONSET);
    my_i2c.CONSET.set_raw(1024);
    regs.print(my_i2c.CONSET);
    std.log.info("Raw value:{}\n", .{my_i2c.CONSET.get_raw()});
    //my_i2c.CONSET.copy_to(&dest_i2c.CONSET);
}

test "register get/set test" {
    var my_i2c = regs.I2C{};
    var sto = my_i2c.CONSET.getf("STO");
    try std.testing.expectEqual(sto, 0);
    my_i2c.CONSET.setf("STO", 1);
    try std.testing.expectEqual(my_i2c.CONSET.getf("STO"), 1);
}
