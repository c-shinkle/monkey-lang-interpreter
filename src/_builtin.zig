const std = @import("std");
const obj = @import("object.zig");
const evaluator = @import("evaluator.zig");

const Allocator = std.mem.Allocator;

pub const BuiltinError = std.fmt.AllocPrintError;

pub const BuiltinFnPointer = *const fn (
    alloc: Allocator,
    args: []const obj.Object,
) BuiltinError!obj.Object;

pub var builtin_map = std.StaticStringMap(obj.Object).initComptime(.{
    .{
        "len", obj.Object{ .builtin = obj.Builtin{ ._fn = len } },
    },
});

fn len(alloc: Allocator, args: []const obj.Object) BuiltinError!obj.Object {
    if (args.len != 1) {
        return evaluator.newError(alloc, "wrong number of arguments. got={d}, want=1", .{args.len});
    }
    return switch (args[0]) {
        .string => |str| obj.Object{ .integer = obj.Integer{ .value = @intCast(str.value.len) } },
        else => blk: {
            const fmt = "argument to 'len' not supported, got {s}";
            break :blk try evaluator.newError(alloc, fmt, .{args[0]._type()});
        },
    };
}
