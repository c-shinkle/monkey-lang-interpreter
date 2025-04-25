const std = @import("std");
const Allocator = std.mem.Allocator;
pub const BuiltinError = std.fmt.AllocPrintError;

const evaluator = @import("evaluator.zig");
const obj = @import("object.zig");

pub const BuiltinFnPointer = *const fn (
    alloc: Allocator,
    args: []const obj.Object,
) BuiltinError!obj.Object;

pub const builtin_map = std.StaticStringMap(obj.Object).initComptime(.{
    .{ "len", obj.Object{ .builtin = obj.Builtin{ ._fn = len } } },
    .{ "first", obj.Object{ .builtin = obj.Builtin{ ._fn = first } } },
});

fn len(alloc: Allocator, args: []const obj.Object) BuiltinError!obj.Object {
    if (args.len != 1) {
        return evaluator.newError(alloc, "wrong number of arguments. got={d}, want=1", .{args.len});
    }
    return switch (args[0]) {
        .string => |str| obj.Object{ .integer = obj.Integer{ .value = @intCast(str.value.len) } },
        .array => |arr| obj.Object{ .integer = obj.Integer{ .value = @intCast(arr.elements.len) } },
        else => blk: {
            const fmt = "argument to 'len' not supported, got {s}";
            break :blk try evaluator.newError(alloc, fmt, .{args[0]._type()});
        },
    };
}

fn first(alloc: Allocator, args: []const obj.Object) BuiltinError!obj.Object {
    if (args.len != 1) {
        const fmt = "wrong number of arguments. got={d}, want=1";
        return try evaluator.newError(alloc, fmt, .{args.len});
    }

    if (args[0] != .array) {
        const fmt = "argument to 'first' must be ARRAY, got {s}";
        return try evaluator.newError(alloc, fmt, .{args[0]._type()});
    }

    const elements = args[0].array.elements;
    if (elements.len == 0) {
        return obj.NULL;
    }

    return elements[0].dupe(alloc);
}

test {
    std.testing.refAllDecls(@This());
}
