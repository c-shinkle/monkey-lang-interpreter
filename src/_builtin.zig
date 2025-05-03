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
    .{ "last", obj.Object{ .builtin = obj.Builtin{ ._fn = last } } },
    .{ "rest", obj.Object{ .builtin = obj.Builtin{ ._fn = rest } } },
    .{ "push", obj.Object{ .builtin = obj.Builtin{ ._fn = push } } },
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

fn last(alloc: Allocator, args: []const obj.Object) BuiltinError!obj.Object {
    if (args.len != 1) {
        const fmt = "wrong number of arguments. got={d}, want=1";
        return try evaluator.newError(alloc, fmt, .{args.len});
    }

    if (args[0] != .array) {
        const fmt = "argument to 'last' must be ARRAY, got {s}";
        return try evaluator.newError(alloc, fmt, .{args[0]._type()});
    }

    const elements = args[0].array.elements;
    if (elements.len == 0) {
        return obj.NULL;
    }

    return elements[elements.len - 1].dupe(alloc);
}

fn rest(alloc: Allocator, args: []const obj.Object) BuiltinError!obj.Object {
    if (args.len != 1) {
        const fmt = "wrong number of arguments. got={d}, want=1";
        return try evaluator.newError(alloc, fmt, .{args.len});
    }

    if (args[0] != .array) {
        const fmt = "argument to 'rest' must be ARRAY, got {s}";
        return try evaluator.newError(alloc, fmt, .{args[0]._type()});
    }

    const original_elements = args[0].array.elements;
    if (original_elements.len == 0) {
        return obj.NULL;
    }

    const duped_elements = try alloc.alloc(obj.Object, original_elements.len - 1);
    for (0..duped_elements.len) |i| {
        duped_elements[i] = try original_elements[i + 1].dupe(alloc);
    }

    return obj.Object{ .array = obj.Array{ .elements = duped_elements } };
}

fn push(alloc: Allocator, args: []const obj.Object) BuiltinError!obj.Object {
    if (args.len != 2) {
        const fmt = "wrong number of arguments. got={d}, want=2";
        return try evaluator.newError(alloc, fmt, .{args.len});
    }

    if (args[0] != .array) {
        const fmt = "argument to 'push' must be ARRAY, got {s}";
        return try evaluator.newError(alloc, fmt, .{args[0]._type()});
    }

    const original_elements = args[0].array.elements;

    const duped_elements = try alloc.alloc(obj.Object, original_elements.len + 1);
    for (0..original_elements.len) |i| {
        duped_elements[i] = try original_elements[i].dupe(alloc);
    }
    duped_elements[original_elements.len] = try args[1].dupe(alloc);

    return obj.Object{ .array = obj.Array{ .elements = duped_elements } };
}

test {
    std.testing.refAllDecls(@This());
}
