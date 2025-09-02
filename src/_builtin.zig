const std = @import("std");
const Allocator = std.mem.Allocator;
const evaluator = @import("evaluator.zig");
const obj = @import("object.zig");

pub const BuiltinError = Allocator.Error || obj.ObjectError;

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
    .{ "puts", obj.Object{ .builtin = obj.Builtin{ ._fn = puts } } },
});

fn len(alloc: Allocator, args: []const obj.Object) BuiltinError!obj.Object {
    if (args.len != 1) {
        return evaluator.newError(alloc, "wrong number of arguments. got={d}, want=1", .{args.len});
    }
    return switch (args[0]) {
        .string => |str| obj.Object{ .integer = obj.Integer{ .value = @intCast(str.value.len) } },
        .array => |arr| obj.Object{ .integer = obj.Integer{ .value = @intCast(arr.elements.len) } },
        else => new_error: {
            const fmt = "argument to 'len' not supported, got {s}";
            break :new_error try evaluator.newError(alloc, fmt, .{args[0]._type()});
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

    return elements[0];
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

    return elements[elements.len - 1];
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

    return obj.Object{ .array = obj.Array{ .elements = original_elements[1..] } };
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

    const next_elements = try alloc.alloc(obj.Object, original_elements.len + 1);
    std.mem.copyForwards(obj.Object, next_elements, original_elements);
    next_elements[original_elements.len] = args[1];

    return obj.Object{ .array = obj.Array{ .elements = next_elements } };
}

fn puts(_: Allocator, args: []const obj.Object) BuiltinError!obj.Object {
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_temp = std.fs.File.stdout().writer(&stdout_buffer);
    var stdout_writer = &stdout_temp.interface;
    defer stdout_writer.flush() catch std.debug.print("Failed to flush stdout_buffer!\n", .{});

    for (args) |arg| {
        try arg.inspect(stdout_writer);
        try stdout_writer.writeByte('\n');
    }
    return obj.NULL;
}
