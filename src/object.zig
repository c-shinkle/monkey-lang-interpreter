const std = @import("std");
const testing = std.testing;

const ObjectType = []const u8;
const ObjectError = error{} || std.fmt.AllocPrintError;

const INTEGER_OBJ = "INTEGER";
const BOOLEAN_OBJ = "BOOLEAN";
const NULL_OBJ = "NULL";

pub const Object = union(enum) {
    integer: Integer,
    boolean: Boolean,
    _null: Null,

    pub fn inspect(self: *const Object, allocator: std.mem.Allocator) ObjectError![]const u8 {
        return switch (self.*) {
            inline else => |obj| obj.inspect(allocator),
        };
    }

    pub fn _type(self: *const Object) ObjectType {
        return switch (self.*) {
            inline else => |obj| obj._type(),
        };
    }
};

pub const Integer = struct {
    value: i64,

    pub fn inspect(self: Integer, allocator: std.mem.Allocator) ObjectError![]const u8 {
        return std.fmt.allocPrint(allocator, "{d}", .{self.value});
    }

    pub fn _type(_: *const Integer) ObjectType {
        return INTEGER_OBJ;
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn inspect(self: *const Boolean, allocator: std.mem.Allocator) ObjectError![]const u8 {
        return std.fmt.allocPrint(allocator, "{any}", .{self.value});
    }

    pub fn _type(_: *const Boolean) ObjectType {
        return BOOLEAN_OBJ;
    }
};

pub const Null = struct {
    pub fn inspect(_: *const Null, allocator: std.mem.Allocator) ObjectError![]const u8 {
        return std.fmt.allocPrint(allocator, "null", .{});
    }

    pub fn _type(_: *const Null) ObjectType {
        return NULL_OBJ;
    }
};

test "Object inspect" {
    const object_tests = [_]struct { object: Object, expected: []const u8 }{
        .{ .object = Object{ .integer = Integer{ .value = 1 } }, .expected = "1" },
        .{ .object = Object{ .boolean = Boolean{ .value = true } }, .expected = "true" },
        .{ .object = Object{ ._null = Null{} }, .expected = "null" },
    };

    for (object_tests) |object_test| {
        const actual = try object_test.object.inspect(testing.allocator);
        defer testing.allocator.free(actual);

        try testing.expectEqualStrings(object_test.expected, actual);
    }
}

test "Object type" {
    const object_tests = [_]struct { object: Object, expected: ObjectType }{
        .{ .object = Object{ .integer = Integer{ .value = 1 } }, .expected = INTEGER_OBJ },
        .{ .object = Object{ .boolean = Boolean{ .value = true } }, .expected = BOOLEAN_OBJ },
        .{ .object = Object{ ._null = Null{} }, .expected = NULL_OBJ },
    };

    for (object_tests) |object_test| {
        const actual = object_test.object._type();
        try testing.expectEqualStrings(object_test.expected, actual);
    }
}
