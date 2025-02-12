const std = @import("std");
const testing = std.testing;

const AnyWriter = std.io.AnyWriter;

const ObjectType = []const u8;

const INTEGER_OBJ = "INTEGER";
const BOOLEAN_OBJ = "BOOLEAN";
const NULL_OBJ = "NULL";

pub const Object = union(enum) {
    integer: Integer,
    boolean: Boolean,
    _null: Null,

    pub fn inspect(self: *const Object, writer: *AnyWriter) AnyWriter.Error!void {
        switch (self.*) {
            inline else => |obj| try obj.inspect(writer),
        }
    }

    pub fn _type(self: *const Object) ObjectType {
        return switch (self.*) {
            inline else => |obj| obj._type(),
        };
    }
};

pub const Integer = struct {
    value: i64,

    pub fn inspect(self: Integer, writer: *AnyWriter) AnyWriter.Error!void {
        return try writer.print("{d}", .{self.value});
    }

    pub fn _type(self: *const Integer) ObjectType {
        _ = self; // autofix
        return INTEGER_OBJ;
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn inspect(self: *const Boolean, writer: *AnyWriter) AnyWriter.Error!void {
        return try writer.print("{any}", .{self.value});
    }

    pub fn _type(self: *const Boolean) ObjectType {
        _ = self; // autofix
        return BOOLEAN_OBJ;
    }
};

pub const Null = struct {
    pub fn inspect(self: *const Null, writer: *AnyWriter) AnyWriter.Error!void {
        _ = self; // autofix
        return try writer.print("null", .{});
    }

    pub fn _type(self: *const Null) ObjectType {
        _ = self; // autofix
        return NULL_OBJ;
    }
};

test "Object inspect" {
    const object_tests = [_]struct { object: Object, expected: []const u8 }{
        .{ .object = Object{ .integer = Integer{ .value = 1 } }, .expected = "1" },
        .{ .object = Object{ .boolean = Boolean{ .value = true } }, .expected = "true" },
        .{ .object = Object{ ._null = Null{} }, .expected = "null" },
    };

    var array_list = std.ArrayList(u8).init(testing.allocator);
    defer array_list.deinit();
    var writer = array_list.writer().any();
    for (object_tests) |object_test| {
        try object_test.object.inspect(&writer);

        try testing.expectEqualStrings(object_test.expected, array_list.items);

        array_list.clearRetainingCapacity();
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
