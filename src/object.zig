const std = @import("std");
const testing = std.testing;

const AnyWriter = std.io.AnyWriter;

const ObjectType = []const u8;

pub const INTEGER_OBJ = "INTEGER";
pub const BOOLEAN_OBJ = "BOOLEAN";
pub const NULL_OBJ = "NULL";
pub const RETURN_VALUE_OBJ = "RETURN_VALUE";

pub const Object = union(enum) {
    integer: Integer,
    boolean: Boolean,
    _null: Null,
    return_value: ReturnValue,

    pub fn _type(self: Object) ObjectType {
        return switch (self) {
            inline else => |obj| obj._type(),
        };
    }

    pub fn inspect(self: Object, writer: AnyWriter) AnyWriter.Error!void {
        switch (self) {
            inline else => |obj| try obj.inspect(writer),
        }
    }

    pub fn deinit(self: Object, allocator: std.mem.Allocator) void {
        switch (self) {
            inline else => |obj| obj.deinit(allocator),
        }
    }
};

pub const Integer = struct {
    value: i64,

    pub fn _type(self: *const Integer) ObjectType {
        _ = self; // autofix
        return INTEGER_OBJ;
    }

    pub fn inspect(self: *const Integer, writer: AnyWriter) AnyWriter.Error!void {
        return try writer.print("{d}", .{self.value});
    }

    pub fn deinit(self: *const Integer, allocator: std.mem.Allocator) void {
        _ = self; // autofix
        _ = allocator; // autofix
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn _type(self: *const Boolean) ObjectType {
        _ = self; // autofix
        return BOOLEAN_OBJ;
    }

    pub fn inspect(self: *const Boolean, writer: AnyWriter) AnyWriter.Error!void {
        try writer.print("{any}", .{self.value});
    }

    pub fn deinit(self: *const Boolean, allocator: std.mem.Allocator) void {
        _ = self; // autofix
        _ = allocator; // autofix
    }
};

pub const Null = struct {
    pub fn _type(self: *const Null) ObjectType {
        _ = self; // autofix
        return NULL_OBJ;
    }
    pub fn inspect(self: *const Null, writer: AnyWriter) AnyWriter.Error!void {
        _ = self; // autofix
        try writer.print("null", .{});
    }

    pub fn deinit(self: *const Null, allocator: std.mem.Allocator) void {
        _ = self; // autofix
        _ = allocator; // autofix
    }
};

pub const ReturnValue = struct {
    value: *?Object,

    pub fn _type(self: *const ReturnValue) ObjectType {
        _ = self; // autofix
        return RETURN_VALUE_OBJ;
    }

    pub fn inspect(self: *const ReturnValue, writer: AnyWriter) AnyWriter.Error!void {
        if (self.value.*) |v| try v.inspect(writer) else try writer.print("null", .{});
    }

    pub fn deinit(self: *const ReturnValue, allocator: std.mem.Allocator) void {
        if (self.value.*) |value| value.deinit(allocator);
        allocator.destroy(self.value);
    }
};

test "Object inspect" {
    var alloc_object: ?Object = Object{ ._null = Null{} };
    const object_tests = [_]struct { object: Object, expected: []const u8 }{
        .{ .object = Object{ .integer = Integer{ .value = 1 } }, .expected = "1" },
        .{ .object = Object{ .boolean = Boolean{ .value = true } }, .expected = "true" },
        .{ .object = Object{ ._null = Null{} }, .expected = "null" },
        .{ .object = Object{
            .return_value = ReturnValue{ .value = &alloc_object },
        }, .expected = "null" },
    };

    var array_list = std.ArrayList(u8).init(testing.allocator);
    defer array_list.deinit();
    const writer = array_list.writer().any();
    for (object_tests) |object_test| {
        try object_test.object.inspect(writer);
        try testing.expectEqualStrings(object_test.expected, array_list.items);

        array_list.clearRetainingCapacity();
    }
}

test "Object type" {
    var alloc_object: ?Object = Object{ ._null = Null{} };
    const object_tests = [_]struct { object: Object, expected: ObjectType }{
        .{ .object = Object{ .integer = Integer{ .value = 1 } }, .expected = INTEGER_OBJ },
        .{ .object = Object{ .boolean = Boolean{ .value = true } }, .expected = BOOLEAN_OBJ },
        .{ .object = Object{ ._null = Null{} }, .expected = NULL_OBJ },
        .{ .object = Object{
            .return_value = ReturnValue{ .value = &alloc_object },
        }, .expected = RETURN_VALUE_OBJ },
    };

    for (object_tests) |object_test| {
        const actual = object_test.object._type();
        try testing.expectEqualStrings(object_test.expected, actual);
    }
}
