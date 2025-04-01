const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;
const AnyWriter = std.io.AnyWriter;

const ast = @import("ast.zig");
const Environment = @import("environment.zig").Environment;
const builtins = @import("_builtin.zig");

const ObjectType = []const u8;

pub const INTEGER_OBJ = "INTEGER";
pub const BOOLEAN_OBJ = "BOOLEAN";
pub const NULL_OBJ = "NULL";
pub const RETURN_VALUE_OBJ = "RETURN_VALUE";
pub const ERROR_OBJ = "ERROR";
pub const FUNCTION_OBJ = "FUNCTION";
pub const STRING_OBJ = "STRING";
pub const BUILTIN_OBJ = "BUILTIN";

pub const TRUE = Object{ .boolean = Boolean{ .value = true } };
pub const FALSE = Object{ .boolean = Boolean{ .value = false } };
pub const NULL = Object{ ._null = Null{} };

pub const Object = union(enum) {
    integer: Integer,
    boolean: Boolean,
    _null: Null,
    return_value: ReturnValue,
    _error: Error,
    function: Function,
    string: String,
    builtin: Builtin,

    pub fn _type(self: Object) ObjectType {
        return switch (self) {
            .integer => INTEGER_OBJ,
            .boolean => BOOLEAN_OBJ,
            ._null => NULL_OBJ,
            .return_value => RETURN_VALUE_OBJ,
            ._error => ERROR_OBJ,
            .function => FUNCTION_OBJ,
            .string => STRING_OBJ,
            .builtin => BUILTIN_OBJ,
        };
    }

    pub fn inspect(self: Object, writer: AnyWriter) AnyWriter.Error!void {
        switch (self) {
            inline else => |obj| try obj.inspect(writer),
        }
    }

    pub fn deinit(self: Object, alloc: Allocator) void {
        switch (self) {
            inline else => |obj| obj.deinit(alloc),
        }
    }

    pub fn eql(self: Object, other: Object) bool {
        return std.meta.activeTag(self) == std.meta.activeTag(other);
    }

    pub fn dupe(self: Object, alloc: Allocator) Allocator.Error!Object {
        return switch (self) {
            inline else => |obj| try obj.dupe(alloc),
        };
    }
};

pub const Integer = struct {
    value: i64,

    pub fn inspect(self: *const Integer, writer: AnyWriter) AnyWriter.Error!void {
        return try writer.print("{d}", .{self.value});
    }

    pub fn deinit(_: *const Integer, _: Allocator) void {}

    pub fn dupe(self: Integer, _: Allocator) Allocator.Error!Object {
        return Object{ .integer = self };
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn inspect(self: *const Boolean, writer: AnyWriter) AnyWriter.Error!void {
        try writer.print("{any}", .{self.value});
    }

    pub fn deinit(_: *const Boolean, _: Allocator) void {}

    pub fn dupe(self: Boolean, _: Allocator) Allocator.Error!Object {
        return Object{ .boolean = self };
    }
};

pub const Null = struct {
    pub fn inspect(_: *const Null, writer: AnyWriter) AnyWriter.Error!void {
        try writer.print("null", .{});
    }

    pub fn deinit(_: *const Null, _: Allocator) void {}

    pub fn dupe(self: Null, _: Allocator) Allocator.Error!Object {
        return Object{ ._null = self };
    }
};

pub const ReturnValue = struct {
    value: ?*Object,

    pub fn inspect(self: *const ReturnValue, writer: AnyWriter) AnyWriter.Error!void {
        if (self.value) |value| try value.inspect(writer) else try writer.print("null", .{});
    }

    pub fn deinit(self: *const ReturnValue, alloc: Allocator) void {
        if (self.value) |value| {
            value.deinit(alloc);
            alloc.destroy(value);
        }
    }

    pub fn dupe(self: ReturnValue, alloc: Allocator) Allocator.Error!Object {
        var duped_value_ptr: ?*Object = null;
        if (self.value) |value| {
            const duped_value = try value.dupe(alloc);
            duped_value_ptr.? = try alloc.create(Object);
            duped_value_ptr.?.* = duped_value;
        }
        return Object{ .return_value = ReturnValue{ .value = duped_value_ptr } };
    }
};

pub const Error = struct {
    message: []const u8,

    pub fn inspect(self: *const Error, writer: AnyWriter) AnyWriter.Error!void {
        try writer.print("ERROR: {s}", .{self.message});
    }

    pub fn deinit(self: *const Error, alloc: Allocator) void {
        alloc.free(self.message);
    }

    pub fn dupe(self: Error, alloc: Allocator) Allocator.Error!Object {
        return Object{ ._error = Error{ .message = try alloc.dupe(u8, self.message) } };
    }
};

pub const Function = struct {
    parameters: []const ast.Identifier,
    body: ast.BlockStatement,
    env: *Environment,

    pub fn inspect(self: *const Function, writer: AnyWriter) AnyWriter.Error!void {
        try writer.writeAll("fn(");

        if (self.parameters.len > 0) {
            try self.parameters[0].string(writer);
            for (self.parameters[1..]) |param| {
                try writer.writeAll(", ");
                try param.string(writer);
            }
        }

        try writer.writeAll(") {\n");

        try self.body.string(writer);
        try writer.writeByte('\n');
    }

    pub fn deinit(self: *const Function, alloc: Allocator) void {
        for (self.parameters) |param| {
            param.dupe_deinit(alloc);
        }
        alloc.free(self.parameters);

        self.body.dupe_deinit(alloc);

        if (!self.env.isRootEnvironment()) {
            self.env.deinit();
            self.env.alloc.destroy(self.env);
        }
    }

    pub fn dupe(self: Function, alloc: Allocator) Allocator.Error!Object {
        var duped_parameters = std.ArrayListUnmanaged(ast.Identifier).empty;
        errdefer {
            for (duped_parameters.items) |item| {
                item.dupe_deinit(alloc);
            }
            duped_parameters.deinit(alloc);
        }
        for (self.parameters) |param| {
            const duped_param = try param.dupe(alloc);
            errdefer duped_param.dupe_deinit(alloc);
            std.debug.assert(duped_param == .identifier);
            try duped_parameters.append(alloc, duped_param.identifier);
        }
        const duped_body = try self.body.dupe(alloc);
        errdefer duped_body.dupe_deinit(alloc);
        std.debug.assert(duped_body == .block_statement);

        var duped_env_ptr = self.env;
        if (!self.env.isRootEnvironment()) {
            var duped_env = try self.env.dupe();
            errdefer duped_env.deinit();
            duped_env_ptr = try self.env.alloc.create(Environment);
            duped_env_ptr.* = duped_env;
        }
        errdefer if (!self.env.isRootEnvironment()) {
            duped_env_ptr.deinit();
            self.env.alloc.destroy(duped_env_ptr);
        };

        return Object{
            .function = Function{
                .parameters = try duped_parameters.toOwnedSlice(alloc),
                .body = duped_body.block_statement,
                .env = duped_env_ptr,
            },
        };
    }
};

pub const String = struct {
    value: []const u8,

    pub fn inspect(self: *const String, writer: AnyWriter) AnyWriter.Error!void {
        return try writer.print("{s}", .{self.value});
    }

    pub fn deinit(self: *const String, alloc: Allocator) void {
        alloc.free(self.value);
    }

    pub fn dupe(self: String, alloc: Allocator) Allocator.Error!Object {
        const duped_value = try alloc.dupe(u8, self.value);
        return Object{ .string = String{ .value = duped_value } };
    }
};

pub const Builtin = struct {
    _fn: builtins.BuiltinFnPointer,

    pub fn inspect(_: *const Builtin, writer: AnyWriter) AnyWriter.Error!void {
        return try writer.writeAll("builtin function");
    }

    pub fn deinit(_: *const Builtin, _: Allocator) void {}

    pub fn dupe(self: Builtin, _: Allocator) Allocator.Error!Object {
        return Object{ .builtin = self };
    }
};

// Test Suite

test "Object inspect" {
    var alloc_object = Object{ ._null = Null{} };
    const object_tests = [_]struct { object: Object, expected: []const u8 }{
        .{ .object = Object{ .integer = Integer{ .value = 1 } }, .expected = "1" },
        .{ .object = Object{ .boolean = Boolean{ .value = true } }, .expected = "true" },
        .{ .object = Object{ ._null = Null{} }, .expected = "null" },
        .{ .object = Object{
            .return_value = ReturnValue{ .value = &alloc_object },
        }, .expected = "null" },
    };

    var array_list = std.ArrayListUnmanaged(u8).empty;
    defer array_list.deinit(testing.allocator);
    const writer = array_list.writer(testing.allocator).any();
    for (object_tests) |object_test| {
        try object_test.object.inspect(writer);
        try testing.expectEqualStrings(object_test.expected, array_list.items);

        array_list.clearRetainingCapacity();
    }
}

test "Object type" {
    var alloc_object = Object{ ._null = Null{} };
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
