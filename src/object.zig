const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;
const AnyWriter = std.io.AnyWriter;

const ast = @import("ast.zig");
const builtins = @import("_builtin.zig");
const Environment = @import("Environment.zig");

const ObjectType = []const u8;

pub const ObjectError = AnyWriter.Error;

pub const INTEGER_OBJ = "INTEGER";
pub const BOOLEAN_OBJ = "BOOLEAN";
pub const NULL_OBJ = "NULL";
pub const RETURN_VALUE_OBJ = "RETURN_VALUE";
pub const ERROR_OBJ = "ERROR";
pub const FUNCTION_OBJ = "FUNCTION";
pub const STRING_OBJ = "STRING";
pub const BUILTIN_OBJ = "BUILTIN";
pub const ARRAY_OBJ = "ARRAY";
pub const DICTIONARY_OBJ = "DICTIONARY";

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
    array: Array,
    dictionary: Dictionary,

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
            .array => ARRAY_OBJ,
            .dictionary => DICTIONARY_OBJ,
        };
    }

    pub fn inspect(self: Object, writer: AnyWriter) ObjectError!void {
        switch (self) {
            inline else => |obj| try obj.inspect(writer),
        }
    }

    pub fn eqlTag(self: Object, other: Object) bool {
        return std.meta.activeTag(self) == std.meta.activeTag(other);
    }

    pub fn dupe(self: Object, alloc: Allocator) Allocator.Error!Object {
        return switch (self) {
            inline else => |obj| try obj.dupe(alloc),
        };
    }

    pub fn HashMap(comptime V: type) type {
        return std.HashMapUnmanaged(
            Object,
            V,
            struct {
                pub fn hash(_: @This(), key: Object) HashMap(Object).Hash {
                    return key.hash();
                }

                pub fn eql(_: @This(), a: Object, b: Object) bool {
                    return a.eql_hash(b);
                }
            },
            std.hash_map.default_max_load_percentage,
        );
    }

    pub fn hash(self: Object) HashMap(Object).Hash {
        var hasher = std.hash.Fnv1a_64.init();
        switch (self) {
            .integer => |int| int.hash(&hasher),
            .boolean => |b| b.hash(&hasher),
            .string => |str| str.hash(&hasher),
            else => unreachable,
        }
        return hasher.final();
    }

    pub fn eql_hash(self: Object, other: Object) bool {
        return switch (self) {
            .integer => |self_int| self_int.eql_hash(other),
            .boolean => |self_b| self_b.eql_hash(other),
            .string => |self_str| self_str.eql_hash(other),
            else => unreachable,
        };
    }
};

pub const Integer = struct {
    value: i64,

    pub fn inspect(self: *const Integer, writer: AnyWriter) ObjectError!void {
        return try writer.print("{d}", .{self.value});
    }

    pub fn dupe(self: Integer, _: Allocator) Allocator.Error!Object {
        return Object{ .integer = self };
    }

    pub fn hash(self: *const Integer, hasher: *std.hash.Fnv1a_64) void {
        hasher.update(std.mem.asBytes(&Object.integer));
        hasher.update(std.mem.asBytes(&self.value));
    }

    pub fn eql_hash(self: Integer, other: Object) bool {
        return switch (other) {
            .integer => |int| self.value == int.value,
            else => false,
        };
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn inspect(self: *const Boolean, writer: AnyWriter) ObjectError!void {
        try writer.print("{any}", .{self.value});
    }

    pub fn dupe(self: Boolean, _: Allocator) Allocator.Error!Object {
        return Object{ .boolean = self };
    }

    pub fn hash(self: *const Boolean, hasher: *std.hash.Fnv1a_64) void {
        hasher.update(std.mem.asBytes(&@intFromEnum(Object.boolean)));
        hasher.update(std.mem.asBytes(&self.value));
    }

    pub fn eql_hash(self: Boolean, other: Object) bool {
        return switch (other) {
            .boolean => |b| self.value == b.value,
            else => false,
        };
    }
};

pub const Null = struct {
    pub fn inspect(_: *const Null, writer: AnyWriter) ObjectError!void {
        try writer.print("null", .{});
    }

    pub fn dupe(self: Null, _: Allocator) Allocator.Error!Object {
        return Object{ ._null = self };
    }
};

pub const ReturnValue = struct {
    value: ?*Object,

    pub fn inspect(self: *const ReturnValue, writer: AnyWriter) ObjectError!void {
        if (self.value) |value| try value.inspect(writer) else try writer.writeAll("null");
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

    pub fn inspect(self: *const Error, writer: AnyWriter) ObjectError!void {
        try writer.print("ERROR: {s}", .{self.message});
    }

    pub fn dupe(self: Error, alloc: Allocator) Allocator.Error!Object {
        return Object{ ._error = Error{ .message = try alloc.dupe(u8, self.message) } };
    }
};

pub const Function = struct {
    parameters: []const ast.Identifier,
    body: ast.BlockStatement,
    env: *Environment,

    pub fn inspect(self: *const Function, writer: AnyWriter) ObjectError!void {
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

    pub fn dupe(self: Function, alloc: Allocator) Allocator.Error!Object {
        var duped_parameters = std.ArrayListUnmanaged(ast.Identifier).empty;
        for (self.parameters) |param| {
            const duped_param = try param.dupe(alloc);
            try duped_parameters.append(alloc, duped_param);
        }
        const duped_body = try self.body.dupe(alloc);

        return Object{
            .function = Function{
                .parameters = try duped_parameters.toOwnedSlice(alloc),
                .body = duped_body,
                .env = self.env,
            },
        };
    }
};

pub const String = struct {
    value: []const u8,

    pub fn inspect(self: *const String, writer: AnyWriter) ObjectError!void {
        return try writer.print("{s}", .{self.value});
    }

    pub fn dupe(self: String, alloc: Allocator) Allocator.Error!Object {
        const duped_value = try alloc.dupe(u8, self.value);
        return Object{ .string = String{ .value = duped_value } };
    }

    pub fn hash(self: *const String, hasher: *std.hash.Fnv1a_64) void {
        hasher.update(std.mem.asBytes(&@intFromEnum(Object.string)));
        hasher.update(self.value);
    }

    pub fn eql_hash(self: String, other: Object) bool {
        return switch (other) {
            .string => |str| std.mem.eql(u8, self.value, str.value),
            else => false,
        };
    }
};

pub const Builtin = struct {
    _fn: builtins.BuiltinFnPointer,

    pub fn inspect(_: *const Builtin, writer: AnyWriter) ObjectError!void {
        return try writer.writeAll("builtin function");
    }

    pub fn dupe(self: Builtin, _: Allocator) Allocator.Error!Object {
        return Object{ .builtin = self };
    }
};

pub const Array = struct {
    elements: []Object,

    pub fn inspect(self: *const Array, writer: AnyWriter) ObjectError!void {
        try writer.writeByte('[');

        if (self.elements.len > 0) {
            try self.elements[0].inspect(writer);
            for (self.elements[1..]) |element| {
                try writer.writeAll(", ");
                try element.inspect(writer);
            }
        }

        try writer.writeByte(']');
    }

    pub fn dupe(self: Array, alloc: Allocator) Allocator.Error!Object {
        const duped_elements = try alloc.alloc(Object, self.elements.len);

        for (0..self.elements.len) |i| {
            duped_elements[i] = try self.elements[i].dupe(alloc);
        }

        return Object{ .array = Array{ .elements = duped_elements } };
    }
};

pub const Dictionary = struct {
    pairs: Object.HashMap(Object),

    pub fn inspect(self: *const Dictionary, writer: AnyWriter) ObjectError!void {
        try writer.writeByte('{');

        var iter = self.pairs.iterator();
        if (iter.next()) |entry| {
            try entry.key_ptr.inspect(writer);
            try writer.writeByte(':');
            try entry.value_ptr.inspect(writer);
        }

        while (iter.next()) |entry| {
            try writer.writeAll(", ");
            try entry.key_ptr.inspect(writer);
            try writer.writeByte(':');
            try entry.value_ptr.inspect(writer);
        }

        try writer.writeByte('}');
    }

    pub fn dupe(self: Dictionary, alloc: Allocator) Allocator.Error!Object {
        var duped_pairs = Object.HashMap(Object).empty;
        try duped_pairs.ensureTotalCapacity(alloc, self.pairs.capacity());

        var iter = self.pairs.iterator();
        while (iter.next()) |self_entry| {
            const duped_key = try self_entry.key_ptr.dupe(alloc);
            const duped_value = try self_entry.value_ptr.dupe(alloc);
            duped_pairs.putAssumeCapacity(duped_key, duped_value);
        }

        return Object{ .dictionary = Dictionary{ .pairs = duped_pairs } };
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

test "String hash" {
    const hello_1 = Object{ .string = String{ .value = "Hello World" } };
    const hello_2 = Object{ .string = String{ .value = "Hello World" } };

    const diff_1 = Object{ .string = String{ .value = "My name is johnny" } };
    const diff_2 = Object{ .string = String{ .value = "My name is johnny" } };

    try testing.expect(hello_1.hash() == hello_2.hash());
    try testing.expect(diff_1.hash() == diff_2.hash());
    try testing.expect(hello_1.hash() != diff_1.hash());
}

test "Boolean hash" {
    const true_1 = Object{ .boolean = Boolean{ .value = true } };
    const true_2 = Object{ .boolean = Boolean{ .value = true } };

    const false_1 = Object{ .boolean = Boolean{ .value = false } };
    const false_2 = Object{ .boolean = Boolean{ .value = false } };

    try testing.expect(true_1.hash() == true_2.hash());
    try testing.expect(false_1.hash() == false_2.hash());
    try testing.expect(true_1.hash() != false_1.hash());
}

test "Integer hash" {
    const one_1 = Object{ .integer = Integer{ .value = 1 } };
    const one_2 = Object{ .integer = Integer{ .value = 1 } };

    const two_1 = Object{ .integer = Integer{ .value = 2 } };
    const two_2 = Object{ .integer = Integer{ .value = 2 } };

    try testing.expect(one_1.hash() == one_2.hash());
    try testing.expect(two_1.hash() == two_2.hash());
    try testing.expect(one_1.hash() != two_1.hash());
}

test {
    std.testing.refAllDecls(@This());
}
