const std = @import("std");
const Allocator = std.mem.Allocator;

const obj = @import("object.zig");

const Environment = @This();

alloc: Allocator,
store: std.StringHashMapUnmanaged(obj.Object),
outer: ?*const Environment,

pub fn init(alloc: Allocator) Environment {
    return Environment{ .alloc = alloc, .store = .empty, .outer = null };
}

pub fn get(self: *const Environment, name: []const u8) ?obj.Object {
    if (self.store.get(name)) |some| {
        return some;
    } else if (self.outer) |outer| {
        return outer.get(name);
    }
    return null;
}

pub fn set(self: *Environment, name: []const u8, value: obj.Object) Allocator.Error!void {
    const duped_name = try self.alloc.dupe(u8, name);
    errdefer self.alloc.free(duped_name);
    const duped_value = try value.dupe(self.alloc);
    errdefer duped_value.deinit(self.alloc);

    try self.store.put(self.alloc, duped_name, duped_value);
}

pub fn isRootEnvironment(self: *const Environment) bool {
    return self.outer == null;
}

pub fn print(self: *const Environment) void {
    if (self.outer) |outer| {
        outer.print();
    }
    std.debug.print("{*}\n", .{self});
    var iter = self.store.keyIterator();
    while (iter.next()) |key| {
        std.debug.print("{s}\n", .{key.*});
    }
}

test {
    std.testing.refAllDecls(Environment);
}
