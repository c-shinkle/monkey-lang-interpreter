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

pub fn deinit(self: *Environment) void {
    var iter = self.store.iterator();
    while (iter.next()) |entry| {
        self.alloc.free(entry.key_ptr.*);
        entry.value_ptr.deinit(self.alloc);
    }
    self.store.deinit(self.alloc);
}

pub fn dupe(self: *const Environment) Allocator.Error!Environment {
    var duped_env = Environment{ .alloc = self.alloc, .store = .empty, .outer = self.outer };
    errdefer duped_env.deinit();

    var iter = self.store.iterator();
    while (iter.next()) |entry| {
        const duped_value = try entry.value_ptr.dupe(duped_env.alloc);
        errdefer duped_value.deinit(duped_env.alloc);
        try duped_env.set(entry.key_ptr.*, duped_value);
    }

    return duped_env;
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
    const key = try self.alloc.dupe(u8, name);
    errdefer self.alloc.free(key);
    const duped_value = try value.dupe(self.alloc);
    errdefer duped_value.deinit(self.alloc);

    try self.store.put(self.alloc, key, duped_value);
}

pub fn isRootEnvironment(self: *const Environment) bool {
    return self.outer == null;
}

test {
    std.testing.refAllDecls(Environment);
}
