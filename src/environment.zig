const std = @import("std");
const Allocator = std.mem.Allocator;

const obj = @import("object.zig");

pub const Environment = struct {
    alloc: Allocator,
    store: std.StringHashMapUnmanaged(obj.Object),

    pub fn init(alloc: Allocator) Environment {
        return Environment{ .alloc = alloc, .store = .empty };
    }

    pub fn deinit(self: *Environment) void {
        var iter = self.store.iterator();
        while (iter.next()) |entry| {
            self.alloc.free(entry.key_ptr.*);
            entry.value_ptr.deinit(self.alloc);
        }
        self.store.deinit(self.alloc);
    }

    pub fn get(self: *const Environment, name: []const u8) ?obj.Object {
        return self.store.get(name);
    }

    pub fn set(self: *Environment, name: []const u8, value: obj.Object) !obj.Object {
        const key = try self.alloc.dupe(u8, name);
        errdefer self.alloc.free(key);

        const duped_value = try value.dupe(self.alloc);
        errdefer duped_value.deinit(self.alloc);

        try self.store.put(self.alloc, key, duped_value);
        return value;
    }
};
