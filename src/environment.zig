const std = @import("std");
const Allocator = std.mem.Allocator;

const obj = @import("object.zig");

const Value = obj.Object;

pub const Environment = struct {
    store: std.StringHashMapUnmanaged(Value),

    pub fn init() Environment {
        const store: std.StringHashMapUnmanaged(Value) = std.StringHashMapUnmanaged(Value).empty;
        return Environment{ .store = store };
    }

    pub fn deinit(self: *Environment, allocator: Allocator) void {
        var iter = self.store.iterator();
        while (iter.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            entry.value_ptr.deinit(allocator);
        }
        self.store.deinit(allocator);
    }

    pub fn get(self: *const Environment, name: []const u8) ?Value {
        return self.store.get(name);
    }

    pub fn set(self: *Environment, allocator: Allocator, name: []const u8, val: Value) !Value {
        const key = try allocator.dupe(u8, name);
        errdefer allocator.free(key);
        try self.store.put(allocator, key, val);
        return val;
    }
};
