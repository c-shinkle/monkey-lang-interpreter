const std = @import("std");
const repl = @import("repl.zig");
const build_config = @import("build_config");

pub fn main() !void {
    const args = try std.process.argsAlloc(std.heap.smp_allocator);
    defer std.process.argsFree(std.heap.smp_allocator, args);
    if (args.len > 1) {
        try repl.replFile(args[1]);
    } else if (build_config.enable_readline) {
        try repl.replReadline();
    } else {
        try repl.replStdIn();
    }
}
