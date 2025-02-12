const std = @import("std");

const repl = @import("repl.zig");

pub fn main() !void {
    var stdout_file = std.io.getStdOut().writer().any();

    try stdout_file.print("Hello! This is the Monkey Programming Language!\n", .{});
    try stdout_file.print("Feel free to type in commands!\n", .{});

    try repl.start(stdout_file);
    try stdout_file.print("Bye bye!\n", .{});
}
