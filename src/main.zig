const std = @import("std");
const repl = @import("repl");

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Hello! This is the Monkey Programming Language!\n", .{});
    try stdout.print("Feel free to type in commands!\n", .{});

    try repl.start(stdout_file, std.io.getStdIn().reader());
}
