const std = @import("std");

const repl = @import("repl");

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var buffered_writer = std.io.bufferedWriter(stdout_file);
    const stdout = buffered_writer.writer();

    try stdout.print("Hello! This is the Monkey Programming Language!\n", .{});
    try stdout.print("Feel free to type in commands!\n", .{});
    try buffered_writer.flush();

    try repl.start(@TypeOf(buffered_writer), &buffered_writer);

    try stdout.print("Bye bye!\n", .{});
    try buffered_writer.flush();
}
