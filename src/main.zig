const std = @import("std");

const repl = @import("repl.zig");
const ast = @import("ast.zig");
const Environment = @import("Environment.zig");
const evaluator = @import("evaluator.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

pub fn main() !void {
    var arena_allocator = std.heap.ArenaAllocator.init(std.heap.smp_allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    const args = try std.process.argsAlloc(arena);
    if (args.len > 1) {
        try repl.processFile(arena, args[1]);
    } else {
        arena_allocator.deinit();

        var stdout = std.io.getStdOut().writer();
        try stdout.print("Hello! This is the Monkey Programming Language!\n", .{});
        try stdout.print("Feel free to type in commands!\n", .{});

        try repl.start();
        try stdout.print("Bye bye!\n", .{});
    }
}
