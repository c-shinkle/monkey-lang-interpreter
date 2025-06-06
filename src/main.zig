const std = @import("std");
const repl = @import("repl.zig");
const build_config = @import("build_config");

const readline = @cImport({
    @cInclude("stdio.h");
    @cInclude("readline/readline.h");
    @cInclude("readline/history.h");
});
const editline = @cImport(@cInclude("editline/readline.h"));

pub fn main() !void {
    const args = try std.process.argsAlloc(std.heap.smp_allocator);
    defer std.process.argsFree(std.heap.smp_allocator, args);
    if (args.len > 1) {
        try repl.fileInterpreter(args[1]);
    } else if (build_config.enable_readline) {
        try repl.libraryRepl(readline);
    } else if (build_config.enable_editline) {
        try repl.libraryRepl(editline);
    } else {
        try repl.stdInRepl();
    }
}
