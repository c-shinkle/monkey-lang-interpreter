const std = @import("std");

const AnyWriter = std.io.AnyWriter;

const ast = @import("ast.zig");
const evaluator = @import("evaluator.zig");
const Lexer = @import("lexer.zig").Lexer;
const object = @import("object.zig");
const Parser = @import("parser.zig").Parser;
const token = @import("token.zig");
const Environment = @import("environment.zig").Environment;

const c_imports = @cImport({
    @cInclude("stdio.h");
    @cInclude("readline/readline.h");
    @cInclude("readline/history.h");
});

pub fn start(stdout: AnyWriter) !void {
    var stdout_buffer = std.io.BufferedWriter(4096, AnyWriter){ .unbuffered_writer = stdout };
    const buffer_writer = stdout_buffer.writer().any();

    var env_alloc = std.heap.ArenaAllocator.init(std.heap.c_allocator);
    defer env_alloc.deinit();
    var env = Environment.init(env_alloc.allocator());

    while (true) : (try stdout_buffer.flush()) {
        const raw_input = c_imports.readline(">> ") orelse return;
        defer c_imports.rl_free(raw_input);
        const slice_input = std.mem.span(raw_input);
        if (std.mem.eql(u8, slice_input, "exit")) return;
        if (slice_input.len == 0) continue;
        c_imports.add_history(raw_input);

        var loop_arena = std.heap.ArenaAllocator.init(std.heap.c_allocator);
        defer loop_arena.deinit();
        const loop_alloc = loop_arena.allocator();

        var lexer = Lexer.init(slice_input);
        var parser = try Parser.init(&lexer, loop_alloc);
        const program = try parser.parseProgram(loop_alloc);
        if (program.statements.len > 0) {
            const parent_node = ast.Node{ .program = program };
            if (try evaluator.eval(loop_alloc, parent_node, &env)) |evaluated| {
                try evaluated.inspect(buffer_writer);
                try buffer_writer.writeByte('\n');
            }
        } else {
            for (parser.errors.items) |err| {
                try buffer_writer.print("\t{s}\n", .{err});
            }
        }
    }
}
