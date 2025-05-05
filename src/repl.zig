const std = @import("std");
const AnyWriter = std.io.AnyWriter;

const ast = @import("ast.zig");
const Environment = @import("Environment.zig");
const evaluator = @import("evaluator.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

const c_imports = @cImport(@cInclude("editline/readline.h"));

pub fn start(stdout: AnyWriter) !void {
    var stdout_buffer = std.io.BufferedWriter(4096, AnyWriter){ .unbuffered_writer = stdout };
    const buffer_writer = stdout_buffer.writer().any();

    var env_allocator = std.heap.ArenaAllocator.init(std.heap.smp_allocator);
    defer env_allocator.deinit();
    const env_arena = env_allocator.allocator();
    var env = Environment.init(env_arena);

    while (true) : (try stdout_buffer.flush()) {
        const raw_input = c_imports.readline(">> ") orelse return;
        // defer c_imports.rl_free(raw_input);
        const slice_input = std.mem.span(raw_input);
        if (std.mem.eql(u8, slice_input, ".exit")) return;
        if (slice_input.len == 0) continue;
        _ = c_imports.add_history(raw_input);

        var loop_allocator = std.heap.ArenaAllocator.init(std.heap.smp_allocator);
        defer loop_allocator.deinit();
        const loop_arena = loop_allocator.allocator();

        var lexer = Lexer.init(slice_input);
        var parser = try Parser.init(&lexer, loop_arena);
        const program = try parser.parseProgram(loop_arena);
        if (program.statements.len > 0) {
            const parent_node = ast.Node{ .program = program };
            if (try evaluator.eval(loop_arena, parent_node, &env)) |evaluated| {
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
