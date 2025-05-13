const std = @import("std");

const ast = @import("ast.zig");
const Environment = @import("Environment.zig");
const evaluator = @import("evaluator.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

const c_imports = @cImport(@cInclude("editline/readline.h"));

pub fn start() !void {
    var stdout_buffer = std.io.bufferedWriter(std.io.getStdOut().writer());
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

pub fn processFile(arena: std.mem.Allocator, relative_path: []const u8) !void {
    const file = std.fs.cwd().openFile(relative_path, std.fs.File.OpenFlags{}) catch |e| {
        if (e == error.FileNotFound) {
            std.debug.print("File not found: {s}\n", .{relative_path});
        }
        return e;
    };
    const read_bytes = try file.readToEndAlloc(arena, 1024);

    var lexer = Lexer.init(read_bytes);
    var parser = try Parser.init(&lexer, arena);
    const program = try parser.parseProgram(arena);

    var stdout_buffer = std.io.bufferedWriter(std.io.getStdOut().writer());
    defer stdout_buffer.flush() catch std.debug.print("Failed to flush stdout_buffer!\n", .{});
    const buffer_writer = stdout_buffer.writer().any();

    if (program.statements.len > 0) {
        const parent_node = ast.Node{ .program = program };
        var env = Environment.init(arena);
        if (try evaluator.eval(arena, parent_node, &env)) |evaluated| {
            try evaluated.inspect(buffer_writer);
            try buffer_writer.writeByte('\n');
        }
    } else {
        for (parser.errors.items) |err| {
            try buffer_writer.print("\t{s}\n", .{err});
        }
    }
}
