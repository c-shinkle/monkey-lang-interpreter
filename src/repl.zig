const std = @import("std");

const ast = @import("ast.zig");
const Environment = @import("Environment.zig");
const evaluator = @import("evaluator.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

const c_imports = @cImport({
    @cInclude("stdio.h");
    @cInclude("readline/readline.h");
    @cInclude("readline/history.h");
});

pub fn stdInRepl() !void {
    var stdin_reader = std.io.getStdIn().reader();

    const size: usize = 4096;
    var stream_buffer: [size]u8 = undefined;
    var stream = std.io.fixedBufferStream(&stream_buffer);

    var stdout_buffer = std.io.bufferedWriter(std.io.getStdOut().writer());
    const buffer_writer = stdout_buffer.writer();
    defer {
        buffer_writer.print("Bye bye!\n", .{}) catch {};
        stdout_buffer.flush() catch {};
    }
    try buffer_writer.print("Hello! This is the Monkey Programming Language!\n", .{});
    try buffer_writer.print("Feel free to type in commands!\n", .{});
    try stdout_buffer.flush();

    var env_alloc = std.heap.ArenaAllocator.init(std.heap.smp_allocator);
    defer env_alloc.deinit();
    var env = Environment.init(env_alloc.allocator());

    try buffer_writer.print(">> ", .{});
    try stdout_buffer.flush();
    while (true) : ({
        try buffer_writer.print(">> ", .{});
        try stdout_buffer.flush();
        stream.reset();
    }) {
        stdin_reader.streamUntilDelimiter(stream.writer(), '\n', size) catch |e| return switch (e) {
            error.EndOfStream => try buffer_writer.writeByte('\n'),
            else => e,
        };
        if (std.mem.eql(u8, stream.getWritten(), ".exit")) {
            return;
        }

        var arena = std.heap.ArenaAllocator.init(std.heap.smp_allocator);
        defer arena.deinit();
        var lexer = Lexer.init(stream.getWritten());
        var parser = try Parser.init(&lexer, arena.allocator());
        const program = try parser.parseProgram(arena.allocator());
        if (program.statements.len > 0) {
            const parent_node = ast.Node{ .program = program };
            if (try evaluator.eval(arena.allocator(), parent_node, &env)) |evaluated| {
                try evaluated.inspect(buffer_writer.any());
                try buffer_writer.writeByte('\n');
            }
        } else {
            for (parser.errors.items) |err| {
                try buffer_writer.print("\t{s}\n", .{err});
            }
        }
    }
}

pub fn readlineRepl() !void {
    var stdout_buffer = std.io.bufferedWriter(std.io.getStdOut().writer());
    const buffer_writer = stdout_buffer.writer().any();
    defer {
        buffer_writer.print("Bye bye!\n", .{}) catch {};
        stdout_buffer.flush() catch {};
    }

    try buffer_writer.print("Hello! This is the Monkey Programming Language!\n", .{});
    try buffer_writer.print("Feel free to type in commands!\n", .{});
    try stdout_buffer.flush();

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

pub fn fileInterpreter(relative_path: []const u8) !void {
    const file = std.fs.cwd().openFile(relative_path, std.fs.File.OpenFlags{}) catch |e| {
        if (e == error.FileNotFound) {
            std.debug.print("File not found: {s}\n", .{relative_path});
        }
        return e;
    };
    defer file.close();

    var arena_allocator = std.heap.ArenaAllocator.init(std.heap.smp_allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    const read_bytes = try file.readToEndAlloc(arena, std.math.maxInt(usize));

    var lexer = Lexer.init(read_bytes);
    var parser = try Parser.init(&lexer, arena);
    const program = try parser.parseProgram(arena);

    var stdout_buffer = std.io.bufferedWriter(std.io.getStdOut().writer());
    defer stdout_buffer.flush() catch {};
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
