const std = @import("std");

const AnyWriter = std.io.AnyWriter;

const ast = @import("ast.zig");
const evaluator = @import("evaluator.zig");
const Lexer = @import("lexer.zig").Lexer;
const object = @import("object.zig");
const Parser = @import("parser.zig").Parser;
const token = @import("token.zig");
const Environment = @import("environment.zig").Environment;

pub fn start(stdout: AnyWriter) !void {
    var stdin_reader = std.io.getStdIn().reader();

    const size: usize = 4096;
    var stream_buffer: [size]u8 = undefined;
    var stream = std.io.fixedBufferStream(&stream_buffer);

    var stdout_buffer = std.io.BufferedWriter(size, AnyWriter){ .unbuffered_writer = stdout };
    const buffer_writer = stdout_buffer.writer().any();

    var global_env_alloc = std.heap.ArenaAllocator.init(std.heap.smp_allocator);
    defer global_env_alloc.deinit();
    var env = Environment.init(global_env_alloc.allocator());

    try stdout.print(">> ", .{});
    while (true) : ({
        try buffer_writer.print(">> ", .{});
        try stdout_buffer.flush();
        stream.reset();
    }) {
        stdin_reader.streamUntilDelimiter(stream.writer(), '\n', size) catch |e| return switch (e) {
            error.EndOfStream => try stdout.writeByte('\n'),
            else => e,
        };
        if (std.mem.eql(u8, stream.getWritten(), "exit")) {
            return;
        }
        var lexer = Lexer.init(stream.getWritten());

        var arena = std.heap.ArenaAllocator.init(std.heap.smp_allocator);
        defer arena.deinit();

        var parser = try Parser.init(&lexer, arena.allocator());
        const program = try parser.parseProgram(arena.allocator());
        if (program.statements.len > 0) {
            const parent_node = ast.Node{ .program = program };
            if (try evaluator.eval(arena.allocator(), parent_node, &env)) |evaluated| {
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
