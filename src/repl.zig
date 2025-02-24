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

    var debug_allocator = std.heap.DebugAllocator(.{}){};
    const allocator = debug_allocator.allocator();

    var stdout_buffer = std.io.BufferedWriter(size, AnyWriter){ .unbuffered_writer = stdout };
    const buffer_writer = stdout_buffer.writer().any();

    var env = Environment.init();
    defer env.deinit(allocator);

    try stdout.print(">> ", .{});
    while (true) : ({
        try buffer_writer.print(">> ", .{});
        try stdout_buffer.flush();
        stream.reset();
    }) {
        try stdin_reader.streamUntilDelimiter(stream.writer(), '\n', size);
        if (std.mem.eql(u8, stream.getWritten(), "exit")) {
            return;
        }

        var lexer = Lexer.init(stream.getWritten());
        var parser = try Parser.init(&lexer, allocator);
        defer parser.deinit();
        const program = try parser.parseProgram();
        defer program.deinit();

        if (program.statements.len > 0) {
            if (try evaluator.eval(allocator, ast.Node{ .program = program }, &env)) |evaluated| {
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
