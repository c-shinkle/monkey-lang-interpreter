const std = @import("std");

const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const token = @import("token.zig");

pub fn start(BufferWriter: type, buffered_writer: *BufferWriter) !void {
    const stdout = buffered_writer.writer();

    var reader = std.io.getStdIn().reader();

    var stdout_buffer: [1024]u8 = undefined;
    var fixed_buffer_stream: std.io.FixedBufferStream([]u8) = std.io.fixedBufferStream(&stdout_buffer);
    const fbs_writer = fixed_buffer_stream.writer();

    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = general_purpose_allocator.allocator();

    while (true) : (fixed_buffer_stream.reset()) {
        try stdout.print(">> ", .{});
        try buffered_writer.flush();

        try reader.streamUntilDelimiter(fbs_writer, '\n', 1024);
        const input = fixed_buffer_stream.getWritten();
        if (std.mem.eql(u8, input, "exit")) {
            return;
        }

        var lexer = Lexer.init(input);
        var parser = try Parser.init(&lexer, allocator);
        defer parser.deinit();
        const program = try parser.parseProgram();
        defer program.deinit();

        if (program.statements.len > 0) {
            try program.string(&stdout);
            try stdout.print("\n", .{});
            try buffered_writer.flush();
        } else {
            for (parser.errors.items) |err| {
                try stdout.print("\t{s}\n", .{err});
            }
        }
    }
}
