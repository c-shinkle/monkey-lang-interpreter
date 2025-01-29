const std = @import("std");

const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const token = @import("token.zig");

pub fn start(writer: std.fs.File.Writer, reader: std.fs.File.Reader) !void {
    var bw = std.io.bufferedWriter(writer);
    const stdout = bw.writer();

    var stdout_buffer: [1024]u8 = undefined;
    var fbs: std.io.FixedBufferStream([]u8) = std.io.fixedBufferStream(&stdout_buffer);
    const fbs_writer = fbs.writer();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    while (true) : ({
        fbs.reset();
    }) {
        try stdout.print(">> ", .{});
        try bw.flush();

        try reader.streamUntilDelimiter(fbs_writer, '\n', 1024);
        const input = fbs.getWritten();
        if (std.mem.eql(u8, input, "exit")) {
            return;
        }

        var lexer = Lexer.init(input);
        var parser = try Parser.init(&lexer, allocator);
        defer parser.deinit();
        const program = try parser.parseProgram();
        defer program.deinit();

        if (program.statements.len == 0) {
            for (parser.errors.items) |_error| {
                try stdout.print("\t{s}\n", .{_error});
            }
            continue;
        }

        try program.string(&stdout);
        try stdout.print("\n", .{});
    }
}
