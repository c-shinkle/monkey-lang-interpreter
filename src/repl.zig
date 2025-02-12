const std = @import("std");

const ast = @import("ast.zig");
const evaluator = @import("evaluator.zig");
const Lexer = @import("lexer.zig").Lexer;
const object = @import("object.zig");
const Parser = @import("parser.zig").Parser;
const token = @import("token.zig");

pub fn start(stdout: std.io.AnyWriter) !void {
    const buffers_size = 4096;
    var stdin_reader = std.io.getStdIn().reader();

    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = general_purpose_allocator.allocator();

    var stdout_buffer = std.io.bufferedWriter(stdout);
    var buffer_writer = stdout_buffer.writer().any();

    try stdout.print(">> ", .{});
    while (true) : ({
        try buffer_writer.print("\n>> ", .{});
        try stdout_buffer.flush();
    }) {
        var stream: [buffers_size]u8 = undefined;
        const input = try stdin_reader.readUntilDelimiter(&stream, '\n');
        if (std.mem.eql(u8, input, "exit")) {
            return;
        }

        var lexer = Lexer.init(input);
        var parser = try Parser.init(&lexer, allocator);
        defer parser.deinit();
        const program = try parser.parseProgram();
        defer program.deinit();

        if (program.statements.len > 0) {
            const evaluated = evaluator.eval(ast.Node{ .program = program }).?;
            try evaluated.inspect(&buffer_writer);
        } else {
            for (parser.errors.items) |err| {
                try buffer_writer.print("\t{s}\n", .{err});
            }
        }
    }
}
