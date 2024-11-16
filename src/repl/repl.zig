const std = @import("std");
const token = @import("token");
const lexer = @import("lexer");

pub fn start(writer: std.fs.File.Writer, reader: std.fs.File.Reader) !void {
    var bw = std.io.bufferedWriter(writer);
    const stdout = bw.writer();

    var buffer: [1024]u8 = undefined;
    var fbs: std.io.FixedBufferStream([]u8) = std.io.fixedBufferStream(&buffer);
    const fbs_writer = fbs.writer();

    while (true) {
        try stdout.print(">> ", .{});
        try bw.flush();

        try reader.streamUntilDelimiter(fbs_writer, '\n', 1024);
        const input = fbs.getWritten();
        if (std.mem.eql(u8, input, "exit")) {
            return;
        }

        var l = lexer.Lexer.init(input);
        var tok = l.nextToken();
        while (!std.mem.eql(u8, tok._type, token.EOF)) : (tok = l.nextToken()) {
            try stdout.print("Type = \"{s}\" Literal = \"{s}\"\n", .{ tok._type, tok.literal });
        }
        fbs.reset();
    }
}
