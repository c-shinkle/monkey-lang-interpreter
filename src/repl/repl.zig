const std = @import("std");
const lexer = @import("lexer");
const token = @import("token");

pub fn start(stdout_file: std.fs.File.Writer, reader: std.fs.File.Reader) !void {
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();
    var buffer: [1024]u8 = undefined;

    while (true) {
        try stdout.print(">> ", .{});
        try bw.flush();

        const input = try reader.readUntilDelimiter(&buffer, '\n');
        var l = lexer.Lexer.init(input);
        while (l.nextToken()) |tok| {
            try stdout.print("Type = \"{s}\" Literal = \"{s}\"\n", .{ tok._type, tok.literal });
        }
    }
}
