const std = @import("std");
const token = @import("token");

const testing = std.testing;

pub const Lexer = struct {
    input: []const u8,
    position: usize,
    read_position: usize,
    ch: *const u8,

    fn init(input: []const u8) Lexer {
        var l = Lexer{
            .input = input,
            .position = 0,
            .read_position = 0,
            .ch = &0,
        };
        l.readChar();
        return l;
    }

    fn readChar(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.ch = &0;
        } else {
            self.ch = &self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn nextToken(self: *Lexer) token.Token {
        const ch = self.ch;
        const tok: token.Token = switch (ch.*) {
            '=' => newToken(token.ASSIGN, ch),
            ';' => newToken(token.SEMICOLON, ch),
            '(' => newToken(token.LPAREN, ch),
            ')' => newToken(token.RPAREN, ch),
            ',' => newToken(token.COMMA, ch),
            '+' => newToken(token.PLUS, ch),
            '{' => newToken(token.LBRACE, ch),
            '}' => newToken(token.RBRACE, ch),
            0 => token.Token{ ._type = token.EOF, .literal = "" },
            else => unreachable,
        };
        self.readChar();
        return tok;
    }

    fn newToken(token_type: token.TokenType, ch: *const u8) token.Token {
        return token.Token{
            ._type = token_type,
            .literal = ch[0..1],
        };
    }
};

test "Test Next Token" {
    const input = "=+(){},;";

    const expecteds = [_]struct {
        expected_type: token.TokenType,
        expected_literal: []const u8,
    }{
        .{
            .expected_type = token.ASSIGN,
            .expected_literal = "=",
        },
        .{
            .expected_type = token.PLUS,
            .expected_literal = "+",
        },
        .{
            .expected_type = token.LPAREN,
            .expected_literal = "(",
        },
        .{
            .expected_type = token.RPAREN,
            .expected_literal = ")",
        },
        .{
            .expected_type = token.LBRACE,
            .expected_literal = "{",
        },
        .{
            .expected_type = token.RBRACE,
            .expected_literal = "}",
        },
        .{
            .expected_type = token.COMMA,
            .expected_literal = ",",
        },
        .{
            .expected_type = token.SEMICOLON,
            .expected_literal = ";",
        },
        .{
            .expected_type = token.EOF,
            .expected_literal = "",
        },
    };

    var l = Lexer.init(input);

    for (expecteds) |expected| {
        const tok = l.nextToken();

        try testing.expectEqualStrings(expected.expected_type, tok._type);
        try testing.expectEqualStrings(expected.expected_literal, tok.literal);
    }
}
