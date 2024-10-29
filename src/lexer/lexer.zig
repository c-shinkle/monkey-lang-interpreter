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
        self.skipWhitespace();

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
            else => blk: {
                if (isLetter(ch.*)) {
                    const literal = self.readIdentifier();
                    const _type = token.lookupIdent(literal);
                    return token.Token{
                        ._type = _type,
                        .literal = literal,
                    };
                } else if (std.ascii.isDigit(ch.*)) {
                    return token.Token{
                        ._type = token.INT,
                        .literal = self.readNumber(),
                    };
                } else {
                    break :blk newToken(token.ILLEGAL, ch);
                }
            },
        };
        self.readChar();
        return tok;
    }

    fn skipWhitespace(self: *Lexer) void {
        while (std.ascii.isWhitespace(self.ch.*)) {
            self.readChar();
        }
    }

    fn newToken(token_type: token.TokenType, ch: *const u8) token.Token {
        return token.Token{
            ._type = token_type,
            .literal = ch[0..1],
        };
    }

    fn readIdentifier(self: *Lexer) []const u8 {
        const start = self.position;
        while (isLetter(self.ch.*)) {
            self.readChar();
        }
        return self.input[start..self.position];
    }

    fn isLetter(ch: u8) bool {
        return std.ascii.isAlphabetic(ch) or ch == '_';
    }

    fn readNumber(self: *Lexer) []const u8 {
        const start = self.position;
        while (std.ascii.isDigit(self.ch.*)) {
            self.readChar();
        }
        return self.input[start..self.position];
    }
};

test "Test Next Token" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\    x + y;
        \\};
        \\
        \\let result = add(five, ten);
    ;

    const expecteds = [_]struct {
        expected_type: token.TokenType,
        expected_literal: []const u8,
    }{
        .{
            .expected_type = token.LET,
            .expected_literal = "let",
        },
        .{
            .expected_type = token.IDENT,
            .expected_literal = "five",
        },
        .{
            .expected_type = token.ASSIGN,
            .expected_literal = "=",
        },
        .{
            .expected_type = token.INT,
            .expected_literal = "5",
        },
        .{
            .expected_type = token.SEMICOLON,
            .expected_literal = ";",
        },
        .{
            .expected_type = token.LET,
            .expected_literal = "let",
        },
        .{
            .expected_type = token.IDENT,
            .expected_literal = "ten",
        },
        .{
            .expected_type = token.ASSIGN,
            .expected_literal = "=",
        },
        .{
            .expected_type = token.INT,
            .expected_literal = "10",
        },
        .{
            .expected_type = token.SEMICOLON,
            .expected_literal = ";",
        },
        .{
            .expected_type = token.LET,
            .expected_literal = "let",
        },
        .{
            .expected_type = token.IDENT,
            .expected_literal = "add",
        },
        .{
            .expected_type = token.ASSIGN,
            .expected_literal = "=",
        },
        .{
            .expected_type = token.FUNCTION,
            .expected_literal = "fn",
        },
        .{
            .expected_type = token.LPAREN,
            .expected_literal = "(",
        },
        .{
            .expected_type = token.IDENT,
            .expected_literal = "x",
        },
        .{
            .expected_type = token.COMMA,
            .expected_literal = ",",
        },
        .{
            .expected_type = token.IDENT,
            .expected_literal = "y",
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
            .expected_type = token.IDENT,
            .expected_literal = "x",
        },
        .{
            .expected_type = token.PLUS,
            .expected_literal = "+",
        },
        .{
            .expected_type = token.IDENT,
            .expected_literal = "y",
        },
        .{
            .expected_type = token.SEMICOLON,
            .expected_literal = ";",
        },
        .{
            .expected_type = token.RBRACE,
            .expected_literal = "}",
        },
        .{
            .expected_type = token.SEMICOLON,
            .expected_literal = ";",
        },
        .{
            .expected_type = token.LET,
            .expected_literal = "let",
        },
        .{
            .expected_type = token.IDENT,
            .expected_literal = "result",
        },
        .{
            .expected_type = token.ASSIGN,
            .expected_literal = "=",
        },
        .{
            .expected_type = token.IDENT,
            .expected_literal = "add",
        },
        .{
            .expected_type = token.LPAREN,
            .expected_literal = "(",
        },
        .{
            .expected_type = token.IDENT,
            .expected_literal = "five",
        },
        .{
            .expected_type = token.COMMA,
            .expected_literal = ",",
        },
        .{
            .expected_type = token.IDENT,
            .expected_literal = "ten",
        },
        .{
            .expected_type = token.RPAREN,
            .expected_literal = ")",
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
