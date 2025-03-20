const std = @import("std");
const testing = std.testing;

const token = @import("token.zig");

const Token = token.Token;
const TokenType = token.TokenType;

pub const Lexer = struct {
    input: []const u8,
    position: usize,
    read_position: usize,
    ch: *const u8,

    pub fn init(input: []const u8) Lexer {
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

    pub fn nextToken(self: *Lexer) Token {
        self.skipWhitespace();

        const ch = self.ch;
        const tok: Token = switch (ch.*) {
            0 => Token{ ._type = TokenType.eof, .literal = "" },
            '=' => blk: {
                if (self.peekChar() == '=') {
                    const start = self.position;
                    self.readChar();
                    break :blk Token{
                        ._type = TokenType.eq,
                        .literal = self.input[start .. start + 2],
                    };
                }
                break :blk newToken(TokenType.assign, ch);
            },
            '+' => newToken(TokenType.plus, ch),
            '-' => newToken(TokenType.minus, ch),
            '!' => blk: {
                if (self.peekChar() == '=') {
                    const start = self.position;
                    self.readChar();
                    break :blk Token{
                        ._type = TokenType.not_eq,
                        .literal = self.input[start .. start + 2],
                    };
                }
                break :blk newToken(TokenType.bang, ch);
            },
            '/' => newToken(TokenType.slash, ch),
            '*' => newToken(TokenType.asterisk, ch),
            '<' => newToken(TokenType.lt, ch),
            '>' => newToken(TokenType.gt, ch),
            ';' => newToken(TokenType.semicolon, ch),
            ',' => newToken(TokenType.comma, ch),
            '(' => newToken(TokenType.lparen, ch),
            ')' => newToken(TokenType.rparen, ch),
            '{' => newToken(TokenType.lbrace, ch),
            '}' => newToken(TokenType.rbrace, ch),
            else => blk: {
                if (isLetter(ch.*)) {
                    const literal = self.readIdentifier();
                    const _type = token.lookupIdent(literal);
                    return Token{
                        ._type = _type,
                        .literal = literal,
                    };
                } else if (std.ascii.isDigit(ch.*)) {
                    return Token{
                        ._type = TokenType.int,
                        .literal = self.readNumber(),
                    };
                } else {
                    break :blk newToken(TokenType.illegal, ch);
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

    fn newToken(token_type: TokenType, ch: *const u8) Token {
        return Token{ ._type = token_type, .literal = ch[0..1] };
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

    fn peekChar(self: *const Lexer) u8 {
        if (self.read_position >= self.input.len) {
            return 0;
        } else {
            return self.input[self.read_position];
        }
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
        \\!-/*5;
        \\5 < 10 > 5;
        \\
        \\if (5 < 10) {
        \\  return true;
        \\} else {
        \\  return false;
        \\}
        \\
        \\10 == 10
        \\10 != 9
    ;

    const expecteds = [_]struct {
        expected_type: TokenType,
        expected_literal: []const u8,
    }{
        .{
            .expected_type = TokenType.let,
            .expected_literal = "let",
        },
        .{
            .expected_type = TokenType.identifier,
            .expected_literal = "five",
        },
        .{
            .expected_type = TokenType.assign,
            .expected_literal = "=",
        },
        .{
            .expected_type = TokenType.int,
            .expected_literal = "5",
        },
        .{
            .expected_type = TokenType.semicolon,
            .expected_literal = ";",
        },
        .{
            .expected_type = TokenType.let,
            .expected_literal = "let",
        },
        .{
            .expected_type = TokenType.identifier,
            .expected_literal = "ten",
        },
        .{
            .expected_type = TokenType.assign,
            .expected_literal = "=",
        },
        .{
            .expected_type = TokenType.int,
            .expected_literal = "10",
        },
        .{
            .expected_type = TokenType.semicolon,
            .expected_literal = ";",
        },
        .{
            .expected_type = TokenType.let,
            .expected_literal = "let",
        },
        .{
            .expected_type = TokenType.identifier,
            .expected_literal = "add",
        },
        .{
            .expected_type = TokenType.assign,
            .expected_literal = "=",
        },
        .{
            .expected_type = TokenType._function,
            .expected_literal = "fn",
        },
        .{
            .expected_type = TokenType.lparen,
            .expected_literal = "(",
        },
        .{
            .expected_type = TokenType.identifier,
            .expected_literal = "x",
        },
        .{
            .expected_type = TokenType.comma,
            .expected_literal = ",",
        },
        .{
            .expected_type = TokenType.identifier,
            .expected_literal = "y",
        },
        .{
            .expected_type = TokenType.rparen,
            .expected_literal = ")",
        },
        .{
            .expected_type = TokenType.lbrace,
            .expected_literal = "{",
        },
        .{
            .expected_type = TokenType.identifier,
            .expected_literal = "x",
        },
        .{
            .expected_type = TokenType.plus,
            .expected_literal = "+",
        },
        .{
            .expected_type = TokenType.identifier,
            .expected_literal = "y",
        },
        .{
            .expected_type = TokenType.semicolon,
            .expected_literal = ";",
        },
        .{
            .expected_type = TokenType.rbrace,
            .expected_literal = "}",
        },
        .{
            .expected_type = TokenType.semicolon,
            .expected_literal = ";",
        },
        .{
            .expected_type = TokenType.let,
            .expected_literal = "let",
        },
        .{
            .expected_type = TokenType.identifier,
            .expected_literal = "result",
        },
        .{
            .expected_type = TokenType.assign,
            .expected_literal = "=",
        },
        .{
            .expected_type = TokenType.identifier,
            .expected_literal = "add",
        },
        .{
            .expected_type = TokenType.lparen,
            .expected_literal = "(",
        },
        .{
            .expected_type = TokenType.identifier,
            .expected_literal = "five",
        },
        .{
            .expected_type = TokenType.comma,
            .expected_literal = ",",
        },
        .{
            .expected_type = TokenType.identifier,
            .expected_literal = "ten",
        },
        .{
            .expected_type = TokenType.rparen,
            .expected_literal = ")",
        },
        .{
            .expected_type = TokenType.semicolon,
            .expected_literal = ";",
        },
        .{
            .expected_type = TokenType.bang,
            .expected_literal = "!",
        },
        .{
            .expected_type = TokenType.minus,
            .expected_literal = "-",
        },
        .{
            .expected_type = TokenType.slash,
            .expected_literal = "/",
        },
        .{
            .expected_type = TokenType.asterisk,
            .expected_literal = "*",
        },
        .{
            .expected_type = TokenType.int,
            .expected_literal = "5",
        },
        .{
            .expected_type = TokenType.semicolon,
            .expected_literal = ";",
        },
        .{
            .expected_type = TokenType.int,
            .expected_literal = "5",
        },
        .{
            .expected_type = TokenType.lt,
            .expected_literal = "<",
        },
        .{
            .expected_type = TokenType.int,
            .expected_literal = "10",
        },
        .{
            .expected_type = TokenType.gt,
            .expected_literal = ">",
        },
        .{
            .expected_type = TokenType.int,
            .expected_literal = "5",
        },
        .{
            .expected_type = TokenType.semicolon,
            .expected_literal = ";",
        },
        .{
            .expected_type = TokenType._if,
            .expected_literal = "if",
        },
        .{
            .expected_type = TokenType.lparen,
            .expected_literal = "(",
        },
        .{
            .expected_type = TokenType.int,
            .expected_literal = "5",
        },
        .{
            .expected_type = TokenType.lt,
            .expected_literal = "<",
        },
        .{
            .expected_type = TokenType.int,
            .expected_literal = "10",
        },
        .{
            .expected_type = TokenType.rparen,
            .expected_literal = ")",
        },
        .{
            .expected_type = TokenType.lbrace,
            .expected_literal = "{",
        },
        .{
            .expected_type = TokenType._return,
            .expected_literal = "return",
        },
        .{
            .expected_type = TokenType._true,
            .expected_literal = "true",
        },
        .{
            .expected_type = TokenType.semicolon,
            .expected_literal = ";",
        },
        .{
            .expected_type = TokenType.rbrace,
            .expected_literal = "}",
        },
        .{
            .expected_type = TokenType._else,
            .expected_literal = "else",
        },
        .{
            .expected_type = TokenType.lbrace,
            .expected_literal = "{",
        },
        .{
            .expected_type = TokenType._return,
            .expected_literal = "return",
        },
        .{
            .expected_type = TokenType._false,
            .expected_literal = "false",
        },
        .{
            .expected_type = TokenType.semicolon,
            .expected_literal = ";",
        },
        .{
            .expected_type = TokenType.rbrace,
            .expected_literal = "}",
        },
        .{
            .expected_type = TokenType.int,
            .expected_literal = "10",
        },
        .{
            .expected_type = TokenType.eq,
            .expected_literal = "==",
        },
        .{
            .expected_type = TokenType.int,
            .expected_literal = "10",
        },
        .{
            .expected_type = TokenType.int,
            .expected_literal = "10",
        },
        .{
            .expected_type = TokenType.not_eq,
            .expected_literal = "!=",
        },
        .{
            .expected_type = TokenType.int,
            .expected_literal = "9",
        },
        .{
            .expected_type = TokenType.eof,
            .expected_literal = "",
        },
    };

    var l = Lexer.init(input);

    for (expecteds) |expected| {
        const tok = l.nextToken();
        try testing.expectEqual(expected.expected_type, tok._type);
        try testing.expectEqualStrings(expected.expected_literal, tok.literal);
    }
}
