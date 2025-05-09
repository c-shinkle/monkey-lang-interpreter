const std = @import("std");
const testing = std.testing;

const Token = @import("Token.zig");
const TokenType = Token.TokenType;

const Lexer = @This();

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
        0 => Token{ .token_type = TokenType.eof, .literal = Token.EOF },
        '=' => blk: {
            if (self.peekChar() == '=') {
                self.readChar();
                break :blk Token{ .token_type = TokenType.eq, .literal = Token.EQ };
            }
            break :blk Token{ .token_type = TokenType.assign, .literal = Token.ASSIGN };
        },
        '+' => Token{ .token_type = TokenType.plus, .literal = Token.PLUS },
        '-' => Token{ .token_type = TokenType.minus, .literal = Token.MINUS },
        '!' => blk: {
            if (self.peekChar() == '=') {
                const start = self.position;
                self.readChar();
                break :blk Token{
                    .token_type = TokenType.not_eq,
                    .literal = self.input[start .. start + 2],
                };
            }
            break :blk Token{ .token_type = TokenType.bang, .literal = Token.BANG };
        },
        '/' => Token{ .token_type = TokenType.slash, .literal = Token.SLASH },
        '*' => Token{ .token_type = TokenType.asterisk, .literal = Token.ASTERISK },
        '<' => Token{ .token_type = TokenType.lt, .literal = Token.LT },
        '>' => Token{ .token_type = TokenType.gt, .literal = Token.GT },
        ';' => Token{ .token_type = TokenType.semicolon, .literal = Token.SEMICOLON },
        ',' => Token{ .token_type = TokenType.comma, .literal = Token.COMMA },
        '(' => Token{ .token_type = TokenType.lparen, .literal = Token.LPAREN },
        ')' => Token{ .token_type = TokenType.rparen, .literal = Token.RPAREN },
        '{' => Token{ .token_type = TokenType.lbrace, .literal = Token.LBRACE },
        '}' => Token{ .token_type = TokenType.rbrace, .literal = Token.RBRACE },
        '"' => Token{ .token_type = TokenType.string, .literal = self.readString() },
        '[' => Token{ .token_type = TokenType.lbracket, .literal = Token.LBRACKET },
        ']' => Token{ .token_type = TokenType.rbracket, .literal = Token.RBRACKET },
        ':' => Token{ .token_type = TokenType.colon, .literal = Token.COLON },
        else => blk: {
            if (isLetter(ch.*)) {
                const literal = self.readIdentifier();
                const token_type = Token.getKeywordByLiteral(literal);
                return Token{ .token_type = token_type, .literal = literal };
            } else if (std.ascii.isDigit(ch.*)) {
                return Token{ .token_type = TokenType.int, .literal = self.readNumber() };
            }
            break :blk Token{ .token_type = TokenType.illegal, .literal = ch[0..1] };
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
    return if (self.read_position >= self.input.len)
        0
    else
        self.input[self.read_position];
}

fn readString(self: *Lexer) []const u8 {
    const start = self.position + 1;
    while (true) {
        self.readChar();
        if (self.ch.* == '"' or self.ch.* == 0) break;
    }
    return self.input[start..self.position];
}

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
        \\"foobar"
        \\"foo bar"
        \\[1, 2];
        \\{"foo": "bar"}
    ;

    const expecteds = [_]struct {
        expected_type: TokenType,
        expected_literal: []const u8,
    }{
        .{ .expected_type = TokenType.let, .expected_literal = "let" },
        .{ .expected_type = TokenType.identifier, .expected_literal = "five" },
        .{ .expected_type = TokenType.assign, .expected_literal = "=" },
        .{ .expected_type = TokenType.int, .expected_literal = "5" },
        .{ .expected_type = TokenType.semicolon, .expected_literal = ";" },
        .{ .expected_type = TokenType.let, .expected_literal = "let" },
        .{ .expected_type = TokenType.identifier, .expected_literal = "ten" },
        .{ .expected_type = TokenType.assign, .expected_literal = "=" },
        .{ .expected_type = TokenType.int, .expected_literal = "10" },
        .{ .expected_type = TokenType.semicolon, .expected_literal = ";" },
        .{ .expected_type = TokenType.let, .expected_literal = "let" },
        .{ .expected_type = TokenType.identifier, .expected_literal = "add" },
        .{ .expected_type = TokenType.assign, .expected_literal = "=" },
        .{ .expected_type = TokenType._function, .expected_literal = "fn" },
        .{ .expected_type = TokenType.lparen, .expected_literal = "(" },
        .{ .expected_type = TokenType.identifier, .expected_literal = "x" },
        .{ .expected_type = TokenType.comma, .expected_literal = "," },
        .{ .expected_type = TokenType.identifier, .expected_literal = "y" },
        .{ .expected_type = TokenType.rparen, .expected_literal = ")" },
        .{ .expected_type = TokenType.lbrace, .expected_literal = "{" },
        .{ .expected_type = TokenType.identifier, .expected_literal = "x" },
        .{ .expected_type = TokenType.plus, .expected_literal = "+" },
        .{ .expected_type = TokenType.identifier, .expected_literal = "y" },
        .{ .expected_type = TokenType.semicolon, .expected_literal = ";" },
        .{ .expected_type = TokenType.rbrace, .expected_literal = "}" },
        .{ .expected_type = TokenType.semicolon, .expected_literal = ";" },
        .{ .expected_type = TokenType.let, .expected_literal = "let" },
        .{ .expected_type = TokenType.identifier, .expected_literal = "result" },
        .{ .expected_type = TokenType.assign, .expected_literal = "=" },
        .{ .expected_type = TokenType.identifier, .expected_literal = "add" },
        .{ .expected_type = TokenType.lparen, .expected_literal = "(" },
        .{ .expected_type = TokenType.identifier, .expected_literal = "five" },
        .{ .expected_type = TokenType.comma, .expected_literal = "," },
        .{ .expected_type = TokenType.identifier, .expected_literal = "ten" },
        .{ .expected_type = TokenType.rparen, .expected_literal = ")" },
        .{ .expected_type = TokenType.semicolon, .expected_literal = ";" },
        .{ .expected_type = TokenType.bang, .expected_literal = "!" },
        .{ .expected_type = TokenType.minus, .expected_literal = "-" },
        .{ .expected_type = TokenType.slash, .expected_literal = "/" },
        .{ .expected_type = TokenType.asterisk, .expected_literal = "*" },
        .{ .expected_type = TokenType.int, .expected_literal = "5" },
        .{ .expected_type = TokenType.semicolon, .expected_literal = ";" },
        .{ .expected_type = TokenType.int, .expected_literal = "5" },
        .{ .expected_type = TokenType.lt, .expected_literal = "<" },
        .{ .expected_type = TokenType.int, .expected_literal = "10" },
        .{ .expected_type = TokenType.gt, .expected_literal = ">" },
        .{ .expected_type = TokenType.int, .expected_literal = "5" },
        .{ .expected_type = TokenType.semicolon, .expected_literal = ";" },
        .{ .expected_type = TokenType._if, .expected_literal = "if" },
        .{ .expected_type = TokenType.lparen, .expected_literal = "(" },
        .{ .expected_type = TokenType.int, .expected_literal = "5" },
        .{ .expected_type = TokenType.lt, .expected_literal = "<" },
        .{ .expected_type = TokenType.int, .expected_literal = "10" },
        .{ .expected_type = TokenType.rparen, .expected_literal = ")" },
        .{ .expected_type = TokenType.lbrace, .expected_literal = "{" },
        .{ .expected_type = TokenType._return, .expected_literal = "return" },
        .{ .expected_type = TokenType._true, .expected_literal = "true" },
        .{ .expected_type = TokenType.semicolon, .expected_literal = ";" },
        .{ .expected_type = TokenType.rbrace, .expected_literal = "}" },
        .{ .expected_type = TokenType._else, .expected_literal = "else" },
        .{ .expected_type = TokenType.lbrace, .expected_literal = "{" },
        .{ .expected_type = TokenType._return, .expected_literal = "return" },
        .{ .expected_type = TokenType._false, .expected_literal = "false" },
        .{ .expected_type = TokenType.semicolon, .expected_literal = ";" },
        .{ .expected_type = TokenType.rbrace, .expected_literal = "}" },
        .{ .expected_type = TokenType.int, .expected_literal = "10" },
        .{ .expected_type = TokenType.eq, .expected_literal = "==" },
        .{ .expected_type = TokenType.int, .expected_literal = "10" },
        .{ .expected_type = TokenType.int, .expected_literal = "10" },
        .{ .expected_type = TokenType.not_eq, .expected_literal = "!=" },
        .{ .expected_type = TokenType.int, .expected_literal = "9" },
        .{ .expected_type = TokenType.string, .expected_literal = "foobar" },
        .{ .expected_type = TokenType.string, .expected_literal = "foo bar" },
        .{ .expected_type = TokenType.lbracket, .expected_literal = "[" },
        .{ .expected_type = TokenType.int, .expected_literal = "1" },
        .{ .expected_type = TokenType.comma, .expected_literal = "," },
        .{ .expected_type = TokenType.int, .expected_literal = "2" },
        .{ .expected_type = TokenType.rbracket, .expected_literal = "]" },
        .{ .expected_type = TokenType.semicolon, .expected_literal = ";" },
        .{ .expected_type = TokenType.lbrace, .expected_literal = "{" },
        .{ .expected_type = TokenType.string, .expected_literal = "foo" },
        .{ .expected_type = TokenType.colon, .expected_literal = ":" },
        .{ .expected_type = TokenType.string, .expected_literal = "bar" },
        .{ .expected_type = TokenType.rbrace, .expected_literal = "}" },
        .{ .expected_type = TokenType.eof, .expected_literal = "" },
    };

    var l = Lexer.init(input);

    for (expecteds) |expected| {
        const tok = l.nextToken();
        try testing.expectEqual(expected.expected_type, tok.token_type);
        try testing.expectEqualStrings(expected.expected_literal, tok.literal);
    }
}

test {
    std.testing.refAllDecls(Lexer);
}
