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

    const expecteds = [_]struct { TokenType, []const u8 }{
        .{ TokenType.let, "let" },
        .{ TokenType.identifier, "five" },
        .{ TokenType.assign, "=" },
        .{ TokenType.int, "5" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.let, "let" },
        .{ TokenType.identifier, "ten" },
        .{ TokenType.assign, "=" },
        .{ TokenType.int, "10" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.let, "let" },
        .{ TokenType.identifier, "add" },
        .{ TokenType.assign, "=" },
        .{ TokenType._function, "fn" },
        .{ TokenType.lparen, "(" },
        .{ TokenType.identifier, "x" },
        .{ TokenType.comma, "," },
        .{ TokenType.identifier, "y" },
        .{ TokenType.rparen, ")" },
        .{ TokenType.lbrace, "{" },
        .{ TokenType.identifier, "x" },
        .{ TokenType.plus, "+" },
        .{ TokenType.identifier, "y" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.rbrace, "}" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.let, "let" },
        .{ TokenType.identifier, "result" },
        .{ TokenType.assign, "=" },
        .{ TokenType.identifier, "add" },
        .{ TokenType.lparen, "(" },
        .{ TokenType.identifier, "five" },
        .{ TokenType.comma, "," },
        .{ TokenType.identifier, "ten" },
        .{ TokenType.rparen, ")" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.bang, "!" },
        .{ TokenType.minus, "-" },
        .{ TokenType.slash, "/" },
        .{ TokenType.asterisk, "*" },
        .{ TokenType.int, "5" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.int, "5" },
        .{ TokenType.lt, "<" },
        .{ TokenType.int, "10" },
        .{ TokenType.gt, ">" },
        .{ TokenType.int, "5" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType._if, "if" },
        .{ TokenType.lparen, "(" },
        .{ TokenType.int, "5" },
        .{ TokenType.lt, "<" },
        .{ TokenType.int, "10" },
        .{ TokenType.rparen, ")" },
        .{ TokenType.lbrace, "{" },
        .{ TokenType._return, "return" },
        .{ TokenType._true, "true" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.rbrace, "}" },
        .{ TokenType._else, "else" },
        .{ TokenType.lbrace, "{" },
        .{ TokenType._return, "return" },
        .{ TokenType._false, "false" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.rbrace, "}" },
        .{ TokenType.int, "10" },
        .{ TokenType.eq, "==" },
        .{ TokenType.int, "10" },
        .{ TokenType.int, "10" },
        .{ TokenType.not_eq, "!=" },
        .{ TokenType.int, "9" },
        .{ TokenType.string, "foobar" },
        .{ TokenType.string, "foo bar" },
        .{ TokenType.lbracket, "[" },
        .{ TokenType.int, "1" },
        .{ TokenType.comma, "," },
        .{ TokenType.int, "2" },
        .{ TokenType.rbracket, "]" },
        .{ TokenType.semicolon, ";" },
        .{ TokenType.lbrace, "{" },
        .{ TokenType.string, "foo" },
        .{ TokenType.colon, ":" },
        .{ TokenType.string, "bar" },
        .{ TokenType.rbrace, "}" },
        .{ TokenType.eof, "" },
    };

    var lexer = Lexer.init(input);

    for (expecteds) |expected| {
        const actual = lexer.nextToken();

        const expected_type, const expected_literal = expected;
        try testing.expectEqual(expected_type, actual.token_type);
        try testing.expectEqualStrings(expected_literal, actual.literal);
    }
}

test {
    std.testing.refAllDecls(Lexer);
}
