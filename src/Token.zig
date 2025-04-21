const std = @import("std");
const Allocator = std.mem.Allocator;

token_type: TokenType,
literal: []const u8,

const Token = @This();

pub fn dupe(self: Token, alloc: Allocator) Allocator.Error!Token {
    const literal = if (self.token_type.isLiteralAllocated())
        try alloc.dupe(u8, self.literal)
    else
        self.literal;
    return Token{ .token_type = self.token_type, .literal = literal };
}

pub fn dupe_deinit(self: Token, alloc: Allocator) void {
    if (self.token_type.isLiteralAllocated()) {
        alloc.free(self.literal);
    }
}

pub const TokenType = enum {
    assign,
    plus,
    minus,
    bang,
    asterisk,
    slash,
    lt,
    gt,
    eq,
    not_eq,
    _function,
    let,
    _true,
    _false,
    _if,
    _else,
    _return,
    comma,
    semicolon,
    lparen,
    rparen,
    lbrace,
    rbrace,
    eof,
    illegal,
    identifier,
    int,
    string,
    lbracket,
    rbracket,

    pub fn scope(self: TokenType) Scope {
        return switch (self) {
            // Operator
            .assign,
            .plus,
            .minus,
            .bang,
            .asterisk,
            .slash,
            .lt,
            .gt,
            .eq,
            .not_eq,
            => .operator,
            // Keyword
            ._function,
            .let,
            ._true,
            ._false,
            ._if,
            ._else,
            ._return,
            // Delimiter
            .comma,
            .semicolon,
            .lparen,
            .rparen,
            .lbrace,
            .rbrace,
            .lbracket,
            .rbracket,
            .eof,
            // Value
            .illegal,
            .identifier,
            .int,
            .string,
            => .not_operator,
        };
    }

    pub fn isLiteralAllocated(self: TokenType) bool {
        return self == .illegal or self == .identifier or self == .int or self == .string;
    }
};

pub const Scope = enum { operator, not_operator };

pub fn ScopedTokenType(comptime s: Scope) type {
    const e_info = @typeInfo(TokenType);
    const all_fields = e_info.@"enum".fields;
    var i: usize = 0;
    var fields: [all_fields.len]std.builtin.Type.EnumField = undefined;
    for (all_fields) |field| {
        const token_type: TokenType = @enumFromInt(field.value);
        if (token_type.scope() == s) {
            fields[i] = field;
            i += 1;
        }
    }

    return @Type(.{
        .@"enum" = .{
            .tag_type = e_info.@"enum".tag_type,
            .fields = fields[0..i],
            .decls = &.{},
            .is_exhaustive = true,
        },
    });
}

// Operators
pub const Operator = ScopedTokenType(.operator);
pub const ASSIGN = "=";
pub const PLUS = "+";
pub const MINUS = "-";
pub const BANG = "!";
pub const ASTERISK = "*";
pub const SLASH = "/";
pub const LT = "<";
pub const GT = ">";
pub const EQ = "==";
pub const NOT_EQ = "!=";
const operators_map = std.StaticStringMap(Operator).initComptime(.{
    .{ ASSIGN, .assign },
    .{ PLUS, .plus },
    .{ MINUS, .minus },
    .{ BANG, .bang },
    .{ ASTERISK, .asterisk },
    .{ SLASH, .slash },
    .{ LT, .lt },
    .{ GT, .gt },
    .{ EQ, .eq },
    .{ NOT_EQ, .not_eq },
});
pub fn findOperatorByLiteral(literal: []const u8) ?Operator {
    return operators_map.get(literal);
}
pub fn getLiteralByOperator(operator: Operator) []const u8 {
    return switch (operator) {
        .assign => ASSIGN,
        .plus => PLUS,
        .minus => MINUS,
        .bang => BANG,
        .asterisk => ASTERISK,
        .slash => SLASH,
        .lt => LT,
        .gt => GT,
        .eq => EQ,
        .not_eq => NOT_EQ,
    };
}

// Keywords
pub const FUNCTION = "fn";
pub const LET = "let";
pub const TRUE = "true";
pub const FALSE = "false";
pub const IF = "if";
pub const ELSE = "else";
pub const RETURN = "return";
const keywords_map = std.StaticStringMap(TokenType).initComptime(.{
    .{ FUNCTION, ._function },
    .{ LET, .let },
    .{ TRUE, ._true },
    .{ FALSE, ._false },
    .{ IF, ._if },
    .{ ELSE, ._else },
    .{ RETURN, ._return },
});
pub fn getKeywordByLiteral(literal: []const u8) TokenType {
    return keywords_map.get(literal) orelse TokenType.identifier;
}

// Delimiters
pub const COMMA = ",";
pub const SEMICOLON = ";";
pub const LPAREN = "(";
pub const RPAREN = ")";
pub const LBRACE = "{";
pub const RBRACE = "}";
pub const LBRACKET = "[";
pub const RBRACKET = "]";
pub const EOF = "";
// pub const DOUBLEQUOTES = "\"";

test {
    std.testing.refAllDecls(Token);
}
