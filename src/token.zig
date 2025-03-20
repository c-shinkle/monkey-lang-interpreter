const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Token = struct {
    _type: TokenType,
    literal: []const u8,

    pub fn dupe(self: Token, alloc: Allocator) !Token {
        const duped_literal = try alloc.dupe(u8, self.literal);
        return Token{
            ._type = self._type,
            .literal = duped_literal,
        };
    }

    pub fn dupe_deinit(self: Token, alloc: Allocator) void {
        alloc.free(self.literal);
    }
};

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
    identifier,
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
    int,
    illegal,
    eof,
};

pub const Scope = enum { operator, keyword, other };

pub fn scope(token_type: TokenType) Scope {
    return switch (token_type) {
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
        .identifier,
        => .keyword,
        // Other
        .illegal,
        .eof,
        .int,
        .comma,
        .semicolon,
        .lparen,
        .rparen,
        .lbrace,
        .rbrace,
        => .other,
    };
}

pub fn ScopedTokenType(comptime s: Scope) type {
    const e_info = @typeInfo(TokenType);
    const all_fields = e_info.@"enum".fields;
    var i: usize = 0;
    var fields: [all_fields.len]std.builtin.Type.EnumField = undefined;
    for (all_fields) |field| {
        if (scope(@enumFromInt(field.value)) == s) {
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

pub fn scoped(self: TokenType, comptime s: Scope) ?ScopedTokenType(s) {
    switch (self) {
        inline else => |token_type| {
            if (comptime scope(token_type) != s) return null;
            return ScopedTokenType(token_type);
        },
    }
}

const keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ "fn", ._function },
    .{ "let", .let },
    .{ "true", ._true },
    .{ "false", ._false },
    .{ "if", ._if },
    .{ "else", ._else },
    .{ "return", ._return },
});

pub fn lookupIdent(ident: []const u8) TokenType {
    return keywords.get(ident) orelse TokenType.identifier;
}

const literals_to_operators = std.StaticStringMap(ScopedTokenType(.operator)).initComptime(.{
    .{ "=", .assign },
    .{ "+", .plus },
    .{ "-", .minus },
    .{ "!", .bang },
    .{ "*", .asterisk },
    .{ "/", .slash },
    .{ "<", .lt },
    .{ ">", .gt },
    .{ "==", .eq },
    .{ "!=", .not_eq },
});

pub fn lookupOperatorLiteral(literal: []const u8) ?ScopedTokenType(.operator) {
    return literals_to_operators.get(literal);
}

pub fn lookupOperatorEnum(operator: ScopedTokenType(.operator)) []const u8 {
    return switch (operator) {
        .assign => "=",
        .plus => "+",
        .minus => "-",
        .bang => "!",
        .asterisk => "*",
        .slash => "/",
        .lt => "<",
        .gt => ">",
        .eq => "==",
        .not_eq => "!=",
    };
}
