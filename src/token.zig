const std = @import("std");
const Allocator = std.mem.Allocator;

pub const TokenType = []const u8;

pub const Token = struct {
    _type: TokenType,
    literal: []const u8,

    pub fn init(_type: TokenType, literal: []const u8, alloc: Allocator) !Token {
        const duped_literal = try alloc.dupe(u8, literal);
        return Token{
            ._type = _type,
            .literal = duped_literal,
        };
    }

    pub fn dupe(self: Token, alloc: Allocator) !Token {
        const duped_literal = try alloc.dupe(u8, self.literal);
        return Token{
            ._type = self._type,
            .literal = duped_literal,
        };
    }

    pub fn deinit(self: Token, alloc: Allocator) void {
        alloc.free(self.literal);
    }
};

pub const ILLEGAL = "ILLEGAL";
pub const EOF = "EOF";

// Identifiers + literals
pub const IDENT = "IDENT"; // add, foobar, x, y, ...
pub const INT = "INT"; // 1343456

// Operators
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
// Delimiters
pub const COMMA = ",";
pub const SEMICOLON = ";";

pub const LPAREN = "(";
pub const RPAREN = ")";
pub const LBRACE = "{";
pub const RBRACE = "}";

// Keywords
pub const FUNCTION = "FUNCTION";
pub const LET = "LET";
pub const TRUE = "TRUE";
pub const FALSE = "FALSE";
pub const IF = "IF";
pub const ELSE = "ELSE";
pub const RETURN = "RETURN";

const keywords = std.StaticStringMap([]const u8).initComptime(.{
    .{ "fn", FUNCTION },
    .{ "let", LET },
    .{ "true", TRUE },
    .{ "false", FALSE },
    .{ "if", IF },
    .{ "else", ELSE },
    .{ "return", RETURN },
});

pub fn lookupIdent(ident: []const u8) TokenType {
    return keywords.get(ident) orelse IDENT;
}
