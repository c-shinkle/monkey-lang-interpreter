const std = @import("std");

pub const TokenType = []const u8;

pub const Token = struct {
    _type: TokenType,
    literal: []const u8,
};

pub const ILLEGAL = "ILLEGAL";
pub const EOF = "EOF";

// Identifiers + literals
pub const IDENT = "IDENT"; // add, foobar, x, y, ...
pub const INT = "INT"; // 1343456

// Operators
pub const ASSIGN = "=";
pub const PLUS = "+";
// pub const MINUS = "-";
// pub const BANG = "!";
// pub const ASTERISK = "*";
// pub const SLASH = "/";
// pub const LT = "<";
// pub const GT = ">";
// pub const EQ = "==";
// pub const NOT_EQ = "!=";
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
// pub const TRUE = "TRUE";
// pub const FALSE = "FALSE";
// pub const IF = "IF";
// pub const ELSE = "ELSE";
// pub const RETURN = "RETURN";

const keywords = std.StaticStringMap([]const u8).initComptime([_]struct { []const u8, []const u8 }{
    .{ "fn", FUNCTION },
    .{ "let", LET },
});

pub fn lookupIdent(ident: []const u8) TokenType {
    if (keywords.get(ident)) |tok| {
        return tok;
    }
    return IDENT;
}
