const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Expression = ast.Expression;
const ExpressionStatement = ast.ExpressionStatement;
const Lexer = @import("Lexer.zig");
const Token = @import("Token.zig");
const TokenType = Token.TokenType;
const Operator = Token.Operator;
const findOperatorByLiteral = Token.findOperatorByLiteral;

lexer: *Lexer,
cur_token: Token,
peek_token: Token,
errors: std.ArrayListUnmanaged([]const u8),

const Parser = @This();

const ParserError = error{
    MissingLetAssign,
    MissingLetIdentifier,
    MissingLeftBrace,
    MissingLeftParenthesis,
    MissingRightBrace,
    MissingRightBracket,
    MissingRightParenthesis,
    MissingColon,
    UnknownPrefixToken,
    UnknownOperatorToken,
} || Allocator.Error || std.fmt.AllocPrintError || std.fmt.ParseIntError;

const PrefixParseFn = *const fn (self: *Parser, alloc: Allocator) ParserError!Expression;
const InfixParseFn = *const fn (
    self: *Parser,
    alloc: Allocator,
    lhs: Expression,
) ParserError!Expression;

const Precedence = enum {
    lowest,
    equals,
    lessgreater,
    sum,
    product,
    prefix,
    call,
    index,
};

fn getPrecedence(token_type: TokenType) Precedence {
    return switch (token_type) {
        .eq, .not_eq => Precedence.equals,
        .lt, .gt => Precedence.lessgreater,
        .plus, .minus => Precedence.sum,
        .slash, .asterisk => Precedence.product,
        .lparen => Precedence.call,
        .lbracket => Precedence.index,
        else => Precedence.lowest,
    };
}

// Initialization

pub fn init(lexer: *Lexer, alloc: Allocator) ParserError!Parser {
    var p = Parser{
        .lexer = lexer,
        .cur_token = undefined,
        .peek_token = undefined,
        .errors = .empty,
    };
    errdefer p.deinit(alloc);

    p.nextToken();
    p.nextToken();

    return p;
}

pub fn deinit(self: *Parser, alloc: Allocator) void {
    for (self.errors.items) |msg| {
        alloc.free(msg);
    }

    self.errors.deinit(alloc);
}

// Program

pub fn parseProgram(self: *Parser, alloc: Allocator) ParserError!ast.Program {
    var list = std.ArrayListUnmanaged(ast.Statement).empty;

    while (self.cur_token.token_type != .eof) {
        const maybe_stmt = self.parseStatement(alloc);

        if (maybe_stmt) |stmt| {
            try list.append(alloc, stmt);
        } else |err| {
            try handleParserError(self, alloc, err);
        }

        self.nextToken();
    }

    return ast.Program{ .statements = try list.toOwnedSlice(alloc) };
}

// Statement

fn parseStatement(self: *Parser, alloc: Allocator) ParserError!ast.Statement {
    return switch (self.cur_token.token_type) {
        .let => ast.Statement{
            .let_statement = try self.parseLetStatement(alloc),
        },
        ._return => ast.Statement{
            .return_statement = try self.parseReturnStatement(alloc),
        },
        else => ast.Statement{
            .expression_statement = try self.parseExpressionStatement(alloc),
        },
    };
}

fn parseLetStatement(self: *Parser, alloc: Allocator) ParserError!ast.LetStatement {
    const let_token = self.cur_token;

    if (!self.expectPeek(.identifier)) {
        return ParserError.MissingLetIdentifier;
    }

    const name = ast.Identifier{
        .token = self.cur_token,
        .value = self.cur_token.literal,
    };

    if (!self.expectPeek(.assign)) {
        return ParserError.MissingLetAssign;
    }

    self.nextToken();

    const value = try self.parseExpression(alloc, Precedence.lowest);

    if (self.peekTokenIs(.semicolon)) {
        self.nextToken();
    }

    return ast.LetStatement{ .token = let_token, .name = name, .value = value };
}

pub fn parseReturnStatement(self: *Parser, alloc: Allocator) ParserError!ast.ReturnStatement {
    const token = self.cur_token;

    self.nextToken();

    const return_value = try self.parseExpression(alloc, Precedence.lowest);

    if (self.peekTokenIs(.semicolon)) {
        self.nextToken();
    }

    return ast.ReturnStatement{ .token = token, .return_value = return_value };
}

pub fn parseExpressionStatement(
    self: *Parser,
    alloc: Allocator,
) ParserError!ExpressionStatement {
    const cur_token = self.cur_token;
    const expression = try self.parseExpression(alloc, Precedence.lowest);

    if (self.peekTokenIs(.semicolon)) {
        self.nextToken();
    }

    return ExpressionStatement{ .token = cur_token, .expression = expression };
}

pub fn parseBlockStatement(self: *Parser, alloc: Allocator) ParserError!ast.BlockStatement {
    const token = self.cur_token;
    var statements = std.ArrayListUnmanaged(ast.Statement).empty;
    self.nextToken();

    while (!self.curTokenIs(.rbrace) and
        !self.curTokenIs(.eof)) : (self.nextToken())
    {
        const statement = try self.parseStatement(alloc);
        try statements.append(alloc, statement);
    }

    return ast.BlockStatement{
        .token = token,
        .statements = try statements.toOwnedSlice(alloc),
    };
}

// Expression

pub fn parseExpression(
    self: *Parser,
    alloc: Allocator,
    precedence: Precedence,
) ParserError!Expression {
    const precedence_int = @intFromEnum(precedence);
    const prefix = lookupPrefixParseFns(self.cur_token.token_type) orelse {
        return ParserError.UnknownPrefixToken;
    };

    var left_exp = try prefix(self, alloc);

    while (!self.peekTokenIs(.semicolon) and
        precedence_int < @intFromEnum(self.peekPrecedence()))
    {
        const infix = lookupInfixParseFns(self.peek_token.token_type) orelse return left_exp;
        self.nextToken();
        left_exp = try infix(self, alloc, left_exp);
    }

    return left_exp;
}

fn parseIdentifier(self: *const Parser, _: Allocator) ParserError!Expression {
    return Expression{
        .identifier = ast.Identifier{
            .token = self.cur_token,
            .value = self.cur_token.literal,
        },
    };
}

fn parseIntegerLiteral(self: *Parser, _: Allocator) ParserError!Expression {
    const cur_token = self.cur_token;
    const value = try std.fmt.parseInt(i64, cur_token.literal, 10);

    const integer_literal = ast.IntegerLiteral{ .token = cur_token, .value = value };
    return Expression{ .integer_literal = integer_literal };
}

fn parsePrefixExpression(self: *Parser, alloc: Allocator) ParserError!Expression {
    const token = self.cur_token;
    const operator = findOperatorByLiteral(self.cur_token.literal) orelse
        return ParserError.UnknownOperatorToken;

    self.nextToken();

    const right = try alloc.create(Expression);
    errdefer alloc.destroy(right);
    right.* = try self.parseExpression(alloc, Precedence.prefix);

    return Expression{
        .prefix_expression = ast.PrefixExpression{
            .token = token,
            .operator = operator,
            .right = right,
        },
    };
}

fn parseInfixExpression(
    self: *Parser,
    alloc: Allocator,
    lhs: Expression,
) ParserError!Expression {
    const token = self.cur_token;
    const operator = findOperatorByLiteral(self.cur_token.literal) orelse
        return ParserError.UnknownOperatorToken;

    const precedence = self.curPrecedence();
    self.nextToken();

    const left = try alloc.create(Expression);
    errdefer alloc.destroy(left);
    left.* = lhs;

    const right = try alloc.create(Expression);
    errdefer alloc.destroy(right);
    right.* = try self.parseExpression(alloc, precedence);

    return Expression{
        .infix_expression = ast.InfixExpression{
            .token = token,
            .left = left,
            .operator = operator,
            .right = right,
        },
    };
}

fn parseBoolean(self: *Parser, _: Allocator) ParserError!Expression {
    return Expression{
        .boolean_expression = ast.Boolean{
            .token = self.cur_token,
            .value = self.curTokenIs(._true),
        },
    };
}

fn parseGroupedExpression(self: *Parser, alloc: Allocator) ParserError!Expression {
    self.nextToken();

    const exp = try self.parseExpression(alloc, Precedence.lowest);

    if (!self.expectPeek(.rparen)) {
        return ParserError.MissingRightParenthesis;
    }

    return exp;
}

fn parseIfExpression(self: *Parser, alloc: Allocator) ParserError!Expression {
    const token = self.cur_token;
    if (!self.expectPeek(.lparen)) {
        return ParserError.MissingLeftParenthesis;
    }
    self.nextToken();

    const condition = try alloc.create(Expression);
    condition.* = try self.parseExpression(alloc, Precedence.lowest);

    if (!self.expectPeek(.rparen)) {
        return ParserError.MissingRightParenthesis;
    }
    if (!self.expectPeek(.lbrace)) {
        return ParserError.MissingLeftBrace;
    }

    const consequence = try alloc.create(ast.BlockStatement);
    consequence.* = try self.parseBlockStatement(alloc);

    var maybe_alt: ?*ast.BlockStatement = null;
    if (self.peekTokenIs(._else)) {
        self.nextToken();

        if (!self.expectPeek(.lbrace)) {
            return ParserError.MissingLeftBrace;
        }
        maybe_alt = try alloc.create(ast.BlockStatement);
        maybe_alt.?.* = try self.parseBlockStatement(alloc);
    }

    return Expression{
        .if_expression = ast.IfExpression{
            .token = token,
            .condition = condition,
            .consequence = consequence,
            .alternative = maybe_alt,
        },
    };
}

fn parseFunctionLiteral(self: *Parser, alloc: Allocator) ParserError!Expression {
    const token = self.cur_token;
    if (!self.expectPeek(.lparen)) {
        return ParserError.MissingLeftParenthesis;
    }
    const parameters = try self.parseFunctionParameters(alloc);
    errdefer alloc.free(parameters);

    if (!self.expectPeek(.lbrace)) {
        return error.MissingLeftBrace;
    }
    const body = try self.parseBlockStatement(alloc);

    return Expression{
        .function_literal = ast.FunctionLiteral{
            .token = token,
            .parameters = parameters,
            .body = body,
        },
    };
}

fn parseFunctionParameters(
    self: *Parser,
    alloc: Allocator,
) ParserError![]ast.Identifier {
    var identifiers = std.ArrayListUnmanaged(ast.Identifier).empty;
    errdefer identifiers.deinit(alloc);

    if (self.peekTokenIs(.rparen)) {
        self.nextToken();
        return try identifiers.toOwnedSlice(alloc);
    }

    self.nextToken();

    try identifiers.append(alloc, ast.Identifier{
        .token = self.cur_token,
        .value = self.cur_token.literal,
    });

    while (self.peekTokenIs(.comma)) {
        self.nextToken();
        self.nextToken();
        try identifiers.append(alloc, ast.Identifier{
            .token = self.cur_token,
            .value = self.cur_token.literal,
        });
    }

    if (!self.expectPeek(.rparen)) {
        return ParserError.MissingRightParenthesis;
    }

    return try identifiers.toOwnedSlice(alloc);
}

fn parseCallExpression(
    self: *Parser,
    alloc: Allocator,
    lhs: Expression, // Identifier or Function Literal
) ParserError!Expression {
    const function = try alloc.create(Expression);
    errdefer alloc.destroy(function);
    function.* = lhs;
    const token = self.cur_token;

    const arguments = try self.parseCallArguments(alloc);
    return Expression{
        .call_expression = ast.CallExpression{
            .token = token,
            .function = function,
            .arguments = arguments,
        },
    };
}

fn parseCallArguments(self: *Parser, alloc: Allocator) ParserError![]const Expression {
    var args = std.ArrayListUnmanaged(Expression).empty;

    if (self.peekTokenIs(.rparen)) {
        self.nextToken();
        return try args.toOwnedSlice(alloc);
    }

    self.nextToken();

    const first_exp = try self.parseExpression(alloc, Precedence.lowest);
    try args.append(alloc, first_exp);

    while (self.peekTokenIs(.comma)) {
        self.nextToken();
        self.nextToken();
        const loop_exp = try self.parseExpression(alloc, Precedence.lowest);
        try args.append(alloc, loop_exp);
    }

    if (!self.expectPeek(.rparen)) {
        return ParserError.MissingRightParenthesis;
    }

    return try args.toOwnedSlice(alloc);
}

fn parseStringLiteral(self: *Parser, _: Allocator) ParserError!Expression {
    return Expression{ .string_literal = ast.StringLiteral{ .token = self.cur_token } };
}

fn parseArrayLiteral(self: *Parser, alloc: Allocator) ParserError!Expression {
    const token = self.cur_token;

    const elements = try parseExpressionList(self, TokenType.rbracket, alloc);

    const array_literal = ast.ArrayLiteral{ .token = token, .elements = elements };
    return Expression{ .array_literal = array_literal };
}

fn parseExpressionList(
    self: *Parser,
    end: TokenType,
    alloc: Allocator,
) ParserError![]const Expression {
    var list: std.ArrayListUnmanaged(Expression) = .empty;

    if (self.peekTokenIs(end)) {
        self.nextToken();
        return try list.toOwnedSlice(alloc);
    }

    self.nextToken();
    const first = try self.parseExpression(alloc, Precedence.lowest);
    try list.append(alloc, first);

    while (self.peekTokenIs(TokenType.comma)) {
        self.nextToken();
        self.nextToken();

        const next = try self.parseExpression(alloc, Precedence.lowest);
        try list.append(alloc, next);
    }

    if (!self.expectPeek(end)) {
        return ParserError.MissingRightBracket;
    }

    return try list.toOwnedSlice(alloc);
}

fn parseIndexExpression(
    self: *Parser,
    alloc: Allocator,
    left: Expression,
) ParserError!Expression {
    const token = self.cur_token;
    self.nextToken();

    const index = try alloc.create(Expression);
    errdefer alloc.destroy(index);
    index.* = try self.parseExpression(alloc, Precedence.lowest);

    if (!self.expectPeek(TokenType.rbracket)) {
        return ParserError.MissingRightBracket;
    }

    const left_ptr = try alloc.create(Expression);
    errdefer alloc.destroy(left_ptr);
    left_ptr.* = left;

    return Expression{
        .index_expression = ast.IndexExpression{
            .token = token,
            .index = index,
            .left = left_ptr,
        },
    };
}

fn parseHashLiteral(self: *Parser, alloc: Allocator) ParserError!Expression {
    const token = self.cur_token;

    var pairs = Expression.HashMap.empty;

    while (!self.peekTokenIs(TokenType.rbrace)) {
        self.nextToken();
        const key = try self.parseExpression(alloc, Precedence.lowest);

        if (!self.expectPeek(TokenType.colon)) {
            return ParserError.MissingColon;
        }

        self.nextToken();
        const value = try self.parseExpression(alloc, Precedence.lowest);

        try pairs.put(alloc, key, value);

        if (!self.peekTokenIs(TokenType.rbrace) and !self.expectPeek(TokenType.comma)) {
            return ParserError.MissingRightBrace;
        }
    }

    if (!self.expectPeek(TokenType.rbrace)) {
        return ParserError.MissingRightBrace;
    }

    return Expression{ .hash_literal = ast.HashLiteral{ .pairs = pairs, .token = token } };
}

// Helper Methods

fn nextToken(self: *Parser) void {
    self.cur_token = self.peek_token;
    self.peek_token = self.lexer.nextToken();
}

fn curTokenIs(self: *const Parser, t: TokenType) bool {
    return self.cur_token.token_type == t;
}

fn peekTokenIs(self: *const Parser, t: TokenType) bool {
    return self.peek_token.token_type == t;
}

fn expectPeek(self: *Parser, token: TokenType) bool {
    if (self.peekTokenIs(token)) {
        self.nextToken();
        return true;
    } else {
        return false;
    }
}

fn handleParserError(self: *Parser, alloc: Allocator, err: ParserError) !void {
    switch (err) {
        ParserError.MissingLeftBrace,
        ParserError.MissingLeftParenthesis,
        ParserError.MissingRightBrace,
        ParserError.MissingRightBracket,
        ParserError.MissingRightParenthesis,
        ParserError.MissingColon,
        ParserError.UnknownOperatorToken,
        => {},
        ParserError.MissingLetAssign => {
            const msg = try std.fmt.allocPrint(
                alloc,
                "expected next token to be TokenType.assign, got TokenType.{s} instead",
                .{@tagName(self.peek_token.token_type)},
            );
            errdefer alloc.free(msg);
            try self.errors.append(alloc, msg);
        },
        ParserError.MissingLetIdentifier => {
            const msg = try std.fmt.allocPrint(
                alloc,
                "expected next token to be TokenType.identifier, got TokenType.{s} instead",
                .{@tagName(self.peek_token.token_type)},
            );
            errdefer alloc.free(msg);
            try self.errors.append(alloc, msg);
        },
        ParserError.UnknownPrefixToken => {
            const msg = try std.fmt.allocPrint(
                alloc,
                "no prefix parse function for {any} found",
                .{self.cur_token.token_type},
            );
            errdefer alloc.free(msg);
            try self.errors.append(alloc, msg);
        },
        ParserError.InvalidCharacter, ParserError.Overflow => {
            const msg = try std.fmt.allocPrint(
                alloc,
                "could not parse {s} as integer",
                .{self.cur_token.literal},
            );
            errdefer alloc.free(msg);
            try self.errors.append(alloc, msg);
        },
        ParserError.OutOfMemory => return ParserError.OutOfMemory,
    }
}

fn peekPrecedence(self: *const Parser) Precedence {
    return getPrecedence(self.peek_token.token_type);
}

fn curPrecedence(self: *const Parser) Precedence {
    return getPrecedence(self.cur_token.token_type);
}

fn lookupPrefixParseFns(token_type: TokenType) ?PrefixParseFn {
    return switch (token_type) {
        .identifier => parseIdentifier,
        .int => parseIntegerLiteral,
        .bang, .minus => parsePrefixExpression,
        ._true, ._false => parseBoolean,
        .lparen => parseGroupedExpression,
        ._if => parseIfExpression,
        ._function => parseFunctionLiteral,
        .string => parseStringLiteral,
        .lbracket => parseArrayLiteral,
        .lbrace => parseHashLiteral,
        else => null,
    };
}

fn lookupInfixParseFns(token_type: TokenType) ?InfixParseFn {
    return switch (token_type) {
        .plus, .minus, .slash, .asterisk, .eq, .not_eq, .lt, .gt => parseInfixExpression,
        .lparen => parseCallExpression,
        .lbracket => parseIndexExpression,
        else => null,
    };
}

// Test Suite

test "Let Statement" {
    const let_tests = .{
        .{ .input = "let x = 5;", .expected_identifier = "x", .expected_value = 5 },
        .{ .input = "let y = true;", .expected_identifier = "y", .expected_value = true },
        .{ .input = "let foobar = y;", .expected_identifier = "foobar", .expected_value = "y" },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    inline for (let_tests) |let_test| {
        var lexer = Lexer.init(let_test.input);
        var parser = try Parser.init(&lexer, arena.allocator());
        const program = try parser.parseProgram(arena.allocator());
        try checkParserErrors(&parser);

        const stmts = program.statements;
        try testing.expectEqual(1, stmts.len);
        try testLetStatement(stmts[0], let_test.expected_identifier);
        const let_stmt = switch (stmts[0]) {
            .let_statement => |let_stmt| let_stmt,
            else => |other| {
                const fmt = "Expect LetStatement, got {s}";
                const type_name = @typeName(@TypeOf(other));
                @panic(std.fmt.comptimePrint(fmt, .{type_name}));
            },
        };
        try testLiteralExpression(let_stmt.value, let_test.expected_value);
    }
}

test "Return Statement" {
    const return_tests = .{
        .{ .input = "return 5;", .expected_value = 5 },
        .{ .input = "return true;", .expected_value = true },
        .{ .input = "return foobar;", .expected_value = "foobar" },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    inline for (return_tests) |return_test| {
        var lexer = Lexer.init(return_test.input);
        var parser = try Parser.init(&lexer, arena.allocator());
        const program = try parser.parseProgram(arena.allocator());
        _ = try parser.parseProgram(testing.allocator);
        try checkParserErrors(&parser);

        const stmts = program.statements;
        try testing.expectEqual(1, stmts.len);

        const return_stmt = switch (stmts[0]) {
            .return_statement => |ret_stmt| ret_stmt,
            else => |other| {
                const fmt = "Expect ReturnStatement, got {s}";
                const type_name = @typeName(@TypeOf(other));
                @panic(std.fmt.comptimePrint(fmt, .{type_name}));
            },
        };
        try testing.expectEqualStrings("return", return_stmt.tokenLiteral());
        try testLiteralExpression(return_stmt.return_value, return_test.expected_value);
    }
}

test "Identifier Expression" {
    const input = "foobar;";

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, arena.allocator());
    const program = try parser.parseProgram(arena.allocator());
    _ = try parser.parseProgram(testing.allocator);
    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);

    const stmt = switch (program.statements[0]) {
        .expression_statement => |stmt| stmt,
        else => |stmt| {
            const fmt = "Expect ExpressionStatement, got {s}";
            const type_name = @typeName(@TypeOf(stmt));
            @panic(std.fmt.comptimePrint(fmt, .{type_name}));
        },
    };
    const ident = switch (stmt.expression) {
        .identifier => |ident| ident,
        else => |ident| {
            const fmt = "Expect Identifier, got {s}";
            const type_name = @typeName(@TypeOf(ident));
            @panic(std.fmt.comptimePrint(fmt, .{type_name}));
        },
    };

    try testing.expectEqualStrings("foobar", ident.value);
    try testing.expectEqualStrings("foobar", ident.tokenLiteral());
}

test "Integer Literal Expression" {
    const input = "5;";

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, arena.allocator());
    const program = try parser.parseProgram(arena.allocator());
    _ = try parser.parseProgram(testing.allocator);
    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);

    const stmt = switch (program.statements[0]) {
        .expression_statement => |stmt| stmt,
        else => |stmt| {
            const fmt = "Expect ExpressionStatement, got {s}";
            const type_name = @typeName(@TypeOf(stmt));
            @panic(std.fmt.comptimePrint(fmt, .{type_name}));
        },
    };
    const literal = switch (stmt.expression) {
        .integer_literal => |literal| literal,
        else => |exp| {
            const fmt = "Expect Integer Literal, got {s}";
            const type_name = @typeName(@TypeOf(exp));
            @panic(std.fmt.comptimePrint(fmt, .{type_name}));
        },
    };

    try testing.expectEqual(5, literal.value);
    try testing.expectEqualStrings("5", literal.tokenLiteral());
}

test "Prefix Expression" {
    const prefix_tests = .{
        .{ .input = "!5;", .operator = Operator.bang, .value = 5 },
        .{ .input = "-15;", .operator = Operator.minus, .value = 15 },
        .{ .input = "!true;", .operator = Operator.bang, .value = true },
        .{ .input = "!false;", .operator = Operator.bang, .value = false },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    inline for (prefix_tests) |prefix_test| {
        var lexer = Lexer.init(prefix_test.input);
        var parser = try Parser.init(&lexer, arena.allocator());
        const program = try parser.parseProgram(arena.allocator());
        _ = try parser.parseProgram(testing.allocator);
        try checkParserErrors(&parser);

        const stmt = program.statements;
        try testing.expectEqual(1, stmt.len);

        const prefix_stmt: ExpressionStatement = switch (stmt[0]) {
            .expression_statement => |exp_stmt| exp_stmt,
            else => @panic("statement is not ExpressionStatement"),
        };
        const exp: ast.PrefixExpression = switch (prefix_stmt.expression) {
            .prefix_expression => |prefix| prefix,
            else => @panic("statement was not ast.PrefixExpression"),
        };

        try testing.expectEqual(prefix_test.operator, exp.operator);
        try testLiteralExpression(exp.right.*, prefix_test.value);
    }
}

test "Infix Expression" {
    const infixBoolTests = .{
        .{
            .input = "5 + 5;",
            .left_value = 5,
            .operator = Operator.plus,
            .right_value = 5,
        },
        .{
            .input = "5 - 5;",
            .left_value = 5,
            .operator = Operator.minus,
            .right_value = 5,
        },
        .{
            .input = "5 * 5;",
            .left_value = 5,
            .operator = Operator.asterisk,
            .right_value = 5,
        },
        .{
            .input = "5 / 5;",
            .left_value = 5,
            .operator = Operator.slash,
            .right_value = 5,
        },
        .{
            .input = "5 > 5;",
            .left_value = 5,
            .operator = Operator.gt,
            .right_value = 5,
        },
        .{
            .input = "5 < 5;",
            .left_value = 5,
            .operator = Operator.lt,
            .right_value = 5,
        },
        .{
            .input = "5 == 5;",
            .left_value = 5,
            .operator = Operator.eq,
            .right_value = 5,
        },
        .{
            .input = "5 != 5;",
            .left_value = 5,
            .operator = Operator.not_eq,
            .right_value = 5,
        },
        .{
            .input = "true == true",
            .left_value = true,
            .operator = Operator.eq,
            .right_value = true,
        },
        .{
            .input = "true != false",
            .left_value = true,
            .operator = Operator.not_eq,
            .right_value = false,
        },
        .{
            .input = "false == false",
            .left_value = false,
            .operator = Operator.eq,
            .right_value = false,
        },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    inline for (infixBoolTests) |infix_test| {
        var lexer = Lexer.init(infix_test.input);
        var parser = try Parser.init(&lexer, arena.allocator());
        const program = try parser.parseProgram(arena.allocator());
        _ = try parser.parseProgram(testing.allocator);

        try checkParserErrors(&parser);

        const stmts = program.statements;
        try testing.expectEqual(1, stmts.len);

        const exp_stmt = switch (stmts[0]) {
            .expression_statement => |exp_stmt| exp_stmt,
            else => @panic("stmts[0] is not ExpressionStatement"),
        };

        try testInfixExpression(
            exp_stmt.expression,
            infix_test.left_value,
            infix_test.operator,
            infix_test.right_value,
        );
    }
}

test "Operator Precedence" {
    const string_tests = [_]struct {
        input: []const u8,
        expected: []const u8,
    }{
        .{
            .input = "-a * b",
            .expected = "((-a) * b)",
        },
        .{
            .input = "!-a",
            .expected = "(!(-a))",
        },
        .{
            .input = "a + b + c",
            .expected = "((a + b) + c)",
        },
        .{
            .input = "a + b - c",
            .expected = "((a + b) - c)",
        },
        .{
            .input = "a * b * c",
            .expected = "((a * b) * c)",
        },
        .{
            .input = "a * b / c",
            .expected = "((a * b) / c)",
        },
        .{
            .input = "a + b / c",
            .expected = "(a + (b / c))",
        },
        .{
            .input = "a + b * c + d / e - f",
            .expected = "(((a + (b * c)) + (d / e)) - f)",
        },
        .{
            .input = "3 + 4; -5 * 5",
            .expected = "(3 + 4)((-5) * 5)",
        },
        .{
            .input = "5 > 4 == 3 < 4",
            .expected = "((5 > 4) == (3 < 4))",
        },
        .{
            .input = "5 < 4 != 3 > 4",
            .expected = "((5 < 4) != (3 > 4))",
        },
        .{
            .input = "3 + 4 * 5 == 3 * 1 + 4 * 5",
            .expected = "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        },
        .{
            .input = "true",
            .expected = "true",
        },
        .{
            .input = "false",
            .expected = "false",
        },
        .{
            .input = "3 > 5 == false",
            .expected = "((3 > 5) == false)",
        },
        .{
            .input = "3 < 5 == true",
            .expected = "((3 < 5) == true)",
        },
        .{
            .input = "1 + (2 + 3) + 4",
            .expected = "((1 + (2 + 3)) + 4)",
        },
        .{
            .input = "(5 + 5) * 2",
            .expected = "((5 + 5) * 2)",
        },
        .{
            .input = "2 / (5 + 5)",
            .expected = "(2 / (5 + 5))",
        },
        .{
            .input = "(5 + 5) * 2 * (5 + 5)",
            .expected = "(((5 + 5) * 2) * (5 + 5))",
        },
        .{
            .input = "-(5 + 5)",
            .expected = "(-(5 + 5))",
        },
        .{
            .input = "!(true == true)",
            .expected = "(!(true == true))",
        },
        .{
            .input = "a + add(b * c) + d",
            .expected = "((a + add((b * c))) + d)",
        },
        .{
            .input = "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            .expected = "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        },
        .{
            .input = "add(a + b + c * d / f + g)",
            .expected = "add((((a + b) + ((c * d) / f)) + g))",
        },
        .{
            .input = "a * [1, 2, 3, 4][b * c] *d",
            .expected = "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        },
        .{
            .input = "add(a * b[2], b[1], 2 * [1, 2][1])",
            .expected = "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
        },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    var array_list = std.ArrayList(u8).init(arena.allocator());
    const writer = array_list.writer().any();
    for (string_tests) |string_test| {
        var lexer = Lexer.init(string_test.input);
        var parser = try Parser.init(&lexer, arena.allocator());
        const program = try parser.parseProgram(arena.allocator());
        try checkParserErrors(&parser);

        try program.string(writer);
        try testing.expectEqualStrings(string_test.expected, array_list.items);

        array_list.clearRetainingCapacity();
    }
}

test "Boolean Expression" {
    const bool_tests = [_]struct {
        input: []const u8,
        expected_boolean: bool,
    }{
        .{ .input = "true;", .expected_boolean = true },
        .{ .input = "false;", .expected_boolean = false },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    for (bool_tests) |bool_test| {
        var lexer = Lexer.init(bool_test.input);
        var parser = try Parser.init(&lexer, arena.allocator());
        const program = try parser.parseProgram(arena.allocator());
        try checkParserErrors(&parser);

        const stmts = program.statements;
        try testing.expectEqual(1, stmts.len);

        const boolean_expression = switch (stmts[0]) {
            .expression_statement => |exp_stmt| switch (exp_stmt.expression) {
                .boolean_expression => |boolean| boolean,
                else => |other| {
                    const fmt = "exp not ast.BooleanExpression. got={s}\n";
                    const type_name = @typeName(@TypeOf(other));
                    @panic(std.fmt.comptimePrint(fmt, .{type_name}));
                },
            },
            else => unreachable,
        };
        try testing.expectEqual(bool_test.expected_boolean, boolean_expression.value);
    }
}

test "If Expression" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const input = "if (x < y) { x }";
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, arena.allocator());
    const program = try parser.parseProgram(arena.allocator());
    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);

    const exp_stmt = switch (program.statements[0]) {
        .expression_statement => |exp_stmt| exp_stmt,
        else => @panic("stmts[0] is not ExpressionStatement"),
    };
    const if_exp = switch (exp_stmt.expression) {
        .if_expression => |if_exp| if_exp,
        else => |other| {
            const fmt = "Expect IfExpression, got {s}";
            const type_name = @typeName(@TypeOf(other));
            @panic(std.fmt.comptimePrint(fmt, .{type_name}));
        },
    };

    try testInfixExpression(if_exp.condition.*, "x", Operator.lt, "y");

    try testing.expectEqual(1, if_exp.consequence.statements.len);
    const consequence = switch (if_exp.consequence.statements[0]) {
        .expression_statement => |con_stmt| con_stmt,
        else => @panic("if_exp.consequence.statements[0] is not ExpressionStatement"),
    };
    try testIdentifier(consequence.expression, "x");
    try testing.expectEqual(if_exp.alternative, null);
}

test "If Else Expression" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const input = "if (x < y) { x } else { y }";
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, arena.allocator());
    const program = try parser.parseProgram(arena.allocator());
    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);

    const exp_stmt = switch (program.statements[0]) {
        .expression_statement => |exp_stmt| exp_stmt,
        else => @panic("stmts[0] is not ExpressionStatement"),
    };

    const if_exp = switch (exp_stmt.expression) {
        .if_expression => |if_exp| if_exp,
        else => |other| {
            const fmt = "Expect IfExpression, got {s}";
            const type_name = @typeName(@TypeOf(other));
            @panic(std.fmt.comptimePrint(fmt, .{type_name}));
        },
    };

    try testInfixExpression(if_exp.condition.*, "x", Operator.lt, "y");

    try testing.expectEqual(1, if_exp.consequence.statements.len);
    const consequence = switch (if_exp.consequence.statements[0]) {
        .expression_statement => |con_stmt| con_stmt,
        else => @panic("if_exp.consequence.statements[0] is not ExpressionStatement"),
    };
    try testIdentifier(consequence.expression, "x");

    const alternative = if_exp.alternative orelse return error.TestExpectedEqual;
    try testing.expectEqual(1, alternative.statements.len);
    const alt_stmt = switch (alternative.statements[0]) {
        .expression_statement => |alt_stmt| alt_stmt,
        else => @panic("if_exp.alternative.statements[0] is not ExpressionStatement"),
    };
    try testIdentifier(alt_stmt.expression, "y");
}

test "Function Literal Expression" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const input = "fn(x, y) { x + y }";
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, arena.allocator());
    const program = try parser.parseProgram(arena.allocator());
    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);

    const fn_lit_exp_stmt = switch (program.statements[0]) {
        .expression_statement => |exp_stmt| exp_stmt,
        else => @panic("stmts[0] is not ExpressionStatement"),
    };

    const fn_lit = switch (fn_lit_exp_stmt.expression) {
        .function_literal => |fn_lit| fn_lit,
        else => |other| {
            const fmt = "Expect FunctionLiteral, got {s}";
            const type_name = @typeName(@TypeOf(other));
            @panic(std.fmt.comptimePrint(fmt, .{type_name}));
        },
    };

    try testing.expectEqual(2, fn_lit.parameters.len);

    try testLiteralExpression(Expression{ .identifier = fn_lit.parameters[0] }, "x");
    try testLiteralExpression(Expression{ .identifier = fn_lit.parameters[1] }, "y");

    try testing.expectEqual(1, fn_lit.body.statements.len);

    const body_exp_stmt = switch (fn_lit.body.statements[0]) {
        .expression_statement => |exp_stmt| exp_stmt,
        else => @panic("stmts[0] is not ExpressionStatement"),
    };
    try testInfixExpression(body_exp_stmt.expression, "x", Operator.plus, "y");
}

test "Function Parameter Parsing" {
    const param_tests = [_]struct {
        input: []const u8,
        expected_params: []const []const u8,
    }{
        .{
            .input = "fn() {};",
            .expected_params = &[_][]const u8{},
        },
        .{
            .input = "fn(x) {};",
            .expected_params = &[_][]const u8{"x"},
        },
        .{
            .input = "fn(x, y, z) {};",
            .expected_params = &[_][]const u8{ "x", "y", "z" },
        },
    };

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    for (param_tests) |param_test| {
        var lexer = Lexer.init(param_test.input);
        var parser = try Parser.init(&lexer, arena.allocator());
        const program = try parser.parseProgram(arena.allocator());
        try checkParserErrors(&parser);

        const stmt = switch (program.statements[0]) {
            .expression_statement => |stmt| stmt,
            else => @panic("stmts[0] is not ExpressionStatement"),
        };
        const function = switch (stmt.expression) {
            .function_literal => |function| function,
            else => |other| {
                const fmt = "Expect FunctionLiteral, got {s}";
                const type_name = @typeName(@TypeOf(other));
                @panic(std.fmt.comptimePrint(fmt, .{type_name}));
            },
        };
        try testing.expectEqual(param_test.expected_params.len, function.parameters.len);
        for (param_test.expected_params, 0..) |param, i| {
            const exp = Expression{ .identifier = function.parameters[i] };
            try testLiteralExpression(exp, param);
        }
    }
}

test "Call Expression Parsing" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const input = "add(1, 2 * 3, 4 + 5);";
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, arena.allocator());
    const program = try parser.parseProgram(arena.allocator());
    try checkParserErrors(&parser);

    const stmts = program.statements;
    try testing.expectEqual(1, stmts.len);

    const exp_stmt = switch (stmts[0]) {
        .expression_statement => |exp_stmt| exp_stmt,
        else => |other| {
            const fmt = "Expect ExpressionStatement, got {s}";
            const type_name = @typeName(@TypeOf(other));
            @panic(std.fmt.comptimePrint(fmt, .{type_name}));
        },
    };
    const call_exp = switch (exp_stmt.expression) {
        .call_expression => |call_exp| call_exp,
        else => |other| {
            const fmt = "Expect CallExpression, got {s}";
            const type_name = @typeName(@TypeOf(other));
            @panic(std.fmt.comptimePrint(fmt, .{type_name}));
        },
    };
    try testIdentifier(call_exp.function.*, "add");

    const args = call_exp.arguments;
    try testing.expectEqual(3, args.len);
    try testLiteralExpression(args[0], 1);
    try testInfixExpression(args[1], 2, Operator.asterisk, 3);
    try testInfixExpression(args[2], 4, Operator.plus, 5);
}

test "String Literal Expression" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const input = "\"hello world\"";
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, arena.allocator());
    const program = try parser.parseProgram(arena.allocator());
    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);
    const stmt = switch (program.statements[0]) {
        .expression_statement => |exp_stmt| exp_stmt,
        else => @panic("stmts[0] is not ExpressionStatement"),
    };

    try testing.expect(stmt.expression == .string_literal);
    const literal = stmt.expression.string_literal;

    try testing.expectEqualStrings("hello world", literal.token.literal);
}

test "Array Literal" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const input = "[1, 2 * 2, 3 + 3]";
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, arena.allocator());
    const program = try parser.parseProgram(arena.allocator());
    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);
    const expression = switch (program.statements[0]) {
        .expression_statement => |exp_stmt| exp_stmt.expression,
        else => @panic("stmts[0] is not ExpressionStatement"),
    };

    const array = switch (expression) {
        .array_literal => |arr| arr,
        else => @panic("exp not ast.ArrayLiteral"),
    };

    try testing.expectEqual(3, array.elements.len);
    try testIntegerLiteral(array.elements[0], 1);
    try testInfixExpression(array.elements[1], 2, Operator.asterisk, 2);
    try testInfixExpression(array.elements[2], 3, Operator.plus, 3);
}

test "Index Expression" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const input = "myArray[1 + 1]";
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, arena.allocator());
    const program = try parser.parseProgram(arena.allocator());
    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);
    const exp_stmt = switch (program.statements[0]) {
        .expression_statement => |exp_stmt| exp_stmt,
        else => @panic("stmts[0] is not ExpressionStatement"),
    };

    try testing.expect(exp_stmt.expression == .index_expression);
    const index_exp = exp_stmt.expression.index_expression;

    try testIdentifier(index_exp.left.*, "myArray");
    try testInfixExpression(index_exp.index.*, 1, Operator.plus, 1);
}

test "Hash Literals, no Keys" {
    var arena_allocator = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    const input = "{}";
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, arena);
    const program = try parser.parseProgram(arena);
    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);
    const exp_stmt = switch (program.statements[0]) {
        .expression_statement => |exp_stmt| exp_stmt,
        else => @panic("stmts[0] is not ExpressionStatement"),
    };

    try testing.expect(exp_stmt.expression == .hash_literal);
    const hash = exp_stmt.expression.hash_literal;

    try testing.expectEqual(0, hash.pairs.size);
}

test "Hash Literals, String Keys" {
    var arena_allocator = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    const input = "{\"one\": 1, \"two\": 2, \"three\": 3}";
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, arena);
    const program = try parser.parseProgram(arena);
    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);
    const exp_stmt = switch (program.statements[0]) {
        .expression_statement => |exp_stmt| exp_stmt,
        else => @panic("stmts[0] is not ExpressionStatement"),
    };

    try testing.expect(exp_stmt.expression == .hash_literal);
    const hash = exp_stmt.expression.hash_literal;

    try testing.expectEqual(3, hash.pairs.size);

    var expected = std.StaticStringMap(i64).initComptime(.{
        .{ "one", 1 },
        .{ "two", 2 },
        .{ "three", 3 },
    });

    var iter = hash.pairs.iterator();
    while (iter.next()) |entry| {
        try testing.expect(entry.key_ptr.* == .string_literal);
        const expected_value = expected.get(entry.key_ptr.*.string_literal.token.literal) orelse
            return error.TestExpectedEqual;

        try testIntegerLiteral(entry.value_ptr.*, expected_value);
    }
}

test "Hash Literals, Boolean Keys" {
    var arena_allocator = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    const input = "{false: 0, true: 1}";
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, arena);
    const program = try parser.parseProgram(arena);
    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);
    const exp_stmt = switch (program.statements[0]) {
        .expression_statement => |exp_stmt| exp_stmt,
        else => @panic("stmts[0] is not ExpressionStatement"),
    };

    try testing.expect(exp_stmt.expression == .hash_literal);
    const hash = exp_stmt.expression.hash_literal;

    try testing.expectEqual(2, hash.pairs.size);

    var expected_map = std.AutoHashMapUnmanaged(bool, i64).empty;
    try expected_map.put(arena, false, 0);
    try expected_map.put(arena, true, 1);

    var iter = hash.pairs.iterator();
    while (iter.next()) |entry| {
        try testing.expect(entry.key_ptr.* == .boolean_expression);

        const expected = expected_map.get(entry.key_ptr.*.boolean_expression.value) orelse
            return error.TestExpectedEqual;

        try testing.expect(entry.value_ptr.* == .integer_literal);
        try testing.expectEqual(expected, entry.value_ptr.*.integer_literal.value);
    }
}

test "Hash Literals, Integer Keys" {
    var arena_allocator = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    const input = "{1: \"one\", 2: \"two\", 3: \"three\"}";
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, arena);
    const program = try parser.parseProgram(arena);
    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);
    const exp_stmt = switch (program.statements[0]) {
        .expression_statement => |exp_stmt| exp_stmt,
        else => @panic("stmts[0] is not ExpressionStatement"),
    };

    try testing.expect(exp_stmt.expression == .hash_literal);
    const hash = exp_stmt.expression.hash_literal;

    try testing.expectEqual(3, hash.pairs.size);

    var expected_map = std.AutoHashMapUnmanaged(i64, []const u8).empty;
    try expected_map.put(arena, 1, "one");
    try expected_map.put(arena, 2, "two");
    try expected_map.put(arena, 3, "three");

    var iter = hash.pairs.iterator();
    while (iter.next()) |entry| {
        try testing.expect(entry.key_ptr.* == .integer_literal);

        const expected = expected_map.get(entry.key_ptr.*.integer_literal.value) orelse
            return error.TestExpectedEqual;

        try testing.expect(entry.value_ptr.* == .string_literal);
        try testing.expectEqualStrings(expected, entry.value_ptr.*.string_literal.token.literal);
    }
}

//Test Helpers

fn testOutOfMemory(alloc: Allocator, input: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, arena.allocator());
    _ = try parser.parseProgram(arena.allocator());
}

fn testOutOfMemoryWithParserErrors(
    alloc: Allocator,
    input: []const u8,
    expecteds: []const []const u8,
) !void {
    var arena = std.heap.ArenaAllocator.init(alloc);
    defer arena.deinit();
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, arena.allocator());
    _ = try parser.parseProgram(arena.allocator());

    const parser_errors = parser.errors.items;
    try testing.expectEqual(expecteds.len, parser_errors.len);
    for (parser_errors, 0..) |parser_error, i| {
        try testing.expectEqualStrings(expecteds[i], parser_error);
    }
}

fn checkParserErrors(p: *const Parser) !void {
    const errors = p.errors.items;
    if (errors.len == 0) {
        return;
    }

    std.debug.print("parser has {d} errors\n", .{errors.len});
    for (errors) |msg| {
        std.debug.print("parser error: {s}\n", .{msg});
    }
    return error.FoundParserErrors;
}

fn testLetStatement(s: ast.Statement, value: []const u8) !void {
    try testing.expectEqualStrings(s.tokenLiteral(), "let");

    var let_stmt: ast.LetStatement = switch (s) {
        .let_statement => s.let_statement,
        else => {
            std.debug.print("s is not *ast.LetStatement. got={s}", .{@typeName(@TypeOf(s))});
            return error.TestExpectedEqual;
        },
    };

    try testing.expectEqualStrings(value, let_stmt.name.value);

    try testing.expectEqualStrings(value, let_stmt.name.tokenLiteral());
}

fn testIntegerLiteral(il: Expression, value: i64) !void {
    const integ = switch (il) {
        .integer_literal => |integer| integer,
        else => |exp| {
            std.debug.print("il not ast.IntegerLiteral. got={s}\n", .{@typeName(@TypeOf(exp))});
            return error.TestExpectedEqual;
        },
    };

    try testing.expectEqual(value, integ.value);
    const integer_literal = std.fmt.allocPrint(
        testing.allocator,
        "{d}",
        .{value},
    ) catch return error.TestExpectedEqual;
    defer testing.allocator.free(integer_literal);
    try testing.expectEqualStrings(integer_literal, integ.tokenLiteral());
}

fn testIdentifier(expression: Expression, value: []const u8) !void {
    const ident = switch (expression) {
        .identifier => |ident| ident,
        else => |exp| {
            std.debug.print("il not ast.Identifier. got={s}\n", .{@typeName(@TypeOf(exp))});
            return error.TestExpectedEqual;
        },
    };

    try testing.expectEqualStrings(ident.value, value);

    try testing.expectEqualStrings(ident.tokenLiteral(), value);
}

fn testLiteralExpression(exp: Expression, value: anytype) !void {
    const type_info = @typeInfo(@TypeOf(value));
    // std.debug.print("{}\n", .{type_info});
    switch (type_info) {
        .comptime_int, .int => try testIntegerLiteral(exp, value),
        .bool => try testBooleanLiteral(exp, value),
        .pointer => |pointer| switch (pointer.size) {
            .one, .slice => try testIdentifier(exp, value),
            else => return error.TestExpectedEqual,
        },
        .array => try testIdentifier(exp, &value),
        else => return error.TestExpectedEqual,
    }
}

fn testInfixExpression(exp: Expression, left: anytype, operator: Operator, right: anytype) !void {
    const op_exp: ast.InfixExpression = switch (exp) {
        .infix_expression => |infix| infix,
        else => |e| {
            std.debug.print("exp not ast.InfixExpression. got={s}\n", .{@typeName(@TypeOf(e))});
            return error.TestExpectedEqual;
        },
    };

    try testLiteralExpression(op_exp.left.*, left);

    try testing.expectEqual(op_exp.operator, operator);

    try testLiteralExpression(op_exp.right.*, right);
}

fn testBooleanLiteral(exp: Expression, value: bool) !void {
    const bool_exp: ast.Boolean = switch (exp) {
        .boolean_expression => |boolean| boolean,
        else => |e| {
            std.debug.print("exp not ast.Boolean. got={s}\n", .{@typeName(@TypeOf(e))});
            return error.TestExpectedEqual;
        },
    };

    try testing.expectEqual(bool_exp.value, value);

    const actual = try std.fmt.allocPrint(testing.allocator, "{any}", .{bool_exp.value});
    defer testing.allocator.free(actual);
    try testing.expectEqualStrings(bool_exp.tokenLiteral(), actual);
}

test {
    std.testing.refAllDecls(Parser);
}
