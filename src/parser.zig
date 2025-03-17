const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Lexer = @import("lexer.zig").Lexer;
const token = @import("token.zig");

const ParserError = error{
    MissingLetAssign,
    MissingLetIdentifier,
    MissingLeftBrace,
    MissingLeftParenthesis,
    MissingRightBrace,
    MissingRightParenthesis,
    UnknownPrefixToken,
    UnknownOperatorToken,
} || Allocator.Error || std.fmt.AllocPrintError || std.fmt.ParseIntError;

const PrefixParseFn = *const fn (self: *Parser, alloc: Allocator) ParserError!ast.Expression;
const InfixParseFn = *const fn (
    self: *Parser,
    alloc: Allocator,
    lhs: ast.Expression,
) ParserError!ast.Expression;

const Precedence = enum {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
};

const precedences = std.StaticStringMap(Precedence).initComptime(.{
    .{ token.EQ, Precedence.EQUALS },
    .{ token.NOT_EQ, Precedence.EQUALS },
    .{ token.LT, Precedence.LESSGREATER },
    .{ token.GT, Precedence.LESSGREATER },
    .{ token.PLUS, Precedence.SUM },
    .{ token.MINUS, Precedence.SUM },
    .{ token.SLASH, Precedence.PRODUCT },
    .{ token.ASTERISK, Precedence.PRODUCT },
    .{ token.LPAREN, Precedence.CALL },
});

pub const Parser = struct {
    lexer: *Lexer,
    cur_token: token.Token,
    peek_token: token.Token,
    errors: std.ArrayListUnmanaged([]const u8),
    prefix_parse_fns: std.StringHashMapUnmanaged(PrefixParseFn),
    infix_parse_fns: std.StringHashMapUnmanaged(InfixParseFn),

    // Initialization

    pub fn init(lexer: *Lexer, alloc: Allocator) ParserError!Parser {
        var p = Parser{
            .lexer = lexer,
            .cur_token = undefined,
            .peek_token = undefined,
            .errors = .empty,
            .prefix_parse_fns = .empty,
            .infix_parse_fns = .empty,
        };
        errdefer p.deinit(alloc);

        try p.prefix_parse_fns.put(alloc, token.IDENT, parseIdentifier);
        try p.prefix_parse_fns.put(alloc, token.INT, parseIntegerLiteral);
        try p.prefix_parse_fns.put(alloc, token.BANG, parsePrefixExpression);
        try p.prefix_parse_fns.put(alloc, token.MINUS, parsePrefixExpression);
        try p.prefix_parse_fns.put(alloc, token.TRUE, parseBoolean);
        try p.prefix_parse_fns.put(alloc, token.FALSE, parseBoolean);
        try p.prefix_parse_fns.put(alloc, token.LPAREN, parseGroupedExpression);
        try p.prefix_parse_fns.put(alloc, token.IF, parseIfExpression);
        try p.prefix_parse_fns.put(alloc, token.FUNCTION, parseFunctionLiteral);

        try p.infix_parse_fns.put(alloc, token.PLUS, parseInfixExpression);
        try p.infix_parse_fns.put(alloc, token.MINUS, parseInfixExpression);
        try p.infix_parse_fns.put(alloc, token.SLASH, parseInfixExpression);
        try p.infix_parse_fns.put(alloc, token.ASTERISK, parseInfixExpression);
        try p.infix_parse_fns.put(alloc, token.EQ, parseInfixExpression);
        try p.infix_parse_fns.put(alloc, token.NOT_EQ, parseInfixExpression);
        try p.infix_parse_fns.put(alloc, token.LT, parseInfixExpression);
        try p.infix_parse_fns.put(alloc, token.GT, parseInfixExpression);
        try p.infix_parse_fns.put(alloc, token.LPAREN, parseCallExpression);

        p.nextToken();
        p.nextToken();

        return p;
    }

    pub fn deinit(self: *Parser, alloc: Allocator) void {
        for (self.errors.items) |msg| {
            alloc.free(msg);
        }

        self.infix_parse_fns.deinit(alloc);
        self.prefix_parse_fns.deinit(alloc);

        self.errors.deinit(alloc);
    }

    // Program

    pub fn parseProgram(self: *Parser, alloc: Allocator) ParserError!ast.Program {
        var list = std.ArrayListUnmanaged(ast.Statement).empty;
        errdefer {
            for (list.items) |stmt| {
                stmt.parser_deinit(alloc);
            }
            list.deinit(alloc);
        }

        while (!std.mem.eql(u8, self.cur_token._type, token.EOF)) {
            const maybe_stmt = self.parseStatement(alloc);
            errdefer {
                if (maybe_stmt) |stmt| {
                    stmt.parser_deinit(alloc);
                } else |_| {}
            }

            if (maybe_stmt) |stmt| {
                try list.append(alloc, stmt);
            } else |err| switch (err) {
                ParserError.UnknownPrefixToken,
                ParserError.MissingLetIdentifier,
                ParserError.MissingLetAssign,
                ParserError.MissingLeftParenthesis,
                ParserError.MissingRightParenthesis,
                ParserError.MissingLeftBrace,
                ParserError.MissingRightBrace,
                ParserError.UnknownOperatorToken,
                => {},
                ParserError.InvalidCharacter, ParserError.Overflow => {
                    const fmt = "could not parse {s} as integer";
                    const args = .{self.cur_token.literal};
                    const msg = try std.fmt.allocPrint(alloc, fmt, args);
                    errdefer alloc.free(msg);
                    try self.errors.append(alloc, msg);
                },
                ParserError.OutOfMemory => return ParserError.OutOfMemory,
            }

            self.nextToken();
        }

        return ast.Program{ .statements = try list.toOwnedSlice(alloc) };
    }

    // Statement

    fn parseStatement(self: *Parser, alloc: Allocator) ParserError!ast.Statement {
        if (std.mem.eql(u8, token.LET, self.cur_token._type)) {
            const let_statement = try self.parseLetStatement(alloc);
            return ast.Statement{ .let_statement = let_statement };
        } else if (std.mem.eql(u8, token.RETURN, self.cur_token._type)) {
            const return_statement = try self.parseReturnStatement(alloc);
            return ast.Statement{ .return_statement = return_statement };
        } else {
            const expression_statement = try self.parseExpressionStatement(alloc);
            return ast.Statement{ .expression_statement = expression_statement };
        }
    }

    fn parseLetStatement(self: *Parser, alloc: Allocator) ParserError!ast.LetStatement {
        const let_token = self.cur_token;

        if (!self.expectPeek(token.IDENT)) {
            try self.peekErrors(alloc, token.IDENT);
            return ParserError.MissingLetIdentifier;
        }

        const name = ast.Identifier{
            ._token = self.cur_token,
            .value = self.cur_token.literal,
        };

        if (!self.expectPeek(token.ASSIGN)) {
            try self.peekErrors(alloc, token.ASSIGN);
            return ParserError.MissingLetAssign;
        }

        self.nextToken();

        const value = try self.parseExpression(alloc, Precedence.LOWEST);

        if (self.peekTokenIs(token.SEMICOLON)) {
            self.nextToken();
        }

        return ast.LetStatement{ ._token = let_token, .name = name, .value = value };
    }

    pub fn parseReturnStatement(self: *Parser, alloc: Allocator) ParserError!ast.ReturnStatement {
        const _token = self.cur_token;

        self.nextToken();

        const return_value = try self.parseExpression(alloc, Precedence.LOWEST);

        if (self.peekTokenIs(token.SEMICOLON)) {
            self.nextToken();
        }

        return ast.ReturnStatement{ ._token = _token, .return_value = return_value };
    }

    pub fn parseExpressionStatement(self: *Parser, alloc: Allocator) ParserError!ast.ExpressionStatement {
        const cur_token = self.cur_token;
        const expression = try self.parseExpression(alloc, Precedence.LOWEST);

        if (self.peekTokenIs(token.SEMICOLON)) {
            self.nextToken();
        }

        return ast.ExpressionStatement{ ._token = cur_token, .expression = expression };
    }

    pub fn parseBlockStatement(self: *Parser, alloc: Allocator) ParserError!ast.BlockStatement {
        const _token = self.cur_token;
        var statements = std.ArrayListUnmanaged(ast.Statement).empty;
        errdefer {
            for (statements.items) |stmt| {
                stmt.parser_deinit(alloc);
            }
            statements.deinit(alloc);
        }
        self.nextToken();

        while (!self.curTokenIs(token.RBRACE) and
            !self.curTokenIs(token.EOF)) : (self.nextToken())
        {
            const statement = try self.parseStatement(alloc);
            errdefer statement.parser_deinit(alloc);
            try statements.append(alloc, statement);
        }

        return ast.BlockStatement{
            ._token = _token,
            .statements = try statements.toOwnedSlice(alloc),
        };
    }

    // Expression

    pub fn parseExpression(self: *Parser, alloc: Allocator, precedence: Precedence) ParserError!ast.Expression {
        const precedence_int = @intFromEnum(precedence);
        const prefix = self.prefix_parse_fns.get(self.cur_token._type) orelse {
            try self.noPrefixParseFnError(alloc, self.cur_token._type);
            return ParserError.UnknownPrefixToken;
        };

        var left_exp = try prefix(self, alloc);

        while (!self.peekTokenIs(token.SEMICOLON) and
            precedence_int < @intFromEnum(self.peekPrecedence()))
        {
            errdefer left_exp.parser_deinit(alloc);

            const infix = self.infix_parse_fns.get(self.peek_token._type) orelse return left_exp;
            self.nextToken();
            left_exp = try infix(self, alloc, left_exp);
        }

        return left_exp;
    }

    fn parseIdentifier(self: *const Parser, _: Allocator) ParserError!ast.Expression {
        return ast.Expression{
            .identifier = ast.Identifier{
                ._token = self.cur_token,
                .value = self.cur_token.literal,
            },
        };
    }

    fn parseIntegerLiteral(self: *Parser, _: Allocator) ParserError!ast.Expression {
        const cur_token = self.cur_token;
        const value = try std.fmt.parseInt(i64, cur_token.literal, 10);

        const integer_literal = ast.IntegerLiteral{ ._token = cur_token, .value = value };
        return ast.Expression{ .integer_literal = integer_literal };
    }

    fn parsePrefixExpression(self: *Parser, alloc: Allocator) ParserError!ast.Expression {
        const _token = self.cur_token;
        const operator = token.lookupOperatorLiteral(self.cur_token.literal) orelse
            return ParserError.UnknownOperatorToken;

        self.nextToken();

        const right = try alloc.create(ast.Expression);
        errdefer alloc.destroy(right);
        right.* = try self.parseExpression(alloc, Precedence.PREFIX);

        return ast.Expression{
            .prefix_expression = ast.PrefixExpression{
                ._token = _token,
                .operator = operator,
                .right = right,
            },
        };
    }

    fn parseInfixExpression(self: *Parser, alloc: Allocator, lhs: ast.Expression) ParserError!ast.Expression {
        const _token = self.cur_token;
        const operator = token.lookupOperatorLiteral(self.cur_token.literal) orelse
            return ParserError.UnknownOperatorToken;

        const precedence = self.curPrecedence();
        self.nextToken();

        const left = try alloc.create(ast.Expression);
        errdefer alloc.destroy(left);
        left.* = lhs;

        const right = try alloc.create(ast.Expression);
        errdefer alloc.destroy(right);
        right.* = try self.parseExpression(alloc, precedence);

        return ast.Expression{
            .infix_expression = ast.InfixExpression{
                ._token = _token,
                .left = left,
                .operator = operator,
                .right = right,
            },
        };
    }

    fn parseBoolean(self: *Parser, _: Allocator) ParserError!ast.Expression {
        return ast.Expression{
            .boolean_expression = ast.Boolean{
                ._token = self.cur_token,
                .value = self.curTokenIs(token.TRUE),
            },
        };
    }

    fn parseGroupedExpression(self: *Parser, alloc: Allocator) ParserError!ast.Expression {
        self.nextToken();

        const exp = try self.parseExpression(alloc, Precedence.LOWEST);

        if (!self.expectPeek(token.RPAREN)) {
            return ParserError.MissingRightParenthesis;
        }

        return exp;
    }

    fn parseIfExpression(self: *Parser, alloc: Allocator) ParserError!ast.Expression {
        const _token = self.cur_token;
        if (!self.expectPeek(token.LPAREN)) {
            return ParserError.MissingLeftParenthesis;
        }
        self.nextToken();

        const condition = try alloc.create(ast.Expression);
        errdefer alloc.destroy(condition);
        condition.* = try self.parseExpression(alloc, Precedence.LOWEST);
        errdefer condition.parser_deinit(alloc);

        if (!self.expectPeek(token.RPAREN)) {
            return ParserError.MissingRightParenthesis;
        }
        if (!self.expectPeek(token.LBRACE)) {
            return ParserError.MissingLeftBrace;
        }

        const consequence = try alloc.create(ast.BlockStatement);
        errdefer alloc.destroy(consequence);
        consequence.* = try self.parseBlockStatement(alloc);
        errdefer consequence.parser_deinit(alloc);

        var maybe_alt: ?*ast.BlockStatement = null;
        if (self.peekTokenIs(token.ELSE)) {
            self.nextToken();

            if (!self.expectPeek(token.LBRACE)) {
                return ParserError.MissingLeftBrace;
            }
            maybe_alt = try alloc.create(ast.BlockStatement);
            errdefer alloc.destroy(maybe_alt.?);
            maybe_alt.?.* = try self.parseBlockStatement(alloc);
            // errdefer maybe_alt.?.parser_deinit(alloc);
        }

        return ast.Expression{
            .if_expression = ast.IfExpression{
                ._token = _token,
                .condition = condition,
                .consequence = consequence,
                .alternative = maybe_alt,
            },
        };
    }

    fn parseFunctionLiteral(self: *Parser, alloc: Allocator) ParserError!ast.Expression {
        const _token = self.cur_token;
        if (!self.expectPeek(token.LPAREN)) {
            return ParserError.MissingLeftParenthesis;
        }
        const parameters = try self.parseFunctionParameters(alloc);
        errdefer alloc.free(parameters);

        if (!self.expectPeek(token.LBRACE)) {
            return error.MissingLeftBrace;
        }
        const body = try self.parseBlockStatement(alloc);

        return ast.Expression{
            .function_literal = ast.FunctionLiteral{
                ._token = _token,
                .parameters = parameters,
                .body = body,
            },
        };
    }

    fn parseFunctionParameters(self: *Parser, alloc: Allocator) ParserError![]ast.Identifier {
        var identifiers = std.ArrayListUnmanaged(ast.Identifier).empty;
        errdefer identifiers.deinit(alloc);

        if (self.peekTokenIs(token.RPAREN)) {
            self.nextToken();
            return try identifiers.toOwnedSlice(alloc);
        }

        self.nextToken();

        try identifiers.append(alloc, ast.Identifier{
            ._token = self.cur_token,
            .value = self.cur_token.literal,
        });

        while (self.peekTokenIs(token.COMMA)) {
            self.nextToken();
            self.nextToken();
            try identifiers.append(alloc, ast.Identifier{
                ._token = self.cur_token,
                .value = self.cur_token.literal,
            });
        }

        if (!self.expectPeek(token.RPAREN)) {
            return ParserError.MissingRightParenthesis;
        }

        return try identifiers.toOwnedSlice(alloc);
    }

    fn parseCallExpression(
        self: *Parser,
        alloc: Allocator,
        lhs: ast.Expression, // Identifier or Function Literal
    ) ParserError!ast.Expression {
        const function = try alloc.create(ast.Expression);
        errdefer alloc.destroy(function);
        function.* = lhs;
        const _token = self.cur_token;

        const arguments = try self.parseCallArguments(alloc);
        return ast.Expression{
            .call_expression = ast.CallExpression{
                ._token = _token,
                .function = function,
                .arguments = arguments,
            },
        };
    }

    fn parseCallArguments(self: *Parser, alloc: Allocator) ParserError![]const ast.Expression {
        var args = std.ArrayListUnmanaged(ast.Expression).empty;
        errdefer {
            if (args.items.len > 0) for (args.items[1..]) |arg| {
                arg.parser_deinit(alloc);
            };
            args.deinit(alloc);
        }

        if (self.peekTokenIs(token.RPAREN)) {
            self.nextToken();
            return try args.toOwnedSlice(alloc);
        }

        self.nextToken();

        const first_exp = try self.parseExpression(alloc, Precedence.LOWEST);
        errdefer first_exp.parser_deinit(alloc);
        try args.append(alloc, first_exp);

        while (self.peekTokenIs(token.COMMA)) {
            self.nextToken();
            self.nextToken();
            const loop_exp = try self.parseExpression(alloc, Precedence.LOWEST);
            errdefer loop_exp.parser_deinit(alloc);
            try args.append(alloc, loop_exp);
        }

        if (!self.expectPeek(token.RPAREN)) {
            return ParserError.MissingRightParenthesis;
        }

        return try args.toOwnedSlice(alloc);
    }

    // Helper Methods

    fn nextToken(self: *Parser) void {
        self.cur_token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
    }

    fn curTokenIs(self: *const Parser, t: token.TokenType) bool {
        return std.mem.eql(u8, self.cur_token._type, t);
    }

    fn peekTokenIs(self: *const Parser, t: token.TokenType) bool {
        return std.mem.eql(u8, self.peek_token._type, t);
    }

    fn expectPeek(self: *Parser, _token: token.TokenType) bool {
        if (self.peekTokenIs(_token)) {
            self.nextToken();
            return true;
        } else {
            return false;
        }
    }

    fn peekErrors(self: *Parser, alloc: Allocator, t: token.TokenType) !void {
        const fmt = "expected next token to be {s}, got {s} instead";
        const msg = try std.fmt.allocPrint(alloc, fmt, .{ t, self.peek_token._type });
        errdefer alloc.free(msg);
        try self.errors.append(alloc, msg);
    }

    fn noPrefixParseFnError(self: *Parser, alloc: Allocator, t: token.TokenType) !void {
        const fmt = "no prefix parse function for {s} found";
        const msg = try std.fmt.allocPrint(alloc, fmt, .{t});
        errdefer alloc.free(msg);
        try self.errors.append(alloc, msg);
    }

    fn peekPrecedence(self: *const Parser) Precedence {
        return precedences.get(self.peek_token._type) orelse Precedence.LOWEST;
    }

    fn curPrecedence(self: *const Parser) Precedence {
        return precedences.get(self.cur_token._type) orelse Precedence.LOWEST;
    }
};

// Test Suite

test "Out of Memory, no Parser errors" {
    const inputs = [_][]const u8{
        "let x = 5;",
        "let x = 2 + 3;",
        "let x = (2 + 3) * 1;",
        "foobar;",
        "return x;",
        "5;",
        "!5;",
        "-5;",
        "!true;",
        "!false;",
        "5 + 5;",
        "5 == 5;",
        "5 != 5;",
        "true == true;",
        "true != false;",
        "if (x < y) { 1 } else { (2 + 3) * 4 };",
        "if (x < y) { (1 + 2) * 3 } else { 4 };",
        "fn(x, y) { x + y };",
        "fn(x, y) { (1 + 2) * 3 };",
        "add(1);",
        "add((1 + 2) * 3);",
        "add(1, (2 + 3) * 4);",
    };
    for (inputs) |input| {
        try testing.checkAllAllocationFailures(
            testing.allocator,
            testOutOfMemory,
            .{input},
        );
    }
}

test "Out of Memory, with Parser errors" {
    const input =
        \\let x 5;
        \\let = 10;
        \\let 838383
    ;

    const expecteds = [_][]const u8{
        "expected next token to be =, got INT instead",
        "expected next token to be IDENT, got = instead",
        "no prefix parse function for = found",
        "expected next token to be IDENT, got INT instead",
    };

    try testing.checkAllAllocationFailures(
        testing.allocator,
        testOutOfMemoryWithParserErrors,
        .{ input, &expecteds },
    );
}

test "Let Statement" {
    const let_tests = .{
        .{ .input = "let x = 5;", .expected_identifier = "x", .expected_value = 5 },
        .{ .input = "let y = true;", .expected_identifier = "y", .expected_value = true },
        .{ .input = "let foobar = y;", .expected_identifier = "foobar", .expected_value = "y" },
    };

    inline for (let_tests) |let_test| {
        var l = Lexer.init(let_test.input);
        var parser = try Parser.init(&l, testing.allocator);
        defer parser.deinit(testing.allocator);
        const program = try parser.parseProgram(testing.allocator);
        defer program.parser_deinit(testing.allocator);
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

    inline for (return_tests) |return_test| {
        var l = Lexer.init(return_test.input);
        var parser = try Parser.init(&l, testing.allocator);
        defer parser.deinit(testing.allocator);
        const program = try parser.parseProgram(testing.allocator);
        defer program.parser_deinit(testing.allocator);
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

    var l = Lexer.init(input);
    var parser = try Parser.init(&l, testing.allocator);
    defer parser.deinit(testing.allocator);
    const program = try parser.parseProgram(testing.allocator);
    defer program.parser_deinit(testing.allocator);
    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);

    const stmt: ast.ExpressionStatement = switch (program.statements[0]) {
        .expression_statement => |stmt| stmt,
        else => |stmt| {
            const fmt = "Expect ExpressionStatement, got {s}";
            const type_name = @typeName(@TypeOf(stmt));
            @panic(std.fmt.comptimePrint(fmt, .{type_name}));
        },
    };
    const ident: ast.Identifier = switch (stmt.expression) {
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

    var l = Lexer.init(input);
    var parser = try Parser.init(&l, testing.allocator);
    defer parser.deinit(testing.allocator);
    const program = try parser.parseProgram(testing.allocator);
    defer program.parser_deinit(testing.allocator);
    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);

    const stmt: ast.ExpressionStatement = switch (program.statements[0]) {
        .expression_statement => |stmt| stmt,
        else => |stmt| {
            const fmt = "Expect ExpressionStatement, got {s}";
            const type_name = @typeName(@TypeOf(stmt));
            @panic(std.fmt.comptimePrint(fmt, .{type_name}));
        },
    };
    const literal: ast.IntegerLiteral = switch (stmt.expression) {
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
        .{ .input = "!5;", .operator = token.Operator.bang, .value = 5 },
        .{ .input = "-15;", .operator = token.Operator.minus, .value = 15 },
        .{ .input = "!true;", .operator = token.Operator.bang, .value = true },
        .{ .input = "!false;", .operator = token.Operator.bang, .value = false },
    };

    inline for (prefix_tests) |prefix_test| {
        var l = Lexer.init(prefix_test.input);
        var parser = try Parser.init(&l, testing.allocator);
        defer parser.deinit(testing.allocator);
        const program = try parser.parseProgram(testing.allocator);
        defer program.parser_deinit(testing.allocator);
        try checkParserErrors(&parser);

        const stmt = program.statements;
        try testing.expectEqual(1, stmt.len);

        const prefix_stmt: ast.ExpressionStatement = switch (stmt[0]) {
            .expression_statement => |exp_stmt| exp_stmt,
            else => @panic("statement is not ast.ExpressionStatement"),
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
            .operator = token.Operator.plus,
            .right_value = 5,
        },
        .{
            .input = "5 - 5;",
            .left_value = 5,
            .operator = token.Operator.minus,
            .right_value = 5,
        },
        .{
            .input = "5 * 5;",
            .left_value = 5,
            .operator = token.Operator.asterisk,
            .right_value = 5,
        },
        .{
            .input = "5 / 5;",
            .left_value = 5,
            .operator = token.Operator.slash,
            .right_value = 5,
        },
        .{
            .input = "5 > 5;",
            .left_value = 5,
            .operator = token.Operator.gt,
            .right_value = 5,
        },
        .{
            .input = "5 < 5;",
            .left_value = 5,
            .operator = token.Operator.lt,
            .right_value = 5,
        },
        .{
            .input = "5 == 5;",
            .left_value = 5,
            .operator = token.Operator.eq,
            .right_value = 5,
        },
        .{
            .input = "5 != 5;",
            .left_value = 5,
            .operator = token.Operator.not_eq,
            .right_value = 5,
        },
        .{
            .input = "true == true",
            .left_value = true,
            .operator = token.Operator.eq,
            .right_value = true,
        },
        .{
            .input = "true != false",
            .left_value = true,
            .operator = token.Operator.not_eq,
            .right_value = false,
        },
        .{
            .input = "false == false",
            .left_value = false,
            .operator = token.Operator.eq,
            .right_value = false,
        },
    };

    inline for (infixBoolTests) |infix_test| {
        var l = Lexer.init(infix_test.input);
        var parser = try Parser.init(&l, testing.allocator);
        defer parser.deinit(testing.allocator);
        const program = try parser.parseProgram(testing.allocator);
        defer program.parser_deinit(testing.allocator);

        try checkParserErrors(&parser);

        const stmts = program.statements;
        try testing.expectEqual(1, stmts.len);

        const exp_stmt = switch (stmts[0]) {
            .expression_statement => |exp_stmt| exp_stmt,
            else => @panic("stmts[0] is not ast.ExpressionStatement"),
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
    };

    var array_list = std.ArrayList(u8).init(testing.allocator);
    defer array_list.deinit();
    const writer = array_list.writer().any();
    for (string_tests) |string_test| {
        var lexer = Lexer.init(string_test.input);
        var parser = try Parser.init(&lexer, testing.allocator);
        defer parser.deinit(testing.allocator);
        const program = try parser.parseProgram(testing.allocator);
        defer program.parser_deinit(testing.allocator);
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

    for (bool_tests) |bool_test| {
        var lexer = Lexer.init(bool_test.input);
        var parser = try Parser.init(&lexer, testing.allocator);
        defer parser.deinit(testing.allocator);
        const program = try parser.parseProgram(testing.allocator);
        defer program.parser_deinit(testing.allocator);
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
    const input = "if (abc < y) { abc }";
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, testing.allocator);
    defer parser.deinit(testing.allocator);
    const program = try parser.parseProgram(testing.allocator);
    defer program.parser_deinit(testing.allocator);
    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);

    const exp_stmt = switch (program.statements[0]) {
        .expression_statement => |exp_stmt| exp_stmt,
        else => @panic("stmts[0] is not ast.ExpressionStatement"),
    };

    const if_exp = switch (exp_stmt.expression) {
        .if_expression => |if_exp| if_exp,
        else => |other| {
            const fmt = "Expect IfExpression, got {s}";
            const type_name = @typeName(@TypeOf(other));
            @panic(std.fmt.comptimePrint(fmt, .{type_name}));
        },
    };

    const abc = [3]u8{ 'a', 'b', 'c' };
    try testInfixExpression(if_exp.condition.*, abc, token.Operator.lt, "y");

    try testing.expectEqual(1, if_exp.consequence.statements.len);
    const consequence = switch (if_exp.consequence.statements[0]) {
        .expression_statement => |con_stmt| con_stmt,
        else => @panic("if_exp.consequence.statements[0] is not ast.ExpressionStatement"),
    };
    try testIdentifier(consequence.expression, "abc");
    try testing.expectEqual(if_exp.alternative, null);
}

test "If Else Expression" {
    const input = "if (x < y) { x } else { y }";
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, testing.allocator);
    defer parser.deinit(testing.allocator);
    const program = try parser.parseProgram(testing.allocator);
    defer program.parser_deinit(testing.allocator);
    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);

    const exp_stmt = switch (program.statements[0]) {
        .expression_statement => |exp_stmt| exp_stmt,
        else => @panic("stmts[0] is not ast.ExpressionStatement"),
    };

    const if_exp = switch (exp_stmt.expression) {
        .if_expression => |if_exp| if_exp,
        else => |other| {
            const fmt = "Expect IfExpression, got {s}";
            const type_name = @typeName(@TypeOf(other));
            @panic(std.fmt.comptimePrint(fmt, .{type_name}));
        },
    };

    try testInfixExpression(if_exp.condition.*, "x", token.Operator.lt, "y");

    try testing.expectEqual(1, if_exp.consequence.statements.len);
    const consequence = switch (if_exp.consequence.statements[0]) {
        .expression_statement => |con_stmt| con_stmt,
        else => @panic("if_exp.consequence.statements[0] is not ast.ExpressionStatement"),
    };
    try testIdentifier(consequence.expression, "x");

    const alternative = if_exp.alternative orelse return error.TestExpectedEqual;
    try testing.expectEqual(1, alternative.statements.len);
    const alt_stmt = switch (alternative.statements[0]) {
        .expression_statement => |alt_stmt| alt_stmt,
        else => @panic("if_exp.alternative.statements[0] is not ast.ExpressionStatement"),
    };
    try testIdentifier(alt_stmt.expression, "y");
}

test "Function Literal Expression" {
    const input = "fn(x, y) { x + y }";
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, testing.allocator);
    defer parser.deinit(testing.allocator);
    const program = try parser.parseProgram(testing.allocator);
    defer program.parser_deinit(testing.allocator);
    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);

    const fn_lit_exp_stmt = switch (program.statements[0]) {
        .expression_statement => |exp_stmt| exp_stmt,
        else => @panic("stmts[0] is not ast.ExpressionStatement"),
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

    try testLiteralExpression(ast.Expression{ .identifier = fn_lit.parameters[0] }, "x");
    try testLiteralExpression(ast.Expression{ .identifier = fn_lit.parameters[1] }, "y");

    try testing.expectEqual(1, fn_lit.body.statements.len);

    const body_exp_stmt: ast.ExpressionStatement = switch (fn_lit.body.statements[0]) {
        .expression_statement => |exp_stmt| exp_stmt,
        else => @panic("stmts[0] is not ast.ExpressionStatement"),
    };
    try testInfixExpression(body_exp_stmt.expression, "x", token.Operator.plus, "y");
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
    for (param_tests) |param_test| {
        var lexer = Lexer.init(param_test.input);
        var parser = try Parser.init(&lexer, testing.allocator);
        defer parser.deinit(testing.allocator);
        const program = try parser.parseProgram(testing.allocator);
        defer program.parser_deinit(testing.allocator);
        try checkParserErrors(&parser);

        const stmt = switch (program.statements[0]) {
            .expression_statement => |stmt| stmt,
            else => @panic("stmts[0] is not ast.ExpressionStatement"),
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
            const exp = ast.Expression{ .identifier = function.parameters[i] };
            try testLiteralExpression(exp, param);
        }
    }
}

test "Call Expression Parsing" {
    const input = "add(1, 2 * 3, 4 + 5);";
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, testing.allocator);
    defer parser.deinit(testing.allocator);
    const program = try parser.parseProgram(testing.allocator);
    defer program.parser_deinit(testing.allocator);
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
    try testInfixExpression(args[1], 2, token.Operator.asterisk, 3);
    try testInfixExpression(args[2], 4, token.Operator.plus, 5);
}

//Test Helpers

fn testOutOfMemory(allocator: Allocator, input: []const u8) !void {
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit(testing.allocator);
    const program = try parser.parseProgram(testing.allocator);
    defer program.parser_deinit(testing.allocator);
}

fn testOutOfMemoryWithParserErrors(
    allocator: Allocator,
    input: []const u8,
    expecteds: []const []const u8,
) !void {
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit(testing.allocator);

    const program = try parser.parseProgram(testing.allocator);
    defer program.parser_deinit(testing.allocator);

    const parser_errors = parser.errors.items;

    try testing.expect(expecteds.len == parser_errors.len);

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

fn testIntegerLiteral(il: ast.Expression, value: i64) !void {
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

fn testIdentifier(expression: ast.Expression, value: []const u8) !void {
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

fn testLiteralExpression(exp: ast.Expression, value: anytype) !void {
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

fn testInfixExpression(
    exp: ast.Expression,
    left: anytype,
    operator: token.Operator,
    right: anytype,
) !void {
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

fn testBooleanLiteral(exp: ast.Expression, value: bool) !void {
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
