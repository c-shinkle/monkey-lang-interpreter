const std = @import("std");
const testing = std.testing;

const ast = @import("ast.zig");
const Lexer = @import("lexer.zig").Lexer;
const token = @import("token.zig");

const ProgramError = std.mem.Allocator.Error || std.fmt.AllocPrintError;
const StatementError = error{ MissingLetAssign, MissingLetIdentifier } || std.mem.Allocator.Error;
const ExpressionError = error{
    MissingLeftBrace,
    MissingLeftParenthesis,
    MissingRightBrace,
    MissingRightParenthesis,
    UnknownPrefixToken,
} || StatementError || std.fmt.ParseIntError || std.mem.Allocator.Error;

const PrefixParseFn = *const fn (self: *Parser) ExpressionError!ast.Expression;
const InfixParseFn = *const fn (self: *Parser, lhs: ast.Expression) ExpressionError!ast.Expression;

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

const Parser = struct {
    lexer: *Lexer,
    cur_token: token.Token,
    peek_token: token.Token,
    allocator: std.mem.Allocator,
    errors: std.ArrayList([]const u8),
    prefix_parse_fns: std.StringHashMap(PrefixParseFn),
    infix_parse_fns: std.StringHashMap(InfixParseFn),

    // Initialization

    pub fn init(lexer: *Lexer, allocator: std.mem.Allocator) std.mem.Allocator.Error!Parser {
        var p = Parser{
            .lexer = lexer,
            .cur_token = undefined,
            .peek_token = undefined,
            .allocator = allocator,
            .errors = std.ArrayList([]const u8).init(allocator),
            .prefix_parse_fns = std.StringHashMap(PrefixParseFn).init(allocator),
            .infix_parse_fns = std.StringHashMap(InfixParseFn).init(allocator),
        };
        errdefer p.deinit();

        try p.prefix_parse_fns.put(token.IDENT, parseIdentifier);
        try p.prefix_parse_fns.put(token.INT, parseIntegerLiteral);
        try p.prefix_parse_fns.put(token.BANG, parsePrefixExpression);
        try p.prefix_parse_fns.put(token.MINUS, parsePrefixExpression);
        try p.prefix_parse_fns.put(token.TRUE, parseBoolean);
        try p.prefix_parse_fns.put(token.FALSE, parseBoolean);
        try p.prefix_parse_fns.put(token.LPAREN, parseGroupedExpression);
        try p.prefix_parse_fns.put(token.IF, parseIfExpression);
        try p.prefix_parse_fns.put(token.FUNCTION, parseFunctionLiteral);

        try p.infix_parse_fns.put(token.PLUS, parseInfixExpression);
        try p.infix_parse_fns.put(token.MINUS, parseInfixExpression);
        try p.infix_parse_fns.put(token.SLASH, parseInfixExpression);
        try p.infix_parse_fns.put(token.ASTERISK, parseInfixExpression);
        try p.infix_parse_fns.put(token.EQ, parseInfixExpression);
        try p.infix_parse_fns.put(token.NOT_EQ, parseInfixExpression);
        try p.infix_parse_fns.put(token.LT, parseInfixExpression);
        try p.infix_parse_fns.put(token.GT, parseInfixExpression);
        try p.infix_parse_fns.put(token.LPAREN, parseCallExpression);

        p.nextToken();
        p.nextToken();

        return p;
    }

    pub fn deinit(self: *Parser) void {
        for (self.errors.items) |msg| {
            self.allocator.free(msg);
        }

        self.infix_parse_fns.deinit();
        self.prefix_parse_fns.deinit();

        self.errors.deinit();
    }

    // Program

    fn parseProgram(self: *Parser) ProgramError!ast.Program {
        var list = std.ArrayList(ast.Statement).init(self.allocator);
        errdefer {
            for (list.items) |stmt| {
                stmt.deinit(self.allocator);
            }
            list.deinit();
        }

        while (!std.mem.eql(u8, self.cur_token._type, token.EOF)) {
            const maybe_stmt = self.parseStatement();
            errdefer {
                if (maybe_stmt) |stmt| {
                    stmt.deinit(self.allocator);
                } else |_| {}
            }

            if (maybe_stmt) |stmt| {
                try list.append(stmt);
            } else |err| switch (err) {
                ExpressionError.UnknownPrefixToken,
                ExpressionError.MissingLetIdentifier,
                ExpressionError.MissingLetAssign,
                ExpressionError.MissingLeftParenthesis,
                ExpressionError.MissingRightParenthesis,
                ExpressionError.MissingLeftBrace,
                ExpressionError.MissingRightBrace,
                => {},
                ExpressionError.InvalidCharacter, ExpressionError.Overflow => {
                    const fmt = "could not parse {s} as integer";
                    const msg = try std.fmt.allocPrint(self.allocator, fmt, .{self.cur_token.literal});
                    errdefer self.allocator.free(msg);
                    try self.errors.append(msg);
                },
                ExpressionError.OutOfMemory => return ProgramError.OutOfMemory,
            }

            self.nextToken();
        }

        return ast.Program{
            .allocator = self.allocator,
            .statements = try list.toOwnedSlice(),
        };
    }

    // Statement

    fn parseStatement(self: *Parser) ExpressionError!ast.Statement {
        if (std.mem.eql(u8, token.LET, self.cur_token._type)) {
            const let_statement = try self.parseLetStatement();
            return ast.Statement{ .let_statement = let_statement };
        } else if (std.mem.eql(u8, token.RETURN, self.cur_token._type)) {
            const return_statement = self.parseReturnStatement();
            return ast.Statement{ .return_statement = return_statement };
        } else {
            const expression_statement = try self.parseExpressionStatement();
            return ast.Statement{ .expression_statement = expression_statement };
        }
    }

    fn parseLetStatement(self: *Parser) StatementError!ast.LetStatement {
        const let_token = self.cur_token;

        if (!self.expectPeek(token.IDENT)) {
            try self.peekErrors(token.IDENT);
            return StatementError.MissingLetIdentifier;
        }

        const name = try self.allocator.create(ast.Identifier);
        errdefer self.allocator.destroy(name);
        name.* = ast.Identifier{
            ._token = self.cur_token,
            .value = self.cur_token.literal,
        };

        if (!self.expectPeek(token.ASSIGN)) {
            try self.peekErrors(token.ASSIGN);
            return StatementError.MissingLetAssign;
        }

        // TODO

        while (!self.curTokenIs(token.SEMICOLON)) {
            self.nextToken();
        }

        return ast.LetStatement{ ._token = let_token, .name = name, .value = null };
    }

    pub fn parseReturnStatement(self: *Parser) ast.ReturnStatement {
        const return_token = self.cur_token;

        self.nextToken();

        while (!self.curTokenIs(token.SEMICOLON)) {
            self.nextToken();
        }

        return ast.ReturnStatement{ ._token = return_token, .return_value = null };
    }

    pub fn parseExpressionStatement(self: *Parser) ExpressionError!ast.ExpressionStatement {
        const cur_token = self.cur_token;
        const exp = try self.parseExpression(Precedence.LOWEST);

        if (self.peekTokenIs(token.SEMICOLON)) {
            self.nextToken();
        }

        return ast.ExpressionStatement{ ._token = cur_token, .expression = exp };
    }

    pub fn parseBlockStatement(self: *Parser) ExpressionError!ast.BlockStatement {
        const _token = self.cur_token;
        var statements = std.ArrayList(ast.Statement).init(self.allocator);
        self.nextToken();

        while (!self.curTokenIs(token.RBRACE) and !self.curTokenIs(token.EOF)) : (self.nextToken()) {
            try statements.append(try self.parseStatement());
        }

        return ast.BlockStatement{ ._token = _token, .statements = try statements.toOwnedSlice() };
    }

    // Expression

    pub fn parseExpression(self: *Parser, precedence: Precedence) ExpressionError!ast.Expression {
        const precedence_int = @intFromEnum(precedence);
        const prefix = self.prefix_parse_fns.get(self.cur_token._type) orelse {
            try self.noPrefixParseFnError(self.cur_token._type);
            return ExpressionError.UnknownPrefixToken;
        };

        var left_exp = try prefix(self);

        while (!self.peekTokenIs(token.SEMICOLON) and precedence_int < @intFromEnum(self.peekPrecedence())) {
            const infix = self.infix_parse_fns.get(self.peek_token._type) orelse return left_exp;

            self.nextToken();

            left_exp = try infix(self, left_exp);
        }

        return left_exp;
    }

    fn parseIdentifier(self: *const Parser) ExpressionError!ast.Expression {
        const identifier = ast.Identifier{ ._token = self.cur_token, .value = self.cur_token.literal };
        return ast.Expression{ .identifier = identifier };
    }

    fn parseIntegerLiteral(self: *Parser) ExpressionError!ast.Expression {
        const cur_token = self.cur_token;
        const value = try std.fmt.parseInt(i64, cur_token.literal, 10);

        const integer_literal = ast.IntegerLiteral{ ._token = cur_token, .value = value };
        return ast.Expression{ .integer_literal = integer_literal };
    }

    fn parsePrefixExpression(self: *Parser) ExpressionError!ast.Expression {
        const _token = self.cur_token;
        const operator = self.cur_token.literal;

        self.nextToken();

        const right = try self.allocator.create(ast.Expression);
        errdefer self.allocator.destroy(right);
        right.* = try self.parseExpression(Precedence.PREFIX);

        return ast.Expression{ .prefix_expression = ast.PrefixExpression{
            ._token = _token,
            .operator = operator,
            .right = right,
        } };
    }

    fn parseInfixExpression(self: *Parser, lhs: ast.Expression) ExpressionError!ast.Expression {
        const _token = self.cur_token;
        const operator = self.cur_token.literal;

        const precedence = self.curPrecedence();
        self.nextToken();

        const left = try self.allocator.create(ast.Expression);
        errdefer self.allocator.destroy(left);
        left.* = lhs;

        const right = try self.allocator.create(ast.Expression);
        errdefer self.allocator.destroy(right);
        right.* = try self.parseExpression(precedence);

        return ast.Expression{ .infix_expression = ast.InfixExpression{
            ._token = _token,
            .left = left,
            .operator = operator,
            .right = right,
        } };
    }

    fn parseBoolean(self: *Parser) ExpressionError!ast.Expression {
        return ast.Expression{ .boolean_expression = ast.Boolean{
            ._token = self.cur_token,
            .value = self.curTokenIs(token.TRUE),
        } };
    }

    fn parseGroupedExpression(self: *Parser) ExpressionError!ast.Expression {
        self.nextToken();

        const exp = try self.parseExpression(Precedence.LOWEST);

        if (!self.expectPeek(token.RPAREN)) {
            return ExpressionError.MissingRightParenthesis;
        }

        return exp;
    }

    fn parseIfExpression(self: *Parser) ExpressionError!ast.Expression {
        const _token = self.cur_token;
        if (!self.expectPeek(token.LPAREN)) {
            return ExpressionError.MissingLeftParenthesis;
        }
        self.nextToken();

        const condition = try self.allocator.create(ast.Expression);
        errdefer self.allocator.destroy(condition);
        condition.* = try self.parseExpression(Precedence.LOWEST);

        if (!self.expectPeek(token.RPAREN)) {
            return ExpressionError.MissingRightParenthesis;
        }
        if (!self.expectPeek(token.LBRACE)) {
            return ExpressionError.MissingLeftBrace;
        }

        const consequence = try self.allocator.create(ast.BlockStatement);
        errdefer self.allocator.destroy(consequence);
        consequence.* = try self.parseBlockStatement();

        var maybe_alt: ?*ast.BlockStatement = null;
        if (self.peekTokenIs(token.ELSE)) {
            self.nextToken();

            if (!self.expectPeek(token.LBRACE)) {
                return ExpressionError.MissingLeftBrace;
            }
            maybe_alt = try self.allocator.create(ast.BlockStatement);
            errdefer self.allocator.destroy(maybe_alt.?);
            maybe_alt.?.* = try self.parseBlockStatement();
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

    fn parseFunctionLiteral(self: *Parser) ExpressionError!ast.Expression {
        const _token = self.cur_token;
        if (!self.expectPeek(token.LPAREN)) {
            return ExpressionError.MissingLeftParenthesis;
        }
        const parameters = try self.parseFunctionParameters();
        if (!self.expectPeek(token.LBRACE)) {
            return error.MissingLeftBrace;
        }
        const body = try self.allocator.create(ast.BlockStatement);
        errdefer self.allocator.destroy(body);
        body.* = try self.parseBlockStatement();

        return ast.Expression{
            .function_literal = ast.FunctionLiteral{
                ._token = _token,
                .parameters = parameters,
                .body = body,
            },
        };
    }

    fn parseFunctionParameters(self: *Parser) ExpressionError![]const *ast.Identifier {
        var identifiers = std.ArrayList(*ast.Identifier).init(self.allocator);

        if (self.peekTokenIs(token.RPAREN)) {
            self.nextToken();
            return try identifiers.toOwnedSlice();
        }

        self.nextToken();

        const first_temp = try self.allocator.create(ast.Identifier);
        errdefer self.allocator.destroy(first_temp);
        first_temp.* = ast.Identifier{
            ._token = self.cur_token,
            .value = self.cur_token.literal,
        };

        try identifiers.append(first_temp);

        while (self.peekTokenIs(token.COMMA)) {
            self.nextToken();
            self.nextToken();
            const loop_temp = try self.allocator.create(ast.Identifier);
            errdefer self.allocator.destroy(loop_temp);
            loop_temp.* = ast.Identifier{
                ._token = self.cur_token,
                .value = self.cur_token.literal,
            };
            try identifiers.append(loop_temp);
        }

        if (!self.expectPeek(token.RPAREN)) {
            return ExpressionError.MissingRightParenthesis;
        }

        return try identifiers.toOwnedSlice();
    }

    fn parseCallExpression(self: *Parser, lhs: ast.Expression) ExpressionError!ast.Expression {
        const function = try self.allocator.create(ast.Expression);
        errdefer self.allocator.destroy(function);
        function.* = lhs;
        const _token = self.cur_token;

        const arguments = try self.parseCallArguments();
        return ast.Expression{
            .call_expression = ast.CallExpression{
                ._token = _token,
                .function = function,
                .arguments = arguments,
            },
        };
    }

    fn parseCallArguments(self: *Parser) ExpressionError![]const *ast.Expression {
        var args = std.ArrayList(*ast.Expression).init(self.allocator);

        if (self.peekTokenIs(token.RPAREN)) {
            self.nextToken();
            return try args.toOwnedSlice();
        }

        self.nextToken();

        const first_temp = try self.allocator.create(ast.Expression);
        errdefer self.allocator.destroy(first_temp);
        first_temp.* = try self.parseExpression(Precedence.LOWEST);
        try args.append(first_temp);

        while (self.peekTokenIs(token.COMMA)) {
            self.nextToken();
            self.nextToken();
            const loop_temp = try self.allocator.create(ast.Expression);
            errdefer self.allocator.destroy(loop_temp);
            loop_temp.* = try self.parseExpression(Precedence.LOWEST);
            try args.append(loop_temp);
        }

        if (!self.expectPeek(token.RPAREN)) {
            return ExpressionError.MissingRightParenthesis;
        }

        return try args.toOwnedSlice();
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

    fn peekErrors(self: *Parser, t: token.TokenType) !void {
        const fmt = "expected next token to be {s}, got {s} instead";
        const msg = try std.fmt.allocPrint(self.allocator, fmt, .{ t, self.peek_token._type });
        errdefer self.allocator.free(msg);
        try self.errors.append(msg);
    }

    fn noPrefixParseFnError(self: *Parser, t: token.TokenType) !void {
        const fmt = "no prefix parse function for {s} found";
        const msg = try std.fmt.allocPrint(self.allocator, fmt, .{t});
        errdefer self.allocator.free(msg);
        try self.errors.append(msg);
    }

    fn peekPrecedence(self: *const Parser) Precedence {
        return precedences.get(self.peek_token._type) orelse Precedence.LOWEST;
    }

    fn curPrecedence(self: *const Parser) Precedence {
        return precedences.get(self.cur_token._type) orelse Precedence.LOWEST;
    }
};

// Test Suite

test "Out of Memory, Program Statement, no Parser errors" {
    const input = "let x = 5;";

    const expecteds = [_]Expected{.{ .expected_identifiers = "x" }};

    try testing.checkAllAllocationFailures(testing.allocator, testOutOfMemory, .{ input, &expecteds });
}

test "Out of Memory, Program Statement, with Parser errors" {
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

    try testing.checkAllAllocationFailures(testing.allocator, testOutOfMemoryWithParserErrors, .{ input, &expecteds });
}

test "Let Statement" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;
    var l = Lexer.init(input);
    var parser = try Parser.init(&l, testing.allocator);
    defer parser.deinit();

    const program = try parser.parseProgram();
    defer program.deinit();
    try checkParserErrors(&parser);

    const len = 3;
    try testing.expectEqual(len, program.statements.len);

    const expecteds = [len]struct {
        expected_identifiers: []const u8,
    }{
        .{ .expected_identifiers = "x" },
        .{ .expected_identifiers = "y" },
        .{ .expected_identifiers = "foobar" },
    };

    for (expecteds, 0..) |expected, i| {
        try testLetStatement(program.statements[i], expected.expected_identifiers);
    }
}

test "Return Statement" {
    const input =
        \\return 5;
        \\return 10;
        \\return 993322;
    ;

    var l = Lexer.init(input);
    var parser = try Parser.init(&l, testing.allocator);
    defer parser.deinit();

    const program = try parser.parseProgram();
    defer program.deinit();
    try checkParserErrors(&parser);

    try testing.expectEqual(3, program.statements.len);

    for (program.statements) |stmt| {
        switch (stmt) {
            .return_statement => {
                try testing.expectEqualStrings("return", stmt.return_statement.tokenLiteral());
            },
            else => {
                std.debug.print("stmt is not *ast.ReturnStatement. got={s}\n", .{@typeName(@TypeOf(stmt))});
                continue;
            },
        }
    }
}

test "Identifier Expression" {
    const input = "foobar;";

    var l = Lexer.init(input);
    var parser = try Parser.init(&l, testing.allocator);
    defer parser.deinit();
    const program = try parser.parseProgram();
    defer program.deinit();
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
    const ident: ast.Identifier = switch (stmt.expression.?) {
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
    defer parser.deinit();
    const program = try parser.parseProgram();
    defer program.deinit();
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
    const literal: ast.IntegerLiteral = switch (stmt.expression.?) {
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
        .{ .input = "!5;", .operator = "!", .value = 5 },
        .{ .input = "-15;", .operator = "-", .value = 15 },
        .{ .input = "!true;", .operator = "!", .value = true },
        .{ .input = "!false;", .operator = "!", .value = false },
    };

    inline for (prefix_tests) |prefix_test| {
        var l = Lexer.init(prefix_test.input);
        var parser = try Parser.init(&l, testing.allocator);
        defer parser.deinit();
        const program = try parser.parseProgram();
        defer program.deinit();
        try checkParserErrors(&parser);

        const stmt = program.statements;
        try testing.expectEqual(1, stmt.len);

        const prefix_stmt: ast.ExpressionStatement = switch (stmt[0]) {
            .expression_statement => |exp_stmt| exp_stmt,
            else => @panic("statement is not ast.ExpressionStatement"),
        };
        const exp: ast.PrefixExpression = switch (prefix_stmt.expression.?) {
            .prefix_expression => |prefix| prefix,
            else => @panic("statement was not ast.PrefixExpression"),
        };

        try testing.expectEqualStrings(prefix_test.operator, exp.operator);
        try testLiteralExpression(exp.right.*, prefix_test.value);
    }
}

test "Infix Expression" {
    const infixBoolTests = .{
        .{ .input = "5 + 5;", .left_value = 5, .operator = "+", .right_value = 5 },
        .{ .input = "5 - 5;", .left_value = 5, .operator = "-", .right_value = 5 },
        .{ .input = "5 * 5;", .left_value = 5, .operator = "*", .right_value = 5 },
        .{ .input = "5 / 5;", .left_value = 5, .operator = "/", .right_value = 5 },
        .{ .input = "5 > 5;", .left_value = 5, .operator = ">", .right_value = 5 },
        .{ .input = "5 < 5;", .left_value = 5, .operator = "<", .right_value = 5 },
        .{ .input = "5 == 5;", .left_value = 5, .operator = "==", .right_value = 5 },
        .{ .input = "5 != 5;", .left_value = 5, .operator = "!=", .right_value = 5 },
        .{ .input = "true == true", .left_value = true, .operator = "==", .right_value = true },
        .{ .input = "true != false", .left_value = true, .operator = "!=", .right_value = false },
        .{ .input = "false == false", .left_value = false, .operator = "==", .right_value = false },
    };

    inline for (infixBoolTests) |infix_test| {
        var l = Lexer.init(infix_test.input);
        var parser = try Parser.init(&l, testing.allocator);
        defer parser.deinit();
        const program = try parser.parseProgram();
        defer program.deinit();

        try checkParserErrors(&parser);

        const stmts = program.statements;
        try testing.expectEqual(1, stmts.len);

        const exp_stmt = switch (stmts[0]) {
            .expression_statement => |exp_stmt| exp_stmt,
            else => @panic("stmts[0] is not ast.ExpressionStatement"),
        };

        try testInfixExpression(
            exp_stmt.expression.?,
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
    var writer = array_list.writer();
    for (string_tests) |string_test| {
        var lexer = Lexer.init(string_test.input);
        var parser = try Parser.init(&lexer, testing.allocator);
        defer parser.deinit();
        const program = try parser.parseProgram();
        defer program.deinit();
        try checkParserErrors(&parser);

        try program.string(&writer);
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
        defer parser.deinit();
        const program = try parser.parseProgram();
        defer program.deinit();
        try checkParserErrors(&parser);

        const stmts = program.statements;
        try testing.expectEqual(1, stmts.len);

        const boolean_expression = switch (stmts[0]) {
            .expression_statement => |exp_stmt| switch (exp_stmt.expression.?) {
                .boolean_expression => |boolean| boolean,
                else => |e| {
                    std.debug.print("exp not ast.BooleanExpression. got={s}\n", .{@typeName(@TypeOf(e))});
                    unreachable;
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
    defer parser.deinit();
    const program = try parser.parseProgram();
    defer program.deinit();
    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);

    const exp_stmt = switch (program.statements[0]) {
        .expression_statement => |exp_stmt| exp_stmt,
        else => @panic("stmts[0] is not ast.ExpressionStatement"),
    };

    const if_exp = switch (exp_stmt.expression.?) {
        .if_expression => |if_exp| if_exp,
        else => |other| {
            const fmt = "Expect IfExpression, got {s}";
            const type_name = @typeName(@TypeOf(other));
            @panic(std.fmt.comptimePrint(fmt, .{type_name}));
        },
    };

    const abc = [3]u8{ 'a', 'b', 'c' };
    try testInfixExpression(if_exp.condition.*, abc, "<", "y");

    try testing.expectEqual(1, if_exp.consequence.statements.len);
    const consequence = switch (if_exp.consequence.statements[0]) {
        .expression_statement => |con_stmt| con_stmt,
        else => @panic("if_exp.consequence.statements[0] is not ast.ExpressionStatement"),
    };
    try testIdentifier(consequence.expression.?, "abc");
    try testing.expectEqual(if_exp.alternative, null);
}

test "If Else Expression" {
    const input = "if (x < y) { x } else { y }";
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, testing.allocator);
    defer parser.deinit();
    const program = try parser.parseProgram();
    defer program.deinit();
    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);

    const exp_stmt = switch (program.statements[0]) {
        .expression_statement => |exp_stmt| exp_stmt,
        else => @panic("stmts[0] is not ast.ExpressionStatement"),
    };

    const if_exp = switch (exp_stmt.expression.?) {
        .if_expression => |if_exp| if_exp,
        else => |other| {
            const fmt = "Expect IfExpression, got {s}";
            const type_name = @typeName(@TypeOf(other));
            @panic(std.fmt.comptimePrint(fmt, .{type_name}));
        },
    };

    try testInfixExpression(if_exp.condition.*, "x", "<", "y");

    try testing.expectEqual(1, if_exp.consequence.statements.len);
    const consequence = switch (if_exp.consequence.statements[0]) {
        .expression_statement => |con_stmt| con_stmt,
        else => @panic("if_exp.consequence.statements[0] is not ast.ExpressionStatement"),
    };
    try testIdentifier(consequence.expression.?, "x");

    const alternative = if (if_exp.alternative) |alt| alt else return error.TestExpectedEqual;
    try testing.expectEqual(1, alternative.statements.len);
    const alt_stmt = switch (alternative.statements[0]) {
        .expression_statement => |alt_stmt| alt_stmt,
        else => @panic("if_exp.alternative.statements[0] is not ast.ExpressionStatement"),
    };
    try testIdentifier(alt_stmt.expression.?, "y");
}

test "Function Literal Expression" {
    const input = "fn(x, y) { x + y }";
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, testing.allocator);
    defer parser.deinit();
    const program = try parser.parseProgram();
    defer program.deinit();
    try checkParserErrors(&parser);

    try testing.expectEqual(1, program.statements.len);

    const fn_lit_exp_stmt = switch (program.statements[0]) {
        .expression_statement => |exp_stmt| exp_stmt,
        else => @panic("stmts[0] is not ast.ExpressionStatement"),
    };

    const fn_lit = switch (fn_lit_exp_stmt.expression.?) {
        .function_literal => |fn_lit| fn_lit,
        else => |other| {
            const fmt = "Expect FunctionLiteral, got {s}";
            const type_name = @typeName(@TypeOf(other));
            @panic(std.fmt.comptimePrint(fmt, .{type_name}));
        },
    };

    try testing.expectEqual(2, fn_lit.parameters.len);

    try testLiteralExpression(ast.Expression{ .identifier = fn_lit.parameters[0].* }, "x");
    try testLiteralExpression(ast.Expression{ .identifier = fn_lit.parameters[1].* }, "y");

    try testing.expectEqual(1, fn_lit.body.statements.len);

    const body_exp_stmt: ast.ExpressionStatement = switch (fn_lit.body.statements[0]) {
        .expression_statement => |exp_stmt| exp_stmt,
        else => @panic("stmts[0] is not ast.ExpressionStatement"),
    };
    try testInfixExpression(body_exp_stmt.expression.?, "x", "+", "y");
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
        defer parser.deinit();
        const program = try parser.parseProgram();
        defer program.deinit();
        try checkParserErrors(&parser);

        const stmt = switch (program.statements[0]) {
            .expression_statement => |stmt| stmt,
            else => @panic("stmts[0] is not ast.ExpressionStatement"),
        };
        const function = switch (stmt.expression.?) {
            .function_literal => |function| function,
            else => |other| {
                const fmt = "Expect FunctionLiteral, got {s}";
                const type_name = @typeName(@TypeOf(other));
                @panic(std.fmt.comptimePrint(fmt, .{type_name}));
            },
        };
        try testing.expectEqual(param_test.expected_params.len, function.parameters.len);
        for (param_test.expected_params, 0..) |param, i| {
            const exp = ast.Expression{ .identifier = function.parameters[i].* };
            try testLiteralExpression(exp, param);
        }
    }
}

test "Call Expression Parsing" {
    const input = "add(1, 2 * 3, 4 + 5);";
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, testing.allocator);
    defer parser.deinit();
    const program = try parser.parseProgram();
    defer program.deinit();
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
    const call_exp = switch (exp_stmt.expression.?) {
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
    try testLiteralExpression(args[0].*, 1);
    try testInfixExpression(args[1].*, 2, "*", 3);
    try testInfixExpression(args[2].*, 4, "+", 5);
}

//Test Helpers

const Expected = struct {
    expected_identifiers: []const u8,
};

fn testOutOfMemory(allocator: std.mem.Allocator, input: []const u8, expecteds: []const Expected) !void {
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    const program = try parser.parseProgram();
    defer program.deinit();

    try checkParserErrors(&parser);

    try testing.expect(expecteds.len == program.statements.len);

    for (expecteds, 0..) |expected, i| {
        try testLetStatement(program.statements[i], expected.expected_identifiers);
    }
}

fn testOutOfMemoryWithParserErrors(allocator: std.mem.Allocator, input: []const u8, expecteds: []const []const u8) !void {
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    const program = try parser.parseProgram();
    defer program.deinit();

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

fn testLetStatement(s: ast.Statement, expected: []const u8) !void {
    try testing.expectEqualStrings(s.tokenLiteral(), "let");

    var let_stmt: ast.LetStatement = switch (s) {
        .let_statement => s.let_statement,
        else => {
            std.debug.print("s is not *ast.LetStatement. got={s}", .{@typeName(@TypeOf(s))});
            return error.TestExpectedEqual;
        },
    };

    try testing.expectEqualStrings(expected, let_stmt.name.value);

    try testing.expectEqualStrings(expected, let_stmt.name.tokenLiteral());
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
    const integer_literal = std.fmt.allocPrint(testing.allocator, "{d}", .{value}) catch return error.TestExpectedEqual;
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

fn testLiteralExpression(exp: ast.Expression, expected: anytype) !void {
    const type_info = @typeInfo(@TypeOf(expected));
    // std.debug.print("{}\n", .{type_info});
    switch (type_info) {
        .comptime_int, .int => try testIntegerLiteral(exp, expected),
        .bool => try testBooleanLiteral(exp, expected),
        .pointer => |pointer| switch (pointer.size) {
            .one => try testIdentifier(exp, expected),
            .slice => try testIdentifier(exp, expected),
            else => return error.TestExpectedEqual,
        },
        .array => try testIdentifier(exp, &expected),
        else => return error.TestExpectedEqual,
    }
}

fn testInfixExpression(exp: ast.Expression, left: anytype, operator: []const u8, right: anytype) !void {
    const op_exp: ast.InfixExpression = switch (exp) {
        .infix_expression => |infix| infix,
        else => |e| {
            std.debug.print("exp not ast.InfixExpression. got={s}\n", .{@typeName(@TypeOf(e))});
            return error.TestExpectedEqual;
        },
    };

    try testLiteralExpression(op_exp.left.*, left);

    try testing.expectEqualStrings(op_exp.operator, operator);

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
