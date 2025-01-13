const std = @import("std");
const testing = std.testing;

const ast = @import("./ast.zig");
const Lexer = @import("./lexer.zig").Lexer;
const token = @import("./token.zig");

const ExpressionError = error{UnknownPrefixToken} || std.fmt.ParseIntError || std.mem.Allocator.Error;

const LetStatementError = error{ MissingLetIdentifier, MissingLetAssign } || std.mem.Allocator.Error;

const StatementError = ExpressionError || LetStatementError;

const ProgramError = std.mem.Allocator.Error || std.fmt.AllocPrintError;

const PrefixParseFn = *const fn (self: *Parser) ExpressionError!ast.Expression;
const InfixParseFn = *const fn (self: *const Parser, lhs: ast.Expression) ExpressionError!ast.Expression;

const Precedence = enum {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
};

const Parser = struct {
    lexer: *Lexer,
    cur_token: token.Token,
    peek_token: token.Token,
    allocator: std.mem.Allocator,
    errors: std.ArrayList([]const u8),
    prefix_parse_fns: std.StringHashMap(PrefixParseFn),
    infix_parse_fns: std.StringHashMap(InfixParseFn),

    // Initialization

    pub fn init(lexer: *Lexer, allocator: std.mem.Allocator) !Parser {
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

        try p.registerPrefixFns(token.IDENT, parseIdentifier);
        try p.registerPrefixFns(token.INT, parseIntegerLiteral);
        try p.registerPrefixFns(token.BANG, parsePrefixExpression);
        try p.registerPrefixFns(token.MINUS, parsePrefixExpression);

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
            for (list.items) |stmt| switch (stmt) {
                .let_statement => |let_stmt| self.allocator.destroy(let_stmt.name),
                .expression_statement => {},
                .return_statement => {},
            };
            list.deinit();
        }

        while (!std.mem.eql(u8, self.cur_token._type, token.EOF)) {
            const maybe_stmt = self.parseStatement();
            errdefer {
                if (maybe_stmt) |stmt| switch (stmt) {
                    .let_statement => |let_stmt| self.allocator.destroy(let_stmt.name),
                    .expression_statement => {},
                    .return_statement => {},
                } else |_| {}
            }

            if (maybe_stmt) |stmt| {
                try list.append(stmt);
            } else |err| switch (err) {
                StatementError.UnknownPrefixToken,
                StatementError.MissingLetIdentifier,
                StatementError.MissingLetAssign,
                => {},
                StatementError.InvalidCharacter, StatementError.Overflow => {
                    const fmt = "could not parse {s} as integer";
                    const msg = try std.fmt.allocPrint(self.allocator, fmt, .{self.cur_token.literal});
                    errdefer self.allocator.free(msg);
                    try self.errors.append(msg);
                },
                StatementError.OutOfMemory => return ProgramError.OutOfMemory,
            }

            self.nextToken();
        }

        return ast.Program{
            .allocator = self.allocator,
            .statements = try list.toOwnedSlice(),
        };
    }

    // Statement

    fn parseStatement(self: *Parser) StatementError!ast.Statement {
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

    fn parseLetStatement(self: *Parser) LetStatementError!ast.LetStatement {
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

    // Expression

    pub fn parseExpression(self: *Parser, precedence: Precedence) ExpressionError!ast.Expression {
        _ = @intFromEnum(precedence);
        const prefix = self.prefix_parse_fns.get(self.cur_token._type) orelse {
            try self.noPrefixParseFnError(self.cur_token._type);
            return StatementError.UnknownPrefixToken;
        };
        return try prefix(self);
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

    pub fn getErrors(self: *const Parser) [][]const u8 {
        return self.errors.items;
    }

    fn peekErrors(self: *Parser, t: token.TokenType) !void {
        const fmt = "expected next token to be {s}, got {s} instead";
        const msg = try std.fmt.allocPrint(self.allocator, fmt, .{ t, self.peek_token._type });
        errdefer self.allocator.free(msg);
        try self.errors.append(msg);
    }

    fn registerPrefixFns(self: *Parser, token_type: token.TokenType, func: PrefixParseFn) !void {
        try self.prefix_parse_fns.put(token_type, func);
    }

    fn registerInfixFns(self: *Parser, token_type: token.TokenType, func: InfixParseFn) !void {
        try self.infix_parse_fns.put(token_type, func);
    }

    fn noPrefixParseFnError(self: *Parser, t: token.TokenType) !void {
        const fmt = "no prefix parse function for {s} found";
        const msg = try std.fmt.allocPrint(self.allocator, fmt, .{t});
        errdefer self.allocator.free(msg);
        try self.errors.append(msg);
    }
};

test "Out of Memory, Program Statement, no Parser errors" {
    const input = "let x = 5;";

    const expecteds = [_]Expected{.{ .expected_identifiers = "x" }};

    try testing.checkAllAllocationFailures(testing.allocator, outOfMemoryTest, .{ input, &expecteds });
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

    try testing.checkAllAllocationFailures(testing.allocator, outOfMemoryWithParserErrorsTest, .{ input, &expecteds });
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
        try testing.expect(testLetStatement(program.statements[i], expected.expected_identifiers));
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
                std.debug.print("stmt is not *ast.ReturnStatement. got={s}", .{@typeName(@TypeOf(stmt))});
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
    const PrefixTest = struct {
        input: []const u8,
        operator: []const u8,
        integer_value: i64,
    };

    const prefix_tests = [_]PrefixTest{
        .{ .input = "!5;", .operator = "!", .integer_value = 5 },
        .{ .input = "-15;", .operator = "-", .integer_value = 15 },
    };

    for (prefix_tests) |prefix_test| {
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
        if (!testIntegerLiteral(exp.right.*, prefix_test.integer_value)) {
            @panic("");
        }
    }
}

const Expected = struct {
    expected_identifiers: []const u8,
};

fn checkParserErrors(p: *const Parser) !void {
    const errors = p.getErrors();
    if (errors.len == 0) {
        return;
    }

    std.debug.print("parser has {d} errors\n", .{errors.len});
    for (errors) |msg| {
        std.debug.print("parser error: {s}\n", .{msg});
    }
    return error.FoundParserErrors;
}

fn testLetStatement(s: ast.Statement, expected: []const u8) bool {
    if (!std.mem.eql(u8, s.tokenLiteral(), "let")) {
        std.debug.print("s.tokenLiteral not \"let\". got={s}", .{s.tokenLiteral()});
        return false;
    }

    var let_stmt: ast.LetStatement = switch (s) {
        .let_statement => s.let_statement,
        else => {
            std.debug.print("s is not *ast.LetStatement. got={s}", .{@typeName(@TypeOf(s))});
            return false;
        },
    };

    testing.expectEqualStrings(expected, let_stmt.name.value) catch {
        std.debug.print("let_stmt.name.value not {s}. got={s}", .{ expected, let_stmt.name.value });
        return false;
    };

    testing.expectEqualStrings(expected, let_stmt.name.tokenLiteral()) catch {
        std.debug.print("let_stmt.name.tokenLiteral not {s}. got={s}", .{ expected, let_stmt.name.tokenLiteral() });
        return false;
    };

    return true;
}

fn testIntegerLiteral(il: ast.Expression, value: i64) bool {
    const integ = switch (il) {
        .integer_literal => |integer| integer,
        else => |exp| {
            std.debug.print("il not ast.IntegerLiteral. got={s}", .{@typeName(@TypeOf(exp))});
            return false;
        },
    };

    testing.expectEqual(value, integ.value) catch {
        std.debug.print("integ.value not {d}. got={d}", .{ value, integ.value });
        return false;
    };
    const integer_literal = std.fmt.allocPrint(testing.allocator, "{d}", .{value}) catch return false;
    defer testing.allocator.free(integer_literal);
    testing.expectEqualStrings(integer_literal, integ.tokenLiteral()) catch {
        std.debug.print("integ.TokenLiteral not {d}. got={d}", .{ integer_literal, integ.tokenLiteral() });
        return false;
    };
    return true;
}

fn outOfMemoryTest(allocator: std.mem.Allocator, input: []const u8, expecteds: []const Expected) !void {
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    const program = try parser.parseProgram();
    defer program.deinit();

    try checkParserErrors(&parser);

    try testing.expect(expecteds.len == program.statements.len);

    for (expecteds, 0..) |expected, i| {
        try testing.expect(testLetStatement(program.statements[i], expected.expected_identifiers));
    }
}

fn outOfMemoryWithParserErrorsTest(allocator: std.mem.Allocator, input: []const u8, expecteds: []const []const u8) !void {
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();

    const program = try parser.parseProgram();
    defer program.deinit();

    const parser_errors = parser.getErrors();

    try testing.expect(expecteds.len == parser_errors.len);

    for (parser_errors, 0..) |parser_error, i| {
        try testing.expectEqualStrings(expecteds[i], parser_error);
    }
}
