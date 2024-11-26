const std = @import("std");
const testing = std.testing;

const ast = @import("./ast.zig");
const Lexer = @import("./lexer.zig").Lexer;
const token = @import("./token.zig");

const ExpressionError =
    error{UnknownPrefixToken} ||
    std.fmt.ParseIntError;

const LetStatementError = error{ MissingLetIdentifier, MissingLetAssign } || std.mem.Allocator.Error;

const StatementError =
    ExpressionError ||
    LetStatementError ||
    std.fmt.ParseIntError ||
    std.fmt.AllocPrintError ||
    std.mem.Allocator.Error;

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

    pub fn init(lexer: *Lexer, allocator: std.mem.Allocator) Parser {
        var p = Parser{
            .lexer = lexer,
            .cur_token = undefined,
            .peek_token = undefined,
            .allocator = allocator,
            .errors = std.ArrayList([]const u8).init(allocator),
            .prefix_parse_fns = std.StringHashMap(PrefixParseFn).init(allocator),
            .infix_parse_fns = std.StringHashMap(InfixParseFn).init(allocator),
        };

        p.registerPrefixFns(token.IDENT, parseIdentifier);
        p.registerPrefixFns(token.INT, parseIntegerLiteral);

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

    fn parseProgram(self: *Parser) error{OutOfMemory}!ast.Program {
        var list = std.ArrayList(ast.Statement).init(self.allocator);
        errdefer list.deinit();

        while (!std.mem.eql(u8, self.cur_token._type, token.EOF)) {
            if (self.parseStatement()) |stmt| {
                list.append(stmt) catch @panic("Failed to append to statements ArrayList");
            } else |err| {
                switch (err) {
                    StatementError.UnknownPrefixToken => {},
                    StatementError.MissingLetIdentifier => {
                        self.peekErrors(token.IDENT);
                    },
                    StatementError.MissingLetAssign => {
                        self.peekErrors(token.ASSIGN);
                    },
                    StatementError.InvalidCharacter, StatementError.Overflow => {
                        const fmt = "could not parse {s} as integer";
                        const msg = try std.fmt.allocPrint(self.allocator, fmt, .{self.cur_token.literal});
                        try self.errors.append(msg);
                    },
                    StatementError.OutOfMemory => return StatementError.OutOfMemory,
                }
            }

            self.nextToken();
        }
        return ast.Program{
            .allocator = self.allocator,
            .statements = list.toOwnedSlice() catch @panic("Failed toOwnedSlice()"),
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
            return StatementError.MissingLetIdentifier;
        }

        const name: *ast.Identifier = try self.allocator.create(ast.Identifier);
        errdefer self.allocator.destroy(name);

        name.* = ast.Identifier{
            ._token = self.cur_token,
            .value = self.cur_token.literal,
        };

        if (!self.expectPeek(token.ASSIGN)) {
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

        while (self.curTokenIs(token.SEMICOLON)) {
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
        const prefix = self.prefix_parse_fns.get(self.cur_token._type) orelse
            return StatementError.UnknownPrefixToken;
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

    fn peekErrors(self: *Parser, t: token.TokenType) void {
        const fmt = "expected next token to be {s}, got {s} instead";
        const maybe_msg = std.fmt.allocPrint(self.allocator, fmt, .{ t, self.peek_token._type });
        const msg = maybe_msg catch @panic("Failed to alloc msg!");
        self.errors.append(msg) catch @panic("Failed to append error!");
    }

    fn registerPrefixFns(self: *Parser, token_type: token.TokenType, func: PrefixParseFn) void {
        self.prefix_parse_fns.put(token_type, func) catch @panic("Failed to put!");
    }

    fn registerInfixFns(self: *Parser, token_type: token.TokenType, func: InfixParseFn) void {
        self.infix_parse_fns.put(token_type, func) catch @panic("Failed to put!");
    }
};

test "Let Statement" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;
    var l = Lexer.init(input);
    var parser = Parser.init(&l, testing.allocator);
    defer parser.deinit();

    const program = try parser.parseProgram();
    defer program.deinit();
    checkParserErrors(&parser);

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

test "Let Statement missing assign token" {
    const input = "let x;";
    var l = Lexer.init(input);
    var parser = Parser.init(&l, testing.allocator);
    defer parser.deinit();

    const program = try parser.parseProgram();
    defer program.deinit();

    const errors = parser.getErrors();
    try testing.expectEqual(1, errors.len);
    try testing.expectEqualStrings("expected next token to be =, got ; instead", errors[0]);
}

test "Return Statement" {
    const input =
        \\return 5;
        \\return 10;
        \\return 993322;
    ;

    var l = Lexer.init(input);
    var parser = Parser.init(&l, testing.allocator);
    defer parser.deinit();

    const program = try parser.parseProgram();
    defer program.deinit();
    checkParserErrors(&parser);

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
    var parser = Parser.init(&l, testing.allocator);
    defer parser.deinit();
    const program = try parser.parseProgram();
    defer program.deinit();
    checkParserErrors(&parser);

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
    var parser = Parser.init(&l, testing.allocator);
    defer parser.deinit();
    const program = try parser.parseProgram();
    defer program.deinit();
    checkParserErrors(&parser);

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

fn checkParserErrors(p: *const Parser) void {
    const errors = p.getErrors();
    if (errors.len == 0) {
        return;
    }

    std.debug.print("parser has {d} errors\n", .{errors.len});
    for (errors) |msg| {
        std.debug.print("parser error: {s}\n", .{msg});
    }
    @panic("fail now");
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
