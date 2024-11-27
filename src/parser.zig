const std = @import("std");
const testing = std.testing;

const ast = @import("./ast.zig");
const Lexer = @import("./lexer.zig").Lexer;
const token = @import("./token.zig");

const PrefixParseFn = *const fn (self: *Parser) ?ast.Expression;
const InfixParseFn = *const fn (self: *const Parser, lhs: ast.Expression) ast.Expression;

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

    fn parseIdentifier(self: *const Parser) ?ast.Expression {
        const identifier = ast.Identifier{ ._token = self.cur_token, .value = self.cur_token.literal };
        return ast.Expression{ .identifier = identifier };
    }

    pub fn deinit(self: *Parser) void {
        for (self.errors.items) |msg| {
            self.allocator.free(msg);
        }
        self.errors.deinit();

        self.prefix_parse_fns.deinit();
        self.infix_parse_fns.deinit();
    }

    fn nextToken(self: *Parser) void {
        self.cur_token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
    }

    fn parseProgram(self: *Parser) ast.Program {
        var list = std.ArrayList(ast.Statement).init(self.allocator);

        while (!std.mem.eql(u8, self.cur_token._type, token.EOF)) {
            if (self.parseStatement()) |stmt| {
                list.append(stmt) catch @panic("Failed to append to statements ArrayList");
            }
            self.nextToken();
        }
        return ast.Program{
            .allocator = self.allocator,
            .statements = list.toOwnedSlice() catch @panic("Failed toOwnedSlice()"),
        };
    }

    fn parseStatement(self: *Parser) ?ast.Statement {
        if (std.mem.eql(u8, token.LET, self.cur_token._type)) {
            const let_statement = self.parseLetStatement() orelse return null;
            return ast.Statement{ .let_statement = let_statement };
        } else if (std.mem.eql(u8, token.RETURN, self.cur_token._type)) {
            const return_statement = self.parseReturnStatemetn();
            return ast.Statement{ .return_statement = return_statement };
        } else {
            const expression_statement = self.parseExpressionStatement() orelse return null;
            return ast.Statement{ .expression_statement = expression_statement };
        }
    }

    fn parseLetStatement(self: *Parser) ?ast.LetStatement {
        const let_token = self.cur_token;

        if (!self.expectPeek(token.IDENT)) {
            return null;
        }

        const name: *ast.Identifier = self.allocator.create(ast.Identifier) catch @panic("Failed to allocate ast.Identifier");
        name.* = ast.Identifier{
            ._token = self.cur_token,
            .value = self.cur_token.literal,
        };

        if (!self.expectPeek(token.ASSIGN)) {
            self.allocator.destroy(name);
            return null;
        }

        // TODO

        while (!self.curTokenIs(token.SEMICOLON)) {
            self.nextToken();
        }

        return ast.LetStatement{ ._token = let_token, .name = name, .value = null };
    }

    pub fn parseReturnStatemetn(self: *Parser) ast.ReturnStatement {
        const return_token = self.cur_token;

        self.nextToken();

        while (self.curTokenIs(token.SEMICOLON)) {
            self.nextToken();
        }

        return ast.ReturnStatement{ ._token = return_token, .return_value = null };
    }

    pub fn parseExpressionStatement(self: *Parser) ?ast.ExpressionStatement {
        const cur_token = self.cur_token;
        const exp = self.parseExpression(Precedence.LOWEST) orelse return null;

        if (self.peekTokenIs(token.SEMICOLON)) {
            self.nextToken();
        }

        return ast.ExpressionStatement{ ._token = cur_token, .expression = exp };
    }

    pub fn parseExpression(self: *Parser, precedence: Precedence) ?ast.Expression {
        _ = @intFromEnum(precedence);
        const prefix = self.prefix_parse_fns.get(self.cur_token._type) orelse return null;
        const left_exp = prefix(self);
        return left_exp;
    }

    pub fn parseIntegerLiteral(self: *Parser) ?ast.Expression {
        const cur_token = self.cur_token;

        const value = std.fmt.parseInt(i64, cur_token.literal, 10) catch {
            const fmt = "could not parse {s} as integer";
            const maybe_msg = std.fmt.allocPrint(self.allocator, fmt, .{cur_token.literal});
            const msg = maybe_msg catch @panic("Failed to alloc msg!");
            self.errors.append(msg) catch @panic("Failed to append error!");
            return null;
        };

        const integer_literal = ast.IntegerLiteral{ ._token = cur_token, .value = value };
        return ast.Expression{ .integer_literal = integer_literal };
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
            self.peekErrors(_token);
            return false;
        }
    }

    pub fn getErrors(self: *const Parser) [][]const u8 {
        return self.errors.items;
    }

    pub fn peekErrors(self: *Parser, t: token.TokenType) void {
        const fmt = "expected next token to be {s}, got {s} instead";
        const maybe_msg = std.fmt.allocPrint(self.allocator, fmt, .{ t, self.peek_token._type });
        const msg = maybe_msg catch @panic("Failed to alloc msg!");
        self.errors.append(msg) catch @panic("Failed to append error!");
    }

    pub fn registerPrefixFns(self: *Parser, t: token.TokenType, prefix_parse_fn: PrefixParseFn) void {
        self.prefix_parse_fns.put(t, prefix_parse_fn) catch @panic("Failed to put!");
    }

    pub fn registerInfixFns(self: *Parser, t: token.TokenType, infix_parse_fn: InfixParseFn) void {
        self.infix_parse_fns.put(t, infix_parse_fn) catch @panic("Failed to put!");
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

    const program = parser.parseProgram();
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

test "Return Statement" {
    const input =
        \\return 5;
        \\return 10;
        \\return 993322;
    ;

    var l = Lexer.init(input);
    var parser = Parser.init(&l, testing.allocator);
    defer parser.deinit();

    const program = parser.parseProgram();
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

test "Identifier Expression" {
    const input = "foobar;";

    var l = Lexer.init(input);
    var parser = Parser.init(&l, testing.allocator);
    defer parser.deinit();
    const program = parser.parseProgram();
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
    const program = parser.parseProgram();
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
