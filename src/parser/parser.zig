const std = @import("std");
const testing = std.testing;

const ast = @import("ast");
const Lexer = @import("lexer").Lexer;
const token = @import("token");

const PrefixParseFn = *const fn () ast.Expression;
const InfixParseFn = *const fn (lhs: ast.Expression) ast.Expression;

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

        p.nextToken();
        p.nextToken();

        return p;
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
            const maybe_statement = self.parseStatement();
            if (maybe_statement) |stmt| {
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
        }
        if (std.mem.eql(u8, token.RETURN, self.cur_token._type)) {
            const return_statement = self.parseReturnStatemetn();
            return ast.Statement{ .return_statement = return_statement };
        }

        return null;
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
        const msg = maybe_msg catch @panic("Failed to alloc peek error!");
        self.errors.append(msg) catch @panic("Failed to append error!");
    }

    pub fn registerPrefixFns(self: *Parser, t: token.TokenType, prefix_parse_fn: PrefixParseFn) void {
        self.prefix_parse_fns.put(t, prefix_parse_fn);
    }

    pub fn registerInfixFns(self: *Parser, t: token.TokenType, infix_parse_fn: InfixParseFn) void {
        self.infix_parse_fns.put(t, infix_parse_fn);
    }
};

test "Let Statement tests" {
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

    try testing.expectEqual(3, program.statements.len);

    const expecteds = [_]struct {
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

test "Return Statement tests" {
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

fn checkParserErrors(p: *Parser) void {
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
