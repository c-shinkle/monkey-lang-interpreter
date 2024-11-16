const std = @import("std");
const testing = std.testing;

const ast = @import("ast");
const Lexer = @import("lexer").Lexer;
const token = @import("token");

const Parser = struct {
    lexer: *Lexer,
    errors: std.ArrayList([]const u8),
    cur_token: token.Token,
    peek_token: token.Token,

    pub fn init(l: *Lexer, allocator: std.mem.Allocator) Parser {
        var p = Parser{
            .lexer = l,
            .errors = std.ArrayList([]const u8).init(allocator),
            .cur_token = undefined,
            .peek_token = undefined,
        };

        p.nextToken();
        p.nextToken();

        return p;
    }

    pub fn deinit(self: *Parser, allocator: std.mem.Allocator) void {
        for (self.errors.items) |msg| {
            allocator.free(msg);
        }
        self.errors.deinit();
    }

    fn nextToken(self: *Parser) void {
        self.cur_token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
    }

    fn parseProgram(p: *Parser, allocator: std.mem.Allocator) ast.Program {
        var list = std.ArrayList(ast.Statement).init(allocator);

        while (!std.mem.eql(u8, p.cur_token._type, token.EOF)) {
            const maybe_statement = p.parseStatement(allocator);
            if (maybe_statement) |stmt| {
                list.append(stmt) catch {
                    @panic("Failed to append to statements ArrayList");
                };
            }
            p.nextToken();
        }
        return ast.Program{
            .statements = list.toOwnedSlice() catch @panic("Failed toOwnedSlice()"),
        };
    }

    fn parseStatement(self: *Parser, allocator: std.mem.Allocator) ?ast.Statement {
        if (std.mem.eql(u8, token.LET, self.cur_token._type)) {
            const let_statement = self.parseLetStatement(allocator) orelse return null;
            return ast.Statement{ .let_statement = let_statement };
        }
        if (std.mem.eql(u8, token.RETURN, self.cur_token._type)) {
            const return_statement = self.parseReturnStatemetn();
            return ast.Statement{ .return_statement = return_statement };
        }

        return null;
    }

    fn parseLetStatement(self: *Parser, allocator: std.mem.Allocator) ?ast.LetStatement {
        const let_token = self.cur_token;

        if (!self.expectPeek(token.IDENT, allocator)) {
            return null;
        }

        const name: *ast.Identifier = allocator.create(ast.Identifier) catch {
            @panic("Failed to allocate ast.Identifier");
        };
        name.* = ast.Identifier{
            ._token = self.cur_token,
            .value = self.cur_token.literal,
        };

        if (!self.expectPeek(token.ASSIGN, allocator)) {
            allocator.destroy(name);
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

    fn expectPeek(self: *Parser, _token: token.TokenType, allocator: std.mem.Allocator) bool {
        if (self.peekTokenIs(_token)) {
            self.nextToken();
            return true;
        } else {
            self.peekErrors(_token, allocator);
            return false;
        }
    }

    pub fn getErrors(self: *const Parser) [][]const u8 {
        return self.errors.items;
    }

    pub fn peekErrors(self: *Parser, t: token.TokenType, allocator: std.mem.Allocator) void {
        const fmt = "expected next token to be {s}, got {s} instead";
        const maybe_msg = std.fmt.allocPrint(allocator, fmt, .{ t, self.peek_token._type });
        const msg = maybe_msg catch @panic("Failed to alloc peek error!");
        self.errors.append(msg) catch @panic("Failed to append error!");
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
    defer parser.deinit(testing.allocator);

    const program = parser.parseProgram(testing.allocator);
    defer program.deinit(testing.allocator);
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
    defer parser.deinit(testing.allocator);

    const program = parser.parseProgram(testing.allocator);
    defer program.deinit(testing.allocator);
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
