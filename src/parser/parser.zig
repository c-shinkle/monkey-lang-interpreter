const std = @import("std");
const testing = std.testing;

const ast = @import("ast");
const Lexer = @import("lexer").Lexer;
const token = @import("token");

const Parser = struct {
    lexer: *Lexer,
    cur_token: ?token.Token,
    peek_token: ?token.Token,

    pub fn init(l: *Lexer) Parser {
        var p = Parser{
            .lexer = l,
            .cur_token = null,
            .peek_token = null,
        };

        p.nextToken();
        p.nextToken();

        return p;
    }

    fn nextToken(self: *Parser) void {
        self.cur_token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
    }

    fn parseProgram(_: *Parser) ?*ast.Program {
        return null;
    }
};

test "Parser tests" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;
    var l = Lexer.init(input);
    var parser = Parser.init(&l);

    const program = parser.parseProgram();
    try testing.expect(program != null);

    try testing.expectEqual(3, program.?.statements.len);

    const expecteds = [_]struct {
        expected_identifiers: []const u8,
    }{
        .{ .expected_identifiers = "x" },
        .{ .expected_identifiers = "y" },
        .{ .expected_identifiers = "foobar" },
    };

    for (expecteds, 0..) |expected, i| {
        if (!testLetStatement(program.?.statements[i], expected.expected_identifiers)) {
            return;
        }
    }
}

fn testLetStatement(s: ast.Statement, expected: []const u8) bool {
    // TODO make sure this print statement works as intended
    if (std.mem.eql(u8, s.tokenLiteral(), "let")) {
        std.debug.print("s.tokenLiteral not \"let\". got={any}", .{s});
        return false;
    }

    var let_stmt: ast.LetStatement = switch (s) {
        .let_statement => s.let_statement,
        // else => {
        //     std.debug.print("s is not *ast.LetStatement. got={s}", .{@typeName(s)});
        //     return false;
        // },
    };

    testing.expectEqualStrings(expected, let_stmt.name.value) catch {
        std.debug.print("let_stmt.name.value not {s}. got={s}", .{ expected, let_stmt.name.value });
        return false;
    };

    testing.expectEqualStrings(expected, let_stmt.tokenLiteral()) catch {
        std.debug.print("let_stmt.name.tokenLiteral not {s}. got={s}", .{ expected, let_stmt.name.tokenLiteral() });
        return false;
    };

    return true;
}
