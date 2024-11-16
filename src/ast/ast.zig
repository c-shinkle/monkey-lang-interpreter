const std = @import("std");
const token = @import("token");

const testing = std.testing;

pub const Node = union(enum) {
    program: Program,
    statement: Statement,
    expression: Expression,

    pub fn tokenLiteral(self: *const Node) []const u8 {
        return switch (self.*) {
            .program => self.program.tokenLiteral(),
            .statement => self.statement.tokenLiteral(),
            .expression => self.expression.tokenLiteral(),
        };
    }

    pub fn string(self: *const Node, allocator: std.mem.Allocator) []const u8 {
        return switch (self.*) {
            .program => self.program.string(allocator),
            .statement => self.statement.string(allocator),
            .expression => self.expression.string(),
        };
    }
};

pub const Statement = union(enum) {
    let_statement: LetStatement,
    return_statement: ReturnStatement,
    expression_statement: ExpressionStatement,

    pub fn tokenLiteral(self: *const Statement) []const u8 {
        return switch (self.*) {
            .let_statement => self.let_statement.tokenLiteral(),
            .return_statement => self.return_statement.tokenLiteral(),
            .expression_statement => self.expression_statement.tokenLiteral(),
        };
    }

    pub fn statementNode(self: *const Statement) void {
        switch (self.*) {
            .let_statement => self.let_statement.statementNode(),
            .return_statement => self.return_statement.statementNode(),
            .expression_statement => self.expression_statement.statementNode(),
        }
    }

    pub fn string(self: *const Statement, allocator: std.mem.Allocator) []const u8 {
        return switch (self.*) {
            .let_statement => self.let_statement.string(allocator),
            .return_statement => self.return_statement.string(allocator),
            .expression_statement => self.expression_statement.string(),
        };
    }
};

pub const Expression = union(enum) {
    identifier: Identifier,

    pub fn tokenLiteral(self: *const Expression) []const u8 {
        return switch (self.*) {
            .identifier => self.identifier.tokenLiteral(),
        };
    }

    pub fn expressionNode(self: *const Expression) void {
        switch (self.*) {
            .identifier => self.identifier.expressionNode(),
        }
    }

    pub fn string(self: *const Expression) []const u8 {
        return switch (self.*) {
            .identifier => self.identifier.string(),
        };
    }
};

pub const Program = struct {
    statements: []const Statement,

    pub fn deinit(self: *const Program, allocator: std.mem.Allocator) void {
        for (self.statements) |stmt| {
            switch (stmt) {
                .let_statement => {
                    allocator.destroy(stmt.let_statement.name);
                },
                .return_statement => {},
                .expression_statement => {},
            }
        }
        allocator.free(self.statements);
    }

    pub fn tokenLiteral(self: *const Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].tokenLiteral();
        } else {
            return "";
        }
    }

    pub fn string(self: *const Program, allocator: std.mem.Allocator) []const u8 {
        var strings = std.ArrayList(u8).init(allocator);
        var writer = strings.writer();
        for (self.statements) |stmt| {
            writer.writeAll(stmt.string(allocator)) catch @panic("Failed to writeAll()");
        }
        return strings.toOwnedSlice() catch @panic("Failed toOwnedSlice()");
    }
};

pub const LetStatement = struct {
    _token: token.Token,
    name: *const Identifier,
    value: ?Expression,

    pub fn tokenLiteral(self: *const LetStatement) []const u8 {
        return self._token.literal;
    }

    pub fn statementNode(_: *const LetStatement) void {}

    pub fn string(self: *const LetStatement, allocator: std.mem.Allocator) []const u8 {
        var strings = std.ArrayList(u8).init(allocator);
        var writer = strings.writer();

        writer.writeAll(self.tokenLiteral()) catch @panic("Failed to write!");
        writer.writeByte(' ') catch @panic("Failed to write!");
        writer.writeAll(self.name.string()) catch @panic("Failed to write!");
        writer.writeAll(" = ") catch @panic("Failed to write!");
        if (self.value) |value| {
            writer.writeAll(value.string()) catch @panic("Failed to write!");
        } else {
            writer.writeAll("null") catch @panic("Failed to write!");
        }
        writer.writeByte(';') catch @panic("Failed to write!");

        return strings.toOwnedSlice() catch @panic("Failed toOwnedSlice()");
    }
};

pub const Identifier = struct {
    _token: token.Token,
    value: []const u8,

    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self._token.literal;
    }

    pub fn expressionNode(_: *const Identifier) void {}

    pub fn string(self: *const Identifier) []const u8 {
        return self.value;
    }
};

pub const ReturnStatement = struct {
    _token: token.Token,
    return_value: ?Expression,

    pub fn statementNode(_: *const ReturnStatement) void {}

    pub fn tokenLiteral(self: *const ReturnStatement) []const u8 {
        return self._token.literal;
    }

    pub fn string(self: *const ReturnStatement, allocator: std.mem.Allocator) []const u8 {
        var strings = std.ArrayList(u8).init(allocator);
        var writer = strings.writer();

        writer.writeAll(self.tokenLiteral()) catch @panic("Failed to write!");
        writer.writeByte(' ') catch @panic("Failed to write!");

        if (self.return_value) |return_value| {
            writer.writeAll(return_value.string()) catch @panic("Failed to write!");
        } else {
            writer.writeAll("null") catch @panic("Failed to write!");
        }
        writer.writeByte(' ') catch @panic("Failed to write!");

        return strings.toOwnedSlice() catch @panic("Failed toOwnedSlice()");
    }
};

pub const ExpressionStatement = struct {
    _token: token.Token,
    expression: ?Expression,

    pub fn statementNode(_: *const ExpressionStatement) void {}

    pub fn tokenLiteral(self: *const ExpressionStatement) []const u8 {
        return self._token.literal;
    }

    pub fn string(self: *const ExpressionStatement) []const u8 {
        if (self.expression) |expression| {
            return expression.string();
        } else {
            return "null";
        }
    }
};

test "ast foo" {
    const name = try testing.allocator.create(Identifier);
    name.* = Identifier{ ._token = token.Token{ ._type = token.IDENT, .literal = "x" }, .value = "x" };

    var statements = std.ArrayList(Statement).init(testing.allocator);
    try statements.append(Statement{ .let_statement = LetStatement{
        ._token = token.Token{ ._type = token.LET, .literal = "let" },
        .name = name,
        .value = Expression{ .identifier = Identifier{ ._token = token.Token{ ._type = token.INT, .literal = "10" }, .value = "10" } },
    } });

    const program = Program{
        .statements = try statements.toOwnedSlice(),
    };
    defer program.deinit(testing.allocator);

    const node = Node{
        .program = program,
    };

    const string = node.string(testing.allocator);
    try testing.expectEqualStrings("let x = 10;", string);
    testing.allocator.free(string);
}
