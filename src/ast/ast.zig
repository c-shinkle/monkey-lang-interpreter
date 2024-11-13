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
};

pub const Statement = union(enum) {
    let_statement: LetStatement,

    pub fn tokenLiteral(self: *const Statement) []const u8 {
        return switch (self.*) {
            .let_statement => self.let_statement.tokenLiteral(),
        };
    }

    pub fn statementNode(self: *const Statement) void {
        switch (self.*) {
            .let_statement => self.let_statement.statementNode(),
        }
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
};

pub const Program = struct {
    statements: []const Statement,

    pub fn tokenLiteral(self: *const Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].tokenLiteral();
        } else {
            return "";
        }
    }
};

pub const LetStatement = struct {
    _token: token.Token,
    name: *const Identifier,
    value: Expression,

    pub fn tokenLiteral(self: *const LetStatement) []const u8 {
        return self._token.literal;
    }

    pub fn statementNode(self: *const LetStatement) void {
        _ = self;
    }
};

pub const Identifier = struct {
    _token: token.Token,
    value: []const u8,

    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self._token.literal;
    }

    pub fn expressionNode(self: *const Identifier) void {
        _ = self;
    }
};

test "ast foo" {
    const identifier = Identifier{ ._token = token.Token{ ._type = token.IDENT, .literal = "x" }, .value = "y" };

    const program = Program{
        .statements = &.{Statement{ .let_statement = LetStatement{
            ._token = token.Token{ ._type = token.ASSIGN, .literal = "z" },
            .name = &identifier,
            .value = Expression{ .identifier = identifier },
        } }},
    };

    const node = Node{
        .program = program,
    };

    try testing.expectEqualStrings(node.tokenLiteral(), "z");
}
