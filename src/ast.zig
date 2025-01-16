const std = @import("std");
const token = @import("./token.zig");

const testing = std.testing;

pub const StringError = std.ArrayList(u8).Writer.Error;

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

    pub fn string(self: *const Node, writer: *std.ArrayList(u8).Writer) StringError!void {
        switch (self.*) {
            .program => try self.program.string(writer),
            .statement => try self.statement.string(writer),
            .expression => try self.expression.string(writer),
        }
    }
};

pub const Program = struct {
    allocator: std.mem.Allocator,
    statements: []const Statement,

    pub fn deinit(self: *const Program) void {
        for (self.statements) |stmt| {
            stmt.deinit(self.allocator);
        }
        self.allocator.free(self.statements);
    }

    pub fn tokenLiteral(self: *const Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].tokenLiteral();
        } else {
            return "";
        }
    }

    pub fn string(self: *const Program, writer: *std.ArrayList(u8).Writer) StringError!void {
        for (self.statements) |stmt| {
            try stmt.string(writer);
        }
    }
};

pub const Statement = union(enum) {
    let_statement: LetStatement,
    return_statement: ReturnStatement,
    expression_statement: ExpressionStatement,

    pub fn deinit(self: Statement, allocator: std.mem.Allocator) void {
        switch (self) {
            .let_statement => {
                allocator.destroy(self.let_statement.name);
            },
            .return_statement => {},
            .expression_statement => |exp_stmt| {
                if (exp_stmt.expression) |exp| {
                    exp.deinit(allocator);
                }
            },
        }
    }

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

    pub fn string(self: *const Statement, writer: *std.ArrayList(u8).Writer) StringError!void {
        switch (self.*) {
            .let_statement => try self.let_statement.string(writer),
            .return_statement => try self.return_statement.string(writer),
            .expression_statement => try self.expression_statement.string(writer),
        }
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

    pub fn string(self: *const LetStatement, writer: *std.ArrayList(u8).Writer) StringError!void {
        try writer.writeAll(self.tokenLiteral());
        try writer.writeByte(' ');

        try self.name.string(writer);

        try writer.writeAll(" = ");
        if (self.value) |value| {
            try value.string(writer);
        } else {
            try writer.writeAll("null");
        }
    }
};

pub const ReturnStatement = struct {
    _token: token.Token,
    return_value: ?Expression,

    pub fn statementNode(_: *const ReturnStatement) void {}

    pub fn tokenLiteral(self: *const ReturnStatement) []const u8 {
        return self._token.literal;
    }

    pub fn string(self: *const ReturnStatement, writer: *std.ArrayList(u8).Writer) StringError!void {
        try writer.writeAll(self.tokenLiteral());
        try writer.writeByte(' ');

        if (self.return_value) |return_value| {
            try return_value.string(writer);
        } else {
            try writer.writeAll("null");
        }
    }
};

pub const ExpressionStatement = struct {
    _token: token.Token,
    expression: ?Expression,

    pub fn statementNode(_: *const ExpressionStatement) void {}

    pub fn tokenLiteral(self: *const ExpressionStatement) []const u8 {
        return self._token.literal;
    }

    pub fn string(self: *const ExpressionStatement, writer: *std.ArrayList(u8).Writer) StringError!void {
        if (self.expression) |expression| {
            try expression.string(writer);
        } else {
            try writer.writeAll("null");
        }
    }
};

pub const Expression = union(enum) {
    identifier: Identifier,
    integer_literal: IntegerLiteral,
    prefix_expression: PrefixExpression,
    infix_expression: InfixExpression,

    pub fn tokenLiteral(self: *const Expression) []const u8 {
        return switch (self.*) {
            .identifier => |ident| ident.tokenLiteral(),
            .integer_literal => |int| int.tokenLiteral(),
            .prefix_expression => |prefix| prefix.tokenLiteral(),
            .infix_expression => |infix| infix.tokenLiteral(),
        };
    }

    pub fn expressionNode(self: *const Expression) void {
        switch (self.*) {
            .identifier => |ident| ident.expressionNode(),
            .integer_literal => |int| int.expressionNode(),
            .prefix_expression => |prefix| prefix.expressionNode(),
            .infix_expression => |infix| infix.expressionNode(),
        }
    }

    pub fn string(self: *const Expression, writer: *std.ArrayList(u8).Writer) StringError!void {
        switch (self.*) {
            .identifier => |ident| try ident.string(writer),
            .integer_literal => |int| try int.string(writer),
            .prefix_expression => |prefix| try prefix.string(writer),
            .infix_expression => |infix| try infix.string(writer),
        }
    }

    pub fn deinit(self: Expression, allocator: std.mem.Allocator) void {
        switch (self) {
            .identifier, .integer_literal => {},
            .prefix_expression => |prefix| {
                prefix.right.deinit(allocator);
                allocator.destroy(prefix.right);
            },
            .infix_expression => |infix| {
                infix.right.deinit(allocator);
                allocator.destroy(infix.right);
                infix.left.deinit(allocator);
                allocator.destroy(infix.left);
            },
        }
    }
};

pub const Identifier = struct {
    _token: token.Token,
    value: []const u8,

    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self._token.literal;
    }

    pub fn expressionNode(_: *const Identifier) void {}

    pub fn string(self: *const Identifier, writer: *std.ArrayList(u8).Writer) StringError!void {
        try writer.writeAll(self.value);
    }
};

pub const IntegerLiteral = struct {
    _token: token.Token,
    value: i64,

    pub fn tokenLiteral(self: *const IntegerLiteral) []const u8 {
        return self._token.literal;
    }

    pub fn expressionNode(_: *const IntegerLiteral) void {}

    pub fn string(self: *const IntegerLiteral, writer: *std.ArrayList(u8).Writer) StringError!void {
        try writer.writeAll(self._token.literal);
    }
};

pub const PrefixExpression = struct {
    _token: token.Token,
    operator: []const u8,
    right: *Expression,

    pub fn tokenLiteral(self: *const PrefixExpression) []const u8 {
        return self._token.literal;
    }

    pub fn expressionNode(_: *const PrefixExpression) void {}

    pub fn string(self: *const PrefixExpression, writer: *std.ArrayList(u8).Writer) StringError!void {
        try writer.writeByte('(');
        try writer.writeAll(self.operator);
        try self.right.string(writer);
        try writer.writeByte(')');
    }
};

pub const InfixExpression = struct {
    _token: token.Token,
    left: *Expression,
    operator: []const u8,
    right: *Expression,

    pub fn tokenLiteral(self: *const InfixExpression) []const u8 {
        return self._token.literal;
    }

    pub fn expressionNode(_: *const InfixExpression) void {}

    pub fn string(self: *const InfixExpression, writer: *std.ArrayList(u8).Writer) StringError!void {
        try writer.writeByte('(');
        try self.left.string(writer);
        try writer.writeByte(' ');
        try writer.writeAll(self.operator);
        try writer.writeByte(' ');
        try self.right.string(writer);
        try writer.writeByte(')');
    }
};

test "ast foo" {
    var allocator = testing.allocator;
    const name = try allocator.create(Identifier);
    name.* = Identifier{ ._token = token.Token{ ._type = token.IDENT, .literal = "x" }, .value = "x" };

    var statements = std.ArrayList(Statement).init(allocator);
    try statements.append(Statement{ .let_statement = LetStatement{
        ._token = token.Token{ ._type = token.LET, .literal = "let" },
        .name = name,
        .value = Expression{ .identifier = Identifier{ ._token = token.Token{ ._type = token.INT, .literal = "10" }, .value = "10" } },
    } });

    try statements.append(Statement{ .expression_statement = ExpressionStatement{
        ._token = token.Token{ ._type = token.IDENT, .literal = "x" },
        .expression = Expression{ .identifier = Identifier{ ._token = token.Token{ ._type = token.IDENT, .literal = "10" }, .value = "10" } },
    } });

    try statements.append(Statement{ .return_statement = ReturnStatement{
        ._token = token.Token{ ._type = token.RETURN, .literal = "return" },
        .return_value = Expression{ .identifier = Identifier{ ._token = token.Token{ ._type = token.IDENT, .literal = "x" }, .value = "x" } },
    } });

    const program = Program{
        .statements = try statements.toOwnedSlice(),
        .allocator = allocator,
    };
    defer program.deinit();

    var string = std.ArrayList(u8).init(allocator);
    defer string.deinit();
    var writer = string.writer();

    try program.statements[0].string(&writer);
    try testing.expectEqualStrings("let x = 10", string.items);

    string.clearRetainingCapacity();
    try program.statements[1].string(&writer);
    try testing.expectEqualStrings("10", string.items);

    string.clearRetainingCapacity();
    try program.statements[2].string(&writer);
    try testing.expectEqualStrings("return x", string.items);
}
