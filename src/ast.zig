const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;
const AnyWriter = std.io.AnyWriter;

const token = @import("token.zig");

pub const Node = union(enum) {
    program: Program,
    statement: Statement,
    expression: Expression,

    pub fn tokenLiteral(self: *const Node) []const u8 {
        return switch (self.*) {
            inline else => |node| node.tokenLiteral(),
        };
    }

    pub fn string(self: *const Node, writer: AnyWriter) AnyWriter.Error!void {
        switch (self.*) {
            inline else => |node| node.string(writer),
        }
    }
};

pub const Program = struct {
    allocator: Allocator,
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

    pub fn string(self: *const Program, writer: AnyWriter) AnyWriter.Error!void {
        for (self.statements) |stmt| {
            try stmt.string(writer);
        }
    }
};

pub const Statement = union(enum) {
    let_statement: LetStatement,
    return_statement: ReturnStatement,
    expression_statement: ExpressionStatement,
    block_statement: BlockStatement,

    pub fn tokenLiteral(self: *const Statement) []const u8 {
        return switch (self.*) {
            inline else => |stmt| stmt.tokenLiteral(),
        };
    }

    pub fn statementNode(self: *const Statement) void {
        switch (self.*) {
            inline else => |stmt| stmt.statementNode(),
        }
    }

    pub fn string(self: *const Statement, writer: AnyWriter) AnyWriter.Error!void {
        switch (self.*) {
            inline else => |stmt| try stmt.string(writer),
        }
    }

    pub fn deinit(self: Statement, allocator: Allocator) void {
        switch (self) {
            inline else => |stmt| stmt.deinit(allocator),
        }
    }

    pub fn dupe(self: Statement, alloc: Allocator) Allocator.Error!Statement {
        return switch (self) {
            inline else => |stmt| try stmt.dupe(alloc),
        };
    }
};

pub const LetStatement = struct {
    _token: token.Token,
    name: Identifier,
    value: Expression,

    pub fn tokenLiteral(self: *const LetStatement) []const u8 {
        return self._token.literal;
    }

    pub fn statementNode(_: *const LetStatement) void {}

    pub fn string(self: *const LetStatement, writer: AnyWriter) AnyWriter.Error!void {
        try writer.writeAll(self.tokenLiteral());
        try writer.writeByte(' ');

        try self.name.string(writer);

        try writer.writeAll(" = ");
        try self.value.string(writer);

        try writer.writeByte(';');
    }

    pub fn deinit(self: *const LetStatement, allocator: Allocator) void {
        self.value.deinit(allocator);
    }

    pub fn dupe(self: LetStatement, alloc: Allocator) !Statement {
        const duped_token = try self._token.dupe(alloc);
        const duped_name = try self.name.dupe(alloc);
        std.debug.assert(duped_name == .identifier);
        const duped_value = try self.value.dupe(alloc);
        return Statement{
            .let_statement = LetStatement{
                ._token = duped_token,
                .name = duped_name.identifier,
                .value = duped_value,
            },
        };
    }
};

pub const ReturnStatement = struct {
    _token: token.Token,
    return_value: Expression,

    pub fn statementNode(_: *const ReturnStatement) void {}

    pub fn tokenLiteral(self: *const ReturnStatement) []const u8 {
        return self._token.literal;
    }

    pub fn string(self: *const ReturnStatement, writer: AnyWriter) AnyWriter.Error!void {
        try writer.writeAll(self.tokenLiteral());
        try writer.writeByte(' ');
        try self.return_value.string(writer);

        try writer.writeByte(';');
    }

    pub fn deinit(self: *const ReturnStatement, allocator: Allocator) void {
        self.return_value.deinit(allocator);
    }

    pub fn dupe(self: ReturnStatement, alloc: Allocator) !Statement {
        const duped_token = try self._token.dupe(alloc);
        const duped_return_value = try self.return_value.dupe(alloc);
        return Statement{
            .return_statement = ReturnStatement{
                ._token = duped_token,
                .return_value = duped_return_value,
            },
        };
    }
};

pub const ExpressionStatement = struct {
    _token: token.Token,
    expression: Expression,

    pub fn statementNode(_: *const ExpressionStatement) void {}

    pub fn tokenLiteral(self: *const ExpressionStatement) []const u8 {
        return self._token.literal;
    }

    pub fn string(self: *const ExpressionStatement, writer: AnyWriter) AnyWriter.Error!void {
        try self.expression.string(writer);
    }

    pub fn deinit(self: *const ExpressionStatement, allocator: Allocator) void {
        self.expression.deinit(allocator);
    }

    pub fn dupe(self: ExpressionStatement, alloc: Allocator) !Statement {
        const duped_token = try self._token.dupe(alloc);
        const duped_expression = try self.expression.dupe(alloc);
        return Statement{
            .expression_statement = ExpressionStatement{
                ._token = duped_token,
                .expression = duped_expression,
            },
        };
    }
};

pub const BlockStatement = struct {
    _token: token.Token,
    statements: []Statement,

    pub fn tokenLiteral(self: *const BlockStatement) []const u8 {
        return self._token.literal;
    }

    pub fn statementNode(_: *const BlockStatement) void {}

    pub fn string(self: *const BlockStatement, writer: AnyWriter) AnyWriter.Error!void {
        for (self.statements) |stmt| {
            try stmt.string(writer);
        }
    }

    pub fn deinit(self: *const BlockStatement, allocator: Allocator) void {
        for (self.statements) |stmt| {
            stmt.deinit(allocator);
        }
        allocator.free(self.statements);
    }

    pub fn dupe(self: BlockStatement, alloc: Allocator) !Statement {
        const duped_token = try self._token.dupe(alloc);
        var duped_array_list = std.ArrayList(Statement).init(alloc);

        for (self.statements) |stmt| {
            const duped_stmt = try stmt.dupe(alloc);
            try duped_array_list.append(duped_stmt);
        }

        return Statement{
            .block_statement = BlockStatement{
                ._token = duped_token,
                .statements = try duped_array_list.toOwnedSlice(),
            },
        };
    }
};

pub const Expression = union(enum) {
    identifier: Identifier,
    integer_literal: IntegerLiteral,
    prefix_expression: PrefixExpression,
    infix_expression: InfixExpression,
    boolean_expression: Boolean,
    if_expression: IfExpression,
    function_literal: FunctionLiteral,
    call_expression: CallExpression,

    pub fn expressionNode(self: *const Expression) void {
        switch (self.*) {
            inline else => |exp| try exp.expressionNode(),
        }
    }

    pub fn tokenLiteral(self: *const Expression) []const u8 {
        return switch (self.*) {
            inline else => |exp| exp.tokenLiteral(),
        };
    }

    pub fn string(self: *const Expression, writer: AnyWriter) AnyWriter.Error!void {
        switch (self.*) {
            inline else => |exp| try exp.string(writer),
        }
    }

    pub fn deinit(self: Expression, allocator: Allocator) void {
        switch (self) {
            inline else => |exp| exp.deinit(allocator),
        }
    }

    pub fn dupe(self: Expression, alloc: Allocator) Allocator.Error!Expression {
        return switch (self) {
            inline else => |exp| try exp.dupe(alloc),
        };
    }
};

pub const Identifier = struct {
    _token: token.Token,
    value: []const u8,

    pub fn expressionNode(_: *const Identifier) void {}

    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self._token.literal;
    }

    pub fn string(self: *const Identifier, writer: AnyWriter) AnyWriter.Error!void {
        try writer.writeAll(self.value);
    }

    // TODO How do I know if I'm a parser-allocated Identifier or a evaluator-allocated one?
    // TODO Do I need to add a flag to enable extra deinits?
    pub fn deinit(self: *const Identifier, allocator: Allocator) void {
        _ = self; // autofix
        _ = allocator; // autofix
    }

    pub fn dupe(self: *const Identifier, alloc: Allocator) !Expression {
        const duped_token = try self._token.dupe(alloc);
        const duped_literal = try alloc.dupe(u8, self.value);
        return Expression{
            .identifier = Identifier{
                ._token = duped_token,
                .value = duped_literal,
            },
        };
    }
};

pub const IntegerLiteral = struct {
    _token: token.Token,
    value: i64,

    pub fn expressionNode(_: *const IntegerLiteral) void {}

    pub fn tokenLiteral(self: *const IntegerLiteral) []const u8 {
        return self._token.literal;
    }

    pub fn string(self: *const IntegerLiteral, writer: AnyWriter) AnyWriter.Error!void {
        try writer.writeAll(self._token.literal);
    }

    pub fn deinit(self: *const IntegerLiteral, allocator: Allocator) void {
        _ = self; // autofix
        _ = allocator; // autofix
    }

    pub fn dupe(self: IntegerLiteral, alloc: Allocator) !Expression {
        const duped_token = try self._token.dupe(alloc);
        return Expression{
            .integer_literal = IntegerLiteral{
                ._token = duped_token,
                .value = self.value,
            },
        };
    }
};

pub const PrefixExpression = struct {
    _token: token.Token,
    operator: []const u8,
    right: *Expression,

    pub fn expressionNode(_: *const PrefixExpression) void {}

    pub fn tokenLiteral(self: *const PrefixExpression) []const u8 {
        return self._token.literal;
    }

    pub fn string(self: *const PrefixExpression, writer: AnyWriter) AnyWriter.Error!void {
        try writer.writeByte('(');
        try writer.writeAll(self.operator);
        try self.right.string(writer);
        try writer.writeByte(')');
    }

    pub fn deinit(self: *const PrefixExpression, allocator: Allocator) void {
        self.right.deinit(allocator);
        allocator.destroy(self.right);
    }

    pub fn dupe(self: PrefixExpression, alloc: Allocator) !Expression {
        const duped_token = try self._token.dupe(alloc);
        const duped_operator = try alloc.dupe(u8, self.operator);

        const duped_right = try self.right.dupe(alloc);
        const duped_right_ptr = try alloc.create(Expression);
        duped_right_ptr.* = duped_right;

        return Expression{
            .prefix_expression = PrefixExpression{
                ._token = duped_token,
                .operator = duped_operator,
                .right = duped_right_ptr,
            },
        };
    }
};

pub const InfixExpression = struct {
    _token: token.Token,
    left: *Expression,
    operator: []const u8,
    right: *Expression,

    pub fn expressionNode(_: *const InfixExpression) void {}

    pub fn tokenLiteral(self: *const InfixExpression) []const u8 {
        return self._token.literal;
    }

    pub fn string(self: *const InfixExpression, writer: AnyWriter) AnyWriter.Error!void {
        try writer.writeByte('(');
        try self.left.string(writer);
        try writer.writeByte(' ');
        try writer.writeAll(self.operator);
        try writer.writeByte(' ');
        try self.right.string(writer);
        try writer.writeByte(')');
    }

    pub fn deinit(self: *const InfixExpression, allocator: Allocator) void {
        self.right.deinit(allocator);
        allocator.destroy(self.right);
        self.left.deinit(allocator);
        allocator.destroy(self.left);
    }

    pub fn dupe(self: InfixExpression, alloc: Allocator) !Expression {
        const duped_token = try self._token.dupe(alloc);

        const duped_left = try self.left.dupe(alloc);
        const duped_left_ptr = try alloc.create(Expression);
        duped_left_ptr.* = duped_left;

        const duped_operator = try alloc.dupe(u8, self.operator);

        const duped_right = try self.right.dupe(alloc);
        const duped_right_ptr = try alloc.create(Expression);
        duped_right_ptr.* = duped_right;

        return Expression{
            .infix_expression = InfixExpression{
                ._token = duped_token,
                .left = duped_left_ptr,
                .operator = duped_operator,
                .right = duped_right_ptr,
            },
        };
    }
};

pub const Boolean = struct {
    _token: token.Token,
    value: bool,

    pub fn expressionNode(_: *const Boolean) void {}

    pub fn tokenLiteral(self: *const Boolean) []const u8 {
        return self._token.literal;
    }

    pub fn string(self: *const Boolean, writer: AnyWriter) AnyWriter.Error!void {
        try writer.writeAll(self._token.literal);
    }

    pub fn deinit(self: *const Boolean, allocator: Allocator) void {
        _ = self; // autofix
        _ = allocator; // autofix
    }

    pub fn dupe(self: Boolean, alloc: Allocator) !Expression {
        const duped_token = try self._token.dupe(alloc);
        return Expression{
            .boolean_expression = Boolean{
                ._token = duped_token,
                .value = self.value,
            },
        };
    }
};

pub const IfExpression = struct {
    _token: token.Token,
    condition: *Expression,
    consequence: *BlockStatement,
    alternative: ?*BlockStatement,

    pub fn expressionNode(_: *const IfExpression) void {}

    pub fn tokenLiteral(self: *const IfExpression) []const u8 {
        return self._token.literal;
    }

    pub fn string(self: *const IfExpression, writer: AnyWriter) AnyWriter.Error!void {
        try writer.writeAll("if ");
        try self.condition.string(writer);
        try writer.writeByte(' ');
        try self.consequence.string(writer);
        if (self.alternative) |alt| {
            try writer.writeAll(" else ");
            try alt.string(writer);
        }
    }

    pub fn deinit(self: *const IfExpression, allocator: Allocator) void {
        if (self.alternative) |alt| {
            alt.deinit(allocator);
            allocator.destroy(alt);
        }
        self.consequence.deinit(allocator);
        allocator.destroy(self.consequence);
        self.condition.deinit(allocator);
        allocator.destroy(self.condition);
    }

    pub fn dupe(self: IfExpression, alloc: Allocator) !Expression {
        const duped_token = try self._token.dupe(alloc);

        const duped_condition = try self.condition.dupe(alloc);
        const duped_condition_ptr = try alloc.create(Expression);
        duped_condition_ptr.* = duped_condition;

        const duped_consequence = try self.consequence.dupe(alloc);
        const duped_consequence_ptr = try alloc.create(BlockStatement);
        std.debug.assert(duped_consequence == .block_statement);
        duped_consequence_ptr.* = duped_consequence.block_statement;

        var duped_alternative_ptr: ?*BlockStatement = null;
        if (self.alternative) |alt| {
            const duped_alt = try alt.dupe(alloc);
            duped_alternative_ptr = try alloc.create(BlockStatement);
            std.debug.assert(duped_alt == .block_statement);
            duped_alternative_ptr.?.* = duped_alt.block_statement;
        }

        return Expression{
            .if_expression = IfExpression{
                ._token = duped_token,
                .condition = duped_condition_ptr,
                .consequence = duped_consequence_ptr,
                .alternative = duped_alternative_ptr,
            },
        };
    }
};

pub const FunctionLiteral = struct {
    _token: token.Token,
    parameters: []Identifier,
    body: BlockStatement,

    pub fn expressionNode(_: *const FunctionLiteral) void {}

    pub fn tokenLiteral(self: *const FunctionLiteral) []const u8 {
        return self._token.literal;
    }

    pub fn string(self: *const FunctionLiteral, writer: AnyWriter) AnyWriter.Error!void {
        try writer.writeAll(self.tokenLiteral());
        if (self.parameters.len > 0) {
            try self.parameters[0].string(writer);
            for (self.parameters[1..]) |param| {
                try writer.writeAll(", ");
                try param.string(writer);
            }
        }
        try writer.writeAll(") ");
        try self.body.string(writer);
    }

    pub fn deinit(self: *const FunctionLiteral, allocator: Allocator) void {
        self.body.deinit(allocator);
        allocator.free(self.parameters);
    }

    pub fn dupe(self: FunctionLiteral, alloc: Allocator) !Expression {
        const duped_token = try self._token.dupe(alloc);
        var duped_parameters = std.ArrayList(Identifier).init(alloc);
        for (self.parameters) |param| {
            const duped_param = try param.dupe(alloc);
            std.debug.assert(duped_param == .identifier);
            try duped_parameters.append(duped_param.identifier);
        }
        const duped_body = try self.body.dupe(alloc);
        std.debug.assert(duped_body == .block_statement);

        return Expression{
            .function_literal = FunctionLiteral{
                ._token = duped_token,
                .parameters = try duped_parameters.toOwnedSlice(),
                .body = duped_body.block_statement,
            },
        };
    }
};

pub const CallExpression = struct {
    _token: token.Token,
    function: *Expression,
    arguments: []const *Expression,

    pub fn expressionNode(_: *const CallExpression) void {}

    pub fn tokenLiteral(self: *const CallExpression) []const u8 {
        return self._token.literal;
    }

    pub fn string(
        self: *const CallExpression,
        writer: AnyWriter,
    ) AnyWriter.Error!void {
        try self.function.string(writer);
        try writer.writeByte('(');
        if (self.arguments.len > 0) {
            try self.arguments[0].string(writer);
            for (self.arguments[1..]) |arg| {
                try writer.writeAll(", ");
                try arg.string(writer);
            }
        }
        try writer.writeByte(')');
    }

    pub fn deinit(self: *const CallExpression, allocator: Allocator) void {
        for (self.arguments) |arg| {
            arg.deinit(allocator);
            allocator.destroy(arg);
        }
        allocator.free(self.arguments);
        allocator.destroy(self.function);
    }

    pub fn dupe(self: CallExpression, alloc: Allocator) !Expression {
        const duped_token = try self._token.dupe(alloc);

        const duped_function = try self.function.dupe(alloc);
        const duped_function_ptr = try alloc.create(Expression);
        duped_function_ptr.* = duped_function;

        var duped_arguments = std.ArrayList(*Expression).init(alloc);
        for (self.arguments) |arg| {
            const duped_arg = try arg.dupe(alloc);
            const duped_arg_ptr = try alloc.create(Expression);
            duped_arg_ptr.* = duped_arg;
            try duped_arguments.append(duped_arg_ptr);
        }

        return Expression{
            .call_expression = CallExpression{
                ._token = duped_token,
                .function = duped_function_ptr,
                .arguments = try duped_arguments.toOwnedSlice(),
            },
        };
    }
};

test "ast foo" {
    const allocator = testing.allocator;
    const name = Identifier{
        ._token = token.Token{ ._type = token.IDENT, .literal = "x" },
        .value = "x",
    };

    var statements = std.ArrayList(Statement).init(allocator);
    try statements.append(Statement{ .let_statement = LetStatement{
        ._token = token.Token{ ._type = token.LET, .literal = "let" },
        .name = name,
        .value = Expression{
            .identifier = Identifier{
                ._token = token.Token{ ._type = token.INT, .literal = "10" },
                .value = "10",
            },
        },
    } });

    try statements.append(Statement{ .expression_statement = ExpressionStatement{
        ._token = token.Token{ ._type = token.IDENT, .literal = "x" },
        .expression = Expression{
            .identifier = Identifier{
                ._token = token.Token{ ._type = token.IDENT, .literal = "10" },
                .value = "10",
            },
        },
    } });

    try statements.append(Statement{ .return_statement = ReturnStatement{
        ._token = token.Token{ ._type = token.RETURN, .literal = "return" },
        .return_value = Expression{
            .identifier = Identifier{
                ._token = token.Token{ ._type = token.IDENT, .literal = "x" },
                .value = "x",
            },
        },
    } });

    const program = Program{
        .statements = try statements.toOwnedSlice(),
        .allocator = allocator,
    };
    defer program.deinit();

    var string = std.ArrayList(u8).init(allocator);
    defer string.deinit();
    const writer = string.writer().any();

    const string_tests = [_][]const u8{ "let x = 10;", "10", "return x;" };

    for (string_tests, 0..) |string_test, i| {
        try program.statements[i].string(writer);
        try testing.expectEqualStrings(string_test, string.items);
        string.clearRetainingCapacity();
    }
}
