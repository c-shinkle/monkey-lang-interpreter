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

    pub fn deinit(self: *const ExpressionStatement, alloc: Allocator) void {
        self._token.deinit(alloc);

        self.expression.deinit(alloc);
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

    pub fn deinit(self: *const BlockStatement, alloc: Allocator) void {
        self._token.deinit(alloc);

        for (self.statements) |stmt| {
            stmt.deinit(alloc);
        }
        alloc.free(self.statements);
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
    boolean_expression: Boolean,
    prefix_expression: PrefixExpression,
    infix_expression: InfixExpression,
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

    pub fn deinit(self: Expression, alloc: Allocator) void {
        switch (self) {
            inline else => |exp| exp.deinit(alloc),
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

    pub fn deinit(self: *const Identifier, alloc: Allocator) void {
        self._token.deinit(alloc);
    }

    pub fn dupe(self: *const Identifier, alloc: Allocator) !Expression {
        const duped_token = try self._token.dupe(alloc);
        return Expression{
            .identifier = Identifier{
                ._token = duped_token,
                .value = duped_token.literal,
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

    pub fn deinit(self: *const IntegerLiteral, alloc: Allocator) void {
        self._token.deinit(alloc);
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

    pub fn deinit(self: *const PrefixExpression, alloc: Allocator) void {
        self._token.deinit(alloc);
        alloc.free(self.operator);
        self.right.deinit(alloc);
        alloc.destroy(self.right);
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

    pub fn deinit(self: *const InfixExpression, alloc: Allocator) void {
        self._token.deinit(alloc);

        self.left.deinit(alloc);
        alloc.destroy(self.left);

        alloc.free(self.operator);

        self.right.deinit(alloc);
        alloc.destroy(self.right);
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

    pub fn deinit(self: *const Boolean, alloc: Allocator) void {
        self._token.deinit(alloc);
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

    pub fn deinit(self: *const IfExpression, alloc: Allocator) void {
        self._token.deinit(alloc);

        self.consequence.deinit(alloc);
        alloc.destroy(self.consequence);

        self.condition.deinit(alloc);
        alloc.destroy(self.condition);

        if (self.alternative) |alt| {
            alt.deinit(alloc);
            alloc.destroy(alt);
        }
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

    pub fn deinit(self: *const FunctionLiteral, alloc: Allocator) void {
        self._token.deinit(alloc);

        for (self.parameters) |param| {
            param.deinit(alloc);
        }
        alloc.free(self.parameters);

        self.body.deinit(alloc);
    }
};

pub const CallExpression = struct {
    _token: token.Token,
    function: *const Expression,
    arguments: []const Expression,

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

    pub fn deinit(self: *const CallExpression, alloc: Allocator) void {
        self._token.deinit(alloc);

        self.function.deinit(alloc);
        alloc.destroy(self.function);

        for (self.arguments) |arg| {
            arg.deinit(alloc);
        }
        alloc.free(self.arguments);
    }

    pub fn dupe(self: CallExpression, alloc: Allocator) !Expression {
        const duped_token = try self._token.dupe(alloc);

        const duped_function = try self.function.dupe(alloc);
        const duped_function_ptr = try alloc.create(Expression);
        duped_function_ptr.* = duped_function;

        var duped_arguments = std.ArrayList(Expression).init(alloc);
        for (self.arguments) |arg| {
            const duped_arg = try arg.dupe(alloc);
            try duped_arguments.append(duped_arg);
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

// Test Suite

test "Expression deinit" {
    var prefix_right = Expression{
        .boolean_expression = Boolean{
            ._token = token.Token{
                ._type = token.FALSE,
                .literal = "false",
            },
            .value = false,
        },
    };
    var infix_left = Expression{
        .integer_literal = IntegerLiteral{
            ._token = token.Token{
                ._type = token.INT,
                .literal = "0",
            },
            .value = 0,
        },
    };
    var infix_right = Expression{
        .integer_literal = IntegerLiteral{
            ._token = token.Token{
                ._type = token.INT,
                .literal = "1",
            },
            .value = 1,
        },
    };
    var function_parameters = try testing.allocator.alloc(Identifier, 1);
    defer testing.allocator.free(function_parameters);
    function_parameters[0] = Identifier{
        ._token = token.Token{
            ._type = token.IDENT,
            .literal = "a",
        },
        .value = "a",
    };
    var function_statements = [_]Statement{
        Statement{
            .expression_statement = ExpressionStatement{
                ._token = token.Token{
                    ._type = token.INT,
                    .literal = "0",
                },
                .expression = Expression{
                    .integer_literal = IntegerLiteral{
                        ._token = token.Token{
                            ._type = token.INT,
                            .literal = "0",
                        },
                        .value = 0,
                    },
                },
            },
        },
    };
    const function_body = BlockStatement{
        ._token = token.Token{
            ._type = token.INT,
            .literal = "0",
        },
        .statements = &function_statements,
    };
    var if_condition = Expression{
        .boolean_expression = Boolean{
            ._token = token.Token{
                ._type = token.FALSE,
                .literal = "false",
            },
            .value = false,
        },
    };
    var consequence = BlockStatement{
        ._token = token.Token{
            ._type = token.LBRACE,
            .literal = "{",
        },
        .statements = &function_statements,
    };
    var alternative = BlockStatement{
        ._token = token.Token{
            ._type = token.LBRACE,
            .literal = "{",
        },
        .statements = &function_statements,
    };
    const call_function = Expression{
        .identifier = Identifier{
            ._token = token.Token{
                ._type = token.IDENT,
                .literal = "c",
            },
            .value = "c",
        },
    };
    var call_arguments = [_]Expression{
        Expression{
            .integer_literal = IntegerLiteral{
                ._token = token.Token{
                    ._type = token.INT,
                    .literal = "0",
                },
                .value = 0,
            },
        },
    };

    const expressions = .{
        Identifier{
            ._token = token.Token{
                ._type = token.IDENT,
                .literal = "a",
            },
            .value = "a",
        },
        IntegerLiteral{
            ._token = token.Token{
                ._type = token.INT,
                .literal = "0",
            },
            .value = 0,
        },
        Boolean{
            ._token = token.Token{
                ._type = token.FALSE,
                .literal = "false",
            },
            .value = false,
        },
        PrefixExpression{
            ._token = token.Token{
                ._type = token.BANG,
                .literal = "!false",
            },
            .operator = "!",
            .right = &prefix_right,
        },
        InfixExpression{
            ._token = token.Token{
                ._type = token.PLUS,
                .literal = "+",
            },
            .operator = "+",
            .left = &infix_left,
            .right = &infix_right,
        },
        IfExpression{
            ._token = token.Token{
                ._type = token.IF,
                .literal = "if",
            },
            .condition = &if_condition,
            .consequence = &consequence,
            .alternative = &alternative,
        },
        FunctionLiteral{
            ._token = token.Token{
                ._type = token.FUNCTION,
                .literal = "fn",
            },
            .parameters = function_parameters,
            .body = function_body,
        },
        CallExpression{
            ._token = token.Token{
                ._type = token.IDENT,
                .literal = "c",
            },
            .function = &call_function,
            .arguments = &call_arguments,
        },
    };

    inline for (expressions) |exp| {
        var debug_alloc = std.heap.DebugAllocator(.{}){};
        const duped_exp = try exp.dupe(debug_alloc.allocator());
        duped_exp.deinit(debug_alloc.allocator());
        try testing.expectEqual(.ok, debug_alloc.deinit());
    }
}

// test "Statement Deinitialization" {}
