const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;
const AnyWriter = std.io.AnyWriter;

const Token = @import("Token.zig");
const ScopedTokenType = Token.ScopedTokenType;
const getLiteralByOperator = Token.getLiteralByOperator;

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
    statements: []const Statement,

    pub fn parser_deinit(self: *const Program, alloc: Allocator) void {
        for (self.statements) |stmt| {
            stmt.parser_deinit(alloc);
        }
        alloc.free(self.statements);
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

    pub fn dupe(self: *const Program, alloc: Allocator) Allocator.Error!Program {
        var duped_statements = std.ArrayListUnmanaged(Statement).empty;
        for (self.statements) |*stmt| {
            const duped_statement = try stmt.dupe(alloc);
            try duped_statements.append(alloc, duped_statement);
        }

        return Program{ .statements = try duped_statements.toOwnedSlice(alloc) };
    }

    pub fn dupe_deinit(self: *const Program, alloc: Allocator) void {
        for (self.statements) |stmt| {
            stmt.dupe_deinit(alloc);
        }
        alloc.free(self.statements);
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

    pub fn parser_deinit(self: Statement, allocator: std.mem.Allocator) void {
        switch (self) {
            inline else => |stmt| stmt.parser_deinit(allocator),
        }
    }

    pub fn dupe(self: Statement, alloc: Allocator) Allocator.Error!Statement {
        return switch (self) {
            .block_statement => |block| Statement{ .block_statement = try block.dupe(alloc) },
            inline else => |stmt| try stmt.dupe(alloc),
        };
    }

    pub fn dupe_deinit(self: Statement, allocator: Allocator) void {
        switch (self) {
            inline else => |stmt| stmt.dupe_deinit(allocator),
        }
    }
};

pub const LetStatement = struct {
    token: Token,
    name: Identifier,
    value: Expression,

    pub fn tokenLiteral(self: *const LetStatement) []const u8 {
        return self.token.literal;
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

    pub fn parser_deinit(self: *const LetStatement, allocator: std.mem.Allocator) void {
        self.value.parser_deinit(allocator);
    }

    pub fn dupe(self: *const LetStatement, alloc: Allocator) !Statement {
        const duped_token = try self.token.dupe(alloc);
        errdefer duped_token.dupe_deinit(alloc);
        const duped_name = try self.name.dupe(alloc);
        errdefer duped_name.dupe_deinit(alloc);

        const duped_value = try self.value.dupe(alloc);
        // errdefer duped_value.dupe_deinit(alloc);
        return Statement{
            .let_statement = LetStatement{
                .token = duped_token,
                .name = duped_name,
                .value = duped_value,
            },
        };
    }

    pub fn dupe_deinit(self: *const LetStatement, alloc: Allocator) void {
        self.token.dupe_deinit(alloc);

        self.name.dupe_deinit(alloc);

        self.value.dupe_deinit(alloc);
    }
};

pub const ReturnStatement = struct {
    token: Token,
    return_value: Expression,

    pub fn statementNode(_: *const ReturnStatement) void {}

    pub fn tokenLiteral(self: *const ReturnStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const ReturnStatement, writer: AnyWriter) AnyWriter.Error!void {
        try writer.writeAll(self.tokenLiteral());
        try writer.writeByte(' ');
        try self.return_value.string(writer);

        try writer.writeByte(';');
    }

    pub fn parser_deinit(self: *const ReturnStatement, allocator: std.mem.Allocator) void {
        self.return_value.parser_deinit(allocator);
    }

    pub fn dupe(self: *const ReturnStatement, alloc: Allocator) !Statement {
        const duped_token = try self.token.dupe(alloc);
        errdefer duped_token.dupe_deinit(alloc);
        const duped_return_value = try self.return_value.dupe(alloc);
        // errdefer duped_return_value.dupe_deinit(alloc);
        return Statement{
            .return_statement = ReturnStatement{
                .token = duped_token,
                .return_value = duped_return_value,
            },
        };
    }

    pub fn dupe_deinit(self: *const ReturnStatement, alloc: Allocator) void {
        self.token.dupe_deinit(alloc);

        self.return_value.dupe_deinit(alloc);
    }
};

pub const ExpressionStatement = struct {
    token: Token,
    expression: Expression,

    pub fn statementNode(_: *const ExpressionStatement) void {}

    pub fn tokenLiteral(self: *const ExpressionStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const ExpressionStatement, writer: AnyWriter) AnyWriter.Error!void {
        try self.expression.string(writer);
    }

    pub fn parser_deinit(self: *const ExpressionStatement, allocator: std.mem.Allocator) void {
        self.expression.parser_deinit(allocator);
    }

    pub fn dupe(self: *const ExpressionStatement, alloc: Allocator) !Statement {
        const duped_token = try self.token.dupe(alloc);
        errdefer duped_token.dupe_deinit(alloc);
        const duped_expression = try self.expression.dupe(alloc);
        // errdefer duped_expression.dupe_deinit(alloc);
        return Statement{
            .expression_statement = ExpressionStatement{
                .token = duped_token,
                .expression = duped_expression,
            },
        };
    }

    pub fn dupe_deinit(self: *const ExpressionStatement, alloc: Allocator) void {
        self.token.dupe_deinit(alloc);

        self.expression.dupe_deinit(alloc);
    }
};

pub const BlockStatement = struct {
    token: Token,
    statements: []const Statement,

    pub fn tokenLiteral(self: *const BlockStatement) []const u8 {
        return self.token.literal;
    }

    pub fn statementNode(_: *const BlockStatement) void {}

    pub fn string(self: *const BlockStatement, writer: AnyWriter) AnyWriter.Error!void {
        for (self.statements) |stmt| {
            try stmt.string(writer);
        }
    }

    pub fn parser_deinit(self: *const BlockStatement, allocator: std.mem.Allocator) void {
        for (self.statements) |stmt| {
            stmt.parser_deinit(allocator);
        }
        allocator.free(self.statements);
    }

    pub fn dupe(self: *const BlockStatement, alloc: Allocator) !BlockStatement {
        const duped_token = try self.token.dupe(alloc);
        errdefer duped_token.dupe_deinit(alloc);
        var duped_array_list = std.ArrayListUnmanaged(Statement).empty;
        errdefer {
            while (duped_array_list.pop()) |item| item.dupe_deinit(alloc);
            duped_array_list.deinit(alloc);
        }

        for (self.statements) |stmt| {
            const duped_stmt = try stmt.dupe(alloc);
            errdefer duped_stmt.dupe_deinit(alloc);
            try duped_array_list.append(alloc, duped_stmt);
        }

        return BlockStatement{
            .token = duped_token,
            .statements = try duped_array_list.toOwnedSlice(alloc),
        };
    }

    pub fn dupe_deinit(self: *const BlockStatement, alloc: Allocator) void {
        self.token.dupe_deinit(alloc);

        for (self.statements) |stmt| {
            stmt.dupe_deinit(alloc);
        }
        alloc.free(self.statements);
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
    string_literal: StringLiteral,
    array_literal: ArrayLiteral,
    index_expression: IndexExpression,
    hash_literal: HashLiteral,

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

    pub fn parser_deinit(self: Expression, allocator: std.mem.Allocator) void {
        switch (self) {
            inline else => |exp| exp.parser_deinit(allocator),
        }
    }

    pub fn dupe(self: Expression, alloc: Allocator) Allocator.Error!Expression {
        return switch (self) {
            .identifier => |ident| Expression{ .identifier = try ident.dupe(alloc) },
            inline else => |exp| try exp.dupe(alloc),
        };
    }

    pub fn dupe_deinit(self: Expression, alloc: Allocator) void {
        switch (self) {
            inline else => |exp| exp.dupe_deinit(alloc),
        }
    }

    pub const Context = struct {
        pub fn hash(_: Context, key: Expression) HashMap.Hash {
            var hasher = std.hash.Fnv1a_64.init();
            key.hash(&hasher);
            return hasher.final();
        }

        pub fn eql(_: Context, a: Expression, b: Expression) bool {
            return a.eql(&b);
        }
    };

    pub const HashMap = std.HashMapUnmanaged(
        Expression,
        Expression,
        Context,
        std.hash_map.default_max_load_percentage,
    );

    pub fn hash(self: *const Expression, hasher: *std.hash.Fnv1a_64) void {
        switch (self.*) {
            inline else => |exp| exp.hash(hasher),
        }
    }

    pub fn eql(self: *const Expression, other: *const Expression) bool {
        return switch (self.*) {
            inline else => |exp| exp.eql(other),
        };
    }
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,

    pub fn expressionNode(_: *const Identifier) void {}

    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const Identifier, writer: AnyWriter) AnyWriter.Error!void {
        try writer.writeAll(self.value);
    }

    pub fn parser_deinit(_: *const Identifier, _: std.mem.Allocator) void {}

    pub fn dupe(self: *const Identifier, alloc: Allocator) !Identifier {
        const duped_token = try self.token.dupe(alloc);
        return Identifier{
            .token = duped_token,
            .value = duped_token.literal,
        };
    }

    pub fn dupe_deinit(self: *const Identifier, alloc: Allocator) void {
        self.token.dupe_deinit(alloc);
    }

    pub fn hash(_: *const Identifier, _: *std.hash.Fnv1a_64) void {
        unreachable;
    }

    pub fn eql(_: *const Identifier, _: *const Expression) bool {
        unreachable;
        // switch (other.*) {
        //     .identifier => |ident| return std.mem.eql(u8, self.value, ident.value),
        //     else => return false,
        // }
    }
};

pub const IntegerLiteral = struct {
    token: Token,
    value: i64,

    pub fn expressionNode(_: *const IntegerLiteral) void {}

    pub fn tokenLiteral(self: *const IntegerLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const IntegerLiteral, writer: AnyWriter) AnyWriter.Error!void {
        try writer.writeAll(self.token.literal);
    }

    pub fn parser_deinit(_: *const IntegerLiteral, _: std.mem.Allocator) void {}

    pub fn dupe(self: *const IntegerLiteral, alloc: Allocator) !Expression {
        const duped_token = try self.token.dupe(alloc);
        return Expression{
            .integer_literal = IntegerLiteral{
                .token = duped_token,
                .value = self.value,
            },
        };
    }

    pub fn dupe_deinit(self: *const IntegerLiteral, alloc: Allocator) void {
        self.token.dupe_deinit(alloc);
    }

    pub fn hash(self: *const IntegerLiteral, hasher: *std.hash.Fnv1a_64) void {
        hasher.update(std.mem.asBytes(&self.value));
    }

    pub fn eql(self: *const IntegerLiteral, other: *const Expression) bool {
        return switch (other.*) {
            .integer_literal => |int_lit| self.value == int_lit.value,
            else => false,
        };
    }
};

pub const PrefixExpression = struct {
    token: Token,
    operator: ScopedTokenType(.operator),
    right: *Expression,

    pub fn expressionNode(_: *const PrefixExpression) void {}

    pub fn tokenLiteral(self: *const PrefixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const PrefixExpression, writer: AnyWriter) AnyWriter.Error!void {
        try writer.writeByte('(');
        try writer.writeAll(getLiteralByOperator(self.operator));
        try self.right.string(writer);
        try writer.writeByte(')');
    }

    pub fn parser_deinit(self: *const PrefixExpression, allocator: std.mem.Allocator) void {
        self.right.parser_deinit(allocator);
        allocator.destroy(self.right);
    }

    pub fn dupe(self: *const PrefixExpression, alloc: Allocator) !Expression {
        const duped_token = try self.token.dupe(alloc);
        errdefer duped_token.dupe_deinit(alloc);

        const duped_right = try self.right.dupe(alloc);
        errdefer duped_right.dupe_deinit(alloc);
        const duped_right_ptr = try alloc.create(Expression);
        // errdefer alloc.destroy(duped_right_ptr);
        duped_right_ptr.* = duped_right;

        return Expression{
            .prefix_expression = PrefixExpression{
                .token = duped_token,
                .operator = self.operator,
                .right = duped_right_ptr,
            },
        };
    }

    pub fn dupe_deinit(self: *const PrefixExpression, alloc: Allocator) void {
        self.token.dupe_deinit(alloc);
        self.right.dupe_deinit(alloc);
        alloc.destroy(self.right);
    }

    pub fn hash(_: *const PrefixExpression, _: *std.hash.Fnv1a_64) void {
        unreachable;
    }

    pub fn eql(_: *const PrefixExpression, _: *const Expression) bool {
        unreachable;
    }
};

pub const InfixExpression = struct {
    token: Token,
    left: *Expression,
    operator: ScopedTokenType(.operator),
    right: *Expression,

    pub fn expressionNode(_: *const InfixExpression) void {}

    pub fn tokenLiteral(self: *const InfixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const InfixExpression, writer: AnyWriter) AnyWriter.Error!void {
        try writer.writeByte('(');
        try self.left.string(writer);
        try writer.writeByte(' ');
        try writer.writeAll(getLiteralByOperator(self.operator));
        try writer.writeByte(' ');
        try self.right.string(writer);
        try writer.writeByte(')');
    }

    pub fn parser_deinit(self: *const InfixExpression, allocator: std.mem.Allocator) void {
        self.left.parser_deinit(allocator);
        allocator.destroy(self.left);

        self.right.parser_deinit(allocator);
        allocator.destroy(self.right);
    }

    pub fn dupe(self: *const InfixExpression, alloc: Allocator) !Expression {
        const duped_token = try self.token.dupe(alloc);
        errdefer duped_token.dupe_deinit(alloc);

        const duped_left = try self.left.dupe(alloc);
        errdefer duped_left.dupe_deinit(alloc);
        const duped_left_ptr = try alloc.create(Expression);
        errdefer alloc.destroy(duped_left_ptr);
        duped_left_ptr.* = duped_left;

        const duped_right = try self.right.dupe(alloc);
        errdefer duped_right.dupe_deinit(alloc);
        const duped_right_ptr = try alloc.create(Expression);
        errdefer alloc.destroy(duped_right_ptr);
        duped_right_ptr.* = duped_right;

        return Expression{
            .infix_expression = InfixExpression{
                .token = duped_token,
                .left = duped_left_ptr,
                .operator = self.operator,
                .right = duped_right_ptr,
            },
        };
    }

    pub fn dupe_deinit(self: *const InfixExpression, alloc: Allocator) void {
        self.token.dupe_deinit(alloc);

        self.left.dupe_deinit(alloc);
        alloc.destroy(self.left);

        self.right.dupe_deinit(alloc);
        alloc.destroy(self.right);
    }

    pub fn hash(_: *const InfixExpression, _: *std.hash.Fnv1a_64) void {
        unreachable;
    }

    pub fn eql(_: *const InfixExpression, _: *const Expression) bool {
        unreachable;
    }
};

pub const Boolean = struct {
    token: Token,
    value: bool,

    pub fn expressionNode(_: *const Boolean) void {}

    pub fn tokenLiteral(self: *const Boolean) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const Boolean, writer: AnyWriter) AnyWriter.Error!void {
        try writer.writeAll(self.token.literal);
    }

    pub fn parser_deinit(_: *const Boolean, _: std.mem.Allocator) void {}

    pub fn dupe_deinit(self: *const Boolean, alloc: Allocator) void {
        self.token.dupe_deinit(alloc);
    }

    pub fn dupe(self: *const Boolean, alloc: Allocator) !Expression {
        const duped_token = try self.token.dupe(alloc);
        return Expression{
            .boolean_expression = Boolean{
                .token = duped_token,
                .value = self.value,
            },
        };
    }

    pub fn hash(self: *const Boolean, hasher: *std.hash.Fnv1a_64) void {
        hasher.update(std.mem.asBytes(&self.value));
    }

    pub fn eql(self: *const Boolean, other: *const Expression) bool {
        return switch (other.*) {
            .boolean_expression => |bool_exp| self.value == bool_exp.value,
            else => false,
        };
    }
};

pub const IfExpression = struct {
    token: Token,
    condition: *Expression,
    consequence: *BlockStatement,
    alternative: ?*BlockStatement,

    pub fn expressionNode(_: *const IfExpression) void {}

    pub fn tokenLiteral(self: *const IfExpression) []const u8 {
        return self.token.literal;
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

    pub fn parser_deinit(self: *const IfExpression, allocator: std.mem.Allocator) void {
        self.condition.parser_deinit(allocator);
        allocator.destroy(self.condition);

        self.consequence.parser_deinit(allocator);
        allocator.destroy(self.consequence);

        if (self.alternative) |alt| {
            alt.parser_deinit(allocator);
            allocator.destroy(alt);
        }
    }

    pub fn dupe(self: *const IfExpression, alloc: Allocator) !Expression {
        const duped_token = try self.token.dupe(alloc);
        errdefer duped_token.dupe_deinit(alloc);

        const duped_condition = try self.condition.dupe(alloc);
        errdefer duped_condition.dupe_deinit(alloc);
        const duped_condition_ptr = try alloc.create(Expression);
        errdefer alloc.destroy(duped_condition_ptr);
        duped_condition_ptr.* = duped_condition;

        const duped_consequence = try self.consequence.dupe(alloc);
        errdefer duped_consequence.dupe_deinit(alloc);
        const duped_consequence_ptr = try alloc.create(BlockStatement);
        errdefer alloc.destroy(duped_consequence_ptr);
        duped_consequence_ptr.* = duped_consequence;

        var duped_alternative_ptr: ?*BlockStatement = null;
        if (self.alternative) |alt| {
            const duped_alt = try alt.dupe(alloc);
            errdefer duped_alt.dupe_deinit(alloc);
            duped_alternative_ptr = try alloc.create(BlockStatement);
            // errdefer alloc.destroy(duped_alternative_ptr);
            duped_alternative_ptr.?.* = duped_alt;
        }

        return Expression{
            .if_expression = IfExpression{
                .token = duped_token,
                .condition = duped_condition_ptr,
                .consequence = duped_consequence_ptr,
                .alternative = duped_alternative_ptr,
            },
        };
    }

    pub fn dupe_deinit(self: *const IfExpression, alloc: Allocator) void {
        self.token.dupe_deinit(alloc);

        self.consequence.dupe_deinit(alloc);
        alloc.destroy(self.consequence);

        self.condition.dupe_deinit(alloc);
        alloc.destroy(self.condition);

        if (self.alternative) |alt| {
            alt.dupe_deinit(alloc);
            alloc.destroy(alt);
        }
    }

    pub fn hash(_: *const IfExpression, _: *std.hash.Fnv1a_64) void {
        unreachable;
    }

    pub fn eql(_: *const IfExpression, _: *const Expression) bool {
        unreachable;
    }
};

pub const FunctionLiteral = struct {
    token: Token,
    parameters: []const Identifier,
    body: BlockStatement,

    pub fn expressionNode(_: *const FunctionLiteral) void {}

    pub fn tokenLiteral(self: *const FunctionLiteral) []const u8 {
        return self.token.literal;
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

    pub fn parser_deinit(self: *const FunctionLiteral, allocator: std.mem.Allocator) void {
        self.body.parser_deinit(allocator);

        for (self.parameters) |param| {
            param.parser_deinit(allocator);
        }
        allocator.free(self.parameters);
    }

    pub fn dupe(self: *const FunctionLiteral, alloc: Allocator) !Expression {
        const duped_token = try self.token.dupe(alloc);
        errdefer duped_token.dupe_deinit(alloc);
        var duped_array_list = std.ArrayListUnmanaged(Identifier).empty;
        errdefer {
            for (duped_array_list.items) |duped_param| duped_param.dupe_deinit(alloc);
            duped_array_list.deinit(alloc);
        }
        for (self.parameters) |param| {
            const duped_param = try param.dupe(alloc);
            errdefer duped_param.dupe_deinit(alloc);
            try duped_array_list.append(alloc, duped_param);
        }
        const duped_parameters = try duped_array_list.toOwnedSlice(alloc);
        errdefer {
            for (duped_parameters) |duped_param| duped_param.dupe_deinit(alloc);
            alloc.free(duped_parameters);
        }

        const duped_body = try self.body.dupe(alloc);
        // errdefer duped_body.dupe_deinit(alloc);

        return Expression{
            .function_literal = FunctionLiteral{
                .token = duped_token,
                .parameters = duped_parameters,
                .body = duped_body,
            },
        };
    }

    pub fn dupe_deinit(self: *const FunctionLiteral, alloc: Allocator) void {
        self.token.dupe_deinit(alloc);

        for (self.parameters) |param| {
            param.dupe_deinit(alloc);
        }
        alloc.free(self.parameters);

        self.body.dupe_deinit(alloc);
    }

    pub fn hash(_: *const FunctionLiteral, _: *std.hash.Fnv1a_64) void {
        unreachable;
    }

    pub fn eql(_: *const FunctionLiteral, _: *const Expression) bool {
        unreachable;
    }
};

pub const CallExpression = struct {
    token: Token,
    function: *const Expression,
    arguments: []const Expression,

    pub fn expressionNode(_: *const CallExpression) void {}

    pub fn tokenLiteral(self: *const CallExpression) []const u8 {
        return self.token.literal;
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

    pub fn parser_deinit(self: *const CallExpression, allocator: std.mem.Allocator) void {
        self.function.parser_deinit(allocator);
        allocator.destroy(self.function);

        for (self.arguments) |arg| {
            arg.parser_deinit(allocator);
        }
        allocator.free(self.arguments);
    }

    pub fn dupe(self: *const CallExpression, alloc: Allocator) !Expression {
        const duped_token = try self.token.dupe(alloc);
        errdefer duped_token.dupe_deinit(alloc);

        const duped_function = try self.function.dupe(alloc);
        errdefer duped_function.dupe_deinit(alloc);
        const duped_function_ptr = try alloc.create(Expression);
        errdefer alloc.destroy(duped_function_ptr);
        duped_function_ptr.* = duped_function;

        var duped_arguments = std.ArrayListUnmanaged(Expression).empty;
        errdefer {
            for (duped_arguments.items) |duped_arg| duped_arg.dupe_deinit(alloc);
            duped_arguments.deinit(alloc);
        }

        for (self.arguments) |arg| {
            const duped_arg = try arg.dupe(alloc);
            errdefer duped_arg.dupe_deinit(alloc);
            try duped_arguments.append(alloc, duped_arg);
        }

        return Expression{
            .call_expression = CallExpression{
                .token = duped_token,
                .function = duped_function_ptr,
                .arguments = try duped_arguments.toOwnedSlice(alloc),
            },
        };
    }

    pub fn dupe_deinit(self: *const CallExpression, alloc: Allocator) void {
        self.token.dupe_deinit(alloc);

        self.function.dupe_deinit(alloc);
        alloc.destroy(self.function);

        for (self.arguments) |arg| {
            arg.dupe_deinit(alloc);
        }
        alloc.free(self.arguments);
    }

    pub fn hash(_: *const CallExpression, _: *std.hash.Fnv1a_64) void {
        unreachable;
    }

    pub fn eql(_: *const CallExpression, _: *const Expression) bool {
        unreachable;
    }
};

pub const StringLiteral = struct {
    token: Token,

    pub fn expressionNode(_: *const StringLiteral) void {}

    pub fn tokenLiteral(self: *const StringLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const StringLiteral, writer: AnyWriter) AnyWriter.Error!void {
        try writer.writeAll(self.token.literal);
    }

    pub fn parser_deinit(_: *const StringLiteral, _: std.mem.Allocator) void {}

    pub fn dupe(self: *const StringLiteral, alloc: Allocator) !Expression {
        const duped_token = try self.token.dupe(alloc);
        return Expression{ .string_literal = StringLiteral{ .token = duped_token } };
    }

    pub fn dupe_deinit(self: *const StringLiteral, alloc: Allocator) void {
        self.token.dupe_deinit(alloc);
    }

    pub fn hash(self: *const StringLiteral, hasher: *std.hash.Fnv1a_64) void {
        hasher.update(std.mem.asBytes(&self.token.token_type));
        hasher.update(self.token.literal);
    }

    pub fn eql(self: *const StringLiteral, other: *const Expression) bool {
        switch (other.*) {
            .string_literal => |str_lit| {
                return std.mem.eql(u8, self.token.literal, str_lit.token.literal);
            },
            else => return false,
        }
    }
};

pub const ArrayLiteral = struct {
    token: Token,
    elements: []const Expression,

    pub fn expresssionNode(_: *const ArrayLiteral) void {}

    pub fn tokenLiteral(self: *const ArrayLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const ArrayLiteral, writer: AnyWriter) AnyWriter.Error!void {
        try writer.writeByte('[');

        for (self.elements) |*element| {
            try element.string(writer);
        }

        try writer.writeByte(']');
    }

    pub fn parser_deinit(self: *const ArrayLiteral, alloc: Allocator) void {
        alloc.free(self.elements);
    }

    pub fn dupe(self: *const ArrayLiteral, alloc: Allocator) !Expression {
        const duped_token = try self.token.dupe(alloc);

        var duped_elements = try alloc.alloc(Expression, self.elements.len);
        for (self.elements, 0..) |element, i| {
            duped_elements[i] = try element.dupe(alloc);
        }

        return Expression{
            .array_literal = ArrayLiteral{
                .token = duped_token,
                .elements = duped_elements,
            },
        };
    }

    pub fn dupe_deinit(self: *const ArrayLiteral, alloc: Allocator) void {
        self.token.dupe_deinit(alloc);

        for (self.elements) |element| {
            element.dupe_deinit(alloc);
        }
    }

    pub fn hash(_: *const ArrayLiteral, _: *std.hash.Fnv1a_64) void {
        unreachable;
    }

    pub fn eql(_: *const ArrayLiteral, _: *const Expression) bool {
        unreachable;
    }
};

pub const IndexExpression = struct {
    token: Token,
    left: *Expression,
    index: *Expression,

    pub fn expresionNode(_: *const IndexExpression) void {}

    pub fn tokenLiteral(self: *const IndexExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const IndexExpression, writer: AnyWriter) AnyWriter.Error!void {
        try writer.writeByte('(');
        try self.left.string(writer);
        try writer.writeByte('[');
        try self.index.string(writer);
        try writer.writeByte(']');
    }

    pub fn parser_deinit(self: *const IndexExpression, alloc: std.mem.Allocator) void {
        self.left.parser_deinit(alloc);
        alloc.destroy(self.left);

        self.index.parser_deinit(alloc);
        alloc.destroy(self.index);
    }

    pub fn dupe(self: *const IndexExpression, alloc: std.mem.Allocator) !Expression {
        const duped_token = try self.token.dupe(alloc);
        errdefer duped_token.dupe_deinit(alloc);

        const duped_left = try self.left.dupe(alloc);
        errdefer duped_left.dupe_deinit(alloc);
        const duped_left_ptr = try alloc.create(Expression);
        errdefer alloc.destroy(duped_left_ptr);
        duped_left_ptr.* = duped_left;

        const duped_right = try self.index.dupe(alloc);
        errdefer duped_right.dupe_deinit(alloc);
        const duped_right_ptr = try alloc.create(Expression);
        errdefer alloc.destroy(duped_right_ptr);
        duped_right_ptr.* = duped_right;

        return Expression{
            .index_expression = IndexExpression{
                .token = duped_token,
                .left = duped_left_ptr,
                .index = duped_right_ptr,
            },
        };
    }

    pub fn dupe_deinit(self: *const IndexExpression, alloc: Allocator) void {
        self.token.dupe_deinit(alloc);

        self.left.dupe_deinit(alloc);
        alloc.destroy(self.left);

        self.index.dupe_deinit(alloc);
        alloc.destroy(self.index);
    }

    pub fn hash(_: *const IndexExpression, _: *std.hash.Fnv1a_64) void {
        unreachable;
    }

    pub fn eql(_: *const IndexExpression, _: *const Expression) bool {
        unreachable;
    }
};

pub const HashLiteral = struct {
    token: Token,
    pairs: Expression.HashMap,

    pub fn expresionNode(_: *const HashLiteral) void {}

    pub fn tokenLiteral(self: *const HashLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const HashLiteral, writer: AnyWriter) AnyWriter.Error!void {
        try writer.writeAll(Token.LBRACE);

        var iter = self.pairs.iterator();
        while (iter.next()) |entry| {
            try entry.key_ptr.string(writer);
            try writer.writeAll(Token.COLON);
            try entry.value_ptr.string(writer);
        }

        try writer.writeAll(Token.RBRACE);
    }

    pub fn parser_deinit(_: *const HashLiteral, _: std.mem.Allocator) void {
        unreachable;
    }

    pub fn dupe(_: *const HashLiteral, _: std.mem.Allocator) !Expression {
        unreachable;
    }

    pub fn dupe_deinit(_: *const HashLiteral, _: std.mem.Allocator) void {
        unreachable;
    }

    pub fn hash(_: *const HashLiteral, _: *std.hash.Fnv1a_64) void {
        unreachable;
    }

    pub fn eql(_: *const HashLiteral, _: *const Expression) bool {
        unreachable;
    }
};

// Test Suite

test "Expression Out of Memory" {
    var prefix_right = Expression{
        .boolean_expression = Boolean{
            .token = Token{
                .token_type = ._false,
                .literal = "false",
            },
            .value = false,
        },
    };
    var infix_left = Expression{
        .integer_literal = IntegerLiteral{
            .token = Token{
                .token_type = .int,
                .literal = "0",
            },
            .value = 0,
        },
    };
    var infix_right = Expression{
        .integer_literal = IntegerLiteral{
            .token = Token{
                .token_type = .int,
                .literal = "1",
            },
            .value = 1,
        },
    };
    var function_parameters = try testing.allocator.alloc(Identifier, 1);
    defer testing.allocator.free(function_parameters);
    function_parameters[0] = Identifier{
        .token = Token{
            .token_type = .identifier,
            .literal = "a",
        },
        .value = "a",
    };
    var function_statements = [_]Statement{
        Statement{
            .expression_statement = ExpressionStatement{
                .token = Token{
                    .token_type = .int,
                    .literal = "0",
                },
                .expression = Expression{
                    .integer_literal = IntegerLiteral{
                        .token = Token{
                            .token_type = .int,
                            .literal = "0",
                        },
                        .value = 0,
                    },
                },
            },
        },
    };
    const function_body = BlockStatement{
        .token = Token{
            .token_type = .int,
            .literal = "0",
        },
        .statements = &function_statements,
    };
    var if_condition = Expression{
        .boolean_expression = Boolean{
            .token = Token{
                .token_type = ._false,
                .literal = "false",
            },
            .value = false,
        },
    };
    var consequence = BlockStatement{
        .token = Token{
            .token_type = .lbrace,
            .literal = "{",
        },
        .statements = &function_statements,
    };
    var alternative = BlockStatement{
        .token = Token{
            .token_type = .lbrace,
            .literal = "{",
        },
        .statements = &function_statements,
    };
    const call_function = Expression{
        .identifier = Identifier{
            .token = Token{
                .token_type = .identifier,
                .literal = "c",
            },
            .value = "c",
        },
    };
    var call_arguments = [_]Expression{
        Expression{
            .integer_literal = IntegerLiteral{
                .token = Token{
                    .token_type = .int,
                    .literal = "0",
                },
                .value = 0,
            },
        },
    };

    const expressions = [_]Expression{
        Expression{
            .identifier = Identifier{
                .token = Token{
                    .token_type = .identifier,
                    .literal = "a",
                },
                .value = "a",
            },
        },
        Expression{
            .integer_literal = IntegerLiteral{
                .token = Token{
                    .token_type = .int,
                    .literal = "0",
                },
                .value = 0,
            },
        },
        Expression{
            .boolean_expression = Boolean{
                .token = Token{
                    .token_type = ._false,
                    .literal = "false",
                },
                .value = false,
            },
        },
        Expression{
            .prefix_expression = PrefixExpression{
                .token = Token{
                    .token_type = .bang,
                    .literal = "!false",
                },
                .operator = .bang,
                .right = &prefix_right,
            },
        },
        Expression{
            .infix_expression = InfixExpression{
                .token = Token{
                    .token_type = .plus,
                    .literal = "+",
                },
                .operator = .plus,
                .left = &infix_left,
                .right = &infix_right,
            },
        },
        Expression{
            .if_expression = IfExpression{
                .token = Token{
                    .token_type = ._if,
                    .literal = "if",
                },
                .condition = &if_condition,
                .consequence = &consequence,
                .alternative = &alternative,
            },
        },
        Expression{
            .function_literal = FunctionLiteral{
                .token = Token{
                    .token_type = ._function,
                    .literal = "fn",
                },
                .parameters = function_parameters,
                .body = function_body,
            },
        },
        Expression{
            .call_expression = CallExpression{
                .token = Token{
                    .token_type = .identifier,
                    .literal = "c",
                },
                .function = &call_function,
                .arguments = &call_arguments,
            },
        },
    };

    for (expressions) |expression| {
        try testing.checkAllAllocationFailures(
            testing.allocator,
            testExpressionOutOfMemory,
            .{expression},
        );
    }
}

test "Expression deinit" {
    var prefix_right = Expression{
        .boolean_expression = Boolean{
            .token = Token{
                .token_type = ._false,
                .literal = "false",
            },
            .value = false,
        },
    };
    var infix_left = Expression{
        .integer_literal = IntegerLiteral{
            .token = Token{
                .token_type = .int,
                .literal = "0",
            },
            .value = 0,
        },
    };
    var infix_right = Expression{
        .integer_literal = IntegerLiteral{
            .token = Token{
                .token_type = .int,
                .literal = "1",
            },
            .value = 1,
        },
    };
    var function_parameters = try testing.allocator.alloc(Identifier, 1);
    defer testing.allocator.free(function_parameters);
    function_parameters[0] = Identifier{
        .token = Token{
            .token_type = .identifier,
            .literal = "a",
        },
        .value = "a",
    };
    var function_statements = [_]Statement{
        Statement{
            .expression_statement = ExpressionStatement{
                .token = Token{
                    .token_type = .int,
                    .literal = "0",
                },
                .expression = Expression{
                    .integer_literal = IntegerLiteral{
                        .token = Token{
                            .token_type = .int,
                            .literal = "0",
                        },
                        .value = 0,
                    },
                },
            },
        },
    };
    const function_body = BlockStatement{
        .token = Token{
            .token_type = .int,
            .literal = "0",
        },
        .statements = &function_statements,
    };
    var if_condition = Expression{
        .boolean_expression = Boolean{
            .token = Token{
                .token_type = ._false,
                .literal = "false",
            },
            .value = false,
        },
    };
    var consequence = BlockStatement{
        .token = Token{
            .token_type = .lbrace,
            .literal = "{",
        },
        .statements = &function_statements,
    };
    var alternative = BlockStatement{
        .token = Token{
            .token_type = .lbrace,
            .literal = "{",
        },
        .statements = &function_statements,
    };
    const call_function = Expression{
        .identifier = Identifier{
            .token = Token{
                .token_type = .identifier,
                .literal = "c",
            },
            .value = "c",
        },
    };
    var call_arguments = [_]Expression{
        Expression{
            .integer_literal = IntegerLiteral{
                .token = Token{
                    .token_type = .int,
                    .literal = "0",
                },
                .value = 0,
            },
        },
    };

    const expressions = [_]Expression{
        Expression{
            .identifier = Identifier{
                .token = Token{
                    .token_type = .identifier,
                    .literal = "a",
                },
                .value = "a",
            },
        },
        Expression{
            .integer_literal = IntegerLiteral{
                .token = Token{
                    .token_type = .int,
                    .literal = "0",
                },
                .value = 0,
            },
        },
        Expression{
            .boolean_expression = Boolean{
                .token = Token{
                    .token_type = ._false,
                    .literal = "false",
                },
                .value = false,
            },
        },
        Expression{
            .prefix_expression = PrefixExpression{
                .token = Token{
                    .token_type = .bang,
                    .literal = "!false",
                },
                .operator = .bang,
                .right = &prefix_right,
            },
        },
        Expression{
            .infix_expression = InfixExpression{
                .token = Token{
                    .token_type = .plus,
                    .literal = "+",
                },
                .operator = .plus,
                .left = &infix_left,
                .right = &infix_right,
            },
        },
        Expression{
            .if_expression = IfExpression{
                .token = Token{
                    .token_type = ._if,
                    .literal = "if",
                },
                .condition = &if_condition,
                .consequence = &consequence,
                .alternative = &alternative,
            },
        },
        Expression{
            .function_literal = FunctionLiteral{
                .token = Token{
                    .token_type = ._function,
                    .literal = "fn",
                },
                .parameters = function_parameters,
                .body = function_body,
            },
        },
        Expression{
            .call_expression = CallExpression{
                .token = Token{
                    .token_type = .identifier,
                    .literal = "c",
                },
                .function = &call_function,
                .arguments = &call_arguments,
            },
        },
    };

    for (expressions) |exp| {
        var debug_alloc = std.heap.DebugAllocator(.{}){};
        const duped_exp = try exp.dupe(debug_alloc.allocator());
        duped_exp.dupe_deinit(debug_alloc.allocator());
        try testing.expectEqual(.ok, debug_alloc.deinit());
    }
}

test "Statement Out of Memory" {
    const block_statements = [_]Statement{
        Statement{
            .expression_statement = ExpressionStatement{
                .token = Token{
                    .token_type = .int,
                    .literal = "0",
                },
                .expression = Expression{
                    .integer_literal = IntegerLiteral{
                        .token = Token{
                            .token_type = .int,
                            .literal = "0",
                        },
                        .value = 0,
                    },
                },
            },
        },
    };

    const statements = [_]Statement{
        Statement{
            .let_statement = LetStatement{
                .token = Token{
                    .token_type = .let,
                    .literal = "let",
                },
                .name = Identifier{
                    .token = Token{
                        .token_type = .identifier,
                        .literal = "a",
                    },
                    .value = "a",
                },
                .value = Expression{
                    .integer_literal = IntegerLiteral{
                        .token = Token{
                            .token_type = .int,
                            .literal = "0",
                        },
                        .value = 0,
                    },
                },
            },
        },
        Statement{
            .return_statement = ReturnStatement{
                .token = Token{
                    .token_type = ._return,
                    .literal = "return",
                },
                .return_value = Expression{
                    .integer_literal = IntegerLiteral{
                        .token = Token{
                            .token_type = .int,
                            .literal = "0",
                        },
                        .value = 0,
                    },
                },
            },
        },
        Statement{
            .expression_statement = ExpressionStatement{
                .token = Token{
                    .token_type = ._true,
                    .literal = "true",
                },
                .expression = Expression{
                    .boolean_expression = Boolean{
                        .token = Token{
                            .token_type = ._true,
                            .literal = "true",
                        },
                        .value = true,
                    },
                },
            },
        },
        Statement{
            .block_statement = BlockStatement{
                .token = Token{
                    .token_type = .lbrace,
                    .literal = "{",
                },
                .statements = &block_statements,
            },
        },
    };

    for (statements) |statement| {
        try testing.checkAllAllocationFailures(
            testing.allocator,
            testStatementOutOfMemory,
            .{statement},
        );
    }
}

test "Statement deinit" {
    const block_statements = [_]Statement{
        Statement{
            .expression_statement = ExpressionStatement{
                .token = Token{
                    .token_type = .int,
                    .literal = "0",
                },
                .expression = Expression{
                    .integer_literal = IntegerLiteral{
                        .token = Token{
                            .token_type = .int,
                            .literal = "0",
                        },
                        .value = 0,
                    },
                },
            },
        },
    };

    const statements = [_]Statement{
        Statement{
            .let_statement = LetStatement{
                .token = Token{
                    .token_type = .let,
                    .literal = "let",
                },
                .name = Identifier{
                    .token = Token{
                        .token_type = .identifier,
                        .literal = "a",
                    },
                    .value = "a",
                },
                .value = Expression{
                    .integer_literal = IntegerLiteral{
                        .token = Token{
                            .token_type = .int,
                            .literal = "0",
                        },
                        .value = 0,
                    },
                },
            },
        },
        Statement{
            .return_statement = ReturnStatement{
                .token = Token{
                    .token_type = ._return,
                    .literal = "return",
                },
                .return_value = Expression{
                    .integer_literal = IntegerLiteral{
                        .token = Token{
                            .token_type = .int,
                            .literal = "0",
                        },
                        .value = 0,
                    },
                },
            },
        },
        Statement{
            .expression_statement = ExpressionStatement{
                .token = Token{
                    .token_type = ._true,
                    .literal = "true",
                },
                .expression = Expression{
                    .boolean_expression = Boolean{
                        .token = Token{
                            .token_type = ._true,
                            .literal = "true",
                        },
                        .value = true,
                    },
                },
            },
        },
        Statement{
            .block_statement = BlockStatement{
                .token = Token{
                    .token_type = .lbrace,
                    .literal = "{",
                },
                .statements = &block_statements,
            },
        },
    };

    for (statements) |stmt| {
        var debug_alloc = std.heap.DebugAllocator(.{}){};
        const duped_exp = try stmt.dupe(debug_alloc.allocator());
        duped_exp.dupe_deinit(debug_alloc.allocator());
        try testing.expectEqual(.ok, debug_alloc.deinit());
    }
}

test "Program dupe deinit" {
    var block_statements = [_]Statement{
        Statement{
            .expression_statement = ExpressionStatement{
                .token = Token{
                    .token_type = .int,
                    .literal = "0",
                },
                .expression = Expression{
                    .integer_literal = IntegerLiteral{
                        .token = Token{
                            .token_type = .int,
                            .literal = "0",
                        },
                        .value = 0,
                    },
                },
            },
        },
    };
    const statements = .{
        Statement{
            .let_statement = LetStatement{
                .token = Token{
                    .token_type = .let,
                    .literal = "let",
                },
                .name = Identifier{
                    .token = Token{
                        .token_type = .identifier,
                        .literal = "a",
                    },
                    .value = "a",
                },
                .value = Expression{
                    .integer_literal = IntegerLiteral{
                        .token = Token{
                            .token_type = .int,
                            .literal = "0",
                        },
                        .value = 0,
                    },
                },
            },
        },
        Statement{
            .return_statement = ReturnStatement{
                .token = Token{
                    .token_type = ._return,
                    .literal = "return",
                },
                .return_value = Expression{
                    .integer_literal = IntegerLiteral{
                        .token = Token{
                            .token_type = .int,
                            .literal = "0",
                        },
                        .value = 0,
                    },
                },
            },
        },
        Statement{
            .expression_statement = ExpressionStatement{
                .token = Token{
                    .token_type = ._true,
                    .literal = "true",
                },
                .expression = Expression{
                    .boolean_expression = Boolean{
                        .token = Token{
                            .token_type = ._true,
                            .literal = "true",
                        },
                        .value = true,
                    },
                },
            },
        },
        Statement{
            .block_statement = BlockStatement{
                .token = Token{
                    .token_type = .lbrace,
                    .literal = "{",
                },
                .statements = &block_statements,
            },
        },
    };

    var debug_alloc = std.heap.DebugAllocator(.{}){};
    const program = Program{ .statements = &statements };

    const duped_progam = try program.dupe(debug_alloc.allocator());
    duped_progam.dupe_deinit(debug_alloc.allocator());

    try testing.expectEqual(.ok, debug_alloc.deinit());
}

fn testExpressionOutOfMemory(alloc: Allocator, exp: Expression) !void {
    const duped_exp = try exp.dupe(alloc);
    duped_exp.dupe_deinit(alloc);
}

fn testStatementOutOfMemory(alloc: Allocator, stmt: Statement) !void {
    const duped_exp = try stmt.dupe(alloc);
    duped_exp.dupe_deinit(alloc);
}

test {
    std.testing.refAllDecls(@This());
}
