pub const Node = union(enum) {
    program: Program,
    statement: Statement,
    expression: Expression,

    pub fn tokenLiteral(self: Node) []const u8 {
        return switch (self) {
            inline else => |node| node.tokenLiteral(),
        };
    }

    pub fn string(self: Node, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self) {
            inline else => |node| node.string(writer),
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

    pub fn string(self: *const Program, writer: *std.Io.Writer) std.Io.Writer.Error!void {
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
};

pub const Statement = union(enum) {
    let_statement: LetStatement,
    return_statement: ReturnStatement,
    expression_statement: ExpressionStatement,
    block_statement: BlockStatement,

    pub fn tokenLiteral(self: Statement) []const u8 {
        return switch (self) {
            inline else => |stmt| stmt.tokenLiteral(),
        };
    }

    pub fn statementNode(self: Statement) void {
        switch (self) {
            inline else => |stmt| stmt.statementNode(),
        }
    }

    pub fn string(self: Statement, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self) {
            inline else => |stmt| try stmt.string(writer),
        }
    }

    pub fn dupe(self: Statement, alloc: Allocator) Allocator.Error!Statement {
        return switch (self) {
            .block_statement => |block| Statement{ .block_statement = try block.dupe(alloc) },
            inline else => |stmt| try stmt.dupe(alloc),
        };
    }
};

pub const LetStatement = struct {
    token: Token,
    name: Identifier,
    value: Expression,

    pub fn tokenLiteral(self: *const LetStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const LetStatement, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeAll(self.tokenLiteral());
        try writer.writeByte(' ');

        try self.name.string(writer);

        try writer.writeAll(" = ");
        try self.value.string(writer);

        try writer.writeByte(';');
    }

    pub fn dupe(self: *const LetStatement, alloc: Allocator) !Statement {
        const duped_token = try self.token.dupe(alloc);
        const duped_name = try self.name.dupe(alloc);

        const duped_value = try self.value.dupe(alloc);
        return Statement{
            .let_statement = LetStatement{
                .token = duped_token,
                .name = duped_name,
                .value = duped_value,
            },
        };
    }
};

pub const ReturnStatement = struct {
    token: Token,
    return_value: Expression,

    pub fn tokenLiteral(self: *const ReturnStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const ReturnStatement, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeAll(self.tokenLiteral());
        try writer.writeByte(' ');
        try self.return_value.string(writer);

        try writer.writeByte(';');
    }

    pub fn dupe(self: *const ReturnStatement, alloc: Allocator) !Statement {
        const duped_token = try self.token.dupe(alloc);
        const duped_return_value = try self.return_value.dupe(alloc);
        return Statement{
            .return_statement = ReturnStatement{
                .token = duped_token,
                .return_value = duped_return_value,
            },
        };
    }
};

pub const ExpressionStatement = struct {
    token: Token,
    expression: Expression,

    pub fn tokenLiteral(self: *const ExpressionStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const ExpressionStatement, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try self.expression.string(writer);
    }

    pub fn dupe(self: *const ExpressionStatement, alloc: Allocator) !Statement {
        const duped_token = try self.token.dupe(alloc);
        const duped_expression = try self.expression.dupe(alloc);
        return Statement{
            .expression_statement = ExpressionStatement{
                .token = duped_token,
                .expression = duped_expression,
            },
        };
    }
};

pub const BlockStatement = struct {
    token: Token,
    statements: []const Statement,

    pub fn tokenLiteral(self: *const BlockStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const BlockStatement, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        for (self.statements) |stmt| {
            try stmt.string(writer);
        }
    }

    pub fn dupe(self: *const BlockStatement, alloc: Allocator) !BlockStatement {
        const duped_token = try self.token.dupe(alloc);
        var duped_array_list = std.ArrayListUnmanaged(Statement).empty;

        for (self.statements) |stmt| {
            const duped_stmt = try stmt.dupe(alloc);
            try duped_array_list.append(alloc, duped_stmt);
        }

        return BlockStatement{
            .token = duped_token,
            .statements = try duped_array_list.toOwnedSlice(alloc),
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
    string_literal: StringLiteral,
    array_literal: ArrayLiteral,
    index_expression: IndexExpression,
    hash_literal: HashLiteral,

    pub fn tokenLiteral(self: Expression) []const u8 {
        return switch (self) {
            inline else => |exp| exp.tokenLiteral(),
        };
    }

    pub fn string(self: Expression, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self) {
            inline else => |exp| try exp.string(writer),
        }
    }

    pub fn dupe(self: Expression, alloc: Allocator) Allocator.Error!Expression {
        return switch (self) {
            .identifier => |ident| Expression{ .identifier = try ident.dupe(alloc) },
            inline else => |exp| try exp.dupe(alloc),
        };
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

    pub fn hash(self: Expression, hasher: *std.hash.Fnv1a_64) void {
        switch (self) {
            .identifier => |ident| ident.hash(hasher),
            .integer_literal => |int_lit| int_lit.hash(hasher),
            .boolean_expression => |bool_exp| bool_exp.hash(hasher),
            .prefix_expression => |prefix_exp| prefix_exp.hash(hasher),
            .infix_expression => |infix_exp| infix_exp.hash(hasher),
            .string_literal => |str_lit| str_lit.hash(hasher),
            else => unreachable,
        }
    }

    pub fn eql(self: Expression, other: *const Expression) bool {
        return switch (self) {
            .identifier => |ident| ident.eql(other),
            .integer_literal => |int_lit| int_lit.eql(other),
            .boolean_expression => |bool_exp| bool_exp.eql(other),
            .prefix_expression => |prefix_exp| prefix_exp.eql(other),
            .infix_expression => |infix_exp| infix_exp.eql(other),
            .string_literal => |str_lit| str_lit.eql(other),
            else => unreachable,
        };
    }
};

pub const Identifier = struct {
    token: Token,
    value: []const u8,

    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const Identifier, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeAll(self.value);
    }

    pub fn dupe(self: *const Identifier, alloc: Allocator) !Identifier {
        const duped_token = try self.token.dupe(alloc);
        return Identifier{
            .token = duped_token,
            .value = duped_token.literal,
        };
    }

    pub fn hash(self: *const Identifier, hasher: *std.hash.Fnv1a_64) void {
        hasher.update(std.mem.asBytes(&self.token.token_type));
        hasher.update(self.value);
    }

    pub fn eql(self: *const Identifier, other: *const Expression) bool {
        switch (other.*) {
            .identifier => |ident| return std.mem.eql(u8, self.value, ident.value),
            else => return false,
        }
    }
};

pub const IntegerLiteral = struct {
    token: Token,
    value: i64,

    pub fn tokenLiteral(self: *const IntegerLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const IntegerLiteral, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeAll(self.token.literal);
    }

    pub fn dupe(self: *const IntegerLiteral, alloc: Allocator) !Expression {
        const duped_token = try self.token.dupe(alloc);
        return Expression{
            .integer_literal = IntegerLiteral{
                .token = duped_token,
                .value = self.value,
            },
        };
    }

    pub fn hash(self: *const IntegerLiteral, hasher: *std.hash.Fnv1a_64) void {
        hasher.update(std.mem.asBytes(&self.token.token_type));
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

    pub fn tokenLiteral(self: *const PrefixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const PrefixExpression, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeByte('(');
        try writer.writeAll(getLiteralByOperator(self.operator));
        try self.right.string(writer);
        try writer.writeByte(')');
    }

    pub fn dupe(self: *const PrefixExpression, alloc: Allocator) !Expression {
        const duped_token = try self.token.dupe(alloc);

        const duped_right = try self.right.dupe(alloc);
        const duped_right_ptr = try alloc.create(Expression);
        duped_right_ptr.* = duped_right;

        return Expression{
            .prefix_expression = PrefixExpression{
                .token = duped_token,
                .operator = self.operator,
                .right = duped_right_ptr,
            },
        };
    }

    pub fn hash(self: *const PrefixExpression, hasher: *std.hash.Fnv1a_64) void {
        hasher.update(std.mem.asBytes(&self.token.token_type));
        hasher.update(std.mem.asBytes(&self.operator));
        self.right.hash(hasher);
    }

    pub fn eql(self: *const PrefixExpression, other: *const Expression) bool {
        switch (other.*) {
            .prefix_expression => |prefix_exp| {
                return self.operator == prefix_exp.operator and self.right.eql(prefix_exp.right);
            },
            else => return false,
        }
    }
};

pub const InfixExpression = struct {
    token: Token,
    left: *Expression,
    operator: ScopedTokenType(.operator),
    right: *Expression,

    pub fn tokenLiteral(self: *const InfixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const InfixExpression, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeByte('(');
        try self.left.string(writer);
        try writer.writeByte(' ');
        try writer.writeAll(getLiteralByOperator(self.operator));
        try writer.writeByte(' ');
        try self.right.string(writer);
        try writer.writeByte(')');
    }

    pub fn dupe(self: *const InfixExpression, alloc: Allocator) !Expression {
        const duped_token = try self.token.dupe(alloc);

        const duped_left = try self.left.dupe(alloc);
        const duped_left_ptr = try alloc.create(Expression);
        duped_left_ptr.* = duped_left;

        const duped_right = try self.right.dupe(alloc);
        const duped_right_ptr = try alloc.create(Expression);
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

    pub fn hash(self: *const InfixExpression, hasher: *std.hash.Fnv1a_64) void {
        hasher.update(std.mem.asBytes(&self.token.token_type));
        hasher.update(std.mem.asBytes(&self.operator));
        self.left.hash(hasher);
        self.right.hash(hasher);
    }

    pub fn eql(self: *const InfixExpression, other: *const Expression) bool {
        switch (other.*) {
            .infix_expression => |infix_exp| {
                return self.operator == infix_exp.operator and
                    self.left.eql(infix_exp.left) and
                    self.right.eql(infix_exp.right);
            },
            else => return false,
        }
    }
};

pub const Boolean = struct {
    token: Token,
    value: bool,

    pub fn tokenLiteral(self: *const Boolean) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const Boolean, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeAll(self.token.literal);
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

    pub fn tokenLiteral(self: *const IfExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const IfExpression, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeAll("if ");
        try self.condition.string(writer);
        try writer.writeByte(' ');
        try self.consequence.string(writer);
        if (self.alternative) |alt| {
            try writer.writeAll(" else ");
            try alt.string(writer);
        }
    }

    pub fn dupe(self: *const IfExpression, alloc: Allocator) !Expression {
        const duped_token = try self.token.dupe(alloc);

        const duped_condition = try self.condition.dupe(alloc);
        const duped_condition_ptr = try alloc.create(Expression);
        duped_condition_ptr.* = duped_condition;

        const duped_consequence = try self.consequence.dupe(alloc);
        const duped_consequence_ptr = try alloc.create(BlockStatement);
        duped_consequence_ptr.* = duped_consequence;

        var duped_alternative_ptr: ?*BlockStatement = null;
        if (self.alternative) |alt| {
            const duped_alt = try alt.dupe(alloc);
            duped_alternative_ptr = try alloc.create(BlockStatement);
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
};

pub const FunctionLiteral = struct {
    token: Token,
    parameters: []const Identifier,
    body: BlockStatement,

    pub fn tokenLiteral(self: *const FunctionLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const FunctionLiteral, writer: *std.Io.Writer) std.Io.Writer.Error!void {
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

    pub fn dupe(self: *const FunctionLiteral, alloc: Allocator) !Expression {
        const duped_token = try self.token.dupe(alloc);
        var duped_array_list = std.ArrayListUnmanaged(Identifier).empty;
        for (self.parameters) |param| {
            const duped_param = try param.dupe(alloc);
            try duped_array_list.append(alloc, duped_param);
        }
        const duped_parameters = try duped_array_list.toOwnedSlice(alloc);

        const duped_body = try self.body.dupe(alloc);

        return Expression{
            .function_literal = FunctionLiteral{
                .token = duped_token,
                .parameters = duped_parameters,
                .body = duped_body,
            },
        };
    }
};

pub const CallExpression = struct {
    token: Token,
    function: *const Expression,
    arguments: []const Expression,

    pub fn tokenLiteral(self: *const CallExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(
        self: *const CallExpression,
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
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

    pub fn dupe(self: *const CallExpression, alloc: Allocator) !Expression {
        const duped_token = try self.token.dupe(alloc);

        const duped_function = try self.function.dupe(alloc);
        const duped_function_ptr = try alloc.create(Expression);
        duped_function_ptr.* = duped_function;

        var duped_arguments = std.ArrayListUnmanaged(Expression).empty;

        for (self.arguments) |arg| {
            const duped_arg = try arg.dupe(alloc);
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
};

pub const StringLiteral = struct {
    token: Token,

    pub fn tokenLiteral(self: *const StringLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const StringLiteral, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeAll(self.token.literal);
    }

    pub fn dupe(self: *const StringLiteral, alloc: Allocator) !Expression {
        const duped_token = try self.token.dupe(alloc);
        return Expression{ .string_literal = StringLiteral{ .token = duped_token } };
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

    pub fn tokenLiteral(self: *const ArrayLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const ArrayLiteral, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeByte('[');

        if (self.elements.len > 0) {
            try self.elements[0].string(writer);
            for (self.elements[1..]) |elem| {
                try writer.writeAll(", ");
                try elem.string(writer);
            }
        }

        try writer.writeByte(']');
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
};

pub const IndexExpression = struct {
    token: Token,
    left: *Expression,
    index: *Expression,

    pub fn tokenLiteral(self: *const IndexExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const IndexExpression, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeByte('(');
        try self.left.string(writer);
        try writer.writeByte('[');
        try self.index.string(writer);
        try writer.writeByte(']');
        try writer.writeByte(')');
    }

    pub fn dupe(self: *const IndexExpression, alloc: std.mem.Allocator) !Expression {
        const duped_token = try self.token.dupe(alloc);

        const duped_left = try self.left.dupe(alloc);
        const duped_left_ptr = try alloc.create(Expression);
        duped_left_ptr.* = duped_left;

        const duped_right = try self.index.dupe(alloc);
        const duped_right_ptr = try alloc.create(Expression);
        duped_right_ptr.* = duped_right;

        return Expression{
            .index_expression = IndexExpression{
                .token = duped_token,
                .left = duped_left_ptr,
                .index = duped_right_ptr,
            },
        };
    }
};

pub const HashLiteral = struct {
    token: Token,
    pairs: Expression.HashMap,

    pub fn tokenLiteral(self: *const HashLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const HashLiteral, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeAll(Token.LBRACE);

        var iter = self.pairs.iterator();
        while (iter.next()) |entry| {
            try entry.key_ptr.string(writer);
            try writer.writeAll(Token.COLON);
            try entry.value_ptr.string(writer);
        }

        try writer.writeAll(Token.RBRACE);
    }

    pub fn dupe(self: *const HashLiteral, alloc: std.mem.Allocator) !Expression {
        const duped_token = try self.token.dupe(alloc);

        var duped_pairs = Expression.HashMap.empty;
        try duped_pairs.ensureTotalCapacity(alloc, self.pairs.capacity());
        var iter = self.pairs.iterator();
        while (iter.next()) |entry| {
            const duped_key = try entry.key_ptr.dupe(alloc);
            const duped_value = try entry.value_ptr.dupe(alloc);
            try duped_pairs.put(alloc, duped_key, duped_value);
        }

        return Expression{
            .hash_literal = HashLiteral{
                .token = duped_token,
                .pairs = duped_pairs,
            },
        };
    }
};

// Test Suite

test {
    std.testing.refAllDecls(@This());
}

const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;

const Token = @import("Token.zig");
const ScopedTokenType = Token.ScopedTokenType;
const getLiteralByOperator = Token.getLiteralByOperator;
