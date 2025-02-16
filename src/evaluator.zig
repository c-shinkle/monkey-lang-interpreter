const std = @import("std");
const testing = std.testing;

const ast = @import("ast.zig");
const Lexer = @import("lexer.zig").Lexer;
const obj = @import("object.zig");
const Parser = @import("parser.zig").Parser;

pub const TRUE = obj.Object{ .boolean = obj.Boolean{ .value = true } };
pub const FALSE = obj.Object{ .boolean = obj.Boolean{ .value = false } };
pub const NULL = obj.Object{ ._null = obj.Null{} };

pub fn eval(node: ast.Node) ?obj.Object {
    return switch (node) {
        .program => |prog| evalStatements(prog.statements),
        .statement => |stmt| switch (stmt) {
            .expression_statement => |exp_stmt| eval(ast.Node{ .expression = exp_stmt.expression }),
            .block_statement => |blk_stmt| evalStatements(blk_stmt.statements),
            else => null,
        },
        .expression => |exp| switch (exp) {
            .integer_literal => |int| obj.Object{ .integer = obj.Integer{ .value = int.value } },
            .boolean_expression => |boolean| if (boolean.value) TRUE else FALSE,
            .prefix_expression => |prefix| {
                const right = eval(ast.Node{ .expression = prefix.right.* }) orelse return null;
                return evalPrefixExpression(prefix.operator, right);
            },
            .infix_expression => |infix| {
                const left = eval(ast.Node{ .expression = infix.left.* }) orelse return null;
                const right = eval(ast.Node{ .expression = infix.right.* }) orelse return null;
                return evalInfixOperatorExpression(infix.operator, left, right);
            },
            .if_expression => |if_exp| evalIfExpression(if_exp),
            else => null,
        },
    };
}

fn evalStatements(stmts: []const ast.Statement) ?obj.Object {
    var result: ?obj.Object = null;
    for (stmts) |stmt| {
        result = eval(ast.Node{ .statement = stmt });
    }
    return result;
}

fn evalPrefixExpression(operator: []const u8, right: obj.Object) obj.Object {
    if (std.mem.eql(u8, "!", operator)) {
        return evalBangOperatorExpression(right);
    } else if (std.mem.eql(u8, "-", operator)) {
        return evalMinusPrefixOperatorExperssion(right);
    }
    return NULL;
}

fn evalBangOperatorExpression(right: obj.Object) obj.Object {
    return switch (right) {
        .boolean => |b| if (b.value) FALSE else TRUE,
        ._null => TRUE,
        else => FALSE,
    };
}

fn evalMinusPrefixOperatorExperssion(right: obj.Object) obj.Object {
    return switch (right) {
        .integer => |int| obj.Object{ .integer = obj.Integer{ .value = -int.value } },
        else => NULL,
    };
}

fn evalInfixOperatorExpression(
    operator: []const u8,
    left: obj.Object,
    right: obj.Object,
) obj.Object {
    return switch (left) {
        .integer => |l_int| switch (right) {
            .integer => |r_int| evalIntegerInfixExpression(operator, l_int, r_int),
            else => NULL,
        },
        .boolean => |left_bool| switch (right) {
            .boolean => |right_bool| {
                if (std.mem.eql(u8, "==", operator)) {
                    return if (left_bool.value == right_bool.value) TRUE else FALSE;
                } else if (std.mem.eql(u8, "!=", operator)) {
                    return if (left_bool.value != right_bool.value) TRUE else FALSE;
                }
                return NULL;
            },
            else => NULL,
        },
        else => NULL,
    };
}

fn evalIntegerInfixExpression(
    operator: []const u8,
    left: obj.Integer,
    right: obj.Integer,
) obj.Object {
    if (operator.len == 1) return switch (operator[0]) {
        '+' => obj.Object{ .integer = obj.Integer{ .value = left.value + right.value } },
        '-' => obj.Object{ .integer = obj.Integer{ .value = left.value - right.value } },
        '*' => obj.Object{ .integer = obj.Integer{ .value = left.value * right.value } },
        '/' => obj.Object{ .integer = obj.Integer{ .value = @divTrunc(left.value, right.value) } },
        '<' => if (left.value < right.value) TRUE else FALSE,
        '>' => if (left.value > right.value) TRUE else FALSE,
        else => NULL,
    };
    if (std.mem.eql(u8, "==", operator)) {
        return if (left.value == right.value) TRUE else FALSE;
    }
    if (std.mem.eql(u8, "!=", operator)) {
        return if (left.value != right.value) TRUE else FALSE;
    }
    return NULL;
}

fn evalIfExpression(if_exp: ast.IfExpression) ?obj.Object {
    const condition = eval(ast.Node{ .expression = if_exp.condition.* });
    if (isTruthy(condition)) {
        return eval(ast.Node{
            .statement = ast.Statement{ .block_statement = if_exp.consequence.* },
        });
    } else if (if_exp.alternative) |alt| {
        return eval(ast.Node{ .statement = ast.Statement{ .block_statement = alt.* } });
    } else {
        return NULL;
    }
}

fn isTruthy(maybe_object: ?obj.Object) bool {
    return if (maybe_object) |object| switch (object) {
        ._null => false,
        .boolean => |boolean| boolean.value,
        else => true,
    } else true;
}

// Test Suite

test "Integer Expression" {
    const eval_tests = [_]struct { input: []const u8, expected: i64 }{
        .{ .input = "5", .expected = 5 },
        .{ .input = "10", .expected = 10 },
        .{ .input = "-5", .expected = -5 },
        .{ .input = "-10", .expected = -10 },
        .{ .input = "5 + 5 + 5 + 5 - 10", .expected = 10 },
        .{ .input = "2 * 2 * 2 * 2 * 2", .expected = 32 },
        .{ .input = "-50 + 100 + -50", .expected = 0 },
        .{ .input = "5 * 2 + 10", .expected = 20 },
        .{ .input = "5 + 2 * 10", .expected = 25 },
        .{ .input = "20 + 2 * -10", .expected = 0 },
        .{ .input = "50 / 2 * 2 + 10", .expected = 60 },
        .{ .input = "2 * (5 + 10)", .expected = 30 },
        .{ .input = "3 * 3 * 3 + 10", .expected = 37 },
        .{ .input = "3 * (3 * 3) + 10", .expected = 37 },
        .{ .input = "(5 + 10 * 2 + 15 / 3) * 2 + -10", .expected = 50 },
    };

    for (eval_tests) |eval_test| {
        const evaluated = try testEval(eval_test.input);
        try testIntegerObject(evaluated.?, eval_test.expected);
    }
}

test "Boolean Expression" {
    const eval_tests = [_]struct { input: []const u8, expected: bool }{
        .{ .input = "true", .expected = true },
        .{ .input = "false", .expected = false },
        .{ .input = "1 < 2", .expected = true },
        .{ .input = "1 > 2", .expected = false },
        .{ .input = "1 < 1", .expected = false },
        .{ .input = "1 > 1", .expected = false },
        .{ .input = "1 == 1", .expected = true },
        .{ .input = "1 != 1", .expected = false },
        .{ .input = "1 == 2", .expected = false },
        .{ .input = "1 != 2", .expected = true },
    };

    for (eval_tests) |eval_test| {
        const evaluated = try testEval(eval_test.input);
        try testBooleanObject(evaluated.?, eval_test.expected);
    }
}

test "Bang Operator" {
    const eval_tests = [_]struct { input: []const u8, expected: bool }{
        .{ .input = "!true", .expected = false },
        .{ .input = "!false", .expected = true },
        .{ .input = "!5", .expected = false },
        .{ .input = "!!true", .expected = true },
        .{ .input = "!!false", .expected = false },
        .{ .input = "!!5", .expected = true },
        .{ .input = "true == true", .expected = true },
        .{ .input = "false == false", .expected = true },
        .{ .input = "true == false", .expected = false },
        .{ .input = "true != false", .expected = true },
        .{ .input = "false != true", .expected = true },
        .{ .input = "(1 < 2) == true", .expected = true },
        .{ .input = "(1 < 2) == false", .expected = false },
        .{ .input = "(1 > 2) == true", .expected = false },
        .{ .input = "(1 > 2) == false", .expected = true },
    };

    for (eval_tests) |eval_test| {
        const evaluated = try testEval(eval_test.input);
        try testBooleanObject(evaluated.?, eval_test.expected);
    }
}

test "If Else Expressions" {
    const eval_tests = [_]struct { input: []const u8, expected: ?i64 }{
        .{ .input = "if (true) { 10 }", .expected = 10 },
        .{ .input = "if (false) { 10 }", .expected = null },
        .{ .input = "if (1) { 10 }", .expected = 10 },
        .{ .input = "if (1 < 2) { 10 }", .expected = 10 },
        .{ .input = "if (1 > 2) { 10 }", .expected = null },
        .{ .input = "if (1 > 2) { 10 } else { 20 }", .expected = 20 },
        .{ .input = "if (1 < 2) { 10 } else { 20 }", .expected = 10 },
    };

    for (eval_tests) |eval_test| {
        const evaluated = try testEval(eval_test.input);
        switch (evaluated.?) {
            .integer => {
                try testIntegerObject(evaluated.?, eval_test.expected.?);
            },
            else => {
                try testing.expectEqual(NULL, evaluated);
            },
        }
    }
}

// Test Helpers

fn testEval(input: []const u8) !?obj.Object {
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, testing.allocator);
    defer parser.deinit();
    const program = try parser.parseProgram();
    defer program.deinit();

    const node = ast.Node{ .program = program };
    return eval(node);
}

fn testIntegerObject(object: obj.Object, expected: i64) !void {
    switch (object) {
        .integer => |int| try testing.expectEqual(expected, int.value),
        inline else => |other| {
            std.debug.print("object is not Integer. got={s}\n", .{@typeName(@TypeOf(other))});
            return error.TestExpectedEqual;
        },
    }
}

fn testBooleanObject(object: obj.Object, expected: bool) !void {
    switch (object) {
        .boolean => |boolean| try testing.expectEqual(expected, boolean.value),
        inline else => |other| {
            std.debug.print("object is not Boolean. got={s}\n", .{@typeName(@TypeOf(other))});
            return error.TestExpectedEqual;
        },
    }
}

fn testNullObject(object: obj.Object) !void {
    switch (object) {
        ._null => {},
        inline else => |other| {
            std.debug.print("object is not NULL. got={s}\n", .{@typeName(@TypeOf(other))});
            return error.TestExpectedEqual;
        },
    }
}
