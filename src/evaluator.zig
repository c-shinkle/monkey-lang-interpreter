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
        .integer => |left_int| switch (right) {
            .integer => |right_int| evalIntegerInfixExpression(operator, left_int, right_int),
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
    if (std.mem.eql(u8, operator, "+")) {
        return obj.Object{ .integer = obj.Integer{ .value = left.value + right.value } };
    } else if (std.mem.eql(u8, operator, "-")) {
        return obj.Object{ .integer = obj.Integer{ .value = left.value - right.value } };
    } else if (std.mem.eql(u8, operator, "*")) {
        return obj.Object{ .integer = obj.Integer{ .value = left.value * right.value } };
    } else if (std.mem.eql(u8, operator, "/")) {
        return obj.Object{ .integer = obj.Integer{ .value = @divTrunc(left.value, right.value) } };
    }
    return NULL;
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
    };

    for (eval_tests) |eval_test| {
        const evaluated = try testEval(eval_test.input);
        try testBooleanObject(evaluated.?, eval_test.expected);
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
    const actual = switch (object) {
        .integer => |int| int,
        inline else => |other| {
            std.debug.print("object is not Integer. got={s}\n", .{@typeName(@TypeOf(other))});
            return error.TestExpectedEqual;
        },
    };
    try testing.expectEqual(expected, actual.value);
}

fn testBooleanObject(object: obj.Object, expected: bool) !void {
    const actual = switch (object) {
        .boolean => |boolean| boolean,
        inline else => |other| {
            std.debug.print("object is not Boolean. got={s}\n", .{@typeName(@TypeOf(other))});
            return error.TestExpectedEqual;
        },
    };
    try testing.expectEqual(expected, actual.value);
}
