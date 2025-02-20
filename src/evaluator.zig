const std = @import("std");
const testing = std.testing;

const ast = @import("ast.zig");
const Lexer = @import("lexer.zig").Lexer;
const obj = @import("object.zig");
const Parser = @import("parser.zig").Parser;

const EvalError = error{} || std.mem.Allocator.Error;

pub const TRUE = obj.Object{ .boolean = obj.Boolean{ .value = true } };
pub const FALSE = obj.Object{ .boolean = obj.Boolean{ .value = false } };
pub const NULL = obj.Object{ ._null = obj.Null{} };

pub fn eval(node: ast.Node, allocator: std.mem.Allocator) EvalError!?obj.Object {
    return switch (node) {
        .program => |prog| try evalProgram(prog.statements, allocator),
        .statement => |stmt| switch (stmt) {
            .expression_statement => |exp_stmt| try eval(
                ast.Node{ .expression = exp_stmt.expression },
                allocator,
            ),
            .block_statement => |blk_stmt| try evalBlockStatement(blk_stmt, allocator),
            .return_statement => |ret_stmt| {
                const value = try allocator.create(?obj.Object);
                errdefer allocator.destroy(value);
                value.* = try eval(ast.Node{ .expression = ret_stmt.return_value }, allocator);
                return obj.Object{ .return_value = obj.ReturnValue{ .value = value } };
            },
            else => null,
        },
        .expression => |exp| switch (exp) {
            .integer_literal => |int| obj.Object{ .integer = obj.Integer{ .value = int.value } },
            .boolean_expression => |boolean| if (boolean.value) TRUE else FALSE,
            .prefix_expression => |prefix| {
                const maybe_right = try eval(ast.Node{ .expression = prefix.right.* }, allocator);
                const right = maybe_right orelse return null;
                return evalPrefixExpression(prefix.operator, right);
            },
            .infix_expression => |infix| {
                const maybe_left = try eval(ast.Node{ .expression = infix.left.* }, allocator);
                const left = maybe_left orelse return null;
                errdefer left.deinit(allocator);

                const maybe_right = try eval(ast.Node{ .expression = infix.right.* }, allocator);
                const right = maybe_right orelse return null;
                // errdefer right.deinit(allocator);

                return evalInfixOperatorExpression(infix.operator, left, right);
            },
            .if_expression => |if_exp| try evalIfExpression(if_exp, allocator),
            else => null,
        },
    };
}

fn evalProgram(stmts: []const ast.Statement, allocator: std.mem.Allocator) EvalError!?obj.Object {
    var maybe_result: ?obj.Object = null;

    for (stmts) |stmt| {
        maybe_result = try eval(ast.Node{ .statement = stmt }, allocator);

        if (maybe_result != null and maybe_result.? == .return_value) {
            const return_value = maybe_result.?.return_value;
            const temp = return_value.value.*;
            return_value.deinit(allocator);
            return temp;
        }
    }

    return maybe_result;
}

fn evalPrefixExpression(operator: []const u8, right: obj.Object) obj.Object {
    if (operator.len == 1) return switch (operator[0]) {
        '!' => evalBangOperatorExpression(right),
        '-' => evalMinusPrefixOperatorExperssion(right),
        else => NULL,
    };
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
    if (right != .integer) {
        return NULL;
    }
    return obj.Object{ .integer = obj.Integer{ .value = -right.integer.value } };
}

fn evalInfixOperatorExpression(
    operator: []const u8,
    left: obj.Object,
    right: obj.Object,
) obj.Object {
    if (left == .integer and right == .integer) {
        return evalIntegerInfixExpression(operator, left.integer, right.integer);
    } else if (left == .boolean and right == .boolean) {
        if (std.mem.eql(u8, "==", operator)) {
            return if (left.boolean.value == right.boolean.value) TRUE else FALSE;
        } else if (std.mem.eql(u8, "!=", operator)) {
            return if (left.boolean.value != right.boolean.value) TRUE else FALSE;
        }
    }
    return NULL;
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

fn evalIfExpression(if_exp: ast.IfExpression, allocator: std.mem.Allocator) EvalError!?obj.Object {
    const condition = try eval(ast.Node{ .expression = if_exp.condition.* }, allocator);
    errdefer if (condition) |cond| cond.deinit(allocator);
    if (isTruthy(condition)) {
        const stmt = ast.Statement{ .block_statement = if_exp.consequence.* };
        return try eval(ast.Node{ .statement = stmt }, allocator);
    } else if (if_exp.alternative) |alt| {
        const stmt = ast.Statement{ .block_statement = alt.* };
        return try eval(ast.Node{ .statement = stmt }, allocator);
    }
    return NULL;
}

fn isTruthy(maybe_object: ?obj.Object) bool {
    return if (maybe_object) |object| switch (object) {
        ._null => false,
        .boolean => |boolean| boolean.value,
        else => true,
    } else true;
}

fn evalBlockStatement(
    block: ast.BlockStatement,
    allocator: std.mem.Allocator,
) EvalError!?obj.Object {
    var maybe_result: ?obj.Object = null;

    for (block.statements) |stmt| {
        maybe_result = try eval(ast.Node{ .statement = stmt }, allocator);

        if (maybe_result != null and maybe_result.? == .return_value) {
            return maybe_result.?;
        }
    }

    return maybe_result;
}

// Test Suite

test "Out of Memory, Integer Object" {
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
        const input = eval_test.input;
        const expected = eval_test.expected;
        try testing.checkAllAllocationFailures(
            testing.allocator,
            testOutOfMemoryIntegerObject,
            .{ input, expected },
        );
    }
}

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
        const evaluated = try testEval(eval_test.input, testing.allocator);
        defer evaluated.?.deinit(testing.allocator);
        try testIntegerObject(eval_test.expected, evaluated.?);
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
        const evaluated = try testEval(eval_test.input, testing.allocator);
        defer evaluated.?.deinit(testing.allocator);
        try testBooleanObject(eval_test.expected, evaluated.?);
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
        const evaluated = try testEval(eval_test.input, testing.allocator);
        defer evaluated.?.deinit(testing.allocator);
        try testBooleanObject(eval_test.expected, evaluated.?);
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
        const evaluated = try testEval(eval_test.input, testing.allocator);
        defer evaluated.?.deinit(testing.allocator);
        if (eval_test.expected) |expected| {
            try testIntegerObject(expected, evaluated.?);
        } else {
            try testNullObject(evaluated.?);
        }
    }
}

test "Return Statements" {
    const eval_tests = [_]struct { input: []const u8, expected: i64 }{
        .{ .input = "return 10;", .expected = 10 },
        .{ .input = "return 10; 9;", .expected = 10 },
        .{ .input = "return 2 * 5; 9;", .expected = 10 },
        .{ .input = "9; return 2 * 5; 9;", .expected = 10 },
        .{ .input = 
        \\if (10 > 1) {
        \\  if (10 > 1) {
        \\    return 10;
        \\  }
        \\
        \\  return 1;
        \\}
        , .expected = 10 },
    };

    for (eval_tests) |eval_test| {
        const evaluated = try testEval(eval_test.input, testing.allocator);
        defer evaluated.?.deinit(testing.allocator);
        try testIntegerObject(eval_test.expected, evaluated.?);
    }
}

// Test Helpers

fn testEval(input: []const u8, allocator: std.mem.Allocator) !?obj.Object {
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, allocator);
    defer parser.deinit();
    const program = try parser.parseProgram();
    defer program.deinit();

    const node = ast.Node{ .program = program };
    return try eval(node, allocator);
}

fn testIntegerObject(expected: i64, actual: obj.Object) !void {
    if (actual != .integer) {
        std.debug.print("object is not Integer. got={any}\n", .{actual});
        return error.TestExpectedEqual;
    }
    try testing.expectEqual(expected, actual.integer.value);
}

fn testBooleanObject(expected: bool, actual: obj.Object) !void {
    if (actual != .boolean) {
        std.debug.print("object is not Boolean. got={any}\n", .{actual});
        return error.TestExpectedEqual;
    }
    try testing.expectEqual(expected, actual.boolean.value);
}

fn testNullObject(object: obj.Object) !void {
    if (object != ._null) {
        std.debug.print("object is not NULL. got={any}\n", .{object});
        return error.TestExpectedEqual;
    }
}

fn testOutOfMemoryIntegerObject(
    allocator: std.mem.Allocator,
    input: []const u8,
    expected: i64,
) !void {
    const actual = try testEval(input, allocator);
    defer actual.?.deinit(allocator);
    try testIntegerObject(expected, actual.?);
}
