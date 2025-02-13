const std = @import("std");
const testing = std.testing;

const ast = @import("ast.zig");
const Lexer = @import("lexer.zig").Lexer;
const object = @import("object.zig");
const Parser = @import("parser.zig").Parser;

pub const TRUE = object.Boolean{ .value = true };
pub const FALSE = object.Boolean{ .value = false };
pub const NULL = object.Null{};

pub fn eval(node: ast.Node) ?object.Object {
    return switch (node) {
        .program => |prog| evalStatements(prog.statements),
        .statement => |stmt| switch (stmt) {
            .expression_statement => |exp_stmt| eval(ast.Node{ .expression = exp_stmt.expression }),
            else => null,
        },
        .expression => |exp| switch (exp) {
            .integer_literal => |int| object.Object{
                .integer = object.Integer{ .value = int.value },
            },
            .boolean_expression => |boolean| object.Object{
                .boolean = if (boolean.value) TRUE else FALSE,
            },
            .prefix_expression => |prefix| {
                var right = eval(ast.Node{ .expression = prefix.right.* });
                return evalPrefixExpression(prefix.operator, if (right) |*r| r else null);
            },
            else => null,
        },
    };
}

fn evalStatements(stmts: []const ast.Statement) ?object.Object {
    var result: ?object.Object = null;
    for (stmts) |stmt| {
        result = eval(ast.Node{ .statement = stmt });
    }
    return result;
}

fn evalPrefixExpression(operator: []const u8, right: ?*object.Object) object.Object {
    if (std.mem.eql(u8, "!", operator)) {
        return evalBangOperatorExpression(right);
    }
    return object.Object{ ._null = NULL };
}

fn evalBangOperatorExpression(right: ?*object.Object) object.Object {
    return if (right) |r| switch (r.*) {
        .boolean => |b| object.Object{ .boolean = if (b.value) FALSE else TRUE },
        ._null => object.Object{ .boolean = TRUE },
        else => object.Object{ .boolean = FALSE },
    } else object.Object{ .boolean = FALSE };
}

// Test Suite

test "Eval Integer Expression" {
    const eval_tests = [_]struct { input: []const u8, expected: i64 }{
        .{ .input = "5", .expected = 5 },
        .{ .input = "10", .expected = 10 },
    };

    for (eval_tests) |eval_test| {
        const evaluated = try testEval(eval_test.input);

        try testIntegerObject(evaluated.?, eval_test.expected);
    }
}

test "Eval Boolean Expression" {
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

fn testEval(input: []const u8) !?object.Object {
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, testing.allocator);
    defer parser.deinit();
    const program = try parser.parseProgram();
    defer program.deinit();

    const node = ast.Node{ .program = program };
    return eval(node);
}

fn testIntegerObject(obj: object.Object, expected: i64) !void {
    const result = switch (obj) {
        .integer => |int| int,
        else => {
            std.debug.print("object is not Integer. got={s}", .{@typeName(@TypeOf(obj))});
            return error.TestExpectedEqual;
        },
    };

    try testing.expectEqual(expected, result.value);
}

fn testBooleanObject(obj: object.Object, expected: bool) !void {
    const actual = switch (obj) {
        .boolean => |boolean| boolean,
        else => {
            std.debug.print("object is not Boolean. got={s}", .{@typeName(@TypeOf(obj))});
            return error.TestExpectedEqual;
        },
    };

    try testing.expectEqual(expected, actual.value);
}
