const std = @import("std");
const testing = std.testing;

const ast = @import("ast.zig");
const Lexer = @import("lexer.zig").Lexer;
const object = @import("object.zig");
const Parser = @import("parser.zig").Parser;

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
