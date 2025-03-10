const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Lexer = @import("lexer.zig").Lexer;
const obj = @import("object.zig");
const Parser = @import("parser.zig").Parser;
const Environment = @import("environment.zig").Environment;
const Token = @import("token.zig").Token;

const EvalError = Allocator.Error || std.fmt.AllocPrintError;

pub const TRUE = obj.Object{ .boolean = obj.Boolean{ .value = true } };
pub const FALSE = obj.Object{ .boolean = obj.Boolean{ .value = false } };
pub const NULL = obj.Object{ ._null = obj.Null{} };

pub fn eval(alloc: Allocator, node: ast.Node, env: *Environment) EvalError!?obj.Object {
    return switch (node) {
        .program => |prog| try evalProgram(alloc, prog.statements, env),
        .statement => |stmt| switch (stmt) {
            .let_statement => |let_stmt| {
                const maybe = try eval(alloc, ast.Node{ .expression = let_stmt.value }, env);
                if (is_error(maybe)) return maybe;

                const let_value = maybe orelse return null;
                defer let_value.deinit(alloc);
                try env.set(let_stmt.name.value, let_value);
                return null;
            },
            .expression_statement => |exp_stmt| try eval(
                alloc,
                ast.Node{ .expression = exp_stmt.expression },
                env,
            ),
            .block_statement => |blk_stmt| try evalBlockStatement(alloc, blk_stmt, env),
            .return_statement => |ret_stmt| {
                const maybe_object = try eval(
                    alloc,
                    ast.Node{ .expression = ret_stmt.return_value },
                    env,
                );
                errdefer if (maybe_object) |object| object.deinit(alloc);
                if (is_error(maybe_object)) return maybe_object;

                var value: ?*obj.Object = null;
                if (maybe_object) |object| {
                    value = try alloc.create(obj.Object);
                    value.?.* = object;
                }
                return obj.Object{ .return_value = obj.ReturnValue{ .value = value } };
            },
        },
        .expression => |exp| switch (exp) {
            .identifier => |*ident| try eval_identifier(alloc, ident, env),
            .integer_literal => |int| obj.Object{ .integer = obj.Integer{ .value = int.value } },
            .prefix_expression => |prefix| {
                const maybe_right = try eval(
                    alloc,
                    ast.Node{ .expression = prefix.right.* },
                    env,
                );
                if (is_error(maybe_right)) return maybe_right;

                const right = maybe_right orelse return null;
                return try evalPrefixExpression(alloc, prefix.operator, right);
            },
            .infix_expression => |infix| {
                const maybe_left = try eval(
                    alloc,
                    ast.Node{ .expression = infix.left.* },
                    env,
                );
                if (is_error(maybe_left)) return maybe_left;

                const left = maybe_left orelse return null;
                errdefer left.deinit(alloc);

                const maybe_right = try eval(
                    alloc,
                    ast.Node{ .expression = infix.right.* },
                    env,
                );
                if (is_error(maybe_right)) return maybe_right;

                const right = maybe_right orelse return null;
                errdefer right.deinit(alloc);

                return try evalInfixOperatorExpression(alloc, infix.operator, left, right);
            },
            .boolean_expression => |boolean| if (boolean.value) TRUE else FALSE,
            .if_expression => |if_exp| try evalIfExpression(alloc, if_exp, env),
            .function_literal => |fn_lit| {
                const duped_function_literal = try fn_lit.dupe(alloc);
                std.debug.assert(duped_function_literal == .function_literal);
                duped_function_literal.function_literal._token.dupe_deinit(alloc);

                return obj.Object{
                    ._function = obj.Function{
                        .parameters = duped_function_literal.function_literal.parameters,
                        .body = duped_function_literal.function_literal.body,
                        .env = env,
                    },
                };
            },
            .call_expression => |call_fn| {
                const duped_function = try call_fn.dupe(alloc);
                defer duped_function.dupe_deinit(alloc);
                std.debug.assert(duped_function == .call_expression);
                const duped_call_exp = duped_function.call_expression;

                const node_call_exp = ast.Node{ .expression = duped_call_exp.function.* };
                const maybe = try eval(alloc, node_call_exp, env);
                defer if (maybe) |evaluated| evaluated.deinit(alloc);
                if (is_error(maybe)) return maybe;

                const args = try evalExpressions(alloc, duped_call_exp.arguments, env);
                defer {
                    for (args) |arg| {
                        arg.deinit(alloc);
                    }
                    alloc.free(args);
                }
                if (args.len == 1 and is_error(args[0])) return args[0];

                return applyFunction(alloc, maybe.?, args);
            },
        },
    };
}

fn evalProgram(
    alloc: Allocator,
    stmts: []const ast.Statement,
    env: *Environment,
) EvalError!?obj.Object {
    var maybe_result: ?obj.Object = null;

    for (stmts) |stmt| {
        maybe_result = try eval(alloc, ast.Node{ .statement = stmt }, env);

        if (maybe_result) |result| switch (result) {
            .return_value => |ret_val| {
                if (ret_val.value) |value| {
                    const temp = value.*;
                    ret_val.deinit(alloc);
                    return temp;
                }
                return null;
            },
            ._error => return result,
            else => {},
        };
    }

    return maybe_result;
}

fn evalPrefixExpression(
    alloc: Allocator,
    operator: []const u8,
    right: obj.Object,
) EvalError!obj.Object {
    if (operator.len == 1) switch (operator[0]) {
        '!' => return evalBangOperatorExpression(right),
        '-' => return evalMinusPrefixOperatorExperssion(alloc, right),
        else => {},
    };
    return new_error(alloc, "unknown operator: {s}{s}", .{ operator, right._type() });
}

fn evalBangOperatorExpression(right: obj.Object) obj.Object {
    return switch (right) {
        .boolean => |b| if (b.value) FALSE else TRUE,
        ._null => TRUE,
        else => FALSE,
    };
}

fn evalMinusPrefixOperatorExperssion(
    alloc: Allocator,
    right: obj.Object,
) EvalError!obj.Object {
    if (right != .integer) {
        return new_error(alloc, "unknown operator: -{s}", .{right._type()});
    }
    return obj.Object{ .integer = obj.Integer{ .value = -right.integer.value } };
}

fn evalInfixOperatorExpression(
    alloc: Allocator,
    operator: []const u8,
    left: obj.Object,
    right: obj.Object,
) EvalError!obj.Object {
    if (left == .integer and right == .integer) {
        return evalIntegerInfixExpression(alloc, operator, left.integer, right.integer);
    } else if (left == .boolean and right == .boolean) {
        if (std.mem.eql(u8, "==", operator)) {
            return if (left.boolean.value == right.boolean.value) TRUE else FALSE;
        } else if (std.mem.eql(u8, "!=", operator)) {
            return if (left.boolean.value != right.boolean.value) TRUE else FALSE;
        }
    } else if (!left.eql(right)) {
        return new_error(
            alloc,
            "type mismatch: {s} {s} {s}",
            .{ left._type(), operator, right._type() },
        );
    }
    return new_error(
        alloc,
        "unknown operator: {s} {s} {s}",
        .{ left._type(), operator, right._type() },
    );
}

fn evalIntegerInfixExpression(
    alloc: Allocator,
    operator: []const u8,
    left: obj.Integer,
    right: obj.Integer,
) EvalError!obj.Object {
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
    return new_error(
        alloc,
        "unknown operator: {s} {s} {s}",
        .{ obj.INTEGER_OBJ, operator, obj.INTEGER_OBJ },
    );
}

fn evalIfExpression(
    alloc: Allocator,
    if_exp: ast.IfExpression,
    env: *Environment,
) EvalError!?obj.Object {
    const condition = try eval(alloc, ast.Node{ .expression = if_exp.condition.* }, env);
    errdefer if (condition) |cond| cond.deinit(alloc);
    if (is_error(condition)) return condition;

    if (isTruthy(condition)) {
        const stmt = ast.Statement{ .block_statement = if_exp.consequence.* };
        return try eval(alloc, ast.Node{ .statement = stmt }, env);
    } else if (if_exp.alternative) |alt| {
        const stmt = ast.Statement{ .block_statement = alt.* };
        return try eval(alloc, ast.Node{ .statement = stmt }, env);
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
    alloc: Allocator,
    block: ast.BlockStatement,
    env: *Environment,
) EvalError!?obj.Object {
    var maybe: ?obj.Object = null;

    for (block.statements) |stmt| {
        maybe = try eval(alloc, ast.Node{ .statement = stmt }, env);
        if (maybe) |result| switch (result) {
            .return_value, ._error => return result,
            else => {},
        };
    }

    return maybe;
}

fn new_error(
    alloc: Allocator,
    comptime fmt: []const u8,
    args: anytype,
) EvalError!obj.Object {
    const _error = obj.Error{ .message = try std.fmt.allocPrint(alloc, fmt, args) };
    return obj.Object{ ._error = _error };
}

fn is_error(maybe_object: ?obj.Object) bool {
    return (maybe_object orelse return false) == ._error;
}

fn eval_identifier(
    alloc: Allocator,
    ident: *const ast.Identifier,
    env: *Environment,
) EvalError!obj.Object {
    const maybe = env.get(ident.value);
    if (maybe) |identifier_value| {
        return try identifier_value.dupe(alloc);
    }
    return new_error(alloc, "identifier not found: {s}", .{ident.value});
}

fn evalExpressions(
    alloc: Allocator,
    exps: []const ast.Expression,
    env: *Environment,
) Allocator.Error![]const obj.Object {
    var evaluateds = std.ArrayListUnmanaged(obj.Object).empty;
    errdefer {
        for (evaluateds.items) |object| object.deinit(alloc);
        evaluateds.deinit(alloc);
    }

    for (exps) |exp| {
        const maybe_eval = try eval(alloc, ast.Node{ .expression = exp }, env);
        const evaluated = maybe_eval.?;
        errdefer evaluated.deinit(alloc);

        if (is_error(evaluated)) {
            while (evaluateds.pop()) |object| object.deinit(alloc);
            try evaluateds.append(alloc, evaluated);
            return try evaluateds.toOwnedSlice(alloc);
        }

        try evaluateds.append(alloc, evaluated);
    }

    return try evaluateds.toOwnedSlice(alloc);
}

fn applyFunction(
    alloc: Allocator,
    func: obj.Object,
    arguments: []const obj.Object,
) EvalError!?obj.Object {
    if (func != ._function) {
        return try new_error(alloc, "not a function: {}", .{func});
    }
    const function = func._function;

    const node = ast.Node{ .statement = ast.Statement{ .block_statement = function.body } };

    var extended_environment = try extendFunctionEnv(function, arguments);
    defer extended_environment.deinit();

    const evaluated = try eval(alloc, node, &extended_environment);
    return unwrapReturnValue(alloc, evaluated);
}

fn extendFunctionEnv(function: obj.Function, arguments: []const obj.Object) !Environment {
    var inner_env = Environment{
        .alloc = function.env.alloc,
        .store = .empty,
        .outer = function.env,
    };
    errdefer inner_env.deinit();

    for (function.parameters, 0..) |param, i| {
        try inner_env.set(param.value, arguments[i]);
    }

    return inner_env;
}

fn unwrapReturnValue(alloc: Allocator, maybe_object: ?obj.Object) ?obj.Object {
    if (maybe_object) |object| {
        if (object == .return_value) {
            if (object.return_value.value) |val| {
                const temp = val.*;
                alloc.destroy(val);
                return temp;
            }
            return null;
        }
    }
    return maybe_object;
}

// Test Suite

test "Out of Memory" {
    const inputs = [_][]const u8{
        "5",
        "-5",
        "2 * (5 + 10)",
        "3 * (3 * 3) + 10",
        "true",
        "false",
        "1 < 2",
        "1 != 2",
        "!!false",
        "!!5",
        "if (1 < 2) { 10 }",
        "if (1 > 2) { 10 }",
        "if (1 < 2) { 10 } else { 20 }",
        "if (1 > 2) { 10 } else { 20 }",
        "9; return 2 * 5; 9;",
        "return if (1 < 2) { 10 } else { 20 };",
        \\if (10 > 1) {
        \\  if (10 > 1) {
        \\    return 10;
        \\  }
        \\
        \\  return 1;
        \\}
        \\let a = 5; let b = a; b;
        ,
        "fn(x) { x + 2 };",
        "let identity = fn(x) { x } ; identity(5);",
        "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
        "fn(x) { x; }(5)",
    };

    for (inputs) |input| {
        try testing.checkAllAllocationFailures(
            testing.allocator,
            testOutOfMemory,
            .{input},
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
    var env = Environment.init(testing.allocator);
    defer env.deinit();

    for (eval_tests) |eval_test| {
        const evaluated = try testEval(eval_test.input, testing.allocator, &env);
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
    var env = Environment.init(testing.allocator);
    defer env.deinit();

    for (eval_tests) |eval_test| {
        const evaluated = try testEval(eval_test.input, testing.allocator, &env);
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
    };
    var env = Environment.init(testing.allocator);
    defer env.deinit();

    for (eval_tests) |eval_test| {
        const evaluated = try testEval(eval_test.input, testing.allocator, &env);
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
    var env = Environment.init(testing.allocator);
    defer env.deinit();

    for (eval_tests) |eval_test| {
        const evaluated = try testEval(eval_test.input, testing.allocator, &env);
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
    var env = Environment.init(testing.allocator);
    defer env.deinit();

    for (eval_tests) |eval_test| {
        const evaluated = try testEval(eval_test.input, testing.allocator, &env);
        defer evaluated.?.deinit(testing.allocator);
        try testIntegerObject(eval_test.expected, evaluated.?);
    }
}

test "Error Handling" {
    const eval_tests = [_]struct { input: []const u8, expected: []const u8 }{
        .{
            .input = "5 + true;",
            .expected = "type mismatch: INTEGER + BOOLEAN",
        },
        .{
            .input = "5 + true; 5;",
            .expected = "type mismatch: INTEGER + BOOLEAN",
        },
        .{
            .input = "-true",
            .expected = "unknown operator: -BOOLEAN",
        },
        .{
            .input = "true + false;",
            .expected = "unknown operator: BOOLEAN + BOOLEAN",
        },
        .{
            .input = "true + false + true + false;",
            .expected = "unknown operator: BOOLEAN + BOOLEAN",
        },
        .{
            .input = "5; true + false; 5",
            .expected = "unknown operator: BOOLEAN + BOOLEAN",
        },
        .{
            .input = "if (10 > 1) { true + false; }",
            .expected = "unknown operator: BOOLEAN + BOOLEAN",
        },
        .{
            .input =
            \\if (10 > 1) {
            \\  if (10 > 1) {
            \\    return true + false;
            \\  }
            \\
            \\  return 1;
            \\}
            ,
            .expected = "unknown operator: BOOLEAN + BOOLEAN",
        },
        .{
            .input = "foobar;",
            .expected = "identifier not found: foobar",
        },
    };

    for (eval_tests) |eval_test| {
        var env = Environment.init(testing.allocator);
        defer env.deinit();

        const maybe = try testEval(eval_test.input, testing.allocator, &env);
        const evaluated = maybe orelse return error.TestExpectedEqual;
        defer evaluated.deinit(testing.allocator);

        if (evaluated != ._error) {
            std.debug.print("object is not Error. got={any}\n", .{evaluated});
            continue;
        }
        try testing.expectEqualStrings(eval_test.expected, evaluated._error.message);
    }
}

test "Let Statements" {
    const eval_tests = [_]struct { input: []const u8, expected: i64 }{
        .{ .input = "let a = 5; a;", .expected = 5 },
        .{ .input = "let a = 5 * 5; a;", .expected = 25 },
        .{ .input = "let a = 5 * 5; a;", .expected = 25 },
        .{ .input = "let a = 5; let b = a; b;", .expected = 5 },
        .{ .input = "let a = 5; let b = a; let c = a + b + 5; c;", .expected = 15 },
    };

    for (eval_tests) |eval_test| {
        var env = Environment.init(testing.allocator);
        defer env.deinit();
        const actual = try testEval(eval_test.input, testing.allocator, &env);
        try testIntegerObject(eval_test.expected, actual.?);
    }
}

test "Function Object" {
    const input = "fn(x) { x + 2 };";
    var env = Environment.init(testing.allocator);
    defer env.deinit();

    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, testing.allocator);
    defer parser.deinit(testing.allocator);
    const program = try parser.parseProgram(testing.allocator);
    defer program.parser_deinit(testing.allocator);

    const node = ast.Node{ .program = program };
    const evaluated = try eval(testing.allocator, node, &env);
    if (evaluated.? != ._function) {
        std.debug.print("object is not Function. got={?any}\n", .{evaluated});
        return error.TestExpectedEqual;
    }

    const func = evaluated.?._function;
    defer func.deinit(testing.allocator);

    try testing.expectEqual(1, func.parameters.len);

    var array_list = std.ArrayListUnmanaged(u8).empty;
    defer array_list.deinit(testing.allocator);
    const writer = array_list.writer(testing.allocator).any();

    try func.parameters[0].string(writer);
    try testing.expectEqualStrings("x", array_list.items);
    array_list.clearRetainingCapacity();

    try func.body.string(writer);
    try testing.expectEqualStrings("(x + 2)", array_list.items);
}

test "Function Application" {
    const eval_tests = [_]struct { input: []const u8, expected: i64 }{
        .{ .input = "let identity = fn(x) { x } ; identity(5);", .expected = 5 },
        .{ .input = "let identity = fn(x) { return x; } ; identity(5);", .expected = 5 },
        .{ .input = "let double = fn(x) { x * 2; }; double(5);", .expected = 10 },
        .{ .input = "let add = fn(x, y) { x + y; }; add(5, 5);", .expected = 10 },
        .{ .input = "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", .expected = 20 },
        .{ .input = "fn(x) { x; }(5)", .expected = 5 },
    };

    for (eval_tests) |eval_test| {
        var env = Environment.init(testing.allocator);
        defer env.deinit();

        var lexer = Lexer.init(eval_test.input);
        var parser = try Parser.init(&lexer, testing.allocator);
        defer parser.deinit(testing.allocator);
        const program = try parser.parseProgram(testing.allocator);
        defer program.parser_deinit(testing.allocator);

        const node = ast.Node{ .program = program };
        const actual = try eval(testing.allocator, node, &env);
        defer actual.?.deinit(testing.allocator);

        try testIntegerObject(eval_test.expected, actual.?);
    }
}

// test "Nested functions" {
//     const input =
//         \\let newAdder = fn(x) { fn(y) { x + y} };
//         \\let addTwo = newAdder(2);
//         \\addTwo(3);
//     ;
//     var env = Environment.init(testing.allocator);
//     defer env.deinit();
//
//     var lexer = Lexer.init(input);
//     var parser = try Parser.init(&lexer, testing.allocator);
//     defer parser.deinit(testing.allocator);
//     const program = try parser.parseProgram(testing.allocator);
//     defer program.parser_deinit(testing.allocator);
//
//     const node = ast.Node{ .program = program };
//     const actual = try eval(testing.allocator, node, &env);
//     defer actual.?.deinit(testing.allocator);
//     try testIntegerObject(5, actual.?);
// }

// Test Helpers

fn testEval(input: []const u8, alloc: Allocator, env: *Environment) !?obj.Object {
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, alloc);
    defer parser.deinit(alloc);
    const program = try parser.parseProgram(alloc);
    defer program.parser_deinit(alloc);

    const node = ast.Node{ .program = program };
    return try eval(alloc, node, env);
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

fn testOutOfMemory(allocator: Allocator, input: []const u8) !void {
    var env = Environment.init(allocator);
    defer env.deinit();

    const maybe = try testEval(input, allocator, &env);
    if (maybe) |actual| actual.deinit(allocator);
}
