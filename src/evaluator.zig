const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Environment = @import("environment.zig").Environment;
const Lexer = @import("lexer.zig").Lexer;
const obj = @import("object.zig");
const Parser = @import("parser.zig").Parser;
const token = @import("token.zig");

const EvalError = Allocator.Error || std.fmt.AllocPrintError;

pub fn eval(alloc: Allocator, parent_node: ast.Node, env: *Environment) EvalError!?obj.Object {
    return switch (parent_node) {
        .program => |prog| try evalProgram(alloc, prog.statements, env),
        .statement => |stmt| switch (stmt) {
            .let_statement => |let_stmt| try evalLetStatement(alloc, let_stmt, env),
            .expression_statement => |exp_stmt| {
                const child_node = ast.Node{ .expression = exp_stmt.expression };
                return try eval(alloc, child_node, env);
            },
            .block_statement => |blk_stmt| try evalBlockStatement(alloc, blk_stmt, env),
            .return_statement => |ret_stmt| try evalReturnStatement(alloc, ret_stmt, env),
        },
        .expression => |exp| switch (exp) {
            .identifier => |ident| try evalIdentifier(alloc, ident, env),
            .integer_literal => |int| obj.Object{ .integer = obj.Integer{ .value = int.value } },
            .prefix_expression => |prefix| try evalPrefixExpression(alloc, prefix, env),
            .infix_expression => |infix| try evalInfixOperatorExpression(alloc, infix, env),
            .boolean_expression => |boolean| if (boolean.value) obj.TRUE else obj.FALSE,
            .if_expression => |if_exp| try evalIfExpression(alloc, if_exp, env),
            .function_literal => |fn_lit| try evalFunctionLiteral(alloc, fn_lit, env),
            .call_expression => |call_fn| try evalCallExpression(alloc, call_fn, env),
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

fn evalLetStatement(
    alloc: Allocator,
    let_stmt: ast.LetStatement,
    env: *Environment,
) EvalError!?obj.Object {
    const child_node = ast.Node{ .expression = let_stmt.value };
    const let_value = try eval(alloc, child_node, env) orelse return null;
    if (isError(let_value)) return let_value;
    errdefer let_value.deinit(alloc);

    try env.set(let_stmt.name.value, let_value);
    return null;
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

fn evalReturnStatement(
    alloc: Allocator,
    stmt: ast.ReturnStatement,
    env: *Environment,
) EvalError!?obj.Object {
    const child_node = ast.Node{ .expression = stmt.return_value };
    const object = try eval(alloc, child_node, env) orelse
        return obj.Object{ .return_value = obj.ReturnValue{ .value = null } };
    if (isError(object)) return object;
    errdefer object.deinit(alloc);

    const value = try alloc.create(obj.Object);
    value.* = object;
    return obj.Object{ .return_value = obj.ReturnValue{ .value = value } };
}

fn evalPrefixExpression(
    alloc: Allocator,
    prefix: ast.PrefixExpression,
    env: *Environment,
) EvalError!?obj.Object {
    const child_node = ast.Node{ .expression = prefix.right.* };
    const right = try eval(alloc, child_node, env) orelse return null;
    if (isError(right)) return right;
    // TODO how was this never caught?
    // errdefer right.deinit(alloc);

    return switch (prefix.operator) {
        .bang => evalBangOperatorExpression(right),
        .minus => try evalMinusPrefixOperatorExperssion(alloc, right),
        else => blk: {
            const args = .{ token.lookupOperatorEnum(prefix.operator), right._type() };
            break :blk try newError(alloc, "unknown operator: {s}{s}", args);
        },
    };
}

fn evalBangOperatorExpression(right: obj.Object) obj.Object {
    return switch (right) {
        .boolean => |b| if (b.value) obj.FALSE else obj.TRUE,
        ._null => obj.TRUE,
        else => obj.FALSE,
    };
}

fn evalMinusPrefixOperatorExperssion(alloc: Allocator, right: obj.Object) EvalError!obj.Object {
    return switch (right) {
        .integer => obj.Object{ .integer = obj.Integer{ .value = -right.integer.value } },
        else => try newError(alloc, "unknown operator: -{s}", .{right._type()}),
    };
}

fn evalInfixOperatorExpression(
    alloc: Allocator,
    infix: ast.InfixExpression,
    env: *Environment,
) EvalError!?obj.Object {
    const left_node = ast.Node{ .expression = infix.left.* };
    const left_obj = try eval(alloc, left_node, env) orelse return null;
    if (isError(left_obj)) return left_obj;
    errdefer left_obj.deinit(alloc);

    const right_node = ast.Node{ .expression = infix.right.* };
    const right_obj = try eval(alloc, right_node, env) orelse return null;
    if (isError(right_obj)) return right_obj;
    errdefer right_obj.deinit(alloc);

    const op = infix.operator;

    if (left_obj == .integer and right_obj == .integer) {
        return try evalIntegerInfixExpression(alloc, op, left_obj.integer, right_obj.integer);
    } else if (left_obj == .boolean and right_obj == .boolean) {
        return try evalBooleanInfixExpression(alloc, op, left_obj.boolean, right_obj.boolean);
    } else if (!left_obj.eql(right_obj)) {
        const args = .{ left_obj._type(), token.lookupOperatorEnum(op), right_obj._type() };
        return try newError(alloc, "type mismatch: {s} {s} {s}", args);
    }

    const args = .{ left_obj._type(), token.lookupOperatorEnum(op), right_obj._type() };
    return try newError(alloc, "unknown operator: {s} {s} {s}", args);
}

fn evalIntegerInfixExpression(
    alloc: Allocator,
    operator: token.Operator,
    left: obj.Integer,
    right: obj.Integer,
) EvalError!obj.Object {
    return switch (operator) {
        .plus => obj.Object{ .integer = obj.Integer{ .value = left.value + right.value } },
        .minus => obj.Object{ .integer = obj.Integer{ .value = left.value - right.value } },
        .asterisk => obj.Object{ .integer = obj.Integer{ .value = left.value * right.value } },
        .slash => obj.Object{
            .integer = obj.Integer{ .value = @divTrunc(left.value, right.value) },
        },
        .lt => if (left.value < right.value) obj.TRUE else obj.FALSE,
        .gt => if (left.value > right.value) obj.TRUE else obj.FALSE,
        .eq => if (left.value == right.value) obj.TRUE else obj.FALSE,
        .not_eq => if (left.value != right.value) obj.TRUE else obj.FALSE,
        else => try newError(
            alloc,
            "unknown operator: {s} {any} {s}",
            .{ obj.INTEGER_OBJ, operator, obj.INTEGER_OBJ },
        ),
    };
}

fn evalBooleanInfixExpression(
    alloc: Allocator,
    operator: token.Operator,
    left: obj.Boolean,
    right: obj.Boolean,
) EvalError!obj.Object {
    return switch (operator) {
        .eq => if (left.value == right.value) obj.TRUE else obj.FALSE,
        .not_eq => if (left.value != right.value) obj.TRUE else obj.FALSE,
        else => blk: {
            const args = .{ obj.BOOLEAN_OBJ, token.lookupOperatorEnum(operator), obj.BOOLEAN_OBJ };
            break :blk try newError(alloc, "unknown operator: {s} {s} {s}", args);
        },
    };
}

fn evalIfExpression(
    alloc: Allocator,
    if_exp: ast.IfExpression,
    env: *Environment,
) EvalError!?obj.Object {
    const condition = try eval(alloc, ast.Node{ .expression = if_exp.condition.* }, env);
    errdefer if (condition) |cond| cond.deinit(alloc);
    if (isError(condition)) return condition;

    if (isTruthy(condition)) {
        const stmt = ast.Statement{ .block_statement = if_exp.consequence.* };
        return try eval(alloc, ast.Node{ .statement = stmt }, env);
    } else if (if_exp.alternative) |alt| {
        const stmt = ast.Statement{ .block_statement = alt.* };
        return try eval(alloc, ast.Node{ .statement = stmt }, env);
    }
    return obj.NULL;
}

fn isTruthy(maybe_object: ?obj.Object) bool {
    return if (maybe_object) |object| switch (object) {
        ._null => false,
        .boolean => |boolean| boolean.value,
        else => true,
    } else true;
}

fn newError(alloc: Allocator, comptime fmt: []const u8, args: anytype) EvalError!obj.Object {
    const message = try std.fmt.allocPrint(alloc, fmt, args);
    return obj.Object{ ._error = obj.Error{ .message = message } };
}

fn isError(maybe_object: ?obj.Object) bool {
    return (maybe_object orelse return false) == ._error;
}

fn evalIdentifier(alloc: Allocator, ident: ast.Identifier, env: *Environment) EvalError!obj.Object {
    if (env.get(ident.value)) |identifier_value| {
        return try identifier_value.dupe(alloc);
    }
    return try newError(alloc, "identifier not found: {s}", .{ident.value});
}

fn evalFunctionLiteral(
    alloc: Allocator,
    function_literal: ast.FunctionLiteral,
    env: *Environment,
) EvalError!obj.Object {
    var duped_array_list = std.ArrayListUnmanaged(ast.Identifier).empty;
    errdefer {
        for (duped_array_list.items) |duped_param| duped_param.dupe_deinit(alloc);
        duped_array_list.deinit(alloc);
    }

    for (function_literal.parameters) |param| {
        const duped_param = try param.dupe(alloc);
        errdefer duped_param.dupe_deinit(alloc);
        std.debug.assert(duped_param == .identifier);
        try duped_array_list.append(alloc, duped_param.identifier);
    }

    const duped_parameters = try duped_array_list.toOwnedSlice(alloc);
    errdefer {
        for (duped_parameters) |duped_param| duped_param.dupe_deinit(alloc);
        alloc.free(duped_parameters);
    }

    const duped_body = try function_literal.body.dupe(alloc);
    // errdefer duped_body.dupe_deinit(alloc);
    std.debug.assert(duped_body == .block_statement);

    return obj.Object{
        .function = obj.Function{
            .parameters = duped_parameters,
            .body = duped_body.block_statement,
            .env = env,
        },
    };
}

fn evalCallExpression(
    alloc: Allocator,
    call_fn: ast.CallExpression,
    env: *Environment,
) EvalError!?obj.Object {
    const duped_function = try call_fn.dupe(alloc);
    defer duped_function.dupe_deinit(alloc);
    std.debug.assert(duped_function == .call_expression);
    const duped_call_exp = duped_function.call_expression;

    const node_call_exp = ast.Node{ .expression = duped_call_exp.function.* };
    // Either Identifier or FunctionLiteral
    // Missing Identifier returns an error, FunctionLiteral never returns null
    const evaluated = (try eval(alloc, node_call_exp, env)).?;
    if (isError(evaluated)) return evaluated;
    defer evaluated.deinit(alloc);

    const args = try evalExpressions(alloc, duped_call_exp.arguments, env);
    defer {
        for (args) |arg| arg.deinit(alloc);
        alloc.free(args);
    }
    if (args.len == 1 and isError(args[0])) return args[0];

    if (evaluated != .function) {
        return try newError(alloc, "not a function: {}", .{evaluated});
    }
    return applyFunction(alloc, evaluated.function, args);
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
        const evaluated = (try eval(alloc, ast.Node{ .expression = exp }, env)).?;
        errdefer evaluated.deinit(alloc);

        if (isError(evaluated)) {
            while (evaluateds.pop()) |object| object.deinit(alloc);
            try evaluateds.append(alloc, evaluated);
            return try evaluateds.toOwnedSlice(alloc);
        }

        try evaluateds.append(alloc, evaluated); // fail_index 39 fails here
    }

    return try evaluateds.toOwnedSlice(alloc);
}

fn applyFunction(
    alloc: Allocator,
    function: obj.Function,
    arguments: []const obj.Object,
) EvalError!?obj.Object {
    const node = ast.Node{ .statement = ast.Statement{ .block_statement = function.body } };

    var extended_environment = Environment{
        .alloc = function.env.alloc,
        .store = .empty,
        .outer = function.env,
    };
    var is_function_eval = false;
    defer if (!is_function_eval) extended_environment.deinit();

    for (function.parameters, 0..) |param, i| {
        try extended_environment.set(param.value, arguments[i]);
    }
    const duped_env = try function.env.alloc.create(Environment);
    defer if (!is_function_eval) function.env.alloc.destroy(duped_env);

    duped_env.* = extended_environment;

    const maybe_evaluated = try eval(alloc, node, duped_env);
    if (maybe_evaluated) |object| {
        switch (object) {
            .return_value => |ret_val| {
                if (ret_val.value) |val| {
                    const temp = val.*;
                    alloc.destroy(val);
                    return temp;
                }
                return null;
            },
            .function => {
                is_function_eval = true;
                return maybe_evaluated;
            },
            else => {},
        }
    }
    return maybe_evaluated;
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
        \\let newAdder = fn(x) { fn(y) { x + y }; };
        \\let addTwo = newAdder(2);
        \\addTwo(1);
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

    for (eval_tests) |eval_test| {
        const evaluated = (try testEval(eval_test.input, testing.allocator)).?;
        defer evaluated.deinit(testing.allocator);
        try testIntegerObject(eval_test.expected, evaluated);
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

    for (eval_tests) |eval_test| {
        const evaluated = (try testEval(eval_test.input, testing.allocator)).?;
        defer evaluated.deinit(testing.allocator);
        try testBooleanObject(eval_test.expected, evaluated);
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
        const evaluated = (try testEval(eval_test.input, testing.allocator)).?;
        defer evaluated.deinit(testing.allocator);
        try testBooleanObject(eval_test.expected, evaluated);
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
        const evaluated = (try testEval(eval_test.input, testing.allocator)).?;
        defer evaluated.deinit(testing.allocator);
        if (eval_test.expected) |expected| {
            try testIntegerObject(expected, evaluated);
        } else {
            try testNullObject(evaluated);
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
        .{ .input = 
        \\let f = fn(x) {
        \\  return x;
        \\  x + 10;
        \\};
        \\f(10);
        , .expected = 10 },
        .{ .input = 
        \\let f = fn(x) {
        \\  let result = x + 10;
        \\  return result;
        \\  return 10;
        \\};
        \\f(10);
        , .expected = 20 },
    };

    for (eval_tests) |eval_test| {
        const evaluated = (try testEval(eval_test.input, testing.allocator)).?;
        defer evaluated.deinit(testing.allocator);
        try testIntegerObject(eval_test.expected, evaluated);
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
        const maybe = try testEval(eval_test.input, testing.allocator);
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
        const actual = (try testEval(eval_test.input, testing.allocator)).?;
        try testIntegerObject(eval_test.expected, actual);
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
    const evaluated = (try eval(testing.allocator, node, &env)).?;
    defer evaluated.deinit(testing.allocator);

    try testing.expect(evaluated == .function);
    const function = evaluated.function;

    try testing.expectEqual(1, function.parameters.len);

    var array_list = std.ArrayListUnmanaged(u8).empty;
    defer array_list.deinit(testing.allocator);
    const writer = array_list.writer(testing.allocator).any();

    try function.parameters[0].string(writer);
    try testing.expectEqualStrings("x", array_list.items);
    array_list.clearRetainingCapacity();

    try function.body.string(writer);
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
        .{ .input = 
        \\let newAdder = fn(x) { fn(y) { x + y }; };
        \\let addTwo = newAdder(2);
        \\addTwo(1);
        , .expected = 3 },
    };

    for (eval_tests) |eval_test| {
        const actual = (try testEval(eval_test.input, testing.allocator)).?;
        defer actual.deinit(testing.allocator);
        try testIntegerObject(eval_test.expected, actual);
    }
}

test "Enclosing Environments" {
    const input =
        \\let first = 10;
        \\let second = 10;
        \\let third = 10;
        \\
        \\let ourFunction = fn(first) {
        \\  let second = 20;
        \\
        \\  first + second + third;
        \\};
        \\
        \\ourFunction(20) + first + second;
    ;

    const actual = (try testEval(input, testing.allocator)).?;
    defer actual.deinit(testing.allocator);

    try testIntegerObject(70, actual);
}

// Test Helpers

fn testEval(input: []const u8, alloc: Allocator) !?obj.Object {
    var env = Environment.init(alloc);
    defer env.deinit();

    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, alloc);
    defer parser.deinit(alloc);
    const program = try parser.parseProgram(alloc);
    defer program.parser_deinit(alloc);

    const node = ast.Node{ .program = program };
    return try eval(alloc, node, &env);
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
    var env = Environment.init(testing.allocator);
    defer env.deinit();

    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, testing.allocator);
    defer parser.deinit(testing.allocator);
    const program = try parser.parseProgram(testing.allocator);
    defer program.parser_deinit(testing.allocator);

    const node = ast.Node{ .program = program };
    const maybe = try eval(testing.allocator, node, &env);
    if (maybe) |actual| actual.deinit(allocator);
}
