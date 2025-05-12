const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const _builtin = @import("_builtin.zig");
const ast = @import("ast.zig");
const Environment = @import("Environment.zig");
const Lexer = @import("Lexer.zig");
const obj = @import("object.zig");
const Parser = @import("Parser.zig");
const Token = @import("Token.zig");
const getLiteralByOperator = Token.getLiteralByOperator;
const Operator = Token.Operator;

const EvalError = _builtin.BuiltinError || Allocator.Error || std.fmt.AllocPrintError;

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
            .function_literal => |fn_lit| evalFunctionLiteral(fn_lit, env),
            .call_expression => |call_fn| try evalCallExpression(alloc, call_fn, env),
            .string_literal => |string_lit| obj.Object{
                .string = obj.String{ .value = string_lit.token.literal },
            },
            .array_literal => |array_lit| try evalArrayLiteral(alloc, array_lit, env),
            .index_expression => |index_exp| try evalIndexExpression(alloc, index_exp, env),
            .hash_literal => |hash_lit| try evalHashLiteral(alloc, hash_lit, env),
        },
    };
}

fn evalProgram(
    alloc: Allocator,
    stmts: []const ast.Statement,
    env: *Environment,
) EvalError!?obj.Object {
    std.debug.assert(stmts.len > 0);

    var maybe_result: ?obj.Object = null;

    for (stmts) |stmt| {
        maybe_result = try eval(alloc, ast.Node{ .statement = stmt }, env);
        if (maybe_result) |result| switch (result) {
            .return_value => |ret_val| {
                if (ret_val.value) |value| {
                    return value.*;
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

    if (isError(let_value)) {
        return let_value;
    }

    try env.set(let_stmt.name.value, let_value);
    // std.debug.print("evalLetStatement\n", .{});
    // env.print();
    // std.debug.print("------\n", .{});
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
    if (isError(object)) {
        return object;
    }

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
    if (isError(right)) {
        return right;
    }

    return switch (prefix.operator) {
        .bang => evalBangOperatorExpression(right),
        .minus => try evalMinusPrefixOperatorExperssion(alloc, right),
        else => blk: {
            const args = .{ getLiteralByOperator(prefix.operator), right._type() };
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
    if (isError(left_obj)) {
        return left_obj;
    }

    const right_node = ast.Node{ .expression = infix.right.* };
    const right_obj = try eval(alloc, right_node, env) orelse return null;
    if (isError(right_obj)) {
        return right_obj;
    }

    const op = infix.operator;

    if (left_obj == .integer and right_obj == .integer) {
        return try evalIntegerInfixExpression(alloc, op, left_obj.integer, right_obj.integer);
    } else if (left_obj == .boolean and right_obj == .boolean) {
        return try evalBooleanInfixExpression(alloc, op, left_obj.boolean, right_obj.boolean);
    } else if (left_obj == .string and right_obj == .string) {
        return try evalStringInfixExpression(alloc, op, left_obj.string, right_obj.string);
    } else if (!left_obj.eqlTag(right_obj)) {
        const args = .{ left_obj._type(), getLiteralByOperator(op), right_obj._type() };
        return try newError(alloc, "type mismatch: {s} {s} {s}", args);
    }

    const args = .{ left_obj._type(), getLiteralByOperator(op), right_obj._type() };
    return try newError(alloc, "unknown operator: {s} {s} {s}", args);
}

fn evalIntegerInfixExpression(
    alloc: Allocator,
    operator: Operator,
    left: obj.Integer,
    right: obj.Integer,
) EvalError!obj.Object {
    return switch (operator) {
        .plus => obj.Object{ .integer = obj.Integer{ .value = left.value + right.value } },
        .minus => obj.Object{ .integer = obj.Integer{ .value = left.value - right.value } },
        .asterisk => obj.Object{ .integer = obj.Integer{ .value = left.value * right.value } },
        .slash => obj.Object{
            .integer = obj.Integer{ .value = @divFloor(left.value, right.value) },
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
    operator: Operator,
    left: obj.Boolean,
    right: obj.Boolean,
) EvalError!obj.Object {
    return switch (operator) {
        .eq => if (left.value == right.value) obj.TRUE else obj.FALSE,
        .not_eq => if (left.value != right.value) obj.TRUE else obj.FALSE,
        else => blk: {
            const args = .{ obj.BOOLEAN_OBJ, getLiteralByOperator(operator), obj.BOOLEAN_OBJ };
            break :blk try newError(alloc, "unknown operator: {s} {s} {s}", args);
        },
    };
}

fn evalStringInfixExpression(
    alloc: Allocator,
    operator: Operator,
    left: obj.String,
    right: obj.String,
) EvalError!obj.Object {
    if (operator != .plus) {
        const fmt = "unknown operator: {s} {s} {s}";
        const args = .{ obj.STRING_OBJ, getLiteralByOperator(operator), obj.STRING_OBJ };
        return try newError(alloc, fmt, args);
    }

    const value = try std.mem.concat(alloc, u8, &.{ left.value, right.value });
    return obj.Object{ .string = obj.String{ .value = value } };
}

fn evalIfExpression(
    alloc: Allocator,
    if_exp: ast.IfExpression,
    env: *Environment,
) EvalError!?obj.Object {
    const condition = try eval(alloc, ast.Node{ .expression = if_exp.condition.* }, env);
    if (isError(condition)) {
        return condition;
    }

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

pub fn newError(
    alloc: Allocator,
    comptime fmt: []const u8,
    args: anytype,
) std.fmt.AllocPrintError!obj.Object {
    const message = try std.fmt.allocPrint(alloc, fmt, args);
    return obj.Object{ ._error = obj.Error{ .message = message } };
}

fn isError(maybe_object: ?obj.Object) bool {
    return (maybe_object orelse return false) == ._error;
}

fn evalIdentifier(alloc: Allocator, ident: ast.Identifier, env: *Environment) EvalError!obj.Object {
    if (env.get(ident.value)) |identifier_value| {
        return identifier_value;
    } else if (_builtin.builtin_map.get(ident.value)) |_builtin_value| {
        return _builtin_value;
    } else {
        return try newError(alloc, "identifier not found: {s}", .{ident.value});
    }
}

fn evalFunctionLiteral(fn_lit: ast.FunctionLiteral, env: *Environment) obj.Object {
    const params = fn_lit.parameters;
    const body = fn_lit.body;
    return obj.Object{ .function = obj.Function{ .parameters = params, .body = body, .env = env } };
}

fn evalCallExpression(
    alloc: Allocator,
    call_exp: ast.CallExpression,
    env: *Environment,
) EvalError!?obj.Object {
    const node_call_exp = ast.Node{ .expression = call_exp.function.* };
    // Either Identifier or FunctionLiteral
    // Missing Identifier returns an error, FunctionLiteral never returns null
    const evaluated = (try eval(alloc, node_call_exp, env)).?;
    if (isError(evaluated)) {
        return evaluated;
    }

    const args = try evalExpressions(alloc, call_exp.arguments, env);
    if (args.len == 1 and isError(args[0])) {
        return args[0];
    }

    return switch (evaluated) {
        .function => |func| applyFunction(alloc, func, args),
        .builtin => |builtin| try builtin._fn(alloc, args),
        else => try newError(alloc, "not a function: {}", .{evaluated}),
    };
}

fn evalExpressions(
    alloc: Allocator,
    exps: []const ast.Expression,
    env: *Environment,
) EvalError![]obj.Object {
    var objects = try alloc.alloc(obj.Object, exps.len);

    for (exps, 0..) |exp, i| {
        //TODO how do I know this will never be null today? ... or in the future?
        const evaluated = (try eval(alloc, ast.Node{ .expression = exp }, env)).?;
        objects[i] = evaluated;

        if (isError(evaluated)) {
            return try alloc.realloc(objects, i + 1);
        }
    }

    return objects;
}

fn applyFunction(
    alloc: Allocator,
    function: obj.Function,
    arguments: []const obj.Object,
) EvalError!?obj.Object {
    const node = ast.Node{ .statement = ast.Statement{ .block_statement = function.body } };

    const extended_env = try function.env.alloc.create(Environment);
    extended_env.* = Environment{
        .alloc = function.env.alloc,
        .store = .empty,
        .outer = function.env,
    };

    for (function.parameters, 0..) |param, i| {
        try extended_env.set(param.value, arguments[i]);
    }

    // std.debug.print("applyFunction (extended)\n", .{});
    // extended_env.print();
    // std.debug.print("------\n", .{});

    const maybe_evaluated = try eval(alloc, node, extended_env);
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
            else => {},
        }
    }
    return maybe_evaluated;
}

fn evalArrayLiteral(
    alloc: Allocator,
    array_lit: ast.ArrayLiteral,
    env: *Environment,
) EvalError!obj.Object {
    const elements = try evalExpressions(alloc, array_lit.elements, env);
    if (elements.len == 1 and isError(elements[0])) {
        return elements[0];
    }
    return obj.Object{ .array = obj.Array{ .elements = elements } };
}

fn evalIndexExpression(
    alloc: Allocator,
    index_exp: ast.IndexExpression,
    env: *Environment,
) EvalError!?obj.Object {
    const left_node = ast.Node{ .expression = index_exp.left.* };
    const maybe_left_eval = try eval(alloc, left_node, env);
    if (isError(maybe_left_eval)) {
        return maybe_left_eval;
    }
    const left_eval = maybe_left_eval.?;

    const index_node = ast.Node{ .expression = index_exp.index.* };
    const maybe_index_eval = try eval(alloc, index_node, env);
    if (isError(maybe_index_eval)) {
        return maybe_index_eval;
    }
    const index_eval = maybe_index_eval.?;

    if (left_eval == .array and index_eval == .integer) {
        const index_value = index_eval.integer.value;
        const elements = left_eval.array.elements;

        if (index_value < 0 or index_value >= elements.len) {
            return obj.NULL;
        }

        return elements[@intCast(index_value)];
    }

    if (left_eval == .dictionary) {
        return switch (index_eval) {
            .string, .boolean, .integer => val: {
                // std.debug.print("Get key hash: {d}\n", .{index_eval.hash()});
                break :val left_eval.dictionary.pairs.get(index_eval) orelse obj.NULL;
            },
            else => try newError(alloc, "unusable as hash key: {s}", .{index_eval._type()}),
        };
    }

    const fmt = "index operator not supported: {s}";
    return try newError(alloc, fmt, .{maybe_left_eval.?._type()});
}

fn evalHashLiteral(
    alloc: Allocator,
    hash_lit: ast.HashLiteral,
    env: *Environment,
) EvalError!?obj.Object {
    var pairs = obj.Object.HashMap(obj.Object).empty;

    var iter = hash_lit.pairs.iterator();
    while (iter.next()) |entry| {
        const key_node = ast.Node{ .expression = entry.key_ptr.* };
        const maybe_key = try eval(alloc, key_node, env);
        if (isError(maybe_key)) {
            return maybe_key;
        }
        const key = maybe_key.?;

        const value_node = ast.Node{ .expression = entry.value_ptr.* };
        const maybe_value = try eval(alloc, value_node, env);
        if (isError(maybe_value)) {
            return maybe_value;
        }
        const value = maybe_value.?;

        // std.debug.print("Put key hash: {d}\n", .{key.hash()});
        try pairs.put(alloc, key, value);
    }

    return obj.Object{ .dictionary = obj.Dictionary{ .pairs = pairs } };
}

// Test Suite

test "Out of Memory, Without Errors" {
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
        ,
        "\"Hello, World!\"",
        "\"Hello, \" + \"World!\"",
        "let x = [\"a\", \"b\", \"c\"]; first(a)",
        "let x = [\"a\", \"b\", \"c\"]; last(a)",
        "let x = [\"a\", \"b\", \"c\"]; rest(a)",
        "let x = [\"a\", \"b\", \"c\"]; push(\"d\")",
        \\let map = fn(arr, f) {
        \\    let iter = fn(arr, accumualted) {
        \\        if (len(arr) == 0) {
        \\            accumulated
        \\        }
        \\        else {
        \\            iter(rest(arr), push(accumulated, f(first(arr))));
        \\        }
        \\    };
        \\    iter(arr, []);
        \\};
        \\let a = [1, 2, 3, 4];
        \\let double = fn(x) { x * 2 };
        \\map(a, double);
        ,
    };

    for (inputs) |input| {
        try testing.checkAllAllocationFailures(testing.allocator, testOutOfMemory, .{input});
    }
}

test "Out of Memory, With Errors" {
    const inputs = [_][]const u8{
        "5 + true;",
        "5 + true; 5;",
        "-true",
        "true + false;",
        "true + false + true + false;",
        "5; true + false; 5",
        "if (10 > 1) { true + false; }",
        "let x = fn() {}; -x;",
        "let x = fn() {}; let y = 5; x + y;",
        "let x = 5; let y = fn() {}; x + y;",
    };

    for (inputs) |input| {
        try testing.checkAllAllocationFailures(testing.allocator, testOutOfMemory, .{input});
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
        var arena = ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const actual = (try testEval(eval_test.input, arena.allocator())).?;
        try testIntegerObject(eval_test.expected, actual);
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
        var arena = ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const actual = (try testEval(eval_test.input, arena.allocator())).?;
        try testBooleanObject(eval_test.expected, actual);
    }
}

test "String Expression" {
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    var env = Environment.init(arena.allocator());
    var lexer = Lexer.init("\"Hello, World!\";");
    var parser = try Parser.init(&lexer, arena.allocator());
    const program = try parser.parseProgram(arena.allocator());
    const node = ast.Node{ .program = program };
    const actual = try eval(arena.allocator(), node, &env);
    try testAnyObject("Hello, World!", actual);
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
        var arena = ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const actual = (try testEval(eval_test.input, arena.allocator())).?;
        try testBooleanObject(eval_test.expected, actual);
    }
}

test "String Concatenation" {
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    var env = Environment.init(arena.allocator());

    const input = "\"Hello\" + \" \" + \"World!\"";
    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, arena.allocator());
    const program = try parser.parseProgram(arena.allocator());
    const node = ast.Node{ .program = program };
    const actual = try eval(arena.allocator(), node, &env) orelse
        return error.TestExpectedEqual;

    if (actual != .string) {
        std.debug.print("object is not String. got={any}\n", .{actual});
        return error.TestExpectedEqual;
    }

    try testing.expectEqualStrings("Hello World!", actual.string.value);
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
        var arena = ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const actual = (try testEval(eval_test.input, arena.allocator())).?;
        try testAnyObject(eval_test.expected, actual);
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
        var arena = ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const actual = (try testEval(eval_test.input, arena.allocator())).?;
        try testIntegerObject(eval_test.expected, actual);
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
        .{
            .input = "\"Hello\" - \"World\"",
            .expected = "unknown operator: STRING - STRING",
        },
        .{
            .input = "{\"name\": \"Monkey\"}[fn(x) { x }];",
            .expected = "unusable as hash key: FUNCTION",
        },
    };

    for (eval_tests) |eval_test| {
        var arena = ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const actual = try testEval(eval_test.input, arena.allocator()) orelse
            return error.TestExpectedEqual;

        try testing.expect(._error == actual);
        try testing.expectEqualStrings(eval_test.expected, actual._error.message);
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
        var arena_allocator = ArenaAllocator.init(testing.allocator);
        defer arena_allocator.deinit();
        const actual = (try testEval(eval_test.input, arena_allocator.allocator())).?;
        try testIntegerObject(eval_test.expected, actual);
    }
}

test "Function Object" {
    const input = "fn(x) { x + 2 };";

    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var env = Environment.init(arena.allocator());

    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, arena.allocator());
    const program = try parser.parseProgram(arena.allocator());

    const node = ast.Node{ .program = program };
    const actual = (try eval(arena.allocator(), node, &env)).?;

    try testing.expect(actual == .function);
    const function = actual.function;

    try testing.expectEqual(1, function.parameters.len);

    var array_list = std.ArrayListUnmanaged(u8).empty;
    const writer = array_list.writer(arena.allocator()).any();

    try function.parameters[0].string(writer);
    try testing.expectEqualStrings("x", array_list.items);
    array_list.clearRetainingCapacity();

    try function.body.string(writer);
    try testing.expectEqualStrings("(x + 2)", array_list.items);
}

test "Function Application" {
    const eval_tests = .{
        .{ .input = "let identity = fn(x) { x } ; identity(5);", .expected = 5 },
        .{ .input = "let identity = fn(x) { x } ; identity(false);", .expected = false },
        .{ .input = "let identity = fn(x) { x } ; identity(\"hey\");", .expected = "hey" },
        .{ .input = "let identity = fn(x) { return x; } ; identity(5);", .expected = 5 },
        .{ .input = "let identity = fn(x) { return x; } ; identity(true);", .expected = true },
        .{ .input = "let identity = fn(x) { return x; } ; identity(\"hey\");", .expected = "hey" },
        .{ .input = "let double = fn(x) { x * 2; }; double(5);", .expected = 10 },
        .{ .input = "let add = fn(x, y) { x + y; }; add(5, 5);", .expected = 10 },
        .{ .input = "let add = fn(x, y) { x + y; }; add(\"a\", \"b\");", .expected = "ab" },
        .{ .input = "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", .expected = 20 },
        .{ .input = "fn(x) { x; }(5)", .expected = 5 },
        .{ .input = "fn(x) { x; }(false)", .expected = false },
        .{ .input = "fn(x) { x; }(\"hey\")", .expected = "hey" },
        .{ .input = 
        \\let newAdder = fn(x) { fn(y) { x + y }; };
        \\let addTwo = newAdder(2);
        \\addTwo(1);
        , .expected = 3 },
    };

    inline for (eval_tests) |eval_test| {
        var arena = ArenaAllocator.init(testing.allocator);
        defer arena.deinit();
        const actual = try testEval(eval_test.input, arena.allocator());
        try testAnyObject(eval_test.expected, actual);
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

    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const actual = (try testEval(input, arena.allocator())).?;

    try testIntegerObject(70, actual);
}

test "Builtin Functions, no Errors" {
    const builtin_tests = .{
        .{ .input = "len(\"\");", .expected = 0 },
        .{ .input = "len(\"four\");", .expected = 4 },
        .{ .input = "len(\"hello world\");", .expected = 11 },
        .{ .input = "len([1, 2, 3]);", .expected = 3 },
        .{ .input = "len([]);", .expected = 0 },
        .{ .input = "first([1, 2, 3]);", .expected = 1 },
        .{ .input = "first([\"a\", \"b\", \"c\"]);", .expected = "a" },
        .{ .input = "first([]);", .expected = null },
        .{ .input = "last([1, 2, 3]);", .expected = 3 },
        .{ .input = "last([\"a\", \"b\", \"c\"]);", .expected = "c" },
        .{ .input = "last([]);", .expected = null },
        .{ .input = "rest([1, 2, 3]);", .expected = [_]i64{ 2, 3 } },
        .{ .input = "rest([\"a\", \"b\", \"c\"]);", .expected = [_][]const u8{ "b", "c" } },
        .{ .input = "rest([1]);", .expected = [_]i64{} },
        .{ .input = "rest([]);", .expected = null },
        .{ .input = "push([1, 2, 3], 4)", .expected = [_]i64{ 1, 2, 3, 4 } },
        .{
            .input = "push([\"a\", \"b\", \"c\"], \"d\")",
            .expected = [_][]const u8{ "a", "b", "c", "d" },
        },
        .{ .input = "push([], 1)", .expected = [_]i64{1} },
    };

    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    inline for (builtin_tests) |builtin_test| {
        const actual = try testEval(builtin_test.input, arena.allocator());
        const type_info = @typeInfo(@TypeOf(builtin_test.expected));
        if (type_info != .array) {
            try testAnyObject(builtin_test.expected, actual);
        } else {
            try testing.expect(actual != null and actual.? == .array);
            const actual_arr = actual.?.array;

            try testing.expectEqual(builtin_test.expected.len, actual_arr.elements.len);
            for (0..builtin_test.expected.len) |i| {
                try testAnyObject(builtin_test.expected[i], actual_arr.elements[i]);
            }
        }
    }
}

test "Builtin Functions, with Errors" {
    const builtin_tests = [_]struct { input: []const u8, expected: []const u8 }{
        .{
            .input = "len(1);",
            .expected = "argument to 'len' not supported, got INTEGER",
        },
        .{
            .input = "len(\"one\", \"two\");",
            .expected = "wrong number of arguments. got=2, want=1",
        },
        .{
            .input = "first(1);",
            .expected = "argument to 'first' must be ARRAY, got INTEGER",
        },
        .{
            .input = "first();",
            .expected = "wrong number of arguments. got=0, want=1",
        },
        .{
            .input = "last(1);",
            .expected = "argument to 'last' must be ARRAY, got INTEGER",
        },
        .{
            .input = "last();",
            .expected = "wrong number of arguments. got=0, want=1",
        },
        .{
            .input = "rest(1);",
            .expected = "argument to 'rest' must be ARRAY, got INTEGER",
        },
        .{
            .input = "rest();",
            .expected = "wrong number of arguments. got=0, want=1",
        },
    };

    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    for (builtin_tests) |builtin_test| {
        const actual = (try testEval(builtin_test.input, arena.allocator())).?;

        try testing.expect(actual == ._error);
        try testing.expectEqualStrings(builtin_test.expected, actual._error.message);
    }
}

test "Array Literals" {
    const input = "[1, 2 * 2, 3 + 3]";

    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const actual = try testEval(input, arena.allocator()) orelse
        return error.TestExpectedEqual;

    const array = switch (actual) {
        .array => |array| array,
        else => return error.TestExpectedEqual,
    };

    try testing.expectEqual(3, array.elements.len);
    try testIntegerObject(1, array.elements[0]);
    try testIntegerObject(4, array.elements[1]);
    try testIntegerObject(6, array.elements[2]);
}

test "Array Index Expression" {
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const array_tests = [_]struct { input: []const u8, expected: ?i64 }{
        .{ .input = "[1, 2, 3][0]", .expected = 1 },
        .{ .input = "[1, 2, 3][1]", .expected = 2 },
        .{ .input = "[1, 2, 3][2]", .expected = 3 },
        .{ .input = "let i = 0; [1][i]", .expected = 1 },
        .{ .input = "[1, 2, 3][1 + 1]", .expected = 3 },
        .{ .input = "let myArray = [1, 2, 3]; myArray[2]", .expected = 3 },
        .{ .input = "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2]", .expected = 6 },
        .{ .input = "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];", .expected = 2 },
        .{ .input = "[1, 2, 3][3]", .expected = null },
        .{ .input = "[1, 2, 3][-1]", .expected = null },
        .{ .input = "[][0]", .expected = null },
    };

    for (array_tests) |array_test| {
        const actual = try testEval(array_test.input, arena.allocator());
        try testAnyObject(array_test.expected, actual);
    }
}

test "Function with Builtins" {
    const input =
        \\let map = fn(arr, f) {
        \\    let iter = fn(arr, accumulated) {
        \\        if (len(arr) == 0) {
        \\            accumulated
        \\        } else {
        \\            iter(rest(arr), push(accumulated, f(first(arr))));
        \\        }
        \\    };
        \\    iter(arr, []);
        \\};
        \\let a = [1, 2, 3, 4];
        \\let double = fn(x) { x * 2 };
        \\map(a, double);
    ;
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();

    const maybe_actual = try testEval(input, arena.allocator());

    try testing.expect(maybe_actual != null);
    const actual = maybe_actual.?;

    try testing.expect(actual == .array);
    const elements = actual.array.elements;

    try testAnyObject(2, elements[0]);
    try testAnyObject(4, elements[1]);
    try testAnyObject(6, elements[2]);
    try testAnyObject(8, elements[3]);
}

test "Hash Literals" {
    const input =
        \\let two = "two";
        \\{
        \\    "one": 10 - 9,
        \\    two: 1 + 1,
        \\    "thr" + "ee": 6 / 2,
        \\    4: 4,
        \\    true: 5,
        \\    false: 6,
        \\}
    ;
    var arena_allocator = ArenaAllocator.init(testing.allocator);
    defer arena_allocator.deinit();

    const maybe_actual = try testEval(input, arena_allocator.allocator());
    try testing.expect(maybe_actual != null);
    const actual = maybe_actual.?;

    var expected = obj.Object.HashMap(i64).empty;
    try expected.put(
        arena_allocator.allocator(),
        obj.Object{ .string = obj.String{ .value = "one" } },
        1,
    );

    try testing.expect(actual == .dictionary);
}

test "Hash Index Expressions" {
    const hash_tests = [_]struct { input: []const u8, expected: ?i64 }{
        .{ .input = "{\"foo\": 5}[\"foo\"]", .expected = 5 },
        .{ .input = "{\"foo\": 5}[\"bar\"]", .expected = null },
        .{ .input = "let key = \"foo\"; {\"foo\": 5}[key]", .expected = 5 },
        .{ .input = "{}[\"foo\"]", .expected = null },
        .{ .input = "{5: 5}[5]", .expected = 5 },
        .{ .input = "{true: 5}[true]", .expected = 5 },
        .{ .input = "{false: 5}[false]", .expected = 5 },
    };

    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    for (hash_tests) |hash_test| {
        const maybe_actual = try testEval(hash_test.input, arena.allocator());
        try testAnyObject(hash_test.expected, maybe_actual);
    }
}

// Test Helpers

fn testEval(input: []const u8, alloc: Allocator) !?obj.Object {
    var env = Environment.init(alloc);

    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, alloc);
    const program = try parser.parseProgram(alloc);

    const node = ast.Node{ .program = program };
    return try eval(alloc, node, &env);
}

fn testAnyObject(expected: anytype, maybe_actual: ?obj.Object) !void {
    const type_info = @typeInfo(@TypeOf(expected));
    if (type_info == .pointer) {
        const is_string_slice = type_info.pointer.size == .slice and type_info.pointer.child == u8;
        const child_type_info = @typeInfo(type_info.pointer.child);
        const is_child_array_byte = child_type_info == .array and child_type_info.array.child == u8;
        if (!(is_child_array_byte or is_string_slice)) {
            // @compileLog("parent type info");
            @compileLog(type_info);
            // @compileLog("child type info");
            // @compileLog(child_type_info);
            @compileError("Wrong types. See Compile Log output for details");
        }
    }
    if (maybe_actual == null) {
        switch (type_info) {
            .pointer => std.debug.print("expected = {s}", .{expected}),
            .optional => std.debug.print("expected = {?}", .{expected}),
            else => std.debug.print("expected = {any}", .{expected}),
        }
        std.debug.print(" actual = null\n", .{});
        return error.TestExpectedEqual;
    }

    const actual = maybe_actual.?;
    switch (type_info) {
        .comptime_int, .int => try testIntegerObject(expected, actual),
        .bool => try testBooleanObject(expected, actual),
        .pointer => |pointer| switch (pointer.size) {
            .one, .slice => try testStringObject(expected, actual),
            else => return error.TestExpectedEqual,
        },
        .null => try testNullObject(actual),
        .optional => |optional| {
            if (expected) |some_exp| {
                switch (@typeInfo(optional.child)) {
                    .comptime_int, .int => try testIntegerObject(some_exp, actual),
                    else => return error.TestExpectedEqual,
                }
            } else {
                try testNullObject(actual);
            }
        },
        else => return error.TestExpectedEqual,
    }
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

fn testStringObject(expected: []const u8, actual: obj.Object) !void {
    if (actual != .string) {
        std.debug.print("object is not String. got={any}\n", .{actual});
        return error.TestExpectedEqual;
    }
    try testing.expectEqualStrings(expected, actual.string.value);
}

fn testOutOfMemory(alloc: Allocator, input: []const u8) !void {
    var arena = ArenaAllocator.init(alloc);
    defer arena.deinit();
    var env = Environment.init(arena.allocator());

    var lexer = Lexer.init(input);
    var parser = try Parser.init(&lexer, arena.allocator());
    const program = try parser.parseProgram(arena.allocator());

    const node = ast.Node{ .program = program };
    _ = try eval(arena.allocator(), node, &env);
}

test {
    std.testing.refAllDecls(@This());
}
