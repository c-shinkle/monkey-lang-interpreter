const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const test_step = b.step("test", "Run unit tests");

    const token_unit_tests = b.addTest(.{
        .name = "token_tests",
        .root_source_file = b.path("src/token.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(token_unit_tests);
    const run_token_unit_tests = b.addRunArtifact(token_unit_tests);
    test_step.dependOn(&run_token_unit_tests.step);

    const lexer_unit_tests = b.addTest(.{
        .name = "lexer_tests",
        .root_source_file = b.path("src/lexer.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(lexer_unit_tests);
    const run_lexer_unit_tests = b.addRunArtifact(lexer_unit_tests);
    test_step.dependOn(&run_lexer_unit_tests.step);

    const repl_module = b.addModule("repl", .{
        .root_source_file = b.path("src/repl.zig"),
        .target = target,
        .optimize = optimize,
    });

    const ast_unit_tests = b.addTest(.{
        .name = "ast_tests",
        .root_source_file = b.path("src/ast.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(ast_unit_tests);
    const run_ast_unit_tests = b.addRunArtifact(ast_unit_tests);
    test_step.dependOn(&run_ast_unit_tests.step);

    const parser_unit_tests = b.addTest(.{
        .name = "parser_tests",
        .root_source_file = b.path("src/parser.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(parser_unit_tests);
    const run_parser_unit_tests = b.addRunArtifact(parser_unit_tests);
    test_step.dependOn(&run_parser_unit_tests.step);
    const parser_module = b.addModule("token", .{
        .root_source_file = b.path("src/parser.zig"),
        .target = target,
        .optimize = optimize,
    });
    _ = parser_module;

    const exe = b.addExecutable(.{
        .name = "monkey-lang-interpreter",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("repl", repl_module);
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
