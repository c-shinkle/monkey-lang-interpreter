const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // const exe = b.addExecutable(.{
    //     .name = "monkey-lang-interpreter",
    //     .root_source_file = b.path("src/main.zig"),
    //     .target = target,
    //     .optimize = optimize,
    // });
    // b.installArtifact(exe);
    // const run_cmd = b.addRunArtifact(exe);
    // run_cmd.step.dependOn(b.getInstallStep());
    // const run_step = b.step("run", "Run the app");
    // run_step.dependOn(&run_cmd.step);

    const test_step = b.step("test", "Run unit tests");

    const token_unit_tests = b.addTest(.{
        .name = "token_tests",
        .root_source_file = b.path("src/token/token.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(token_unit_tests);
    const run_token_unit_tests = b.addRunArtifact(token_unit_tests);
    test_step.dependOn(&run_token_unit_tests.step);
    const token_module = b.addModule("token", .{
        .root_source_file = b.path("src/token/token.zig"),
        .target = target,
        .optimize = optimize,
    });

    const lexer_unit_tests = b.addTest(.{
        .name = "lexer_tests",
        .root_source_file = b.path("src/lexer/lexer.zig"),
        .target = target,
        .optimize = optimize,
    });
    lexer_unit_tests.root_module.addImport("token", token_module);
    b.installArtifact(lexer_unit_tests);
    const run_lexer_unit_tests = b.addRunArtifact(lexer_unit_tests);
    test_step.dependOn(&run_lexer_unit_tests.step);
    _ = b.addModule("lexer", .{
        .root_source_file = b.path("src/lexer/lexer.zig"),
        .target = target,
        .optimize = optimize,
    });
}
