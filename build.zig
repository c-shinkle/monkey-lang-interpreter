const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const test_step = b.step("test", "Run unit tests");

    compile_unit_tests(b, "token", target, optimize, test_step);
    compile_unit_tests(b, "ast", target, optimize, test_step);
    compile_unit_tests(b, "parser", target, optimize, test_step);
    compile_unit_tests(b, "object", target, optimize, test_step);
    compile_unit_tests(b, "evaluator", target, optimize, test_step);

    const exe = b.addExecutable(.{
        .name = "monkey-lang-interpreter",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(exe);
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}

fn compile_unit_tests(
    b: *std.Build,
    comptime name: []const u8,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    test_step: *std.Build.Step,
) void {
    const unit_tests = b.addTest(.{
        .name = std.fmt.comptimePrint("{s}_tests", .{name}),
        .root_source_file = b.path(std.fmt.comptimePrint("src/{s}.zig", .{name})),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(unit_tests);
    const run_unit_tests = b.addRunArtifact(unit_tests);
    test_step.dependOn(&run_unit_tests.step);
}
