const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const is_x86_linux = target.result.cpu.arch.isX86() and target.result.os.tag == .linux;
    const test_step = b.step("test", "Run all unit tests");

    compile_unit_tests(b, "Token", target, optimize, test_step);
    compile_unit_tests(b, "Lexer", target, optimize, test_step);
    compile_unit_tests(b, "Parser", target, optimize, test_step);
    compile_unit_tests(b, "Environment", target, optimize, test_step);

    compile_unit_tests(b, "ast", target, optimize, test_step);
    compile_unit_tests(b, "object", target, optimize, test_step);
    compile_unit_tests(b, "evaluator", target, optimize, test_step);

    const options = b.addOptions();
    const enable_readline = b.option(
        bool,
        "enable_readline",
        "Should build link against System Library \"readline\"",
    ) orelse false;
    options.addOption(bool, "enable_readline", enable_readline);

    const exe = b.addExecutable(.{
        .name = "monkey-lang",
        .use_llvm = !is_x86_linux,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .link_libc = enable_readline,
        }),
    });

    const anyline_dep = b.dependency("anyline", .{});
    exe.root_module.addImport("anyline", anyline_dep.module("anyline"));

    if (enable_readline) {
        exe.linkSystemLibrary("readline");
    }
    exe.root_module.addOptions("build_config", options);
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const desc = "Run the REPL with no args, or run interpreter with path as first argument";
    const run_step = b.step("run", desc);
    run_step.dependOn(&run_cmd.step);
}

fn compile_unit_tests(
    b: *std.Build,
    comptime name: []const u8,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
    test_step: *std.Build.Step,
) void {
    const is_x86_linux = target.result.cpu.arch.isX86() and target.result.os.tag == .linux;
    const unit_tests = b.addTest(.{
        .name = std.fmt.comptimePrint("{s}_tests", .{name}),
        .use_llvm = !is_x86_linux,
        .root_module = b.createModule(.{
            .root_source_file = b.path(std.fmt.comptimePrint("src/{s}.zig", .{name})),
            .target = target,
            .optimize = optimize,
        }),
    });

    b.installArtifact(unit_tests);
    const run_unit_tests = b.addRunArtifact(unit_tests);
    test_step.dependOn(&run_unit_tests.step);
    const unit_test_step = b.step(name, "Run " ++ name ++ " tests");
    unit_test_step.dependOn(&run_unit_tests.step);
}
