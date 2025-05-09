const std = @import("std");
const BuildCApi = @import("BuildCApi.zig");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{
        .preferred_optimize_mode = .ReleaseSafe,
    });

    const wasmtime_dep = b.dependency("wasmtime", .{});
    const c_api = try BuildCApi.create(b, .{
        .c_api_dir = wasmtime_dep.path("crates/c-api"),
        .optimize = optimize,
        .target = target,
    });

    const mod = b.addModule("wasmtime", .{
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    mod.addSystemIncludePath(c_api.getIncludeDir());
    mod.addObjectFile(c_api.getObjectFile());

    const lib = b.addLibrary(.{
        .linkage = .static,
        .name = "zig_wasmtime",
        .root_module = mod,
    });
    b.installArtifact(lib);

    const lib_unit_tests = b.addTest(.{
        .root_module = mod,
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
}
