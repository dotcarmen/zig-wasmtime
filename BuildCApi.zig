const BuildCApi = @This();
const std = @import("std");

const Build = std.Build;
const GeneratedFile = Build.GeneratedFile;
const LazyPath = Build.LazyPath;
const OptimizeMode = std.builtin.OptimizeMode;
const Step = Build.Step;

step: Step,

include_dir: GeneratedFile,
object_file: GeneratedFile,

c_api_dir: LazyPath,
optimize: OptimizeMode,
target: Build.ResolvedTarget,

build_dir: []const u8,
install_prefix: []const u8,

always_build: ?bool = null,
build_shared_libs: ?bool = null,
cargo_build_options: []const []const u8 = &.{},
disable_all_features: ?bool = null,
disable_features: []const []const u8 = &.{},
features: []const []const u8 = &.{},
wasmtime_target: []const u8,

pub fn create(b: *Build, opts: struct {
    c_api_dir: LazyPath,
    optimize: OptimizeMode,
    target: Build.ResolvedTarget,

    always_build: ?bool = null,
    build_shared_libs: ?bool = null,
    cargo_build_options: []const []const u8 = &.{},
    disable_all_features: ?bool = null,
    features: []const []const u8 = &.{},
    disable_features: []const []const u8 = &.{},
    wasmtime_target: ?[]const u8 = null,
}) !*BuildCApi {
    const wasmtime_target = opts.wasmtime_target orelse target: {
        const arch = @tagName(opts.target.result.cpu.arch);
        const abi =
            if (opts.target.result.os.tag == .macos)
                "apple-darwin"
            else if (opts.target.result.os.tag == .linux)
                b.fmt("unknown-linux-{s}", .{
                    @tagName(opts.target.result.abi),
                })
            else // TODO: more target support
                return error.UnsupportedWasmtimeTarget;

        break :target b.fmt("{s}-{s}", .{ arch, abi });
    };

    const build_api = try b.allocator.create(BuildCApi);
    const generated_file: GeneratedFile = .{ .step = &build_api.step };

    const build_dir = b.pathJoin(&.{ "cmake", "build" });
    const install_prefix = b.pathJoin(&.{ "cmake", "install" });

    build_api.* = .{
        .step = Step.init(.{
            .id = .custom,
            .name = "build wasmtime c api",
            .owner = b,
            .makeFn = make,
        }),

        .include_dir = generated_file,
        .object_file = generated_file,

        .c_api_dir = opts.c_api_dir,
        .optimize = opts.optimize,
        .target = opts.target,

        .build_dir = build_dir,
        .install_prefix = install_prefix,

        .wasmtime_target = wasmtime_target,
    };

    return build_api;
}

pub fn getIncludeDir(bca: *const BuildCApi) Build.LazyPath {
    return .{ .generated = .{
        .file = &bca.include_dir,
    } };
}

pub fn getObjectFile(bca: *const BuildCApi) Build.LazyPath {
    return .{ .generated = .{
        .file = &bca.object_file,
    } };
}

fn opt(b: *std.Build, name: []const u8, val: bool) []const u8 {
    return b.fmt("-D{s}:bool={b}", .{
        name,
        if (val) "ON" else "OFF",
    });
}

fn make(step: *Step, opts: Step.MakeOptions) !void {
    const api: *BuildCApi = @fieldParentPtr("step", step);
    const b = step.owner;
    const alloc = b.allocator;

    opts.progress_node.setEstimatedTotalItems(3);

    if (!std.process.can_spawn)
        return step.fail("unable to execute cmake: host cannot spawn child processes", .{});

    try b.cache_root.handle.makePath(api.build_dir);
    try b.cache_root.handle.makePath(api.install_prefix);

    const build_dir = try b.cache_root.join(alloc, &.{api.build_dir});
    const install_prefix = try b.cache_root.join(alloc, &.{api.install_prefix});

    // const build_dir = try b.cache_root.handle
    //     .realpathAlloc(alloc, api.build_dir);
    // const install_prefix = try b.cache_root.handle
    //     .realpathAlloc(alloc, api.install_prefix);

    try runCmake(
        api,
        build_dir,
        opts.progress_node.start("cmake (init)", 1),
        args: {
            var args: std.ArrayList([]const u8) = .init(alloc);
            try args.append("cmake");

            if (api.always_build) |always_build|
                try args.append(opt(
                    b,
                    "WASMTIME_ALWAYS_BUILD",
                    always_build,
                ));

            if (api.build_shared_libs) |build_shared_libs|
                try args.append(opt(
                    b,
                    "WASMTIME_BUILD_SHARED_LIBS",
                    build_shared_libs,
                ));

            if (api.cargo_build_options.len > 0)
                try args.append(b.fmt(
                    "WASMTIME_USER_CARGO_BUILD_OPTIONS:string=\"{s}\"",
                    .{try std.mem.join(
                        alloc,
                        " ",
                        api.cargo_build_options,
                    )},
                ));

            if (api.disable_all_features) |disable_all_features|
                try args.append(opt(
                    b,
                    "WASMTIME_DISABLE_ALL_FEATURES",
                    disable_all_features,
                ));

            for (api.disable_features) |feature|
                try args.append(b.fmt("-D{s}:bool=OFF", .{
                    try std.ascii.allocUpperString(alloc, feature),
                }));

            for (api.features) |feature|
                try args.append(b.fmt("-D{s}:bool=ON", .{
                    try std.ascii.allocUpperString(alloc, feature),
                }));

            try args.appendSlice(&.{
                b.fmt(
                    "-DWASMTIME_TARGET:string={s}",
                    .{api.wasmtime_target},
                ),
                b.fmt(
                    "--install-prefix={s}",
                    .{install_prefix},
                ),
                try api.c_api_dir
                    .getPath3(b, step)
                    .toString(alloc),
            });

            break :args args.items;
        },
    );

    try runCmake(
        api,
        build_dir,
        opts.progress_node.start("cmake --build", 1),
        &.{ "cmake", "--build", "." },
    );

    try runCmake(
        api,
        build_dir,
        opts.progress_node.start("cmake --install", 1),
        &.{ "cmake", "--install", "." },
    );

    api.include_dir.path = b.pathJoin(&.{ install_prefix, "include" });
    api.object_file.path = b.pathJoin(&.{
        install_prefix, "lib", b.fmt("{s}wasmtime{s}", .{
            api.target.result.libPrefix(),
            api.target.result.staticLibSuffix(),
        }),
    });
}

fn runCmake(
    api: *BuildCApi,
    dir: []const u8,
    progress: std.Progress.Node,
    args: []const []const u8,
) !void {
    defer progress.end();
    const b = api.step.owner;

    const display = try Step.allocPrintCmd(
        b.allocator,
        dir,
        args,
    );

    if (b.verbose)
        std.debug.print("{s}\n", .{display});

    const result = std.process.Child.run(.{
        .allocator = b.allocator,
        .argv = args,
        .cwd = dir,
        .progress_node = progress,
    }) catch |err|
        return api.step.fail(
            \\failed to run cmake: {s}
            \\  failed while to run: '{s}'
        , .{ @errorName(err), display });

    errdefer {
        if (result.stderr.len > 0)
            api.step.result_error_msgs.append(
                b.allocator,
                result.stderr,
            ) catch {};
    }

    if (result.term != .Exited)
        return api.step.fail(
            \\cmake terminated unexpectedly: {}
            \\  terminated while running: '{s}'
        , .{ result.term, display });

    if (result.term.Exited != 0)
        return api.step.fail(
            \\cmake terminated with exit code {d}
            \\  command failed: '{s}'
        , .{ result.term.Exited, display });
}
