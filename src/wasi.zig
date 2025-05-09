const Feature = @import("conf.zig").Feature;
const wasm = @import("wasm.zig");

const std = @import("std");
const Int = std.meta.Int;
const Tag = std.meta.Tag;
const TagPayload = std.meta.TagPayload;

extern fn wasi_config_delete(*Config) callconv(.c) void;
extern fn wasi_config_inherit_argv(*Config) callconv(.c) void;
extern fn wasi_config_inherit_env(*Config) callconv(.c) void;
extern fn wasi_config_inherit_stdin(*Config) callconv(.c) void;
extern fn wasi_config_inherit_stdout(*Config) callconv(.c) void;
extern fn wasi_config_inherit_stderr(*Config) callconv(.c) void;
extern fn wasi_config_new() callconv(.c) ?*Config;
extern fn wasi_config_preopen_dir(*Config, host_path: [*:0]const u8, guest_path: [*:0]const u8, Config.DirPerms, Config.FilePerms) callconv(.c) bool;
extern fn wasi_config_set_argv(*Config, argc: usize, argv: [*]const [*:0]const u8) callconv(.c) bool;
extern fn wasi_config_set_env(*Config, envc: usize, names: [*]const [*:0]const u8, values: [*]const [*:0]const u8) callconv(.c) bool;
extern fn wasi_config_set_stdin_bytes(*Config, binary: *wasm.ConstVec(u8)) callconv(.c) bool;
extern fn wasi_config_set_stdin_file(*Config, path: [*:0]const u8) callconv(.c) bool;
extern fn wasi_config_set_stdout_file(*Config, path: [*:0]const u8) callconv(.c) bool;
extern fn wasi_config_set_stderr_file(*Config, path: [*:0]const u8) callconv(.c) bool;

pub const Config = opaque {
    pub const deinit = wasi_config_delete;
    pub const preopenDir = wasi_config_preopen_dir;

    pub fn set(
        config: *Config,
        comptime name: Tag(Prop),
        value: TagPayload(Prop, name),
    ) !void {
        switch (name) {
            .argv => switch (value) {
                .inherit => wasi_config_inherit_argv(config),
                .set => |argv| wasi_config_set_argv(config, argv.len, argv.ptr),
            },
            .env => switch (value) {
                .inherit => wasi_config_inherit_env(config),
                .set => |env| wasi_config_set_env(
                    config,
                    env.len,
                    env.field(.name),
                    env.field(.value),
                ),
            },
            .stdin => switch (value) {
                .inherit => wasi_config_inherit_stdin(config),
                .bytes => |stdin| wasi_config_set_stdin_bytes(
                    config,
                    @constCast(&.of(stdin)),
                ),
                .file => |stdin| wasi_config_set_stdin_file(config, stdin),
            },
            .stderr => switch (value) {
                .inherit => wasi_config_inherit_stderr(config),
                .file => |stderr| wasi_config_set_stderr_file(config, stderr),
            },
            .stdout => switch (value) {
                .inherit => wasi_config_inherit_stdout(config),
                .file => |stdout| wasi_config_set_stdout_file(config, stdout),
            },
        }
    }

    pub const Prop = union(enum) {
        pub const Env =
            std.MultiArrayList(struct {
                name: [*:0]const u8,
                value: [*:0]const u8,
            });

        argv: union(enum) { inherit, set: []const [*:0]const u8 },
        env: union(enum) { inherit, set: Env },
        stdin: union(enum) { inherit, bytes: []const u8, file: [*:0]const u8 },
        stdout: union(enum) { inherit, file: [*:0]const u8 },
        stderr: union(enum) { inherit, file: [*:0]const u8 },
    };

    pub const DirPerms = packed struct(usize) {
        read: bool = false,
        write: bool = false,
        _ignore: Int(.unsigned, @bitSizeOf(usize) - 2) = 0,
    };

    pub const FilePerms = packed struct(usize) {
        read: bool = false,
        write: bool = false,
        _ignore: Int(.unsigned, @bitSizeOf(usize) - 2) = 0,
    };
};
