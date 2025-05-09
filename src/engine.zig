const Config = @import("config.zig").Config;
const Err = @import("error.zig").Err;
const Feature = @import("conf.zig").Feature;

const std = @import("std");
const assert = std.debug.assert;

extern fn wasm_engine_delete(*Engine) callconv(.c) void;
extern fn wasm_engine_new() callconv(.c) ?*Engine;
extern fn wasm_engine_new_with_config(*const Config) callconv(.c) ?*Engine;
extern fn wasmtime_engine_clone(*const Engine) callconv(.c) ?*Engine;
extern fn wasmtime_engine_increment_epoch(*Engine) callconv(.c) void;
extern fn wasmtime_engine_is_pulley(*const Engine) callconv(.c) bool;
extern fn wasmtime_module_validate(*Engine, [*]const u8, usize) callconv(.c) ?*Err;

pub const Engine = opaque {
    pub const deinit = wasm_engine_delete;
    pub const incrementEpoch = wasmtime_engine_increment_epoch;
    pub const isPulley = wasmtime_engine_is_pulley;

    pub fn init() *Engine {
        return wasm_engine_new().?;
    }

    pub fn initConfig(config: *const Config) *Engine {
        return wasm_engine_new_with_config(config).?;
    }

    pub fn clone(engine: *const Engine) *Engine {
        return wasmtime_engine_clone(engine).?;
    }

    pub fn validateModule(engine: *Engine, binary: []const u8) ?*Err {
        comptime assert(Feature.compiler.isEnabled());
        return wasmtime_module_validate(engine, binary.ptr, binary.len);
    }
};
