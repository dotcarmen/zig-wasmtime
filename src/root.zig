const c = @cImport(@cInclude("wasmtime.h"));

const std = @import("std");
const testing = std.testing;

pub const VERSION_STRING = c.VERSION;
pub const VERSION: struct {
    major: comptime_int,
    minor: comptime_int,
    patch: comptime_int,
} = .{
    .major = c.WASMTIME_VERSION_MAJOR,
    .minor = c.WASMTIME_VERSION_MINOR,
    .patch = c.WASMTIME_VERSION_PATCH,
};

const @"async" = @import("async.zig");
const conf = @import("conf.zig");
const config = @import("config.zig");
const engine = @import("engine.zig");
const @"error" = @import("error.zig");
const @"extern" = @import("extern.zig");
const func = @import("func.zig");
const instance = @import("instance.zig");
const global = @import("global.zig");
const linker = @import("linker.zig");
const module = @import("module.zig");
const memory = @import("memory.zig");
const profiling = @import("profiling.zig");
const shared_memory = @import("shared_memory.zig");
const store = @import("store.zig");
const table = @import("table.zig");
const trap = @import("trap.zig");
const val = @import("val.zig");
const wasi = @import("wasi.zig");
const wasm = @import("wasm.zig");

test {
    testing.refAllDeclsRecursive(@"async");
    testing.refAllDeclsRecursive(conf);
    testing.refAllDeclsRecursive(config);
    testing.refAllDeclsRecursive(engine);
    testing.refAllDeclsRecursive(@"error");
    testing.refAllDeclsRecursive(@"extern");
    testing.refAllDeclsRecursive(func);
    testing.refAllDeclsRecursive(instance);
    testing.refAllDeclsRecursive(global);
    testing.refAllDeclsRecursive(linker);
    testing.refAllDeclsRecursive(module);
    testing.refAllDeclsRecursive(memory);
    testing.refAllDeclsRecursive(profiling);
    testing.refAllDeclsRecursive(store);
    testing.refAllDeclsRecursive(shared_memory);
    testing.refAllDeclsRecursive(table);
    testing.refAllDeclsRecursive(trap);
    testing.refAllDeclsRecursive(val);
    testing.refAllDeclsRecursive(wasi);
    testing.refAllDeclsRecursive(wasm);
}
