const c = @cImport(@cInclude("wasmtime.h"));

pub const VERSION_STRING = c.WASMTIME_VERSION;
pub const VERSION: struct {
    major: comptime_int,
    minor: comptime_int,
    patch: comptime_int,
} = .{
    .major = c.WASMTIME_VERSION_MAJOR,
    .minor = c.WASMTIME_VERSION_MINOR,
    .patch = c.WASMTIME_VERSION_PATCH,
};

pub const wasm = @import("wasm.zig");
pub const WasiConfig = @import("wasi.zig").Config;

pub const AsyncFunc = @import("async.zig").AsyncFunc;
pub const Feature = @import("conf.zig").Feature;
pub const Config = @import("config.zig").Config;
pub const Engine = @import("engine.zig").Engine;
pub const Err = @import("error.zig").Err;
pub const Extern = @import("extern.zig").Extern;
pub const Func = @import("func.zig").Func;
pub const Instance = @import("instance.zig").Instance;
pub const Global = @import("global.zig").Global;
pub const Linker = @import("linker.zig").Linker;
pub const Memory = @import("memory.zig").Memory;
pub const Module = @import("module.zig").Module;
pub const GuestProfiler = @import("profiling.zig").GuestProfiler;
pub const SharedMemory = @import("shared_memory.zig").SharedMemory;
pub const Store = @import("store.zig").Store;
pub const Table = @import("table.zig").Table;
pub const Trap = @import("trap.zig").Trap;
pub const Val = @import("val.zig").Val;

const std = @import("std");
pub const allocator = std.heap.c_allocator;

test "refAllDeclsRecursive" {
    std.testing.refAllDeclsRecursive(@import("root.zig"));
}

extern fn wasmtime_wat2wasm(
    wat: [*]const u8,
    wat_len: usize,
    ret: *const wasm.ConstVec(u8),
) callconv(.c) ?*Err;

pub fn wat2wasm(wat: []const u8) Err.Result([]const u8) {
    comptime std.debug.assert(Feature.wat.isEnabled());
    var result: wasm.ConstVec(u8) = .empty;
    if (wasmtime_wat2wasm(wat.ptr, wat.len, &result)) |err|
        return .{ .err = err };
    return .{ .ok = result.to().? };
}
