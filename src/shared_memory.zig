const Engine = @import("engine.zig").Engine;
const Err = @import("error.zig").Err;
const Feature = @import("conf.zig").Feature;
const Memory = @import("memory.zig").Memory;
const wasm = @import("wasm.zig");

extern fn wasmtime_sharedmemory_clone(*const SharedMemory) callconv(.c) ?*SharedMemory;
extern fn wasmtime_sharedmemory_data(*const SharedMemory) callconv(.c) [*]u8;
extern fn wasmtime_sharedmemory_data_size(*const SharedMemory) callconv(.c) usize;
extern fn wasmtime_sharedmemory_delete(?*SharedMemory) callconv(.c) void;
extern fn wasmtime_sharedmemory_grow(*const SharedMemory, delta: u64, prev_size: *u64) callconv(.c) ?*Err;
extern fn wasmtime_sharedmemory_new(*const Engine, *const wasm.Memory.Type, *?*SharedMemory) callconv(.c) ?*Err;
extern fn wasmtime_sharedmemory_size(*const SharedMemory) callconv(.c) u64;
extern fn wasmtime_sharedmemory_type(*const SharedMemory) callconv(.c) *const wasm.Memory.Type;

pub const SharedMemory = opaque {
    pub const deinit = wasmtime_sharedmemory_delete;
    pub const getDataPtr = wasmtime_sharedmemory_data;
    pub const getDataSize = wasmtime_sharedmemory_data_size;
    pub const getSize = wasmtime_sharedmemory_size;
    pub const getType = wasmtime_sharedmemory_type;

    pub fn init(
        engine: *const Engine,
        @"type": *const wasm.Memory.Type,
    ) Err.Result(*SharedMemory) {
        var result: ?*SharedMemory = null;
        if (wasmtime_sharedmemory_new(engine, @"type", &result)) |err|
            return .{ .err = err };
        return .{ .ok = result.? };
    }

    pub fn clone(self: *const SharedMemory) *SharedMemory {
        return wasmtime_sharedmemory_clone(self).?;
    }

    pub fn getData(self: *const SharedMemory) []u8 {
        const ptr = getDataPtr(self);
        return ptr[0..getDataSize(self)];
    }

    pub fn grow(self: *const SharedMemory, delta: u64) Err.Result(u64) {
        var prev_size: u64 = undefined;
        if (wasmtime_sharedmemory_grow(self, delta, &prev_size)) |err|
            return .{ .err = err };
        return .{ .ok = prev_size };
    }
};
