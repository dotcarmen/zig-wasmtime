const Err = @import("error.zig").Err;
const Extern = @import("extern.zig").Extern;
const Store = @import("store.zig").Store;
const wasm = @import("wasm.zig");

extern fn wasmtime_memory_data(*const Store.Ctx, *const Memory) callconv(.c) ?[*]u8;
extern fn wasmtime_memory_data_size(*const Store.Ctx, *const Memory) callconv(.c) usize;
extern fn wasmtime_memory_grow(*Store.Ctx, *const Memory, delta: u64, prev_size: *u64) callconv(.c) ?*Err;
extern fn wasmtime_memory_new(*Store.Ctx, *const Memory.Type, ret: *Memory) callconv(.c) ?*Err;
extern fn wasmtime_memory_size(*const Store.Ctx, *const Memory) callconv(.c) u64;
extern fn wasmtime_memory_type(*const Store.Ctx, *const Memory) callconv(.c) ?*Memory.Type;

pub const Memory = extern struct {
    pub const Type = wasm.Memory.Type;

    store_id: Store.Id,
    __private: usize,

    pub const @"null": Memory = .{
        .store_id = .null,
        .__private = undefined,
    };

    pub fn init(ctx: *Store.Ctx, @"type": *const Type) Err.Result(Memory) {
        var result: Memory = .null;
        if (wasmtime_memory_new(ctx, @"type", &result)) |err|
            return .{ .err = err };
        return .{ .ok = result };
    }

    pub fn asExtern(memory: *const Memory) Extern {
        return .init(.memory, .{ .memory = memory.* });
    }

    pub fn getDataPtr(memory: *const Memory, ctx: *const Store.Ctx) [*]u8 {
        return wasmtime_memory_data(ctx, memory).?;
    }

    pub fn getDataSize(memory: *const Memory, ctx: *const Store.Ctx) usize {
        return wasmtime_memory_data_size(ctx, memory);
    }

    pub fn getData(memory: *const Memory, ctx: *const Store.Ctx) []u8 {
        const ptr = memory.getDataPtr(ctx);
        const len = memory.getDataSize(ctx);
        return ptr[0..len];
    }

    pub fn getType(memory: *const Memory, ctx: *const Store.Ctx) *Type {
        return wasmtime_memory_type(ctx, memory).?;
    }

    pub fn size(memory: *const Memory, ctx: *const Store.Ctx) u64 {
        return wasmtime_memory_size(ctx, memory);
    }

    pub fn grow(
        memory: *const Memory,
        ctx: *Store.Ctx,
        delta: u64,
    ) Err.Result(u64) {
        var prev_size: u64 = undefined;
        if (wasmtime_memory_grow(ctx, memory, delta, &prev_size)) |err|
            return .{ .err = err };
        return .{ .ok = prev_size };
    }
};
