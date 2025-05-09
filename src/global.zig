const c = @cImport(@cInclude("wasmtime/global.h"));

const wasm = @import("wasm.zig");
const Err = @import("error.zig").Err;
const Extern = @import("extern.zig").Extern;
const Store = @import("store.zig").Store;
const Val = @import("val.zig").Val;

extern fn wasmtime_global_get(*Store.Ctx, *const Global, out: *Val) callconv(.c) void;
extern fn wasmtime_global_new(*Store.Ctx, @"type": *const Global.Type, val: *const Val, ret: *Global) callconv(.c) ?*Err;
extern fn wasmtime_global_set(*Store.Ctx, *const Global, val: *const Val) callconv(.c) ?*Err;
extern fn wasmtime_global_type(*Store.Ctx, *const Global) callconv(.c) ?*Global.Type;

pub const Global = extern struct {
    const Type = wasm.Global.Type;

    store_id: u64,
    __private: usize,

    pub const @"null": Global = .{
        .store_id = 0,
        .__private = undefined,
    };

    pub fn init(
        ctx: *Store.Ctx,
        @"type": *const Type,
        val: *const Val,
    ) Err.Result(Global) {
        var result: Global = .null;
        if (wasmtime_global_new(ctx, @"type", val, &result)) |err|
            return .{ .err = err };
        return .{ .ok = result };
    }

    pub fn getType(global: *const Global, ctx: *Store.Ctx) *Type {
        return wasmtime_global_type(ctx, global).?;
    }

    pub fn get(global: *const Global, ctx: *Store.Ctx) Val {
        var result: Val = undefined;
        wasmtime_global_get(ctx, global, &result);
        return result;
    }

    pub fn set(global: *const Global, ctx: *Store.Ctx, val: *const Val) ?*Err {
        if (wasmtime_global_set(ctx, global, val)) |err|
            return err;
        return null;
    }
};
