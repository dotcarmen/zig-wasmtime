const Engine = @import("engine.zig").Engine;
const Err = @import("error.zig").Err;
const Extern = @import("extern.zig").Extern;
const Feature = @import("conf.zig").Feature;
const Func = @import("func.zig").Func;
const Instance = @import("instance.zig").Instance;
const Module = @import("module.zig").Module;
const Store = @import("store.zig").Store;
const Trap = @import("trap.zig").Trap;
const wasm = @import("wasm.zig");

const std = @import("std");
const assert = std.debug.assert;

extern fn wasmtime_linker_allow_shadowing(*Linker, bool) callconv(.c) void;
extern fn wasmtime_linker_clone(*const Linker) callconv(.c) *Linker;
extern fn wasmtime_linker_define(*Linker, *Store.Ctx, module: [*]const u8, module_len: usize, name: [*]const u8, name_len: usize, item: *const Extern) callconv(.c) ?*Err;
extern fn wasmtime_linker_define_func(*Linker, module: [*]const u8, module_len: usize, name: [*]const u8, name_len: usize, @"type": *wasm.Func.Type, callback: *const Func.Callback, data: ?*anyopaque, ?wasm.Finalizer.Func) callconv(.c) ?*Err;
extern fn wasmtime_linker_define_func_unchecked(*Linker, module: [*]const u8, module_len: usize, name: [*]const u8, name_len: usize, @"type": *wasm.Func.Type, callback: *const Func.UncheckedCallback, data: ?*anyopaque, ?wasm.Finalizer.Func) callconv(.c) ?*Err;
extern fn wasmtime_linker_define_wasi(*Linker) callconv(.c) ?*Err;
extern fn wasmtime_linker_define_instance(*Linker, *Store.Ctx, name: [*]const u8, name_len: usize, instance: *const Instance) callconv(.c) ?*Err;
extern fn wasmtime_linker_delete(*Linker) callconv(.c) void;
extern fn wasmtime_linker_get(*const Linker, *Store.Ctx, module: [*]const u8, module_len: usize, name: [*]const u8, name_len: usize, item: *Extern) callconv(.c) bool;
extern fn wasmtime_linker_get_default(*const Linker, *Store.Ctx, name: [*]const u8, name_len: usize, *Func) callconv(.c) ?*Err;
extern fn wasmtime_linker_instantiate(*const Linker, *Store.Ctx, *const Module, *Instance, *?*Trap) callconv(.c) ?*Err;
extern fn wasmtime_linker_instantiate_pre(*const Linker, *const Module, **Instance.Pre) callconv(.c) ?*Err;
extern fn wasmtime_linker_new(*Engine) callconv(.c) *Linker;
extern fn wasmtime_linker_module(*Linker, *Store.Ctx, name: [*]const u8, name_len: usize, *const Module) callconv(.c) ?*Err;

pub const Linker = opaque {
    pub const init = wasmtime_linker_new;
    pub const deinit = wasmtime_linker_delete;
    pub const clone = wasmtime_linker_clone;

    pub const allowShadowing = wasmtime_linker_allow_shadowing;

    pub fn defineWasi(linker: *Linker) ?*Err {
        comptime assert(Feature.wasi.isEnabled());
        return wasmtime_linker_define_wasi(linker);
    }

    pub fn define(
        linker: *Linker,
        ctx: *Store.Ctx,
        module: []const u8,
        name: []const u8,
        item: *const Extern,
    ) ?*Err {
        return wasmtime_linker_define(
            linker,
            ctx,
            module.ptr,
            module.len,
            name.ptr,
            name.len,
            item,
        );
    }

    // pub fn defineFunc(
    //     linker: *Linker,
    //     module: []const u8,
    //     name: []const u8,
    //     @"type": *wasm.Func.Type,
    //     callback: *const Func.Callback,
    //     data: ?*anyopaque,
    //     finalizer: ?*wasm.Finalizer,
    // ) ?*Err {
    //     return wasmtime_linker_define_func(
    //         linker,
    //         module.ptr,
    //         module.len,
    //         name.ptr,
    //         name.len,
    //         @"type",
    //         callback,
    //         data,
    //         finalizer,
    //     );
    // }

    // pub fn defineFuncUnchecked(
    //     linker: *Linker,
    //     module: []const u8,
    //     name: []const u8,
    //     @"type": *wasm.Func.Type,
    //     callback: *const Func.UncheckedCallback,
    //     data: ?*anyopaque,
    //     finalizer: ?*wasm.Finalizer,
    // ) ?*Err {
    //     return wasmtime_linker_define_func_unchecked(
    //         linker,
    //         module.ptr,
    //         module.len,
    //         name.ptr,
    //         name.len,
    //         @"type",
    //         callback,
    //         data,
    //         finalizer,
    //     );
    // }

    pub fn defineInstance(
        linker: *Linker,
        ctx: *Store.Ctx,
        name: []const u8,
        instance: *const Instance,
    ) ?*Err {
        return wasmtime_linker_define_instance(
            linker,
            ctx,
            name.ptr,
            name.len,
            instance,
        );
    }

    pub fn get(
        linker: *const Linker,
        ctx: *Store.Ctx,
        module: []const u8,
        name: []const u8,
    ) ?Extern {
        var result: Extern = undefined;
        if (wasmtime_linker_get(
            linker,
            ctx,
            module.ptr,
            module.len,
            name.ptr,
            name.len,
            &result,
        )) return result;
        return null;
    }

    pub fn getDefault(
        linker: *const Linker,
        ctx: *Store.Ctx,
        name: []const u8,
    ) Err.Result(Func) {
        var result: Func = .null;
        return if (wasmtime_linker_get_default(
            linker,
            ctx,
            name.ptr,
            name.len,
            &result,
        )) |err|
            .{ .err = err }
        else
            .{ .ok = result };
    }

    pub fn instantiate(
        linker: *const Linker,
        ctx: *Store.Ctx,
        module: *const Module,
    ) union(enum) {
        ok: Instance,
        err: *Err,
        trap: *Trap,
    } {
        var trap_result: ?*Trap = null;
        var result: Instance = undefined;
        return if (wasmtime_linker_instantiate(
            linker,
            ctx,
            module,
            &result,
            &trap_result,
        )) |err|
            .{ .err = err }
        else if (trap_result) |trap|
            .{ .trap = trap }
        else
            .{ .ok = result };
    }

    pub fn preinstantiate(
        linker: *const Linker,
        module: *const Module,
    ) Err.Result(*Instance.Pre) {
        var pre: *Instance.Pre = undefined;
        return if (wasmtime_linker_instantiate_pre(
            linker,
            module,
            &pre,
        )) |err|
            .{ .err = err }
        else
            .{ .ok = pre };
    }

    pub fn autoInstantiate(
        linker: *Linker,
        ctx: *Store.Ctx,
        name: []const u8,
        module: *const Module,
    ) ?*Err {
        return wasmtime_linker_module(
            linker,
            ctx,
            name.ptr,
            name.len,
            module,
        );
    }
};
