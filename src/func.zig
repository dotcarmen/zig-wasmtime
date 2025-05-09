const AsyncFunc = @import("async.zig").AsyncFunc;
const Err = @import("error.zig").Err;
const Extern = @import("extern.zig").Extern;
const Feature = @import("conf.zig").Feature;
const Store = @import("store.zig").Store;
const Trap = @import("trap.zig").Trap;
const Val = @import("val.zig").Val;
const wasm = @import("wasm.zig");

const std = @import("std");
const assert = std.debug.assert;

extern fn wasmtime_caller_context(*Func.Caller) callconv(.c) *Store.Ctx;
extern fn wasmtime_caller_export_get(*Func.Caller, name: [*]const u8, name_len: usize, item: *Extern) callconv(.c) bool;
extern fn wasmtime_func_call(*Store.Ctx, *const Func, args: [*]const Val, nargs: usize, results: [*]Val, nresults: usize, *?*Trap) callconv(.c) ?*Err;
extern fn wasmtime_func_call_async(*Store.Ctx, *const Func, args: [*]const Val, nargs: usize, results: [*]Val, nresults: usize, *?*Trap, *?*Err) callconv(.c) ?*AsyncFunc.CallFuture;
extern fn wasmtime_func_call_unchecked(*Store.Ctx, *const Func, args_and_results: [*]Val.Raw, args_and_results_len: usize, *?*Trap) callconv(.c) ?*Err;
extern fn wasmtime_func_new(*Store.Ctx, *const Func.Type, *const Func.Callback, env: ?*anyopaque, ?*const wasm.Finalizer, ret: *Func) callconv(.c) void;
extern fn wasmtime_func_new_unchecked(*Store.Ctx, *const Func.Type, *const Func.UncheckedCallback, env: ?*anyopaque, ?*const wasm.Finalizer, ret: *Func) callconv(.c) void;
extern fn wasmtime_func_to_raw(*Store.Ctx, *const Func) callconv(.c) Val.Raw.FuncRef;
extern fn wasmtime_func_type(*const Store.Ctx, *const Func) callconv(.c) ?*Func.Type;

pub const Func = extern struct {
    const Type = wasm.Func.Type;

    pub const Callback = fn (
        env: ?*anyopaque,
        caller: *Func.Caller,
        args: [*]const Val,
        nargs: usize,
        results: [*]Val,
        nresults: usize,
    ) callconv(.c) ?*Trap;

    pub const UncheckedCallback = fn (
        env: ?*anyopaque,
        caller: *Func.Caller,
        args_and_results: [*]Val.Raw,
        num_args_and_results: usize,
    ) callconv(.c) ?*Trap;

    pub fn CallResult(T: type) type {
        return union(enum) {
            ok: T,
            err: *Err,
            trap: *Trap,
        };
    }

    store_id: u64,
    __private: usize,

    pub const @"null": Func = .{
        .store_id = 0,
        .__private = undefined,
    };

    pub fn init(
        ctx: *Store.Ctx,
        @"type": *const Type,
        callback: *const Callback,
        env: ?*anyopaque,
        finalizer: ?*const wasm.Finalizer,
    ) Func {
        var result: Func = .null;
        wasmtime_func_new(
            ctx,
            @"type",
            callback,
            env,
            finalizer,
            &result,
        );
        return result;
    }

    pub fn initUnchecked(
        ctx: *Store.Ctx,
        @"type": *const Type,
        callback: *const UncheckedCallback,
        env: ?*anyopaque,
        finalizer: ?*const wasm.Finalizer,
    ) Func {
        var result: Func = .null;
        wasmtime_func_new_unchecked(
            ctx,
            @"type",
            callback,
            env,
            finalizer,
            &result,
        );
        return result;
    }

    pub fn raw(func: *const Func, ctx: *Store.Ctx) Val.Raw.FuncRef {
        return wasmtime_func_to_raw(ctx, func);
    }

    pub fn isNull(func: *const Func) bool {
        return func.store_id == 0;
    }

    pub fn setNull(func: *Func) void {
        func.store_id = 0;
    }

    pub fn getType(func: *const Func, ctx: *Store.Ctx) *Type {
        return wasmtime_func_type(ctx, func).?;
    }

    pub fn call(
        func: *const Func,
        ctx: *Store.Ctx,
        args: []const Val,
        results: []Val,
    ) CallResult(void) {
        var trap_result: ?*Trap = null;
        return if (wasmtime_func_call(
            ctx,
            func,
            args.ptr,
            args.len,
            results.ptr,
            results.len,
            &trap_result,
        )) |err|
            .{ .err = err }
        else if (trap_result) |trap|
            .{ .trap = trap }
        else
            .ok;
    }

    pub fn callUnchecked(
        func: *const Func,
        ctx: *Store.Ctx,
        args_and_results: []Val.Raw,
    ) CallResult(void) {
        var trap_result: ?*Trap = null;
        return if (wasmtime_func_call_unchecked(
            ctx,
            func,
            args_and_results.ptr,
            args_and_results.len,
            &trap_result,
        )) |err|
            .{ .err = err }
        else if (trap_result) |trap|
            .{ .trap = trap }
        else
            .ok;
    }

    pub fn callAsync(
        func: *const Func,
        ctx: *Store.Ctx,
        args: []const Val,
        results: []Val,
    ) union(enum) {
        trap: *Trap,
        err: *Err,
        future: *AsyncFunc.CallFuture,
    } {
        comptime assert(Feature.@"async".isEnabled());
        var trap_result: ?*Trap = null;
        var err_result: ?*Err = null;
        return if (wasmtime_func_call_async(
            ctx,
            func,
            args.ptr,
            args.len,
            results.ptr,
            results.len,
            &trap_result,
            &err_result,
        )) |fut|
            .{ .future = fut }
        else if (trap_result) |trap|
            .{ .trap = trap }
        else if (err_result) |err|
            .{ .err = err }
        else
            unreachable;
    }

    pub const Caller = opaque {
        pub const getContext = wasmtime_caller_context;

        pub fn getExport(caller: *Caller, name: []const u8) ?Extern {
            var item: Extern = undefined;
            if (wasmtime_caller_export_get(caller, name.ptr, name.len, &item))
                return item;
            return null;
        }
    };
};
