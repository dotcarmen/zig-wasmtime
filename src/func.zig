const AsyncFunc = @import("async.zig").AsyncFunc;
const Err = @import("error.zig").Err;
const Extern = @import("extern.zig").Extern;
const Feature = @import("conf.zig").Feature;
const Store = @import("store.zig").Store;
const Trap = @import("trap.zig").Trap;
const Val = @import("val.zig").Val;
const wasm = @import("wasm.zig");

const std = @import("std");
const alloc = std.heap.c_allocator;
const assert = std.debug.assert;
const testing = std.testing;

extern fn wasmtime_caller_context(*Func.Caller) callconv(.c) *Store.Ctx;
extern fn wasmtime_caller_export_get(*Func.Caller, name: [*]const u8, name_len: usize, item: *Extern) callconv(.c) bool;
extern fn wasmtime_func_call(*Store.Ctx, *const Func, args: [*]const Val, nargs: usize, results: [*]Val, nresults: usize, *?*Trap) callconv(.c) ?*Err;
extern fn wasmtime_func_call_async(*Store.Ctx, *const Func, args: [*]const Val, nargs: usize, results: [*]Val, nresults: usize, *?*Trap, *?*Err) callconv(.c) ?*AsyncFunc.CallFuture;
extern fn wasmtime_func_call_unchecked(*Store.Ctx, *const Func, args_and_results: [*]Val.Raw, args_and_results_len: usize, *?*Trap) callconv(.c) ?*Err;
extern fn wasmtime_func_new(*Store.Ctx, *const Func.Type, *const Callback, env: ?*anyopaque, ?wasm.Finalizer.Func, ret: *Func) callconv(.c) void;
extern fn wasmtime_func_new_unchecked(*Store.Ctx, *const Func.Type, *const UncheckedCallback, env: ?*anyopaque, ?wasm.Finalizer.Func, ret: *Func) callconv(.c) void;
extern fn wasmtime_func_to_raw(*Store.Ctx, *const Func) callconv(.c) Val.Raw.FuncRef;
extern fn wasmtime_func_type(*const Store.Ctx, *const Func) callconv(.c) ?*Func.Type;

fn wasmAssert(ok: bool, comptime msg: []const u8) ?*Trap {
    const message: []const u8 = std.fmt.comptimePrint("assertion failed: {s}", .{msg});
    if (!ok) return .init(message);
    return null;
}

const Callback = fn (
    env: ?*anyopaque,
    caller: *Func.Caller,
    args: [*]const Val,
    nargs: usize,
    results: [*]Val,
    nresults: usize,
) callconv(.c) ?*Trap;

const UncheckedCallback = fn (
    env: ?*anyopaque,
    caller: *Func.Caller,
    args_and_results: [*]Val.Raw,
    num_args_and_results: usize,
) callconv(.c) ?*Trap;

pub const Func = extern struct {
    pub const Type = wasm.Func.Type;

    pub fn CallResult(T: type) type {
        return union(enum) {
            ok: T,
            err: *Err,
            trap: *Trap,

            pub fn okOrError(self: @This()) !T {
                if (T == void and self == .ok) return;
                return switch (self) {
                    .ok => |val| val,
                    .err => |err| return err.testFail(),
                    .trap => |trap| return trap.toError() orelse
                        error.UnexpectedTrap,
                };
            }
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
        callback: fn (*Caller, args: []const Val, results: []Val) ?*Trap,
    ) Func {
        const Wrapper = struct {
            fn callbackFn(
                _: ?*anyopaque,
                caller: *Caller,
                args: [*]const Val,
                nargs: usize,
                results: [*]Val,
                nresults: usize,
            ) callconv(.c) ?*Trap {
                const fn_args = args[0..nargs];
                const fn_results = results[0..nresults];
                // return callback(caller, fn_args, fn_results);
                return @call(
                    .always_inline,
                    callback,
                    .{ caller, fn_args, fn_results },
                );
            }
        };

        var result: Func = .null;
        wasmtime_func_new(
            ctx,
            @"type",
            Wrapper.callbackFn,
            null,
            null,
            &result,
        );
        return result;
    }

    // pub fn initUnchecked(
    //     ctx: *Store.Ctx,
    //     @"type": *const Type,
    //     callback: *const UncheckedCallback,
    //     env: ?*anyopaque,
    //     finalizer: ?*const wasm.Finalizer,
    // ) Func {
    //     var result: Func = .null;
    //     wasmtime_func_new_unchecked(
    //         ctx,
    //         @"type",
    //         callback,
    //         env,
    //         finalizer,
    //         &result,
    //     );
    //     return result;
    // }

    pub fn raw(func: *const Func, ctx: *Store.Ctx) Val.Raw.FuncRef {
        return wasmtime_func_to_raw(ctx, func);
    }

    pub fn asExtern(func: *const Func) Extern {
        return .init(.func, .{ .func = func.* });
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

    pub fn equals(lhs: *const Func, rhs: *const Func) bool {
        return lhs.store_id == rhs.store_id;
    }

    pub fn call(
        func: *const Func,
        ctx: *Store.Ctx,
        args: []const Val,
        comptime nresults: usize,
    ) CallResult(switch (nresults) {
        0 => void,
        1 => Val,
        else => [nresults]Val,
    }) {
        var trap_result: ?*Trap = null;
        var results: [nresults]Val = undefined;
        return if (wasmtime_func_call(
            ctx,
            func,
            args.ptr,
            args.len,
            &results,
            nresults,
            &trap_result,
        )) |err|
            .{ .err = err }
        else if (trap_result) |trap|
            .{ .trap = trap }
        else if (nresults == 0)
            .ok
        else if (nresults == 1)
            .{ .ok = results[0] }
        else
            .{ .ok = results };
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

    test call {
        const store = Store.init(.init(), null);
        defer store.deinit();

        const Impl = struct {
            var called = false;

            fn callback(_: *Func.Caller, _: []const Val, _: []Val) ?*Trap {
                called = true;
                return null;
            }
        };

        const func = Func.init(
            store.ctx(),
            try .init(&.{}, &.{}),
            Impl.callback,
        );

        try testing.expect(!Impl.called);
        try func.call(store.ctx(), &.{}, 0)
            .okOrError();
        try testing.expect(Impl.called);
    }
};

test Func {
    const store = Store.init(.init(), null);
    defer store.deinit();

    const func = Func.init(
        store.ctx(),
        try .init(&.{}, &.{}),
        struct {
            fn callback(_: *Func.Caller, _: []const Val, _: []Val) ?*Trap {
                return null;
            }
        }.callback,
    );

    try func.call(store.ctx(), &.{}, 0)
        .okOrError();
}

test "return trap from call" {
    const store = Store.init(.init(), null);
    defer store.deinit();

    const func = Func.init(
        store.ctx(),
        try .init(&.{}, &.{}),
        struct {
            fn callback(_: *Func.Caller, _: []const Val, _: []Val) ?*Trap {
                return .init("x");
            }
        }.callback,
    );

    switch (func.call(store.ctx(), &.{}, 0)) {
        .ok => return error.ExpectedTrap,
        .err => |err| try testing.expectEqualSlices(u8, "x", err.message()),
        .trap => |trap| return trap.toError() orelse
            error.UnexpectedTrap,
    }
}

test "panic from call" {
    const store = Store.init(.init(), null);
    defer store.deinit();

    const func = Func.init(
        store.ctx(),
        try .init(&.{}, &.{}),
        struct {
            fn callback(_: *Func.Caller, _: []const Val, _: []Val) ?*Trap {
                @panic("x");
            }
        }.callback,
    );

    // TODO: support panics??
    // try func.call(store.ctx(), &.{}, 0)
    //     .okOrError();
    _ = func;
    return error.SkipZigTest;
}

test "args" {
    const store = Store.init(.init(), null);
    defer store.deinit();

    const functype: *Func.Type = try .init(
        &.{ try .init(.i32), try .init(.i64) },
        &.{ try .init(.f32), try .init(.f64) },
    );

    const func = Func.init(
        store.ctx(),
        functype,
        struct {
            fn callback(_: *Func.Caller, args: []const Val, results: []Val) ?*Trap {
                return wasmAssert(args.len == 2, "expected 2 args") orelse
                    wasmAssert(args[0].kind == .i32, "expected arg[0] to be i32") orelse
                    wasmAssert(args[1].kind == .i64, "expected arg[1] to be i64") orelse
                    wasmAssert(results.len == 2, "expected 2 result slots") orelse {
                    results[0] = .init(.f32, .{ .f32 = 123.456 });
                    results[1] = .init(.f64, .{ .f64 = 123.456 });
                    return null;
                };
            }
        }.callback,
    );

    const results = try func.call(
        store.ctx(),
        &.{
            .init(.i32, .{ .u32 = 0xdeadbeef }),
            .init(.i64, .{ .u64 = 0xdeadbeef22222222 }),
        },
        2,
    ).okOrError();
    try testing.expect(results[0].kind == .f32);
    try testing.expect(results[0].of.f32 == 123.456);
    try testing.expect(results[1].kind == .f64);
    try testing.expect(results[1].of.f64 == 123.456);
}

test "single result" {
    const store = Store.init(.init(), null);
    defer store.deinit();

    const func = Func.init(
        store.ctx(),
        try .init(&.{}, &.{try .init(.i32)}),
        struct {
            fn callback(_: *Func.Caller, args: []const Val, results: []Val) ?*Trap {
                return wasmAssert(args.len == 0, "too many args") orelse
                    wasmAssert(results.len == 1, "expected 1 result space") orelse {
                    results[0] = .init(.i32, .{ .u32 = 0xdeadbeef });
                    return null;
                };
            }
        }.callback,
    );

    const result = try func.call(store.ctx(), &.{}, 1)
        .okOrError();
    try testing.expect(result.kind == .i32);
    try testing.expect(result.of.u32 == 0xdeadbeef);
}

test "wrong result" {
    const store = Store.init(.init(), null);
    defer store.deinit();

    const func = Func.init(
        store.ctx(),
        try .init(&.{}, &.{try .init(.i32)}),
        struct {
            fn callback(_: *Func.Caller, _: []const Val, results: []Val) ?*Trap {
                results[0] = .init(.i64, .{ .u64 = 0xdeadbeef });
                return null;
            }
        }.callback,
    );

    const call_result = func.call(store.ctx(), &.{}, 1);
    try testing.expect(call_result == .err);
    call_result.err.deinit();
}
