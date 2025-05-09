const Func = @import("func.zig").Func;
const Trap = @import("trap.zig").Trap;
const Val = @import("val.zig").Val;
const wasm = @import("wasm.zig");

extern fn wasmtime_call_future_delete(future: *AsyncFunc.CallFuture) callconv(.c) void;
extern fn wasmtime_call_future_poll(future: *AsyncFunc.CallFuture) callconv(.c) bool;

pub const AsyncFunc = struct {
    pub const Continuation = extern struct {
        pub const Callback = fn (env: ?*anyopaque) callconv(.c) bool;

        callback: *const Callback,
        env: ?*anyopaque,
        finalizer: ?*const wasm.Finalizer,
    };

    pub const AsyncCallback = fn (
        env: ?*anyopaque,
        caller: *Func.Caller,
        args: [*]const Val,
        nargs: usize,
        results: [*]Val,
        nresults: usize,
        trap_ret: *?*Trap,
        continuation_ret: *Continuation,
    ) callconv(.c) void;

    pub const CallFuture = opaque {
        pub const deinit = wasmtime_call_future_delete;
        pub const poll = wasmtime_call_future_poll;
    };
};
