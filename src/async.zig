const wasmtime = @import("root.zig");
const wasm = wasmtime.wasm;

extern fn wasmtime_call_future_delete(future: *AsyncFunc.CallFuture) callconv(.c) void;
extern fn wasmtime_call_future_poll(future: *AsyncFunc.CallFuture) callconv(.c) bool;

pub const AsyncFunc = struct {
    pub const Continuation = extern struct {
        pub const Callback = fn (env: ?*anyopaque) callconv(.c) bool;

        callback: *const Callback,
        env: ?*anyopaque,
        finalizer: ?wasm.Finalizer.Func,
    };

    pub const AsyncCallback = fn (
        env: ?*anyopaque,
        caller: *wasmtime.Func.Caller,
        args: [*]const wasmtime.Val,
        nargs: usize,
        results: [*]wasmtime.Val,
        nresults: usize,
        trap_ret: *?*wasmtime.Trap,
        continuation_ret: *Continuation,
    ) callconv(.c) void;

    pub const CallFuture = opaque {
        pub const deinit = wasmtime_call_future_delete;
        pub const poll = wasmtime_call_future_poll;
    };
};
