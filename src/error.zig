const wasm = @import("wasm.zig");

const std = @import("std");
const Allocator = std.mem.Allocator;

extern fn wasmtime_error_new([*:0]const u8) callconv(.c) ?*Err;
extern fn wasmtime_error_delete(*Err) callconv(.c) void;
extern fn wasmtime_error_message(*const Err, *wasm.ConstVec(u8)) callconv(.c) void;
extern fn wasmtime_error_exit_status(*const Err, *c_int) callconv(.c) bool;
extern fn wasmtime_error_wasm_trace(*const Err, *wasm.ConstVec(*wasm.Frame)) callconv(.c) void;

pub const Err = opaque {
    pub const deinit = wasmtime_error_delete;

    pub fn init(message: [:0]const u8) *Err {
        return wasmtime_error_new(message.ptr).?;
    }

    pub fn render(err: *const Err) []const u8 {
        var result: wasm.ConstVec(u8) = .empty;
        wasmtime_error_message(err, &result);
        return result.to().?;
    }

    pub fn getExitStatus(err: *const Err) ?c_int {
        var status: c_int = undefined;
        if (wasmtime_error_exit_status(err, &status))
            return status;
        return null;
    }

    pub fn trace(err: *const Err) []const *wasm.Frame {
        var result: wasm.ConstVec(*wasm.Frame) = .empty;
        wasmtime_error_wasm_trace(err, &result);
        return result.to().?;
    }

    pub fn Result(Ok: type) type {
        return union(enum) {
            ok: Ok,
            err: *Err,
        };
    }
};
