const Err = @import("error.zig").Err;
const Module = @import("module.zig").Module;
const Store = @import("store.zig").Store;
const wasm = @import("wasm.zig");

extern fn wasmtime_guestprofiler_delete(*GuestProfiler) callconv(.c) void;
extern fn wasmtime_guestprofiler_finish(*GuestProfiler, *wasm.ConstVec(u8)) callconv(.c) ?*Err;
extern fn wasmtime_guestprofiler_new(
    module_name: *const wasm.ConstVec(u8),
    interval_nanos: u64,
    modules: [*]const GuestProfilerModule,
    modules_len: usize,
) callconv(.c) ?*GuestProfiler;
extern fn wasmtime_guestprofiler_sample(*GuestProfiler, *const Store, delta_nanos: u64) callconv(.c) void;

pub const GuestProfilerModule = extern struct {
    name: wasm.ConstVec(u8),
    mod: *const Module,
};

pub const GuestProfiler = opaque {
    pub const deinit = wasmtime_guestprofiler_delete;
    pub const sample = wasmtime_guestprofiler_sample;

    pub fn init(
        name: []const u8,
        interval_nanos: u64,
        modules: []const GuestProfilerModule,
    ) *GuestProfiler {
        return wasmtime_guestprofiler_new(
            &.of(name),
            interval_nanos,
            modules.ptr,
            modules.len,
        ).?;
    }

    pub fn finish(profiler: *GuestProfiler) Err.Result([]const u8) {
        var result: wasm.ConstVec(u8) = .empty;
        if (wasmtime_guestprofiler_finish(profiler, &result)) |err|
            return .{ .err = err };
        return .{ .ok = result.to().? };
    }
};
