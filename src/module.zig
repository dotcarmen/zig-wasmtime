const wasmtime = @import("root.zig");
const wasm = wasmtime.wasm;
const Engine = wasmtime.Engine;
const Err = wasmtime.Err;
const Feature = wasmtime.Feature;

extern fn wasmtime_module_clone(*const Module) callconv(.c) ?*Module;
extern fn wasmtime_module_delete(*Module) callconv(.c) void;
extern fn wasmtime_module_deserialize(*Engine, [*]const u8, usize, *?*Module) callconv(.c) ?*Err;
extern fn wasmtime_module_deserialize_file(*Engine, [*:0]const u8, *?*Module) callconv(.c) ?*Err;
extern fn wasmtime_module_exports(*const Module, *wasm.ConstVec(*const wasm.ExportType)) callconv(.c) void;
extern fn wasmtime_module_image_range(*const Module, *?*anyopaque, *?*anyopaque) callconv(.c) void;
extern fn wasmtime_module_imports(*const Module, *wasm.ConstVec(*const wasm.ImportType)) callconv(.c) void;
extern fn wasmtime_module_new(*Engine, [*]const u8, usize, *?*Module) callconv(.c) ?*Err;
extern fn wasmtime_module_serialize(*Module, *wasm.ConstVec(u8)) callconv(.c) ?*Err;

pub const Module = opaque {
    const declare = wasm.declareSharableRef(Module, Shared, .module);
    pub const asRef = declare.asRef;
    pub const asConstRef = declare.asConstRef;
    pub const fromRef = declare.fromRef;
    pub const fromConstRef = declare.fromConstRef;
    pub const share = declare.share;

    pub const Shared = opaque {
        pub const obtain = declare.Shared.obtain;
    };

    pub const deinit = wasmtime_module_delete;

    pub fn init(engine: *Engine, binary: []const u8) Err.Result(*Module) {
        var ret: ?*Module = undefined;
        if (wasmtime_module_new(engine, binary.ptr, binary.len, &ret)) |err|
            return .{ .err = err };
        return .{ .ok = ret.? };
    }

    pub fn initWat(engine: *Engine, wat: []const u8) Err.Result(*Module) {
        const translated = switch (wasmtime.wat2wasm(wat)) {
            .ok => |ok| ok,
            .err => |err| return .{ .err = err },
        };

        return init(engine, translated);
    }

    pub fn clone(module: *const Module) !*Module {
        return wasmtime_module_clone(module).?;
    }

    pub fn getImports(module: *const Module) []const *const wasm.ImportType {
        var result: wasm.ConstVec(*const wasm.ImportType) = .empty;
        wasmtime_module_imports(module, &result);
        return result.to().?;
    }

    pub fn getExports(module: *const Module) []const *const wasm.ExportType {
        var result: wasm.ConstVec(*const wasm.ExportType) = .empty;
        wasmtime_module_exports(module, &result);
        return result.to().?;
    }

    pub fn imageRange(module: *const Module) ?[]u8 {
        var start: ?*anyopaque = null;
        var end: ?*anyopaque = null;
        wasmtime_module_image_range(module, &start, &end);

        const ptr = start orelse return null;
        const bytes: [*]u8 = @ptrCast(ptr);
        var len: usize = @intFromPtr(end.?);
        len -= @intFromPtr(ptr);
        return bytes[0..len];
    }

    pub fn serialize(module: *Module) Err.Result([]const u8) {
        var result: wasm.ConstVec(u8) = .empty;
        if (wasmtime_module_serialize(module, &result)) |err|
            return .{ .err = err };
        return .{ .ok = result.to().? };
    }

    pub fn deserialize(engine: *Engine, bytes: []const u8) Err.Result(*Module) {
        var result: ?*Module = null;
        if (wasmtime_module_deserialize(engine, bytes.ptr, bytes.len, &result)) |err|
            return .{ .err = err };
        return .{ .ok = result.? };
    }

    pub fn deserializeFile(engine: *Engine, path: [*:0]const u8) Err.Result(*Module) {
        var result: ?*Module = null;
        if (wasmtime_module_deserialize_file(engine, path, &result)) |err|
            return .{ .err = err };
        return .{ .ok = result.? };
    }
};
