const c = @cImport(@cInclude("wasmtime/extern.h"));

const Func = @import("func.zig").Func;
const Global = @import("global.zig").Global;
const Memory = @import("memory.zig").Memory;
const Module = @import("module.zig").Module;
const SharedMemory = @import("shared_memory.zig").SharedMemory;
const Store = @import("store.zig").Store;
const Table = @import("table.zig").Table;
const wasm = @import("wasm.zig");

extern fn wasmtime_extern_delete(*Extern) callconv(.c) void;
extern fn wasmtime_extern_type(*Store.Ctx, *Extern) callconv(.c) ?*Extern.Type;

pub const Extern = extern struct {
    pub const Type = wasm.Extern.Type;

    pub const Kind = enum(u8) {
        func = c.WASMTIME_EXTERN_FUNC,
        global = c.WASMTIME_EXTERN_GLOBAL,
        table = c.WASMTIME_EXTERN_TABLE,
        memory = c.WASMTIME_EXTERN_MEMORY,
        shared_memory = c.WASMTIME_EXTERN_SHAREDMEMORY,
        _,
    };

    kind: Kind,
    of: extern union {
        func: Func,
        global: Global,
        table: Table,
        memory: Memory,
        shared_memory: *SharedMemory,
    },

    pub const deinit = wasmtime_extern_delete;

    pub fn init(kind: Kind, of: @FieldType(Extern, "of")) Extern {
        return .{ .kind = kind, .of = of };
    }

    pub fn asFunc(self: *const Extern) ?Func {
        if (self.kind != .func) return null;
        return self.of.func;
    }

    pub fn asGlobal(self: *const Extern) ?Global {
        if (self.kind != .global) return null;
        return self.of.global;
    }

    pub fn asTable(self: *const Extern) ?Table {
        if (self.kind != .table) return null;
        return self.of.table;
    }

    pub fn asMemory(self: *const Extern) ?Memory {
        if (self.kind != .memory) return null;
        return self.of.memory;
    }

    pub fn asSharedMemory(self: *const Extern) ?*SharedMemory {
        if (self.kind != .shared_memory) return null;
        return self.of.shared_memory;
    }

    pub fn getType(val: *Extern, context: *Store.Ctx) *Type {
        return wasmtime_extern_type(context, val).?;
    }
};
