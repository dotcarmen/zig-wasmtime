const c = @cImport(@cInclude("wasmtime.h"));

const WasmtimeExtern = @import("extern.zig").Extern;
const Module = @import("module.zig").Module;
const Store = @import("store.zig").Store;
const Trap = @import("trap.zig").Trap;

const std = @import("std");
const assert = std.debug.assert;

extern fn wasm_exporttype_name(*const ExportType) callconv(.c) *const ConstVec(u8);
extern fn wasm_exporttype_new(*const ConstVec(u8), *Extern.Type) callconv(.c) ?*ExportType;
extern fn wasm_exporttype_type(*const ExportType) callconv(.c) *const Extern.Type;
extern fn wasm_externtype_as_functype(*Extern.Type) callconv(.c) ?*Func.Type;
extern fn wasm_externtype_as_functype_const(*const Extern.Type) callconv(.c) ?*const Func.Type;
extern fn wasm_externtype_as_globaltype(*Extern.Type) callconv(.c) ?*Global.Type;
extern fn wasm_externtype_as_globaltype_const(*const Extern.Type) callconv(.c) ?*const Global.Type;
extern fn wasm_externtype_as_tabletype(*Extern.Type) callconv(.c) ?*Table.Type;
extern fn wasm_externtype_as_tabletype_const(*const Extern.Type) callconv(.c) ?*const Table.Type;
extern fn wasm_externtype_as_memorytype(*Extern.Type) callconv(.c) ?*Memory.Type;
extern fn wasm_externtype_as_memorytype_const(*const Extern.Type) callconv(.c) ?*const Memory.Type;
extern fn wasm_externtype_kind(*const Extern.Type) callconv(.c) WasmtimeExtern.Kind;
extern fn wasm_foreign_new(*Store) ?*Foreign;
extern fn wasm_frame_delete(*Frame) callconv(.c) void;
extern fn wasm_frame_copy(*const Frame) callconv(.c) ?*Frame;
extern fn wasm_frame_func_index(*const Frame) callconv(.c) u32;
extern fn wasm_frame_func_offset(*const Frame) callconv(.c) usize;
extern fn wasm_frame_instance(*const Frame) callconv(.c) ?*Instance;
extern fn wasm_frame_module_offset(*const Frame) callconv(.c) usize;
extern fn wasm_func_call(*const Func, args: *const ConstVec(Val), results: *Vec(Val)) callconv(.c) ?*Trap;
extern fn wasm_func_new(*Store, *Func.Type, *const FuncCallback) callconv(.c) ?*Func;
extern fn wasm_func_new_with_env(*Store, *Func.Type, *const FuncWithEnvCallback, env: ?*anyopaque, ?*const Finalizer) callconv(.c) *Func;
extern fn wasm_func_param_arity(*const Func) callconv(.c) usize;
extern fn wasm_func_result_arity(*const Func) callconv(.c) usize;
extern fn wasm_func_type(*const Func) callconv(.c) ?*Func.Type;
extern fn wasm_functype_as_externtype(*Func.Type) callconv(.c) *Extern.Type;
extern fn wasm_functype_as_externtype_const(*const Func.Type) callconv(.c) *const Extern.Type;
extern fn wasm_functype_new(params: *const ConstVec(*const Val.Type), results: *const ConstVec(*const Val.Type)) callconv(.c) ?*Func.Type;
extern fn wasm_functype_params(*const Func.Type) callconv(.c) *const ConstVec(*const Val.Type);
extern fn wasm_functype_results(*const Func.Type) callconv(.c) *const ConstVec(*const Val.Type);
extern fn wasm_global_get(*const Global, *Val) callconv(.c) void;
extern fn wasm_global_new(*Store, *const Global.Type, *const Val) callconv(.c) ?*Global;
extern fn wasm_global_type(*const Global) callconv(.c) ?*Global.Type;
extern fn wasm_global_set(*Global, *const Val) callconv(.c) void;
extern fn wasm_globaltype_as_externtype(*Global.Type) callconv(.c) *Extern.Type;
extern fn wasm_globaltype_as_externtype_const(*const Global.Type) callconv(.c) *const Extern.Type;
extern fn wasm_globaltype_content(*const Global.Type) callconv(.c) *const Val.Type;
extern fn wasm_globaltype_mutability(*const Global.Type) callconv(.c) Mutability;
extern fn wasm_globaltype_new(*Val.Type, Mutability) callconv(.c) ?*Global.Type;
extern fn wasm_importtype_module(*const ImportType) callconv(.c) *const ConstVec(u8);
extern fn wasm_importtype_name(*const ImportType) callconv(.c) *const ConstVec(u8);
extern fn wasm_importtype_new(module: *const Module, name: *const ConstVec(u8), @"type": *const Extern.Type) callconv(.c) ?*ImportType;
extern fn wasm_importtype_type(*const ImportType) callconv(.c) *const Extern.Type;
extern fn wasm_instance_delete(*Frame) callconv(.c) void;
extern fn wasm_instance_copy(*const Frame) callconv(.c) ?*Frame;
extern fn wasm_instance_new(*Store, *const Module, imports: *const ConstVec(*const Extern), *?*Trap) callconv(.c) ?*Instance;
extern fn wasm_instance_same(*const Frame, *const Frame) callconv(.c) bool;
extern fn wasm_instance_as_ref(*Frame) callconv(.c) *Ref;
extern fn wasm_instance_as_ref_const(*const Frame) callconv(.c) *const Ref;
extern fn wasm_memory_data(*Memory) callconv(.c) [*]u8;
extern fn wasm_memory_data_size(*const Memory) callconv(.c) usize;
extern fn wasm_memory_grow(*Memory, delta: u32) callconv(.c) bool;
extern fn wasm_memory_new(*Store, *const Memory.Type) callconv(.c) ?*Memory;
extern fn wasm_memory_size(*const Memory) callconv(.c) u32;
extern fn wasm_memory_type(*const Memory) callconv(.c) ?*Memory.Type;
extern fn wasm_memorytype_as_externtype(*Memory.Type) callconv(.c) *Extern.Type;
extern fn wasm_memorytype_as_externtype_const(*const Memory.Type) callconv(.c) *const Extern.Type;
extern fn wasm_memorytype_limits(*const Memory.Type) callconv(.c) *const Limits;
extern fn wasm_ref_as_instance(*Ref) callconv(.c) ?*Frame;
extern fn wasm_ref_as_instance_const(*const Ref) callconv(.c) ?*const Frame;
extern fn wasm_table_get(*const Table, index: Table.Index) callconv(.c) ?*Ref;
extern fn wasm_table_grow(*Table, delta: Table.Size, init: *Ref) callconv(.c) bool;
extern fn wasm_table_new(*Store, *const Table.Type, init: *Ref) callconv(.c) ?*Table;
extern fn wasm_table_set(*Table, Table.Index, *Ref) callconv(.c) bool;
extern fn wasm_table_size(*const Table) callconv(.c) usize;
extern fn wasm_table_type(*const Table) callconv(.c) *Table.Type;
extern fn wasm_tabletype_as_externtype(*Table.Type) callconv(.c) *Extern.Type;
extern fn wasm_tabletype_as_externtype_const(*const Table.Type) callconv(.c) *const Extern.Type;
extern fn wasm_tabletype_element(*const Table.Type) callconv(.c) *const Val.Type;
extern fn wasm_tabletype_limits(*const Table.Type) callconv(.c) *const Limits;
extern fn wasm_tabletype_new(*Val.Type, *const Limits) callconv(.c) ?*Table.Type;
extern fn wasm_val_copy(out: *Val, *const Val) callconv(.c) void;
extern fn wasm_val_delete(*Val) callconv(.c) void;
extern fn wasm_valtype_kind(*const Val.Type) callconv(.c) Val.Kind;
extern fn wasm_valtype_new(Val.Kind) callconv(.c) ?*Val.Type;

extern fn wasmtime_frame_func_name(*const Frame) callconv(.c) ?*const ConstVec(u8);
extern fn wasmtime_frame_module_name(*const Frame) callconv(.c) ?*const ConstVec(u8);
extern fn wasmtime_memorytype_is64(*const Memory.Type) callconv(.c) bool;
extern fn wasmtime_memorytype_isshared(*const Memory.Type) callconv(.c) bool;
extern fn wasmtime_memorytype_maximum(*const Memory.Type, max: *u64) callconv(.c) bool;
extern fn wasmtime_memorytype_minimum(*const Memory.Type) callconv(.c) u64;
extern fn wasmtime_memorytype_new(min: u64, max_present: bool, max: u64, is64: bool, shared: bool) callconv(.c) *Memory.Type;

pub const FuncCallback = fn (args: *const ConstVec(Val), results: *Vec(Val)) ?*Trap;
pub const FuncWithEnvCallback = fn (env: ?*anyopaque, args: *const ConstVec(Val), results: *Vec(Val)) ?*Trap;
pub const Finalizer = fn (*anyopaque) callconv(.c) void;

pub fn ConstVec(T: type) type {
    return extern struct {
        size: usize,
        data: ?[*]const T,

        pub const empty: @This() = .{
            .size = undefined,
            .data = null,
        };

        pub fn of(data: []const T) @This() {
            return .{
                .size = data.len,
                .data = data.ptr,
            };
        }

        pub fn to(vec: @This()) ?[]const T {
            const data = vec.data orelse return null;
            return data[0..vec.size];
        }
    };
}

pub fn Vec(T: type) type {
    return extern struct {
        size: usize = 0,
        data: ?[*]T = null,

        pub const empty: @This() = .{
            .size = undefined,
            .data = null,
        };

        pub fn of(data: []T) @This() {
            return .{
                .size = data.len,
                .data = data.ptr,
            };
        }

        pub fn to(vec: @This()) ?[]T {
            const data = vec.data orelse return null;
            return data[0..vec.size];
        }
    };
}

pub const Mutability = enum(u8) {
    @"const" = c.WASM_CONST,
    @"var" = c.WASM_VAR,
};

pub const Limits = extern struct {
    min: u32,
    max: u32 = std.math.maxInt(u32),

    pub fn getMax(lim: *const Limits) ?u32 {
        if (lim.max == std.math.maxInt(u32))
            return null;
        return lim.max;
    }
};

pub fn declareType(T: type, name: @Type(.enum_literal)) type {
    const basename = "wasm_" ++ @tagName(name) ++ "type_";

    const wasm_delete = @extern(
        *const fn (*T) callconv(.c) void,
        .{ .name = basename ++ "delete" },
    );

    const wasm_copy = @extern(
        *const fn (*const T) callconv(.c) ?*T,
        .{ .name = basename ++ "delete" },
    );

    return opaque {
        pub const deinit = wasm_delete;

        pub fn clone(@"type": *const T) *T {
            return wasm_copy(@"type").?;
        }
    };
}

pub const Val = extern struct {
    kind: Kind,
    of: extern union {
        i32: i32,
        i64: i64,
        u32: u32,
        u64: u64,
        f32: f32,
        f64: f64,
        ref: *Ref,
    },

    comptime {
        assert(@alignOf(Val) == @alignOf(c.wasm_val_t));
        assert(@sizeOf(Val) == @sizeOf(c.wasm_val_t));
    }

    pub const deinit = wasm_val_delete;

    pub fn init(kind: Kind, of: @FieldType(Val, "of")) Val {
        return .{ .kind = kind, .of = of };
    }

    pub fn initPtr(ptr: ?*anyopaque) Val {
        return switch (@bitSizeOf(usize)) {
            32 => .init(.i32, .{ .u32 = @intCast(@intFromPtr(ptr)) }),
            64 => .init(.i64, .{ .u64 = @intCast(@intFromPtr(ptr)) }),
            else => unreachable,
        };
    }

    pub fn clone(val: *const Val) Val {
        var result: Val = undefined;
        wasm_val_copy(&result, val);
        return result;
    }

    pub const Kind = enum(u8) {
        i32 = c.WASM_I32,
        i64 = c.WASM_I64,
        f32 = c.WASM_F32,
        f64 = c.WASM_F64,
        externref = c.WASM_EXTERNREF,
        funcref = c.WASM_FUNCREF,
        _,

        pub fn isNum(k: Kind) bool {
            return @intFromEnum(k) < @intFromEnum(Kind.externref);
        }

        pub fn isRef(k: Kind) bool {
            return @intFromEnum(k) >= @intFromEnum(Kind.externref);
        }
    };

    pub const Type = opaque {
        pub fn init(kind: Kind) *Type {
            return wasm_valtype_new(kind).?;
        }

        pub fn getKind(@"type": *const Type) Kind {
            return wasm_valtype_kind(@"type");
        }

        pub fn isNum(@"type": *const Type) bool {
            return @"type".getKind().isNum();
        }

        pub fn isRef(@"type": *const Type) bool {
            return @"type".getKind().isRef();
        }
    };
};

pub const ImportType = opaque {
    pub const getType = wasm_importtype_type;

    const declare = declareType(ImportType, .import);
    pub const deinit = declare.deinit;
    pub const clone = declare.clone;

    pub fn getName(@"type": *const ImportType) []const u8 {
        return wasm_importtype_name(@"type").to().?;
    }

    pub fn getModule(@"type": *const ImportType) []const u8 {
        return wasm_importtype_module(@"type").to().?;
    }
};

pub const ExportType = opaque {
    pub const getType = wasm_exporttype_type;

    const declare = declareType(ExportType, .@"export");
    pub const deinit = declare.deinit;
    pub const clone = declare.clone;

    pub fn init(name: []const u8, @"type": *Extern.Type) *ExportType {
        return wasm_exporttype_new(&.of(name), @"type").?;
    }

    pub fn getName(@"type": *const ExportType) []const u8 {
        return wasm_exporttype_name(@"type").to().?;
    }
};

pub fn declareRefBase(T: type, name: @Type(.enum_literal)) type {
    const basename = "wasm_" ++ @tagName(name) ++ "_";

    const deleteFn = @extern(
        *const fn (*T) callconv(.c) void,
        .{ .name = basename ++ "delete" },
    );

    const copyFn = @extern(
        *const fn (*const T) callconv(.c) ?*T,
        .{ .name = basename ++ "copy" },
    );

    const getHostInfoFn = @extern(
        *const fn (*const T) callconv(.c) ?*anyopaque,
        .{ .name = basename ++ "get_host_info" },
    );

    const setHostInfoWithFinalizerFn = @extern(
        *const fn (*T, ?*anyopaque, ?*const Finalizer) callconv(.c) ?*anyopaque,
        .{ .name = basename ++ "set_host_info_with_finalizer" },
    );

    return opaque {
        pub const deinit = deleteFn;
        pub const getHostInfo = getHostInfoFn;

        pub fn clone(self: *const T) *T {
            return copyFn(self).?;
        }

        pub fn setHostInfo(self: *T, env: struct {
            env: ?*anyopaque = null,
            finalizer: ?*const Finalizer = null,
        }) void {
            setHostInfoWithFinalizerFn(self, env.env, env.finalizer);
        }
    };
}

pub fn declareRef(T: type, name: @Type(.enum_literal)) type {
    const basename = "wasm_" ++ @tagName(name) ++ "_";
    const declare = declareRefBase(T, name);

    const asRefFn = @extern(
        *const fn (*T) callconv(.c) *Ref,
        .{ .name = basename ++ "as_ref" },
    );

    const asConstRefFn = @extern(
        *const fn (*const T) callconv(.c) *const Ref,
        .{ .name = basename ++ "as_ref_const" },
    );

    const fromRefFn = @extern(
        *const fn (*Ref) callconv(.c) ?*T,
        .{ .name = "wasm_ref_as_" ++ @tagName(name) },
    );

    const fromConstRefFn = @extern(
        *const fn (*const Ref) callconv(.c) ?*const T,
        .{ .name = "wasm_ref_as_" ++ @tagName(name) ++ "_const" },
    );

    return opaque {
        pub const deinit = declare.deinit;
        pub const clone = declare.clone;
        pub const getHostInfo = declare.getHostInfo;
        pub const setHostInfo = declare.setHostInfo;

        pub const asRef = asRefFn;
        pub const asConstRef = asConstRefFn;
        pub const fromRef = fromRefFn;
        pub const fromConstRef = fromConstRefFn;
    };
}

pub fn declareSharableRef(
    T: type,
    SharedT: type,
    name: @Type(.enum_literal),
) type {
    const declare = declareRef(T, name);

    const shareFn = @extern(
        *const fn (*const T) callconv(.c) ?*SharedT,
        .{ .name = "wasm_" ++ @tagName(name) ++ "_share" },
    );

    const obtainFn = @extern(
        *const fn (*Store, *const SharedT) callconv(.c) ?*T,
        .{ .name = "wasm_" ++ @tagName(name) ++ "_share" },
    );

    return opaque {
        pub const deinit = declare.deinit;
        pub const clone = declare.clone;
        pub const getHostInfo = declare.getHostInfo;
        pub const setHostInfo = declare.setHostInfo;
        pub const asRef = declare.asRef;
        pub const asConstRef = declare.asConstRef;
        pub const fromRef = declare.fromRef;
        pub const fromConstRef = declare.fromConstRef;

        pub fn share(self: *const T) *SharedT {
            return shareFn(self).?;
        }

        pub const Shared = opaque {
            pub fn obtain(shared: *const SharedT, store: *Store) *T {
                return obtainFn(store, shared).?;
            }
        };
    };
}

pub const Ref = opaque {
    const declare = declareRefBase(Ref, .ref);
    pub const deinit = declare.deinit;
    pub const clone = declare.clone;
};

pub const Frame = opaque {
    pub const deinit = wasm_frame_delete;
    pub const getFuncIndex = wasm_frame_func_index;
    pub const getFuncOffset = wasm_frame_func_offset;
    pub const getModuleOffset = wasm_frame_module_offset;

    pub fn clone(frame: *Frame) *Frame {
        return wasm_frame_copy(frame).?;
    }

    pub fn getFuncName(frame: *const Frame) ?[]const u8 {
        if (wasmtime_frame_func_name(frame)) |name|
            return name.to();
        return null;
    }

    pub fn getInstance(frame: *const Frame) *Instance {
        return wasm_frame_instance(frame).?;
    }

    pub fn getModuleName(frame: *const Frame) ?[]const u8 {
        if (wasmtime_frame_module_name(frame)) |name|
            return name.to();
        return null;
    }
};

pub const Foreign = opaque {
    const declare = declareRef(Foreign, .foreign);
    pub const deinit = declare.deinit;
    pub const clone = declare.clone;
    pub const asRef = declare.asRef;
    pub const asConstRef = declare.asConstRef;
    pub const fromRef = declare.fromRef;
    pub const fromConstRef = declare.fromConstRef;

    pub fn init(store: *Store) *Foreign {
        return wasm_foreign_new(store).?;
    }
};

pub const Func = opaque {
    pub const call = wasm_func_call;
    pub const paramArity = wasm_func_param_arity;
    pub const resultArity = wasm_func_result_arity;

    const declare = declareRef(Func, .func);
    pub const deinit = declare.deinit;
    pub const clone = declare.clone;
    pub const asRef = declare.asRef;
    pub const asConstRef = declare.asConstRef;
    pub const fromRef = declare.fromRef;
    pub const fromConstRef = declare.fromConstRef;

    pub fn init(
        store: *Store,
        @"type": *const Func.Type,
        func: FuncCallback,
    ) *Func {
        return wasm_func_new(store, @"type", func).?;
    }

    pub fn initEnv(
        store: *Store,
        @"type": *const Func.Type,
        func: FuncCallback,
        env: ?*anyopaque,
        finalizer: ?*const Finalizer,
    ) *Func {
        return wasm_func_new_with_env(store, @"type", func, env, finalizer).?;
    }

    pub fn getType(func: *const Func) *Func.Type {
        return wasm_func_type(func).?;
    }

    pub const Type = opaque {
        pub const asExternType = wasm_functype_as_externtype;
        pub const asExternTypeConst = wasm_functype_as_externtype_const;

        const declare_type = declareType(Type, .func);
        pub const clone = declare_type.clone;
        pub const deinit = declare_type.deinit;

        pub fn init(params: []const *const Val.Type, results: []const *const Val.Type) *Type {
            return wasm_functype_new(&.of(params), &.of(results)).?;
        }

        pub fn getParams(@"type": *const Func.Type) []const *const Val.Type {
            return wasm_functype_params(@"type").to().?;
        }

        pub fn getResults(@"type": *const Func.Type) []const *const Val.Type {
            return wasm_functype_results(@"type").to().?;
        }
    };
};

pub const Global = opaque {
    pub const set = wasm_global_set;

    const declare = declareRef(Global, .global);
    pub const deinit = declare.deinit;
    pub const clone = declare.clone;
    pub const asRef = declare.asRef;
    pub const asConstRef = declare.asConstRef;
    pub const fromRef = declare.fromRef;
    pub const fromConstRef = declare.fromConstRef;

    pub fn init(store: *Store, @"type": *const Global.Type, initVal: *const Val) *Global {
        return wasm_global_new(store, @"type", initVal).?;
    }

    pub fn get(global: *const Global) Val {
        var result: Val = undefined;
        wasm_global_get(global, &result);
        return result;
    }

    pub fn getType(global: *const Global) *Global.Type {
        return wasm_global_type(global).?;
    }

    pub const Type = opaque {
        pub const asExternType = wasm_globaltype_as_externtype;
        pub const asExternTypeConst = wasm_globaltype_as_externtype_const;
        pub const getContent = wasm_globaltype_content;
        pub const getMutability = wasm_globaltype_mutability;

        const declare_type = declareType(Type, .global);
        pub const deinit = declare_type.deinit;
        pub const clone = declare_type.clone;

        pub fn init(@"type": *Val.Type, mut: Mutability) *Type {
            return wasm_globaltype_new(@"type", mut).?;
        }
    };
};

pub const Table = opaque {
    pub const Size = u32;
    pub const Index = enum(Size) { _ };

    pub const get = wasm_table_get;
    pub const getSize = wasm_table_size;
    pub const getType = wasm_table_type;

    const declare = declareRef(Table, .table);
    pub const deinit = declare.deinit;
    pub const clone = declare.clone;
    pub const asRef = declare.asRef;
    pub const asConstRef = declare.asConstRef;
    pub const fromRef = declare.fromRef;
    pub const fromConstRef = declare.fromConstRef;

    pub const Type = opaque {
        pub const asExternType = wasm_tabletype_as_externtype;
        pub const asExternTypeConst = wasm_tabletype_as_externtype_const;
        pub const getElement = wasm_tabletype_element;
        pub const getLimits = wasm_tabletype_limits;

        const declare_type = declareType(Type, .table);
        pub const deinit = declare_type.deinit;
        pub const clone = declare_type.clone;

        pub fn init(@"type": *Val.Type, limits: *const Limits) *Type {
            return wasm_tabletype_new(@"type", limits).?;
        }
    };
};

pub const Memory = opaque {
    pub const getDataPtr = wasm_memory_data;
    pub const getDataSize = wasm_memory_data_size;
    pub const getSize = wasm_memory_size;

    const declare = declareRef(Memory, .memory);
    pub const deinit = declare.deinit;
    pub const clone = declare.clone;
    pub const asRef = declare.asRef;
    pub const asConstRef = declare.asConstRef;
    pub const fromRef = declare.fromRef;
    pub const fromConstRef = declare.fromConstRef;

    pub fn init(store: *Store, @"type": *const Type) *Memory {
        return wasm_memory_new(store, @"type").?;
    }

    pub fn getData(memory: *Memory) []u8 {
        const ptr = memory.getDataPtr();
        const len = memory.getDataSize();
        return ptr[0..len];
    }

    pub fn getType(memory: *const Memory) *Type {
        return wasm_memory_type(memory).?;
    }

    pub fn grow(memory: *Memory, delta: u32) !void {
        if (!wasm_memory_grow(memory, delta))
            return error.OutOfMemory;
    }

    pub const Type = opaque {
        pub const asExternType = wasm_memorytype_as_externtype;
        pub const asExternTypeConst = wasm_memorytype_as_externtype_const;
        pub const is64 = wasmtime_memorytype_is64;
        pub const isShared = wasmtime_memorytype_isshared;
        pub const getLimits = wasm_memorytype_limits;
        pub const getMinimum = wasmtime_memorytype_minimum;

        const declare_type = declareType(Type, .memory);
        pub const deinit = declare_type.deinit;
        pub const clone = declare_type.clone;

        pub fn init(limits: Limits, is_64: bool, is_shared: bool) *Type {
            const max = limits.getMax();
            return wasmtime_memorytype_new(
                limits.min,
                max != null,
                max orelse undefined,
                is_64,
                is_shared,
            );
        }

        pub fn getMaximum(memtype: *const Type) ?u64 {
            var result: u64 = undefined;
            if (wasmtime_memorytype_maximum(memtype, &result))
                return result;
            return null;
        }
    };
};

pub const Extern = opaque {
    const declare = declareRef(Extern, .@"extern");
    pub const deinit = declare.deinit;
    pub const clone = declare.clone;
    pub const asRef = declare.asRef;
    pub const asConstRef = declare.asConstRef;
    pub const fromRef = declare.fromRef;
    pub const fromConstRef = declare.fromConstRef;

    pub const Type = opaque {
        pub const asFuncType = wasm_externtype_as_functype;
        pub const asFuncTypeConst = wasm_externtype_as_functype_const;
        pub const asGlobalType = wasm_externtype_as_globaltype;
        pub const asGlobalTypeConst = wasm_externtype_as_globaltype_const;
        pub const asTableType = wasm_externtype_as_tabletype;
        pub const asTableTypeConst = wasm_externtype_as_tabletype_const;
        pub const asMemoryType = wasm_externtype_as_memorytype;
        pub const asMemoryTypeConst = wasm_externtype_as_memorytype_const;
        pub const getKind = wasm_externtype_kind;

        const declare_type = declareType(Type, .@"extern");
        pub const deinit = declare_type.deinit;
        pub const clone = declare_type.clone;
    };
};

pub const Instance = opaque {
    pub const equals = wasm_instance_same;

    const declare = declareRef(Instance, .instance);
    pub const deinit = declare.deinit;
    pub const clone = declare.clone;
    pub const asRef = declare.asRef;
    pub const asConstRef = declare.asConstRef;
    pub const fromRef = declare.fromRef;
    pub const fromConstRef = declare.fromConstRef;

    pub fn init(
        store: *Store,
        module: *const Module,
        imports: []const *const Extern,
    ) union(enum) {
        ok: *Instance,
        trap: *Trap,
    } {
        var trap_result: ?*Trap = null;
        const result = wasm_instance_new(
            store,
            module,
            &.of(imports),
            &trap_result,
        );

        return if (trap_result) |trap|
            .{ .trap = trap }
        else if (result) |instance|
            .{ .ok = instance }
        else
            unreachable;
    }
};
