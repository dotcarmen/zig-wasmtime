const c = @cImport(@cInclude("wasmtime/val.h"));
const Extern = @import("extern.zig").Extern;
const Func = @import("func.zig").Func;
const Store = @import("store.zig").Store;

const std = @import("std");
const assert = std.debug.assert;

extern fn wasmtime_anyref_clone(*Store.Ctx, *const Val.AnyRef, *Val.AnyRef) callconv(.c) void;
extern fn wasmtime_anyref_unroot(*Store.Ctx, *Val.AnyRef) callconv(.c) void;
extern fn wasmtime_anyref_from_raw(*Store.Ctx, Val.Raw.AnyRef, *Val.AnyRef) callconv(.c) void;
extern fn wasmtime_anyref_from_i31(*Store.Ctx, I31Wrapper, *Val.AnyRef) callconv(.c) void;
extern fn wasmtime_anyref_i31_get_u(*Store.Ctx, *const Val.AnyRef, *I31Wrapper) callconv(.c) bool;
extern fn wasmtime_anyref_to_raw(*Store.Ctx, *const Val.AnyRef) callconv(.c) Val.Raw.AnyRef;
extern fn wasmtime_externref_clone(*Store.Ctx, *const Val.ExternRef, *Val.ExternRef) callconv(.c) void;
extern fn wasmtime_externref_data(*Store.Ctx, *const Val.ExternRef) callconv(.c) ?*anyopaque;
extern fn wasmtime_externref_from_raw(*Store.Ctx, Val.Raw.ExternRef, *Val.ExternRef) callconv(.c) void;
extern fn wasmtime_externref_new(*Store.Ctx, ?*anyopaque, ?*const fn (*anyopaque) callconv(.c) void, *Val.ExternRef) callconv(.c) bool;
extern fn wasmtime_externref_to_raw(*Store.Ctx, *const Val.ExternRef) callconv(.c) Val.Raw.ExternRef;
extern fn wasmtime_externref_unroot(*Store.Ctx, *Val.ExternRef) callconv(.c) void;
extern fn wasmtime_func_from_raw(ctx: *Store.Ctx, raw: Val.Raw.FuncRef, ret: *Func) callconv(.c) void;
extern fn wasmtime_val_clone(*Store.Ctx, *const Val, *Val) callconv(.c) void;
extern fn wasmtime_val_unroot(*Store.Ctx, *Val) callconv(.c) void;

const I31Wrapper = packed struct(u32) {
    _ignore: u1 = undefined,
    i31: i31,
};

pub const Val = extern struct {
    const V128 = [16]u8;

    kind: Kind,
    of: extern union {
        i32: i32,
        i64: i64,
        u32: u32,
        u64: u64,
        f32: f32,
        f64: f64,
        anyref: AnyRef,
        externref: ExternRef,
        funcref: Func,
        v128: V128,
        ptr: ?*anyopaque,

        comptime {
            assert(@alignOf(@This()) == 8);
            assert(@sizeOf(@This()) == 16);
        }
    } = undefined,

    pub fn init(kind: Kind, of: @FieldType(Val, "of")) Val {
        return .{ .kind = kind, .of = of };
    }

    pub fn clone(val: *const Val, ctx: *Store.Ctx) Val {
        var result: Val = undefined;
        wasmtime_val_clone(ctx, val, &result);
        return result;
    }

    pub fn unroot(val: *Val, ctx: *Store.Ctx) void {
        wasmtime_val_unroot(ctx, val);
    }

    pub fn equals(lhs: *const Val, rhs: *const Val) bool {
        if (lhs.kind != rhs.kind) return false;
        return switch (lhs.kind) {
            inline .i32, .i64, .f32, .f64 => |kind| {
                const lhs_field = @field(lhs.of, @tagName(kind));
                const rhs_field = @field(rhs.of, @tagName(kind));
                return lhs_field == rhs_field;
            },
            inline .anyref, .externref, .funcref => |kind| {
                const lhs_field = &@field(lhs.of, @tagName(kind));
                const rhs_field = &@field(rhs.of, @tagName(kind));
                return lhs_field.equals(rhs_field);
            },
            .v128 => std.mem.eql(u8, &lhs.of.v128, &rhs.of.v128),
            else => lhs.of.ptr == rhs.of.ptr,
        };
    }

    pub const Raw = extern union {
        pub const AnyRef = enum(u32) { _ };
        pub const ExternRef = enum(u32) { _ };

        pub const FuncRef = enum(usize) {
            null = 0,
            _,

            pub fn into(raw: FuncRef, ctx: *Store.Ctx) ?Func {
                if (raw == .null) return null;
                var result: Func = .null;
                wasmtime_func_from_raw(ctx, raw, &result);
                return result;
            }
        };

        i32: i32,
        i64: i64,
        f32: f32,
        f64: f64,
        v128: V128,
        anyref: Raw.AnyRef,
        externref: Raw.ExternRef,
        funcref: Raw.FuncRef,

        comptime {
            assert(@alignOf(@This()) == 8);
            assert(@sizeOf(@This()) == 16);
        }
    };

    pub const AnyRef = extern struct {
        store_id: Store.Id,
        __private1: u32,
        __private2: u32,

        pub const @"null": AnyRef = .{
            .store_id = .null,
            .__private1 = undefined,
            .__private2 = undefined,
        };

        pub fn fromRaw(ctx: *Store.Ctx, raw: Raw.AnyRef) AnyRef {
            var result: AnyRef = .null;
            wasmtime_anyref_from_raw(ctx, raw, &result);
            return result;
        }

        pub fn fromI31(ctx: *Store.Ctx, val: i31) AnyRef {
            var result: AnyRef = .null;
            wasmtime_anyref_from_i31(ctx, .{ .i31 = val }, &result);
            return result;
        }

        pub fn toRaw(ref: *const AnyRef, ctx: *Store.Ctx) Raw.AnyRef {
            return wasmtime_anyref_to_raw(ctx, ref);
        }

        pub fn getI31(ref: *const AnyRef, ctx: *Store.Ctx) ?i31 {
            var result: packed struct {
                _ignore: u1,
                i31: i31,
            } = undefined;
            if (wasmtime_anyref_i31_get_u(ctx, ref, @ptrCast(&result)))
                return result.i31;
            return null;
        }

        pub fn equals(lhs: *const AnyRef, rhs: *const AnyRef) bool {
            return lhs.store_id == rhs.store_id;
        }

        pub fn clone(ref: *const AnyRef, ctx: *Store.Ctx) AnyRef {
            var result: AnyRef = .null;
            wasmtime_anyref_clone(ctx, ref, &result);
            return result;
        }

        pub fn unroot(ref: *AnyRef, ctx: *Store.Ctx) void {
            wasmtime_anyref_unroot(ctx, ref);
        }
    };

    pub const ExternRef = extern struct {
        store_id: Store.Id,
        __private1: u32,
        __private2: u32,

        pub const @"null": ExternRef = .{
            .store_id = .null,
            .__private1 = undefined,
            .__private2 = undefined,
        };

        pub fn fromRaw(ctx: *Store.Ctx, raw: Raw.ExternRef) ExternRef {
            var result: ExternRef = .null;
            wasmtime_externref_from_raw(ctx, raw, &result);
            return result;
        }

        pub fn toRaw(ref: *const ExternRef, ctx: *Store.Ctx) Raw.ExternRef {
            return wasmtime_externref_to_raw(ctx, ref);
        }

        pub fn equals(lhs: *const ExternRef, rhs: *const ExternRef) bool {
            return lhs.store_id == rhs.store_id;
        }

        pub fn clone(ref: *const ExternRef, ctx: *Store.Ctx) ExternRef {
            var result: ExternRef = .null;
            wasmtime_externref_clone(ctx, ref, &result);
            return result;
        }

        pub fn getData(ref: *const ExternRef, ctx: *Store.Ctx) ?*anyopaque {
            return wasmtime_externref_data(ctx, ref);
        }

        pub fn unroot(ref: *ExternRef, ctx: *Store.Ctx) void {
            wasmtime_externref_unroot(ctx, ref);
        }
    };

    pub const Kind = enum(u8) {
        i32 = c.WASMTIME_I32,
        i64 = c.WASMTIME_I64,
        f32 = c.WASMTIME_F32,
        f64 = c.WASMTIME_F64,
        v128 = c.WASMTIME_V128,
        funcref = c.WASMTIME_FUNCREF,
        externref = c.WASMTIME_EXTERNREF,
        anyref = c.WASMTIME_ANYREF,
        _,
    };
};
