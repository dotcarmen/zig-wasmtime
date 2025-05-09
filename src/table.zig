const Err = @import("error.zig").Err;
const Extern = @import("extern.zig").Extern;
const Store = @import("store.zig").Store;
const Val = @import("val.zig").Val;
const wasm = @import("wasm.zig");

extern fn wasmtime_table_get(*Store.Ctx, *const Table, index: Table.Index, result: *Val) callconv(.c) bool;
extern fn wasmtime_table_grow(*Store.Ctx, *const Table, delta: u64, init: *const Val, prev_size: *u64) callconv(.c) ?*Err;
extern fn wasmtime_table_new(*Store.Ctx, *const Table.Type, init: *const Val, result: *Table) callconv(.c) ?*Err;
extern fn wasmtime_table_set(*Store.Ctx, *const Table, index: Table.Index, value: *const Val) callconv(.c) ?*Err;
extern fn wasmtime_table_size(*const Store.Ctx, *const Table) callconv(.c) u64;
extern fn wasmtime_table_type(*const Store.Ctx, *const Table) callconv(.c) ?*Table.Type;

pub const Table = extern struct {
    pub const Index = enum(u64) { _ };
    pub const Type = wasm.Table.Type;

    store_id: Store.Id,
    __private: usize,

    pub const @"null": Table = .{
        .store_id = .null,
        .__private = undefined,
    };

    pub fn init(
        ctx: *Store.Ctx,
        @"type": *const Type,
        initval: *const Val,
    ) Err.Result(Table) {
        var result: Table = .null;
        if (wasmtime_table_new(ctx, @"type", initval, &result)) |err|
            return .{ .err = err };
        return .{ .ok = result };
    }

    pub fn getType(table: *const Table, ctx: *Store.Ctx) *Type {
        return wasmtime_table_type(ctx, table).?;
    }

    pub fn get(
        table: *const Table,
        ctx: *Store.Ctx,
        index: Index,
    ) ?Val {
        var result: Val = undefined;
        if (wasmtime_table_get(ctx, table, index, &result))
            return result;
        return null;
    }

    pub fn set(
        table: *const Table,
        ctx: *Store.Ctx,
        index: Index,
        val: *const Val,
    ) ?*Err {
        return wasmtime_table_set(ctx, table, index, val);
    }

    pub fn size(table: *const Table, ctx: *Store.Ctx) u64 {
        return wasmtime_table_size(ctx, table);
    }

    pub fn grow(
        table: *const Table,
        ctx: *Store.Ctx,
        delta: u64,
        initval: *const Val,
    ) Err.Result(u64) {
        var prev_size: u64 = undefined;
        if (wasmtime_table_grow(
            ctx,
            table,
            delta,
            initval,
            &prev_size,
        )) |err|
            return .{ .err = err };
        return .{ .ok = prev_size };
    }

    pub fn iterator(table: *const Table, ctx: *Store.Ctx) Iterator {
        const len = table.size(ctx);
        return .{
            .table = table.*,
            .ctx = ctx,
            .len = len,
        };
    }

    pub const Iterator = struct {
        ctx: *Store.Ctx,
        table: Table,
        len: u64,
        index: Index = @enumFromInt(0),

        pub fn next(self: *Iterator) ?Val {
            const idx = @intFromEnum(self.index);
            if (idx >= self.len) return null;
            defer self.index = @enumFromInt(1 + idx);
            return self.table.get(self.ctx, self.index).?;
        }
    };
};
