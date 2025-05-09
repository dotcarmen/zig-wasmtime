const wasmtime = @import("root.zig");
const Err = wasmtime.Err;
const Extern = wasmtime.Extern;
const Module = wasmtime.Module;
const Store = wasmtime.Store;
const Trap = wasmtime.Trap;

const testing = @import("std").testing;

extern fn wasmtime_instance_export_get(*Store.Ctx, *const Instance, name: [*]const u8, name_len: usize, item: *Extern) callconv(.c) bool;
extern fn wasmtime_instance_export_nth(*Store.Ctx, *const Instance, usize, name: *[*]const u8, name_len: *usize, item: *Extern) callconv(.c) bool;
extern fn wasmtime_instance_new(*Store.Ctx, *const Module, imports: [*]const Extern, nimports: usize, result: *Instance, *?*Trap) callconv(.c) ?*Err;
extern fn wasmtime_instance_pre_delete(*Instance.Pre) callconv(.c) void;
extern fn wasmtime_instance_pre_instantiate(*const Instance.Pre, *Store.Ctx, *Instance, *?*Trap) callconv(.c) ?*Err;
extern fn wasmtime_instance_pre_module(*Instance.Pre) callconv(.c) ?*Module;

pub const Instance = extern struct {
    pub const Index = enum(usize) { _ };

    store_id: Store.Id,
    index: Index,

    pub fn init(
        ctx: *Store.Ctx,
        module: *const Module,
        imports: []const Extern,
    ) wasmtime.Func.CallResult(Instance) {
        var instance: Instance = undefined;
        var trap_result: ?*Trap = null;
        return if (wasmtime_instance_new(
            ctx,
            module,
            imports.ptr,
            imports.len,
            &instance,
            &trap_result,
        )) |err|
            .{ .err = err }
        else if (trap_result) |trap|
            .{ .trap = trap }
        else
            .{ .ok = instance };
    }

    pub fn getExport(
        instance: *const Instance,
        ctx: *Store.Ctx,
        name: []const u8,
    ) ?Extern {
        var result: Extern = undefined;
        if (wasmtime_instance_export_get(
            ctx,
            instance,
            name.ptr,
            name.len,
            &result,
        )) return result;
        return null;
    }

    const NthExport = struct {
        name: []const u8,
        @"extern": Extern,
    };

    pub fn getNthExport(
        instance: *const Instance,
        ctx: *Store.Ctx,
        index: usize,
    ) ?NthExport {
        var result: NthExport = undefined;

        if (wasmtime_instance_export_nth(
            ctx,
            instance,
            index,
            &result.name.ptr,
            &result.name.len,
            &result.@"extern",
        )) return result;
        return null;
    }

    pub fn exportsIterator(
        instance: *const Instance,
        ctx: *Store.Ctx,
    ) struct {
        instance: *const Instance,
        ctx: *Store.Ctx,
        index: usize = 0,

        pub fn next(self: *@This()) ?NthExport {
            const result = self.instance
                .getNthExport(self.ctx, self.index) orelse
                return null;
            self.index += 1;
            return result;
        }
    } {
        return .{ .instance = instance, .ctx = ctx };
    }

    /// A #wasmtime_instance_t, pre-instantiation, that is ready to be
    /// instantiated.
    ///
    /// Must be deleted using #wasmtime_instance_pre_delete.
    ///
    /// For more information see the Rust documentation:
    /// https://docs.wasmtime.dev/api/wasmtime/struct.InstancePre.html
    pub const Pre = opaque {
        pub const deinit = wasmtime_instance_pre_delete;

        pub fn getModule(pre: *Pre) *Module {
            return wasmtime_instance_pre_module(pre).?;
        }

        pub fn instantiate(
            pre: *const Pre,
            ctx: *Store.Ctx,
        ) union(enum) {
            ok: Instance,
            err: *Err,
            trap: *Trap,
        } {
            var instance: Instance = undefined;
            var trap_result: ?*Trap = null;
            return if (wasmtime_instance_pre_instantiate(pre, ctx, &instance, &trap_result)) |err|
                .{ .err = err }
            else if (trap_result) |trap|
                .{ .trap = trap }
            else
                .{ .ok = instance };
        }
    };

    test "bad" {
        const engine = wasmtime.Engine.init();
        const store = wasmtime.Store.init(engine, null);
        const module = try wasmtime.Module.initWat(engine,
            \\ (module (import "" "" (func)))
        ).testFail();

        switch (Instance.init(store.ctx(), module, &.{})) {
            .err => {},
            else => return error.Expected,
        }

        const func = wasmtime.Func.init(store.ctx(), try .init(&.{try .init(.i32)}, &.{}), struct {
            fn callback(
                _: *wasmtime.Func.Caller,
                _: []const wasmtime.Val,
                _: []wasmtime.Val,
            ) ?*Trap {
                return null;
            }
        }.callback);

        switch (Instance.init(store.ctx(), module, &.{func.asExtern()})) {
            .err => {},
            else => return error.Expected,
        }
    }

    test "get func" {
        const engine = wasmtime.Engine.init();
        const store = wasmtime.Store.init(engine, null);
        const module = try wasmtime.Module.initWat(engine,
            \\ (module
            \\   (func (export "f") (nop))
            \\   (global (export "g") i32 (i32.const 0)))
        ).testFail();

        const instance = try Instance.init(store.ctx(), module, &.{})
            .okOrError();

        const f = instance
            .getExport(store.ctx(), "f").?
            .asFunc().?;

        _ = try f.call(store.ctx(), &.{}, 0)
            .okOrError();
    }
};

test Instance {
    const engine = wasmtime.Engine.init();
    const store = wasmtime.Store.init(engine, null);
    const module = try wasmtime.Module.initWat(engine,
        \\ (module
        \\   (func (export "f"))
        \\   (global (export "g") i32 (i32.const 0))
        \\   (table (export "t") 1 funcref)
        \\   (memory (export "m") 1))
    ).testFail();

    const instance = try Instance.init(store.ctx(), module, &.{})
        .okOrError();
    var exports = instance.exportsIterator(store.ctx());

    const instance_func = exports.next() orelse
        return error.Expected;
    try testing.expectEqualStrings("f", instance_func.name);
    try testing.expect(instance_func.@"extern".kind == .func);

    const functype = instance_func.@"extern".asFunc().?.getType(store.ctx());
    try testing.expectEqual(0, functype.getParams().len);
    try testing.expectEqual(0, functype.getResults().len);

    const instance_global = exports.next() orelse
        return error.Expected;
    try testing.expectEqualStrings("g", instance_global.name);
    try testing.expect(instance_global.@"extern".kind == .global);

    const globaltype = instance_global.@"extern".asGlobal().?.getType(store.ctx());
    try testing.expect(globaltype.getContent().getKind() == .i32);

    const instance_table = exports.next() orelse
        return error.Expected;
    try testing.expectEqualStrings("t", instance_table.name);
    try testing.expect(instance_table.@"extern".kind == .table);

    const tabletype = instance_table.@"extern".asTable().?.getType(store.ctx());
    try testing.expect(tabletype.getElement().getKind() == .funcref);

    const instance_memory = exports.next() orelse
        return error.ExpectedExport;
    try testing.expectEqualStrings("m", instance_memory.name);
    try testing.expect(instance_memory.@"extern".kind == .memory);

    const memorytype = instance_memory.@"extern".asMemory().?.getType(store.ctx());
    try testing.expectEqual(1, memorytype.getMinimum());

    try testing.expect(exports.next() == null);
}
