const Err = @import("error.zig").Err;
const Extern = @import("extern.zig").Extern;
const Module = @import("module.zig").Module;
const Store = @import("store.zig").Store;
const Trap = @import("trap.zig").Trap;

extern fn wasmtime_instance_export_get(*Store.Ctx, *const Instance, name: [*]const u8, name_len: usize, item: *Extern) callconv(.c) bool;
extern fn wasmtime_instance_export_nth(*Store.Ctx, *const Instance, Instance.Index, name: *[*]const u8, name_len: *usize, item: *Extern) callconv(.c) bool;
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
    ) union(enum) {
        ok: Instance,
        err: *Err,
        trap: *Trap,
    } {
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

    pub fn getExportByName(
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

    pub fn getExport(
        instance: *const Instance,
        ctx: *Store.Ctx,
        index: Index,
    ) ?struct {
        name: []const u8,
        item: Extern,
    } {
        var name: [*]const u8 = undefined;
        var name_len: usize = undefined;
        var item: Extern = undefined;
        if (wasmtime_instance_export_nth(
            ctx,
            instance,
            index,
            &name,
            &name_len,
            &item,
        )) return .{ .name = name[0..name_len], .item = item };
        return null;
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
};
