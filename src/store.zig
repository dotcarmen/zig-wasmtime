const c = @cImport(@cInclude("wasmtime/store.h"));
const Engine = @import("engine.zig").Engine;
const Err = @import("error.zig").Err;
const Feature = @import("conf.zig").Feature;
const WasiConfig = @import("wasi.zig").Config;

const std = @import("std");
const assert = std.debug.assert;

extern fn wasmtime_store_new(*Engine, ?*anyopaque, ?*const fn (*anyopaque) callconv(.c) void) ?*Store;
extern fn wasmtime_store_delete(*Store) callconv(.c) void;
extern fn wasmtime_store_context(*Store) callconv(.c) *Context;
extern fn wasmtime_store_limiter(*Store, u64, u64, u64, u64, u64) callconv(.c) void;
extern fn wasmtime_context_epoch_deadline_async_yield_and_update(*Context, delta: u64) callconv(.c) ?*Err;
extern fn wasmtime_context_fuel_async_yield_interval(*Context, interval: u64) callconv(.c) ?*Err;
extern fn wasmtime_context_gc(*Context) callconv(.c) void;
extern fn wasmtime_context_get_data(*const Context) callconv(.c) *anyopaque;
extern fn wasmtime_context_get_fuel(*const Context, *u64) callconv(.c) ?*Err;
extern fn wasmtime_context_set_data(*Context, *anyopaque) callconv(.c) void;
extern fn wasmtime_context_set_epoch_deadline(*Context, u64) callconv(.c) void;
extern fn wasmtime_context_set_fuel(*Context, u64) callconv(.c) ?*Err;
extern fn wasmtime_context_set_wasi(*Context, *WasiConfig) callconv(.c) ?*Err;

extern fn wasmtime_context_epoch_deadline_callback(
    *Context,
    *const fn (
        context: *Context,
        data: ?*anyopaque,
        epoch_deadline_delta: *u64,
        update_kind: *Context.UpdateDeadlineKind,
    ) callconv(.c) ?*Err,
    ?*anyopaque,
    *const fn (*anyopaque) callconv(.c) void,
) callconv(.c) void;

pub const Store = opaque {
    pub const Ctx = Context;
    pub const Id = enum(u64) {
        null = 0,
        _,
    };

    pub const deinit = wasmtime_store_delete;

    pub fn init(engine: *Engine, data: anytype) void {
        const Env = switch (@typeInfo(@TypeOf(data))) {
            .null => return wasmtime_store_new(engine, null, null).?,
            .optional, .pointer => |info| info.child,
            else => @compileError("unsupported type: " ++ @typeName(@TypeOf(data))),
        };

        const finalize: *const fn (*Env) callconv(.c) void =
            if (@hasDecl(Env, "finalize"))
                Env.finalize
            else
                null;

        return wasmtime_store_new(engine, data, finalize).?;
    }

    pub const getContext = wasmtime_store_context;
    pub const getContextConst: *const fn (*const Store) callconv(.c) *const Ctx =
        @ptrCast(&wasmtime_store_context);

    /// \brief Provides limits for a store. Used by hosts to limit resource
    /// consumption of instances. Use negative value to keep the default value
    /// for the limit.
    ///
    /// \param store store where the limits should be set.
    /// \param memory_size the maximum number of bytes a linear memory can grow to.
    /// Growing a linear memory beyond this limit will fail. By default,
    /// linear memory will not be limited.
    /// \param table_elements the maximum number of elements in a table.
    /// Growing a table beyond this limit will fail. By default, table elements
    /// will not be limited.
    /// \param instances the maximum number of instances that can be created
    /// for a Store. Module instantiation will fail if this limit is exceeded.
    /// This value defaults to 10,000.
    /// \param tables the maximum number of tables that can be created for a Store.
    /// Module instantiation will fail if this limit is exceeded. This value
    /// defaults to 10,000.
    /// \param memories the maximum number of linear memories that can be created
    /// for a Store. Instantiation will fail with an error if this limit is exceeded.
    /// This value defaults to 10,000.
    ///
    /// Use any negative value for the parameters that should be kept on
    /// the default values.
    ///
    /// Note that the limits are only used to limit the creation/growth of
    /// resources in the future, this does not retroactively attempt to apply
    /// limits to the store.
    pub fn limit(store: *Store, opts: struct {
        memory_size: u64 = std.math.maxInt(u64),
        table_elements: u64 = std.math.maxInt(u64),
        instances: u64 = 10_000,
        tables: u64 = 10_000,
        memories: u64 = 10_000,
    }) void {
        wasmtime_store_limiter(
            store,
            opts.memory_size,
            opts.table_elements,
            opts.instances,
            opts.tables,
            opts.memories,
        );
    }
};

const Context = opaque {
    pub const UpdateDeadlineKind = enum(u8) {
        @"continue" = c.WASMTIME_UPDATE_DEADLINE_CONTINUE,
        yield = c.WASMTIME_UPDATE_DEADLINE_YIELD,
        _,
    };

    pub const getData = wasmtime_context_get_data;
    pub const setData = wasmtime_context_set_data;
    pub const collectGarbage = wasmtime_context_gc;
    pub const setFuel = wasmtime_context_set_fuel;
    pub const setEpochDeadline = wasmtime_context_set_epoch_deadline;

    pub fn setAsyncFuelYieldInterval(context: *Context, interval: u64) ?*Err {
        comptime assert(Feature.@"async".isEnabled());
        return wasmtime_context_fuel_async_yield_interval(context, interval);
    }

    pub fn setAsyncYieldAndUpdateEpochDeadline(context: *Context, delta: u64) ?*Err {
        comptime assert(Feature.@"async".isEnabled());
        return wasmtime_context_epoch_deadline_async_yield_and_update(context, delta);
    }

    pub fn setWasi(context: *Context, wasi_config: *WasiConfig) ?*Err {
        comptime assert(Feature.wasi.isEnabled());
        return wasmtime_context_set_wasi(context, wasi_config);
    }

    pub fn getFuel(context: *const Context) Err.Result(u64) {
        var result: u64 = undefined;
        if (wasmtime_context_get_fuel(context, &result)) |err|
            return .{ .err = err };
        return .{ .ok = result };
    }

    pub fn epochDeadlineCallback(
        context: *Context,
        comptime Handler: type,
        data: ?*Handler,
    ) ?*Err {
        const callback: fn (*Context, ?*Handler, *u64, *UpdateDeadlineKind) ?*Err =
            Handler.onEpochDeadlineExceeded;

        const Wrapper = opaque {
            fn onEpochDeadlineExceeded(
                ctx: *Context,
                d: ?*Handler,
                epoch_deadline_delta: *u64,
                update_kind: *UpdateDeadlineKind,
            ) callconv(.c) ?*Err {
                return callback(ctx, d, epoch_deadline_delta, update_kind);
            }

            fn finalize(d: *Handler) callconv(.c) void {
                d.finalize();
            }
        };

        return wasmtime_context_epoch_deadline_callback(
            context,
            Wrapper.onEpochDeadlineExceeded,
            data,
            if (@hasDecl(Handler, "finalize"))
                Wrapper.finalize
            else
                null,
        );
    }
};
