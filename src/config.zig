const c = @cImport(@cInclude("wasmtime/config.h"));

const Err = @import("error.zig").Err;
const Feature = @import("conf.zig").Feature;
const wasm = @import("wasm.zig");

const std = @import("std");
const assert = std.debug.assert;
const Tag = std.meta.Tag;
const TagPayload = std.meta.TagPayload;

extern fn wasm_config_new() callconv(.c) ?*Config;
extern fn wasm_config_delete(*Config) callconv(.c) void;
extern fn wasmtime_config_cache_config_load(*Config, ?[*:0]const u8) callconv(.c) ?*Err;
extern fn wasmtime_config_cranelift_flag_enable(*Config, [*:0]const u8) callconv(.c) void;
extern fn wasmtime_config_cranelift_flag_set(*Config, [*:0]const u8, [*:0]const u8) callconv(.c) void;
extern fn wasmtime_config_host_memory_creator_set(*Config, *Config.MemoryCreator) callconv(.c) void;
extern fn wasmtime_config_target_set(*Config, [*:0]const u8) callconv(.c) ?*Err;
extern fn wasmtime_pooling_allocation_config_new() callconv(.c) ?*Config.PoolingAllocationConfig;
extern fn wasmtime_pooling_allocation_config_delete(*Config.PoolingAllocationConfig) callconv(.c) void;

pub const Config = opaque {
    pub const OptimizeLevel = enum(u8) {
        none = c.WASMTIME_OPT_LEVEL_NONE,
        speed = c.WASMTIME_OPT_LEVEL_SPEED,
        speed_and_size = c.WASMTIME_OPT_LEVEL_SPEED_AND_SIZE,
        _,
    };

    pub const ProfilingStrategy = enum(u8) {
        none = c.WASMTIME_PROFILING_STRATEGY_NONE,
        jitdump = c.WASMTIME_PROFILING_STRATEGY_JITDUMP,
        vtune = c.WASMTIME_PROFILING_STRATEGY_VTUNE,
        perfmap = c.WASMTIME_PROFILING_STRATEGY_PERFMAP,
    };

    pub const Strategy = enum(u8) {
        auto = c.WASMTIME_STRATEGY_AUTO,
        cranelift = c.WASMTIME_STRATEGY_CRANELIFT,
        _,
    };

    pub const deinit = wasm_config_delete;

    pub fn init() *Config {
        return wasm_config_new().?;
    }

    /// Configures the target triple that this configuration will produce
    /// machine code for.
    ///
    /// This option defaults to the native host. Calling this method will
    /// additionally disable inference of the native features of the host (e.g.
    /// detection of SSE4.2 on x86_64 hosts). Native features can be reenabled with
    /// the `cranelift_flag_{set,enable}` properties.
    ///
    /// For more information see the Rust documentation at
    /// https://docs.wasmtime.dev/api/wasmtime/struct.Config.html#method.config
    pub const setTarget = wasmtime_config_target_set;

    /// Sets a custom memory creator.
    ///
    /// Custom memory creators are used when creating host Memory objects or when
    /// creating instance linear memories for the on-demand instance allocation
    /// strategy.
    ///
    /// The config does **not** take ownership of the #wasmtime_memory_creator_t
    /// passed in, but instead copies all the values in the struct.
    ///
    /// For more information see the Rust documentation at
    /// https://docs.wasmtime.dev/api/wasmtime/struct.Config.html#method.with_host_memory
    pub const setMemoryCreator = wasmtime_config_host_memory_creator_set;

    /// Enables a target-specific flag in Cranelift.
    ///
    /// This can be used, for example, to enable SSE4.2 on x86_64 hosts. Settings can
    /// be explored with `wasmtime settings` on the CLI.
    ///
    /// For more information see the Rust documentation at
    /// https://docs.wasmtime.dev/api/wasmtime/struct.Config.html#method.cranelift_flag_enable
    pub fn enableCraneliftFlag(config: *Config, flag: [*:0]const u8) void {
        comptime assert(Feature.compiler.isEnabled());
        wasmtime_config_cranelift_flag_enable(config, flag);
    }

    /// Sets a target-specific flag in Cranelift to the specified value.
    ///
    /// This can be used, for example, to enable SSE4.2 on x86_64 hosts. Settings can
    /// be explored with `wasmtime settings` on the CLI.
    ///
    /// For more information see the Rust documentation at
    /// https://docs.wasmtime.dev/api/wasmtime/struct.Config.html#method.cranelift_flag_set
    pub fn setCraneliftFlag(
        config: *Config,
        flag: [*:0]const u8,
        value: [*:0]const u8,
    ) void {
        comptime assert(Feature.compiler.isEnabled());
        wasmtime_config_cranelift_flag_set(config, flag, value);
    }

    /// Enables Wasmtime's cache and loads configuration from the specified
    /// path.
    ///
    /// By default the Wasmtime compilation cache is disabled. The configuration path
    /// here can be `NULL` to use the default settings, and otherwise the argument
    /// here must be a file on the filesystem with TOML configuration -
    /// https://bytecodealliance.github.io/wasmtime/cli-cache.html.
    ///
    /// An error is returned if the cache configuration could not be loaded or if the
    /// cache could not be enabled.
    pub fn loadCacheConfig(
        config: *Config,
        location: union(enum) {
            default,
            path: [*:0]const u8,
        },
    ) ?*Err {
        comptime assert(Feature.cache.isEnabled());
        return wasmtime_config_cache_config_load(
            config,
            switch (location) {
                .path => |path| path,
                .default => null,
            },
        );
    }

    pub fn setProp(
        config: *Config,
        comptime prop: Tag(Prop),
        value: TagPayload(Prop, prop),
    ) void {
        comptime switch (prop) {
            .wasm_threads => assert(Feature.threads.isEnabled()),
            .parallel_compilation => assert(Feature.parallel_compilation.isEnabled()),
            .pooling_allocation_strategy => assert(Feature.cache.isEnabled()),
            .strategy,
            .cranelift_debug_verifier,
            .cranelift_nan_canonicalization,
            .cranelift_opt_level,
            => assert(Feature.compiler.isEnabled()),
            .async_support,
            .async_stack_size,
            .host_stack_creator,
            => assert(Feature.@"async".isEnabled()),
            else => {},
        };

        const imp = @extern(
            *const fn (*Config, TagPayload(Prop, prop)) callconv(.c) void,
            .{
                .name = if (prop == .pooling_allocation_strategy)
                    "wasmtime_pooling_allocation_strategy_set"
                else
                    "wasmtime_config_" ++ @tagName(prop) ++ "_set",
            },
        );

        imp(config, value);
    }

    pub const LinearMemory = extern struct {
        env: ?*anyopaque,

        /// Return the data from a LinearMemory instance.
        ///
        /// The size in bytes as well as the maximum number of bytes that can be
        /// allocated should be returned as well.
        ///
        /// For more information about see the Rust documentation at
        /// https://docs.wasmtime.dev/api/wasmtime/trait.LinearMemory.html
        get_memory: ?*const fn (
            env: ?*anyopaque,
            byte_size: *usize,
            byte_capacity: *usize,
        ) callconv(.c) [*]u8,

        /// Grow the memory to the `new_size` in bytes.
        ///
        /// For more information about the parameters see the Rust documentation at
        /// https://docs.wasmtime.dev/api/wasmtime/trait.LinearMemory.html#tymethod.grow_to
        grow_memory: ?*const fn (
            env: ?*anyopaque,
            new_size: usize,
        ) callconv(.c) ?*Err,

        finalizer: ?wasm.Finalizer.Func,
    };

    /// A representation of custom memory creator and methods for an instance of
    /// LinearMemory.
    ///
    /// For more information see the Rust documentation at
    /// https://docs.wasmtime.dev/api/wasmtime/trait.MemoryCreator.html
    pub const MemoryCreator = extern struct {
        env: ?*anyopaque,

        /// A callback to create a new LinearMemory from the specified parameters.
        ///
        /// The result should be written to `memory_ret` and wasmtime will own the values
        /// written into that struct.
        ///
        /// This callback must be thread-safe.
        ///
        /// For more information about the parameters see the Rust documentation at
        /// https://docs.wasmtime.dev/api/wasmtime/trait.MemoryCreator.html#tymethod.new_memory
        new_memory: ?*const fn (
            env: ?*anyopaque,
            ty: ?*const wasm.Memory.Type,
            minimum: usize,
            maximum: usize,
            reserved_size_in_bytes: usize,
            guard_size_in_bytes: usize,
            memory_ret: *LinearMemory,
        ) callconv(.c) void,

        finalizer: ?wasm.Finalizer.Func,
    };

    /// A representation of custom stack creator.
    ///
    /// For more information see the Rust documentation at
    /// https://docs.wasmtime.dev/api/wasmtime/trait.StackCreator.html
    pub const StackCreator = extern struct {
        pub const NewStackMemoryCallback = fn (
            env: ?*anyopaque,
            size: usize,
            zeroed: usize,
            stack_ret: *void,
        ) callconv(.c) ?*Err;

        env: ?*anyopaque,
        new_stack: *const NewStackMemoryCallback,
        finalizer: ?wasm.Finalizer.Func,
    };

    pub const StackMemory = extern struct {
        pub const GetStackMemoryCallback = fn (
            env: ?*anyopaque,
            out_len: *usize,
        ) callconv(.c) ?[*]u8;

        env: ?*anyopaque,
        get_stack_memory: *const void,
        finalizer: ?wasm.Finalizer.Func,
    };

    pub const Prop = union(enum) {
        /// Configures whether DWARF debug information is constructed at runtime
        /// to describe JIT code.
        ///
        /// This setting is `false` by default. When enabled it will attempt to inform
        /// native debuggers about DWARF debugging information for JIT code to more
        /// easily debug compiled WebAssembly via native debuggers. This can also
        /// sometimes improve the quality of output when profiling is enabled.
        debug_info: bool,

        /// Whether or not fuel is enabled for generated code.
        ///
        /// This setting is `false` by default. When enabled it will enable fuel counting
        /// meaning that fuel will be consumed every time a wasm instruction is executed,
        /// and trap when reaching zero.
        consume_fuel: bool,

        /// Whether or not epoch-based interruption is enabled for generated code.
        ///
        /// This setting is `false` by default. When enabled wasm code will check the
        /// current epoch periodically and abort if the current epoch is beyond a
        /// store-configured limit.
        ///
        /// Note that when this setting is enabled all stores will immediately trap and
        /// need to have their epoch deadline otherwise configured with
        /// #wasmtime_context_set_epoch_deadline.
        ///
        /// Note that the current epoch is engine-local and can be incremented with
        /// #wasmtime_engine_increment_epoch.
        epoch_interruption: bool,

        /// Configures the maximum stack size, in bytes, that JIT code can use.
        ///
        /// This setting is 2MB by default. Configuring this setting will limit the
        /// amount of native stack space that JIT code can use while it is executing. If
        /// you're hitting stack overflow you can try making this setting larger, or if
        /// you'd like to limit wasm programs to less stack you can also configure this.
        ///
        /// Note that this setting is not interpreted with 100% precision. Additionally
        /// the amount of stack space that wasm takes is always relative to the first
        /// invocation of wasm on the stack, so recursive calls with host frames in the
        /// middle will all need to fit within this setting.
        max_wasm_stack: usize,

        /// Configures whether the WebAssembly threading proposal is enabled.
        ///
        /// This setting is `false` by default.
        ///
        /// Note that threads are largely unimplemented in Wasmtime at this time.
        wasm_threads: bool,

        /// Configures whether the WebAssembly tail call proposal is enabled.
        ///
        /// This setting is `false` by default.
        wasm_tail_call: bool,

        /// Configures whether the WebAssembly reference types proposal is
        /// enabled.
        ///
        /// This setting is `false` by default.
        wasm_reference_types: bool,

        /// Configures whether the WebAssembly typed function reference types
        /// proposal is enabled.
        ///
        /// This setting is `false` by default.
        wasm_function_references: bool,

        /// Configures whether the WebAssembly GC proposal is enabled.
        ///
        /// This setting is `false` by default.
        wasm_gc: bool,

        /// Configures whether the WebAssembly SIMD proposal is
        /// enabled.
        ///
        /// This setting is `false` by default.
        wasm_simd: bool,

        /// Configures whether the WebAssembly relaxed SIMD proposal is
        /// enabled.
        ///
        /// This setting is `false` by default.
        wasm_relaxed_simd: bool,

        /// Configures whether the WebAssembly relaxed SIMD proposal is
        /// in deterministic mode.
        ///
        /// This setting is `false` by default.
        wasm_relaxed_simd_deterministic: bool,

        /// Configures whether the WebAssembly bulk memory proposal is
        /// enabled.
        ///
        /// This setting is `false` by default.
        wasm_bulk_memory: bool,

        /// Configures whether the WebAssembly multi value proposal is
        /// enabled.
        ///
        /// This setting is `true` by default.
        wasm_multi_value: bool,

        /// Configures whether the WebAssembly multi-memory proposal is
        /// enabled.
        ///
        /// This setting is `false` by default.
        wasm_multi_memory: bool,

        /// Configures whether the WebAssembly memory64 proposal is
        /// enabled.
        ///
        /// This setting is `false` by default.
        wasm_memory64: bool,

        /// Configures whether the WebAssembly wide-arithmetic proposal is
        /// enabled.
        ///
        /// This setting is `false` by default.
        wasm_wide_arithmetic: bool,

        /// Configures how JIT code will be compiled.
        ///
        /// This setting is #WASMTIME_STRATEGY_AUTO by default.
        strategy: Strategy,

        /// Configure whether wasmtime should compile a module using multiple
        /// threads.
        ///
        /// For more information see the Rust documentation at
        /// https://docs.wasmtime.dev/api/wasmtime/struct.Config.html#method.parallel_compilation.
        parallel_compilation: bool,

        /// Configures whether Cranelift's debug verifier is enabled.
        ///
        /// This setting in `false` by default.
        ///
        /// When cranelift is used for compilation this enables expensive debug checks
        /// within Cranelift itself to verify it's correct.
        cranelift_debug_verifier: bool,

        /// Configures whether Cranelift should perform a NaN-canonicalization
        /// pass.
        ///
        /// When Cranelift is used as a code generation backend this will configure
        /// it to replace NaNs with a single canonical value. This is useful for users
        /// requiring entirely deterministic WebAssembly computation.
        ///
        /// This is not required by the WebAssembly spec, so it is not enabled by
        /// default.
        ///
        /// The default value for this is `false`
        cranelift_nan_canonicalization: bool,

        /// Configures Cranelift's optimization level for JIT code.
        ///
        /// This setting in #WASMTIME_OPT_LEVEL_SPEED by default.
        cranelift_opt_level: OptimizeLevel,

        /// Configures the profiling strategy used for JIT code.
        ///
        /// This setting in #WASMTIME_PROFILING_STRATEGY_NONE by default.
        profiler: ProfilingStrategy,

        /// Configures whether `memory_reservation` is the maximal size of linear
        /// memory.
        ///
        /// This setting is `false` by default.
        ///
        /// For more information see the Rust documentation at
        /// https://bytecodealliance.github.io/wasmtime/api/wasmtime/struct.Config.html#method.memory_may_move.
        memory_may_move: bool,

        /// Configures the size, in bytes, of initial memory reservation size for
        /// linear memories.
        ///
        /// For more information see the Rust documentation at
        /// https://bytecodealliance.github.io/wasmtime/api/wasmtime/struct.Config.html#method.memory_reservation.
        memory_reservation: u64,

        /// Configures the guard region size for linear memory.
        ///
        /// For more information see the Rust documentation at
        /// https://bytecodealliance.github.io/wasmtime/api/wasmtime/struct.Config.html#method.memory_guard_size.
        memory_guard_size: u64,

        /// Configures the size, in bytes, of the extra virtual memory space
        /// reserved for memories to grow into after being relocated.
        ///
        /// For more information see the Rust documentation at
        /// https://docs.wasmtime.dev/api/wasmtime/struct.Config.html#method.memory_reservation_for_growth
        memory_reservation_for_growth: u64,

        /// Configures whether to generate native unwind information (e.g.
        /// .eh_frame on Linux).
        ///
        /// This option defaults to true.
        ///
        /// For more information see the Rust documentation at
        /// https://docs.wasmtime.dev/api/wasmtime/struct.Config.html#method.native_unwind_info
        native_unwind_info: bool,

        /// Configures whether, when on macOS, Mach ports are used for exception
        /// handling instead of traditional Unix-based signal handling.
        ///
        /// This option defaults to true, using Mach ports by default.
        ///
        /// For more information see the Rust documentation at
        /// https://docs.wasmtime.dev/api/wasmtime/struct.Config.html#method.macos_use_mach_ports
        macos_use_mach_ports: bool,

        /// Configures whether copy-on-write memory-mapped data is used to
        /// initialize a linear memory.
        ///
        /// Initializing linear memory via a copy-on-write mapping can drastically
        /// improve instantiation costs of a WebAssembly module because copying memory is
        /// deferred. Additionally if a page of memory is only ever read from WebAssembly
        /// and never written too then the same underlying page of data will be reused
        /// between all instantiations of a module meaning that if a module is
        /// instantiated many times this can lower the overall memory required needed to
        /// run that module.
        ///
        /// This option defaults to true.
        ///
        /// For more information see the Rust documentation at
        /// https://docs.wasmtime.dev/api/wasmtime/struct.Config.html#method.memory_init_cow
        memory_init_cow: bool,

        /// Sets the Wasmtime allocation strategy to use the pooling allocator. It
        /// does not take ownership of the pooling allocation configuration object, which
        /// must be deleted with a call to wasmtime_pooling_allocation_config_delete.
        ///
        /// For more information see the Rust documentation at
        /// https://docs.wasmtime.dev/api/wasmtime/struct.Config.html#method.allocation_strategy.
        pooling_allocation_strategy: *const PoolingAllocationConfig,

        /// Whether or not to enable support for asynchronous functions in
        /// Wasmtime.
        ///
        /// When enabled, the config can optionally define host functions with async.
        /// Instances created and functions called with this Config must be called
        /// through their asynchronous APIs, however. For example using
        /// wasmtime_func_call will panic when used with this config.
        ///
        /// For more information see the Rust documentation at
        /// https://docs.wasmtime.dev/api/wasmtime/struct.Config.html#method.async_support
        async_support: bool,

        /// Configures the size of the stacks used for asynchronous execution.
        ///
        /// This setting configures the size of the stacks that are allocated for
        /// asynchronous execution.
        ///
        /// The value cannot be less than max_wasm_stack.
        ///
        /// The amount of stack space guaranteed for host functions is async_stack_size -
        /// max_wasm_stack, so take care not to set these two values close to one
        /// another; doing so may cause host functions to overflow the stack and abort
        /// the process.
        ///
        /// By default this option is 2 MiB.
        ///
        /// For more information see the Rust documentation at
        /// https://docs.wasmtime.dev/api/wasmtime/struct.Config.html#method.async_stack_size
        async_stack_size: u64,

        /// Sets a custom stack creator.
        ///
        /// Custom memory creators are used when creating creating async instance stacks
        /// for the on-demand instance allocation strategy.
        ///
        /// The config does **not** take ownership of the #wasmtime_stack_creator_t
        /// passed in, but instead copies all the values in the struct.
        ///
        /// For more information see the Rust documentation at
        /// https://docs.wasmtime.dev/api/wasmtime/struct.Config.html#method.with_host_stack
        host_stack_creator: *StackCreator,
    };

    pub const PoolingAllocationConfig = opaque {
        pub const deinit = wasmtime_pooling_allocation_config_delete;

        pub fn init() *PoolingAllocationConfig {
            return wasmtime_pooling_allocation_config_new().?;
        }

        pub fn setProp(
            config: *PoolingAllocationConfig,
            comptime prop: Tag(PoolingAllocationConfig.Prop),
            value: TagPayload(PoolingAllocationConfig.Prop, prop),
        ) void {
            comptime switch (prop) {
                .async_stack_keep_resident,
                .total_stacks,
                => assert(Feature.@"async".isEnabled()),
                else => {},
            };

            const imp = @extern(
                *const fn (*Config, @TypeOf(value)) callconv(.c) void,
                .{ .name = "wasmtime_pooling_allocation_config_" ++ @tagName(prop) ++ "_set" },
            );
            imp(config, value);
        }

        pub const Prop = union(enum) {
            /// Configures the maximum number of “unused warm slots” to retain in the
            /// pooling allocator.
            ///
            /// For more information see the Rust documentation at
            /// https://docs.wasmtime.dev/api/wasmtime/struct.PoolingAllocationConfig.html#method.max_unused_warm_slots.
            max_unused_warm_slots: u32,

            /// The target number of decommits to do per batch.
            ///
            /// For more information see the Rust documentation at
            /// https://docs.wasmtime.dev/api/wasmtime/struct.PoolingAllocationConfig.html#method.decommit_batch_size.
            decommit_batch_size: usize,

            /// How much memory, in bytes, to keep resident for async stacks allocated
            /// with the pooling allocator.
            ///
            /// For more information see the Rust documentation at
            /// https://docs.wasmtime.dev/api/wasmtime/struct.PoolingAllocationConfig.html#method.async_stack_keep_resident.
            async_stack_keep_resident: usize,

            /// How much memory, in bytes, to keep resident for each linear memory
            /// after deallocation.
            ///
            /// For more information see the Rust documentation at
            /// https://docs.wasmtime.dev/api/wasmtime/struct.PoolingAllocationConfig.html#method.linear_memory_keep_resident.
            linear_memory_keep_resident: usize,

            /// How much memory, in bytes, to keep resident for each table after
            /// deallocation.
            ///
            /// For more information see the Rust documentation at
            /// https://docs.wasmtime.dev/api/wasmtime/struct.PoolingAllocationConfig.html#method.table_keep_resident.
            table_keep_resident: usize,

            /// The maximum number of concurrent component instances supported
            /// (default is 1000).
            ///
            /// For more information see the Rust documentation at
            /// https://docs.wasmtime.dev/api/wasmtime/struct.PoolingAllocationConfig.html#method.total_component_instances.
            total_component_instances: u32,

            /// The maximum size, in bytes, allocated for a component instance’s
            /// VMComponentContext metadata.
            ///
            /// For more information see the Rust documentation at
            /// https://docs.wasmtime.dev/api/wasmtime/struct.PoolingAllocationConfig.html#method.max_component_instance_size.
            max_component_instance_size: usize,

            /// The maximum number of core instances a single component may contain
            /// (default is unlimited).
            ///
            /// For more information see the Rust documentation at
            /// https://docs.wasmtime.dev/api/wasmtime/struct.PoolingAllocationConfig.html#method.max_core_instances_per_component.
            max_core_instances_per_component: u32,

            /// The maximum number of Wasm linear memories that a single component may
            /// transitively contain (default is unlimited).
            ///
            /// For more information see the Rust documentation at
            /// https://docs.wasmtime.dev/api/wasmtime/struct.PoolingAllocationConfig.html#method.max_memories_per_component.
            max_memories_per_component: u32,

            /// The maximum number of tables that a single component may transitively
            /// contain (default is unlimited).
            ///
            /// For more information see the Rust documentation at
            /// https://docs.wasmtime.dev/api/wasmtime/struct.PoolingAllocationConfig.html#method.max_tables_per_component.
            max_tables_per_component: u32,

            /// The maximum number of concurrent Wasm linear memories supported
            /// (default is 1000).
            ///
            /// For more information see the Rust documentation at
            /// https://docs.wasmtime.dev/api/wasmtime/struct.PoolingAllocationConfig.html#method.total_memories.
            total_memories: u32,

            /// The maximum number of concurrent tables supported (default is 1000).
            ///
            /// For more information see the Rust documentation at
            /// https://docs.wasmtime.dev/api/wasmtime/struct.PoolingAllocationConfig.html#method.total_tables.
            total_tables: u32,

            /// The maximum number of execution stacks allowed for asynchronous
            /// execution, when enabled (default is 1000).
            ///
            /// For more information see the Rust documentation at
            /// https://docs.wasmtime.dev/api/wasmtime/struct.PoolingAllocationConfig.html#method.total_stacks.
            total_stacks: u32,

            /// The maximum number of concurrent core instances supported (default is
            /// 1000).
            ///
            /// For more information see the Rust documentation at
            /// https://docs.wasmtime.dev/api/wasmtime/struct.PoolingAllocationConfig.html#method.total_core_instances.
            total_core_instances: u32,

            /// The maximum size, in bytes, allocated for a core instance’s VMContext
            /// metadata.
            ///
            /// For more information see the Rust documentation at
            /// https://docs.wasmtime.dev/api/wasmtime/struct.PoolingAllocationConfig.html#method.max_core_instance_size.
            max_core_instance_size: usize,

            /// The maximum number of defined tables for a core module (default is 1).
            ///
            /// For more information see the Rust documentation at
            /// https://docs.wasmtime.dev/api/wasmtime/struct.PoolingAllocationConfig.html#method.max_tables_per_module.
            max_tables_per_module: u32,

            /// The maximum table elements for any table defined in a module (default
            /// is 20000).
            ///
            /// For more information see the Rust documentation at
            /// https://docs.wasmtime.dev/api/wasmtime/struct.PoolingAllocationConfig.html#method.table_elements.
            table_elements: usize,

            /// The maximum number of defined linear memories for a module (default is
            /// 1).
            ///
            /// For more information see the Rust documentation at
            /// https://docs.wasmtime.dev/api/wasmtime/struct.PoolingAllocationConfig.html#method.max_memories_per_module.
            max_memories_per_module: u32,

            /// The maximum byte size that any WebAssembly linear memory may grow to.
            ///
            /// For more information see the Rust documentation at
            /// https://docs.wasmtime.dev/api/wasmtime/struct.PoolingAllocationConfig.html#method.max_memory_size.
            max_memory_size: usize,

            /// The maximum number of concurrent GC heaps supported (default is 1000).
            ///
            /// For more information see the Rust documentation at
            /// https://docs.wasmtime.dev/api/wasmtime/struct.PoolingAllocationConfig.html#method.total_gc_heaps.
            total_gc_heaps: u32,
        };
    };
};

test Config {
    @setEvalBranchQuota(50000);
    const builtin = @import("builtin");

    for ([_]Config.Prop{
        .{ .debug_info = true },
        .{ .max_wasm_stack = 8388608 },
        .{ .wasm_threads = true },
        .{ .wasm_reference_types = true },
        .{ .wasm_simd = true },
        .{ .wasm_relaxed_simd = true },
        .{ .wasm_relaxed_simd_deterministic = true },
        .{ .wasm_bulk_memory = true },
        .{ .wasm_multi_value = true },
        .{ .wasm_multi_memory = true },
        .{ .wasm_tail_call = true },
        .{ .wasm_function_references = true },
        .{ .wasm_gc = true },
        .{ .wasm_wide_arithmetic = true },
        .{ .consume_fuel = true },
        .{ .strategy = .auto },
        .{ .strategy = .cranelift },
        .{ .cranelift_debug_verifier = true },
        .{ .cranelift_opt_level = .none },
        .{ .cranelift_opt_level = .speed },
        .{ .cranelift_opt_level = .speed_and_size },
        .{ .profiler = .none },
        .{ .parallel_compilation = true },
        .{ .cranelift_nan_canonicalization = true },
        .{ .native_unwind_info = true },
        .{ .macos_use_mach_ports = false },
        .{ .memory_init_cow = true },
    }) |prop|
        switch (prop) {
            inline else => |val, tag| Config.init()
                .setProp(tag, val),
        };

    const target_str: ?[*:0]const u8 = switch (builtin.target.os.tag) {
        .linux => std.fmt.comptimePrint(
            "{s}-unknown-linux-gnu",
            .{@tagName(builtin.target.cpu.arch)},
        ),
        .macos => std.fmt.comptimePrint(
            "{s}-apple-darwin",
            .{@tagName(builtin.target.cpu.arch)},
        ),
        else => null,
    };

    if (target_str) |target|
        if (Config.init().setTarget(target)) |err|
            return err.testFail();

    Config.init().setCraneliftFlag("opt_level", "none");
    Config.init().enableCraneliftFlag("unwind_info");

    if (Config.init().loadCacheConfig(.default)) |err|
        return err.testFail();

    if (Config.init().loadCacheConfig(.{
        .path = "nonexistent.toml",
    })) |err|
        err.deinit()
    else
        return error.ExpectedWasmtimeError;
}
