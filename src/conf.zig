const c = @cImport(@cInclude("wasmtime/conf.h"));

const std = @import("std");
const testing = std.testing;

pub const Feature = enum {
    profiling,
    wat,
    cache,
    parallel_compilation,
    wasi,
    logging,
    disable_logging,
    coredump,
    addr2line,
    demangle,
    threads,
    gc,
    gc_drc,
    gc_null,
    @"async",
    cranelift,
    winch,
    debug_builtins,
    pooling_allocator,
    compiler,

    pub fn isEnabled(feat: Feature) bool {
        switch (feat) {
            inline else => |feature| return comptime enabled: {
                const lower_name: []const u8 = @tagName(feature);
                var name: [lower_name.len]u8 = undefined;
                const upper_name = std.ascii.upperString(name[0..], lower_name);
                if (@hasDecl(c, "WASMTIME_FEATURE_" ++ upper_name))
                    break :enabled true;
                break :enabled false;
            },
        }
    }
};

test {
    const profiling = @hasDecl(c, "WASMTIME_FEATURE_PROFILING");
    try testing.expectEqual(profiling, Feature.profiling.isEnabled());
}
