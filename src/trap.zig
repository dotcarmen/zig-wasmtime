const c = @cImport(@cInclude("wasmtime/trap.h"));
const wasm = @import("wasm.zig");

extern fn wasm_trap_message(*const Trap, *wasm.ConstVec(u8)) callconv(.c) void;
extern fn wasm_trap_origin(*const Trap) callconv(.c) ?*wasm.Frame;
extern fn wasm_trap_trace(*const Trap, *wasm.Vec(*wasm.Frame)) callconv(.c) void;
extern fn wasmtime_trap_code(*const Trap, *Trap.Code) callconv(.c) bool;
extern fn wasmtime_trap_new([*]const u8, usize) callconv(.c) ?*Trap;

pub const Trap = opaque {
    const declare = wasm.declareRef(Trap, .trap);
    pub const deinit = declare.deinit;
    pub const clone = declare.clone;
    pub const asRef = declare.asRef;
    pub const asConstRef = declare.asConstRef;
    pub const fromRef = declare.fromRef;
    pub const fromConstRef = declare.fromConstRef;

    pub const Code = enum(u8) {
        /// The current stack space was exhausted.
        stack_overflow = c.WASMTIME_TRAP_CODE_STACK_OVERFLOW,
        /// An out-of-bounds memory access.
        memory_out_of_bounds = c.WASMTIME_TRAP_CODE_MEMORY_OUT_OF_BOUNDS,
        /// A wasm atomic operation was presented with a not-naturally-aligned
        /// linear-memory address.
        heap_misaligned = c.WASMTIME_TRAP_CODE_HEAP_MISALIGNED,
        /// An out-of-bounds access to a table.
        table_out_of_bounds = c.WASMTIME_TRAP_CODE_TABLE_OUT_OF_BOUNDS,
        /// Indirect call to a null table entry.
        indirect_call_to_null = c.WASMTIME_TRAP_CODE_INDIRECT_CALL_TO_NULL,
        /// Signature mismatch on indirect call.
        bad_signature = c.WASMTIME_TRAP_CODE_BAD_SIGNATURE,
        /// An integer arithmetic operation caused an overflow.
        integer_overflow = c.WASMTIME_TRAP_CODE_INTEGER_OVERFLOW,
        /// An integer division by zero.
        division_by_zero = c.WASMTIME_TRAP_CODE_INTEGER_DIVISION_BY_ZERO,
        /// Failed float-to-int conversion.
        bad_conversion_to_integer = c.WASMTIME_TRAP_CODE_BAD_CONVERSION_TO_INTEGER,
        /// Code that was supposed to have been unreachable was reached.
        unreachable_code_reached = c.WASMTIME_TRAP_CODE_UNREACHABLE_CODE_REACHED,
        /// Execution has potentially run too long and may be interrupted.
        interrupt = c.WASMTIME_TRAP_CODE_INTERRUPT,
        /// Execution has run out of the configured fuel amount.
        out_of_fuel = c.WASMTIME_TRAP_CODE_OUT_OF_FUEL,
        _,
    };

    pub const origin = wasm_trap_origin;

    pub fn init(msg: []const u8) *Trap {
        return wasmtime_trap_new(msg.ptr, msg.len).?;
    }

    pub fn message(trap: *const Trap) [:0]const u8 {
        var result: wasm.ConstVec(u8) = .empty;
        wasm_trap_message(trap, &result);
        const slice = result.to().?;
        return slice[0 .. slice.len - 1 :0];
    }

    pub fn trace(trap: *const Trap) []*wasm.Frame {
        var result: wasm.Vec(*wasm.Frame) = .empty;
        wasm_trap_trace(trap, &result);
        return result.to().?;
    }

    pub fn code(trap: *const Trap) ?Code {
        var result: Code = undefined;
        if (wasmtime_trap_code(trap, &result))
            return result;
        return null;
    }

    pub const Error = error{
        StackOverflow,
        MemoryOutOfBounds,
        HeapMisaligned,
        TableOutOfBounds,
        IndirectCallToNull,
        BadSignature,
        IntegerOverflow,
        DivisionByZero,
        BadConversionToInteger,
        UnreachableCodeReached,
        Interrupt,
        OutOfFuel,
    };

    pub fn toError(trap: *const Trap) ?Error {
        const tc = trap.code() orelse return null;
        return switch (tc) {
            .stack_overflow => Error.StackOverflow,
            .memory_out_of_bounds => Error.MemoryOutOfBounds,
            .heap_misaligned => Error.HeapMisaligned,
            .table_out_of_bounds => Error.TableOutOfBounds,
            .indirect_call_to_null => Error.IndirectCallToNull,
            .bad_signature => Error.BadSignature,
            .integer_overflow => Error.IntegerOverflow,
            .division_by_zero => Error.DivisionByZero,
            .bad_conversion_to_integer => Error.BadConversionToInteger,
            .unreachable_code_reached => Error.UnreachableCodeReached,
            .interrupt => Error.Interrupt,
            .out_of_fuel => Error.OutOfFuel,
            else => unreachable,
        };
    }
};
