const c = @cImport(@cInclude("wasmtime/trap.h"));
const wasm = @import("wasm.zig");

extern fn wasm_trap_origin(*const Trap) ?*wasm.Frame;
extern fn wasmtime_trap_code(*const Trap, *Trap.Code) bool;
extern fn wasmtime_trap_new([*]const u8, usize) ?*Trap;

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
    };

    pub const getOrigin = wasm_trap_origin;

    pub fn init(message: []const u8) *Trap {
        return wasmtime_trap_new(message.ptr, message.len).?;
    }

    pub fn getCode(trap: *const Trap) ?Code {
        var code: Code = undefined;
        if (wasmtime_trap_code(trap, &code))
            return code;
        return null;
    }
};
