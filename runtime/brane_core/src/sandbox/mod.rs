#[cfg(windows)]
mod win32;
#[cfg(windows)]
pub use win32::try_run;

#[cfg(unix)]
mod unix;
#[cfg(unix)]
pub use unix::try_run;

#[derive(Debug)]
pub struct Fault {
    /// OS agnostic fault that occured
    pub kind: FaultKind,
    /// Memory address of instruction that caused the fault.
    pub fault_source: Option<usize>,
}
impl std::error::Error for Fault {}

/// OS-agnostic categories of runtime faults code can trigger.
#[derive(Debug)]
pub enum FaultKind {
    /// Invalid or forbidden memory access. Includes the invalid memory address.
    Memory(MemoryFault, usize),

    /// Misaligned memory access. Includes the invalid memory address.
    Alignment(AlignmentFault, usize),

    /// Illegal or privileged instruction executed.
    IllegalInstruction(IllegalInstructionFault),

    /// Arithmetic errors like divide by zero or FP traps.
    Arithmetic(ArithmeticFault),

    /// Stack exhaustion or overflow.
    Stack(StackFault),

    /// Breakpoint or trap instruction hit.
    Trap(TrapFault),
}

/// Specific memory access violations.
#[derive(Debug)]
pub enum MemoryFault {
    /// Windows: `EXCEPTION_ACCESS_VIOLATION`
    WindowsExceptionAccessViolation,

    /// Windows: `EXCEPTION_IN_PAGE_ERROR`
    WindowsExceptionInPageError,

    /// Unix: `SIGSEGV`
    UnixSigsegv,

    /// Unix: `SIGBUS`
    UnixSigbus,
}

/// Misaligned access faults.
#[derive(Debug)]
pub enum AlignmentFault {
    /// Windows: `EXCEPTION_DATATYPE_MISALIGNMENT`
    WindowsExceptionDatatypeMisalignment,

    /// Unix: `SIGBUS`
    UnixSigbus,
}

/// Illegal instruction categories.
#[derive(Debug)]
pub enum IllegalInstructionFault {
    /// Windows: `EXCEPTION_ILLEGAL_INSTRUCTION`
    WindowsExceptionIllegalInstruction,

    /// Windows: `EXCEPTION_PRIV_INSTRUCTION`
    WindowsExceptionPrivInstruction,

    /// Unix: `SIGILL`
    UnixSigill,
}

/// Arithmetic exceptions.
#[derive(Debug)]
pub enum ArithmeticFault {
    /// Windows: `EXCEPTION_INT_DIVIDE_BY_ZERO`
    WindowsExceptionIntDivideByZero,

    /// Windows: `EXCEPTION_FLT_DIVIDE_BY_ZERO`
    WindowsExceptionFltDivideByZero,

    /// Windows: `EXCEPTION_FLT_INVALID_OPERATION`
    WindowsExceptionFltInvalidOperation,

    /// Windows: `EXCEPTION_FLT_OVERFLOW`
    WindowsExceptionFltOverflow,

    /// Windows: `EXCEPTION_FLT_UNDERFLOW`
    WindowsExceptionFltUnderflow,

    /// Windows: `EXCEPTION_FLT_DENORMAL_OPERAND`
    WindowsExceptionFltDenormalOperand,

    /// Unix: `SIGFPE`
    UnixSigfpe,
}

/// Stack faults.
#[derive(Debug)]
pub enum StackFault {
    /// Windows: `EXCEPTION_STACK_OVERFLOW` or `STATUS_STACK_BUFFER_OVERRUN`
    WindowsExceptionStackOverflow,
    /// Windows: `STATUS_STACK_OVERFLOW_READ`
    WindowsExceptionStackOverflowRead,
}

/// Trap/breakpoint-related faults.
#[derive(Debug)]
pub enum TrapFault {
    /// Windows: `EXCEPTION_BREAKPOINT`
    WindowsExceptionBreakpoint,

    /// Windows: `EXCEPTION_SINGLE_STEP`
    WindowsExceptionSingleStep,

    /// Unix: `SIGTRAP`
    UnixSigtrap,
}

use std::fmt;
impl fmt::Display for Fault {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Fault {{ kind: {:?}, fault_source: {:?} }}",
            self.kind, self.fault_source
        )
    }
}

// The ONE time the rust panic checks are not helpful
#[cfg(test)]
mod fault_tests {
    use super::*;
    use std::ptr;

    // Helper function to create misaligned pointers
    fn create_misaligned_ptr<T>() -> *mut T {
        // Create a pointer that's guaranteed to be misaligned for T
        let alignment = std::mem::align_of::<T>();
        if alignment > 1 {
            (1 as *mut u8).cast::<T>() // Always misaligned for types requiring > 1 byte alignment
        } else {
            ptr::null_mut() // For byte-aligned types, just use null to trigger a different fault
        }
    }

    #[test]
    fn test_memory_fault_null_deref() {
        let result = unsafe {
            try_run(&mut || {
                let null_ptr: *mut i32 = ptr::null_mut();
                *std::mem::transmute::<_, &mut i32>(null_ptr) = 42; // Should trigger SIGSEGV/ACCESS_VIOLATION
                0
            })
        };
        assert!(result.is_err());
        if let Err(fault) = result {
            match fault.kind {
                FaultKind::IllegalInstruction(
                    IllegalInstructionFault::WindowsExceptionIllegalInstruction,
                )
                | FaultKind::Memory(MemoryFault::UnixSigsegv, _)
                | FaultKind::IllegalInstruction(IllegalInstructionFault::UnixSigill) => {
                    println!("✓ Null dereference fault caught: {:?}", fault);
                }
                _ => panic!("Expected memory fault, got {:?}", fault.kind),
            }
            assert!(
                fault.fault_source.is_some(),
                "Should have instruction address"
            );
        }
    }

    #[test]
    fn test_memory_fault_invalid_read() {
        let result = unsafe {
            try_run(&mut || {
                let invalid_ptr: *const i32 = 0xDEADBEEF as *const i32;
                *std::mem::transmute::<_, &i32>(invalid_ptr) // Should trigger SIGSEGV/ACCESS_VIOLATION
            })
        };

        assert!(result.is_err());
        if let Err(fault) = result {
            match fault.kind {
                FaultKind::Memory(MemoryFault::WindowsExceptionAccessViolation, addr)
                | FaultKind::Memory(MemoryFault::UnixSigsegv, addr) => {
                    println!("✓ Invalid read fault caught at 0x{:x}: {:?}", addr, fault);
                }
                _ => panic!("Expected memory fault, got {:?}", fault.kind),
            }
        }
    }

    #[test]
    fn test_alignment_fault_u64() {
        let result = unsafe {
            try_run(&mut || {
                let misaligned_ptr: *mut u64 = create_misaligned_ptr::<u64>();
                if !misaligned_ptr.is_null() {
                    *std::mem::transmute::<_, &mut u64>(misaligned_ptr) = 0x1234567890ABCDEF;
                    // Should trigger alignment fault
                }
                0
            })
        };
        // Note: Some architectures (like x86_64) handle misaligned access in hardware
        // This test might not always trigger on all platforms
        if result.is_err() {
            if let Err(fault) = result {
                match fault.kind {
                    FaultKind::Alignment(
                        AlignmentFault::WindowsExceptionDatatypeMisalignment,
                        _,
                    )
                    | FaultKind::Alignment(AlignmentFault::UnixSigbus, _)
                    | FaultKind::Memory(_, _) => {
                        println!("✓ Alignment/memory fault caught: {:?}", fault);
                    }
                    _ => panic!("Expected alignment or memory fault, got {:?}", fault.kind),
                }
            }
        } else {
            println!("⚠ Alignment fault not triggered (architecture may handle misaligned access)");
        }
    }

    #[test]
    fn test_illegal_instruction() {
        let result = unsafe {
            try_run(&mut || {
                // Execute invalid machine code
                // These bytes represent invalid/undefined opcodes on most architectures
                #[cfg(target_arch = "x86_64")]
                {
                    std::arch::asm!(".byte 0x0f, 0x0b"); // UD2 instruction - guaranteed invalid
                }
                #[cfg(target_arch = "aarch64")]
                {
                    std::arch::asm!(".word 0x00000000"); // Invalid instruction
                }
                #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
                {
                    // For other architectures, try to execute data as code
                    let invalid_code: [u8; 4] = [0xFF, 0xFF, 0xFF, 0xFF];
                    let func: fn() = std::mem::transmute(invalid_code.as_ptr());
                    func();
                }
                0
            })
        };
        assert!(result.is_err());
        if let Err(fault) = result {
            match fault.kind {
                FaultKind::IllegalInstruction(
                    IllegalInstructionFault::WindowsExceptionIllegalInstruction,
                )
                | FaultKind::IllegalInstruction(
                    IllegalInstructionFault::WindowsExceptionPrivInstruction,
                )
                | FaultKind::IllegalInstruction(IllegalInstructionFault::UnixSigill) => {
                    println!("✓ Illegal instruction fault caught: {:?}", fault);
                }
                _ => panic!("Expected illegal instruction fault, got {:?}", fault.kind),
            }
        }
    }

    #[test]
    fn test_arithmetic_divide_by_zero() {
        let result = unsafe {
            try_run(&mut || {
                let zero = std::hint::black_box(0i32);
                let result = {
                    #[cfg(target_arch = "x86_64")]
                    {
                        std::arch::asm!("div {0}", in(reg) zero, options(nomem, nostack));
                        // Should trigger divide by zero
                    }
                    #[cfg(target_arch = "aarch64")]
                    {
                        std::arch::asm!("mov x1, #0\nudiv x0, x0, x1", options(nomem, nostack));
                        // Should trigger divide by zero
                    }
                    #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
                    {
                        // Fallback for other architectures
                        std::intrinsics::unchecked_div(42i32, zero); // Use original method
                    }
                };
                result
            })
        };
        // Note: Rust usually panics on divide by zero in debug mode
        // In release mode with optimizations, this might not fault
        if result.is_err() {
            if let Err(fault) = result {
                match fault.kind {
                    FaultKind::Arithmetic(ArithmeticFault::WindowsExceptionIntDivideByZero)
                    | FaultKind::Arithmetic(ArithmeticFault::UnixSigfpe) => {
                        println!("✓ Divide by zero fault caught: {:?}", fault);
                    }
                    _ => panic!("Expected arithmetic fault, got {:?}", fault.kind),
                }
            }
        } else {
            println!("⚠ Divide by zero didn't trigger fault (compiler may have optimized)");
        }
    }

    #[test]
    fn test_arithmetic_floating_point() {
        let result = unsafe {
            try_run(&mut || {
                // Force floating point exceptions by using unsafe operations
                let zero = std::hint::black_box(0.0f64);
                let inf = 1.0f64 / zero;
                let nan = 0.0f64 / zero;
                // Try to trigger FP exception with invalid operation
                let result = inf - inf; // Should be NaN, might trigger exception on some systems
                result as i32
            })
        };

        // Floating point exceptions are often masked by default on modern systems
        if result.is_err() {
            if let Err(fault) = result {
                match fault.kind {
                    FaultKind::Arithmetic(ArithmeticFault::WindowsExceptionFltDivideByZero)
                    | FaultKind::Arithmetic(ArithmeticFault::WindowsExceptionFltInvalidOperation)
                    | FaultKind::Arithmetic(ArithmeticFault::UnixSigfpe) => {
                        println!("✓ Floating point fault caught: {:?}", fault);
                    }
                    _ => panic!("Expected arithmetic fault, got {:?}", fault.kind),
                }
            }
        } else {
            println!("⚠ Floating point exception not triggered (likely masked by default)");
        }
    }

    #[test]
    fn test_stack_overflow() {
        #[inline(never)]
        fn infinite_recursion(depth: usize) -> usize {
            // Prevent tail call optimization
            let local_var = [0u8; 1024]; // Use some stack space
            if depth > 0 {
                std::hint::black_box(local_var[depth % 1024]) as usize
                    + infinite_recursion(depth + 1)
            } else {
                infinite_recursion(depth + 1)
            }
        }

        let result = unsafe { try_run(&mut || infinite_recursion(0)) };

        assert!(result.is_err());
        if let Err(fault) = result {
            match fault.kind {
                FaultKind::Stack(StackFault::WindowsExceptionStackOverflow) |
                FaultKind::Memory(MemoryFault::UnixSigsegv, _) | // Unix often reports stack overflow as SIGSEGV
                FaultKind::Memory(MemoryFault::WindowsExceptionAccessViolation, _) => {
                    println!("✓ Stack overflow fault caught: {:?}", fault);
                }
                _ => panic!("Expected stack overflow or memory fault, got {:?}", fault.kind),
            }
        }
    }

    #[test]
    fn test_trap_breakpoint() {
        let result = unsafe {
            try_run(&mut || {
                #[cfg(target_arch = "x86_64")]
                {
                    std::arch::asm!("int3"); // x86_64 breakpoint instruction
                }
                #[cfg(target_arch = "aarch64")]
                {
                    std::arch::asm!("brk #0"); // ARM64 breakpoint instruction
                }
                #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
                {
                    // Fallback: try to trigger a trap some other way
                    let trap_ptr: *const fn() = std::ptr::null();
                    let trap_fn = trap_ptr.read(); // Might cause a fault
                    trap_fn();
                }
                0
            })
        };

        if result.is_err() {
            if let Err(fault) = result {
                match fault.kind {
                    FaultKind::Trap(TrapFault::WindowsExceptionBreakpoint) |
                    FaultKind::Trap(TrapFault::UnixSigtrap) |
                    FaultKind::IllegalInstruction(_) | // Breakpoint might be reported as illegal instruction
                    FaultKind::Memory(_, _) => { // Or as memory fault
                        println!("✓ Trap/breakpoint fault caught: {:?}", fault);
                    }
                    _ => panic!("Expected trap, illegal instruction, or memory fault, got {:?}", fault.kind),
                }
            }
        } else {
            println!(
                "⚠ Breakpoint instruction didn't trigger fault (debugger might have handled it)"
            );
        }
    }

    #[test]
    fn test_successful_execution() {
        let result = unsafe { try_run(&mut || 42) };

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 42);
        println!("✓ Normal execution works correctly");
    }
}
