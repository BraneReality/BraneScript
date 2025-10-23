use std::cell::RefCell;
use std::sync::Once;
use windows::Win32::Foundation::{
    EXCEPTION_ACCESS_VIOLATION, EXCEPTION_BREAKPOINT, EXCEPTION_DATATYPE_MISALIGNMENT,
    EXCEPTION_FLT_DENORMAL_OPERAND, EXCEPTION_FLT_DIVIDE_BY_ZERO, EXCEPTION_FLT_INVALID_OPERATION,
    EXCEPTION_FLT_OVERFLOW, EXCEPTION_FLT_UNDERFLOW, EXCEPTION_ILLEGAL_INSTRUCTION,
    EXCEPTION_INT_DIVIDE_BY_ZERO, EXCEPTION_IN_PAGE_ERROR, EXCEPTION_PRIV_INSTRUCTION,
    EXCEPTION_SINGLE_STEP, EXCEPTION_STACK_OVERFLOW, STATUS_ACCESS_VIOLATION,
    STATUS_STACK_BUFFER_OVERRUN, STATUS_STACK_OVERFLOW_READ,
};
use windows::Win32::System::Diagnostics::Debug::{
    AddVectoredExceptionHandler, RtlCaptureContext, CONTEXT, EXCEPTION_CONTINUE_EXECUTION,
    EXCEPTION_CONTINUE_SEARCH, EXCEPTION_POINTERS,
};

use super::{
    AlignmentFault, ArithmeticFault, Fault, FaultKind, IllegalInstructionFault, MemoryFault,
    StackFault, TrapFault,
};

static HANDLER_INIT: Once = Once::new();

thread_local! {
    static RECOVER_CTX: RefCell<Option<CONTEXT>> = RefCell::new(None);
    static RECOVER_ERR: RefCell<Option<Fault>> = RefCell::new(None);
}

// VEH handler (runs in context of faulting thread)
unsafe extern "system" fn veh_handler(exception_info: *mut EXCEPTION_POINTERS) -> i32 {
    if exception_info.is_null() {
        return EXCEPTION_CONTINUE_SEARCH;
    }

    let Some(recover_ctx) = RECOVER_CTX.with(|ctx| ctx.borrow_mut().take()) else {
        return EXCEPTION_CONTINUE_SEARCH;
    };

    let exception_pointers = unsafe { &*exception_info };
    if exception_pointers.ExceptionRecord.is_null() {
        return EXCEPTION_CONTINUE_SEARCH;
    }

    let exception_record = unsafe { &*exception_pointers.ExceptionRecord };
    let exception_code = exception_record.ExceptionCode;

    // Convert Windows exception to FaultKind
    let fault_kind = match exception_code {
        // Memory faults

        // Alias for STATUS_ACCESS_VIOLATION
        EXCEPTION_ACCESS_VIOLATION => {
            let address = if exception_record.NumberParameters >= 2 {
                exception_record.ExceptionInformation[1]
            } else {
                0
            };
            Some(FaultKind::Memory(
                MemoryFault::WindowsExceptionAccessViolation,
                address,
            ))
        }

        EXCEPTION_IN_PAGE_ERROR => {
            let address = if exception_record.NumberParameters >= 2 {
                exception_record.ExceptionInformation[1]
            } else {
                0
            };
            Some(FaultKind::Memory(
                MemoryFault::WindowsExceptionInPageError,
                address,
            ))
        }

        // Alignment faults
        EXCEPTION_DATATYPE_MISALIGNMENT => {
            let address = exception_record.ExceptionAddress as usize;
            Some(FaultKind::Alignment(
                AlignmentFault::WindowsExceptionDatatypeMisalignment,
                address,
            ))
        }

        // Illegal instruction faults
        EXCEPTION_ILLEGAL_INSTRUCTION => Some(FaultKind::IllegalInstruction(
            IllegalInstructionFault::WindowsExceptionIllegalInstruction,
        )),

        EXCEPTION_PRIV_INSTRUCTION => Some(FaultKind::IllegalInstruction(
            IllegalInstructionFault::WindowsExceptionPrivInstruction,
        )),

        // Arithmetic faults
        EXCEPTION_INT_DIVIDE_BY_ZERO => Some(FaultKind::Arithmetic(
            ArithmeticFault::WindowsExceptionIntDivideByZero,
        )),

        EXCEPTION_FLT_DIVIDE_BY_ZERO => Some(FaultKind::Arithmetic(
            ArithmeticFault::WindowsExceptionFltDivideByZero,
        )),

        EXCEPTION_FLT_INVALID_OPERATION => Some(FaultKind::Arithmetic(
            ArithmeticFault::WindowsExceptionFltInvalidOperation,
        )),

        EXCEPTION_FLT_OVERFLOW => Some(FaultKind::Arithmetic(
            ArithmeticFault::WindowsExceptionFltOverflow,
        )),

        EXCEPTION_FLT_UNDERFLOW => Some(FaultKind::Arithmetic(
            ArithmeticFault::WindowsExceptionFltUnderflow,
        )),

        EXCEPTION_FLT_DENORMAL_OPERAND => Some(FaultKind::Arithmetic(
            ArithmeticFault::WindowsExceptionFltDenormalOperand,
        )),

        // Stack faults
        STATUS_STACK_BUFFER_OVERRUN | EXCEPTION_STACK_OVERFLOW | STATUS_STACK_OVERFLOW_READ => {
            Some(FaultKind::Stack(StackFault::WindowsExceptionStackOverflow))
        }

        // Trap faults
        EXCEPTION_BREAKPOINT => Some(FaultKind::Trap(TrapFault::WindowsExceptionBreakpoint)),

        EXCEPTION_SINGLE_STEP => Some(FaultKind::Trap(TrapFault::WindowsExceptionSingleStep)),

        // Unknown exception - continue search
        _ => None,
    };

    if let Some(kind) = fault_kind {
        let fault_source = if !exception_record.ExceptionAddress.is_null() {
            Some(exception_record.ExceptionAddress as usize)
        } else {
            None
        };
        let fault = Fault { kind, fault_source };
        RECOVER_ERR.with_borrow_mut(|re| *re = Some(fault));

        unsafe {
            // These two statements are basically the exact same, both restore the recover_ctx
            // stack frame, I felt it's important to keep both here for reference though.
            // RtlRestoreContext(&recover_ctx, None);
            *(*exception_info).ContextRecord = recover_ctx;
        }
        EXCEPTION_CONTINUE_EXECUTION
    } else {
        // No matching fault kind found, let other handlers try
        EXCEPTION_CONTINUE_SEARCH
    }
}

#[repr(align(16))]
#[derive(Default, Clone, Copy)]
struct AlignedContext(CONTEXT);

/// Try to catch exceptions thrown in the passed callback
///
/// If an exception can occur, it's the callers responsability to make sure that nothing with the
/// Drop trait is on the stack before the exception can occur, otherwise this will result in memory
/// leaks and various other undefined nasty things
///
/// Also if random errors start appearing around this code, you're not going insane, the jump
/// behaviour here is not supported by rust at the compiler level, so release optimizations may
/// break things in undefiend ways.
#[inline(never)]
pub unsafe fn try_run<F, T>(callback: &mut F) -> Result<T, super::Fault>
where
    F: FnMut() -> T,
{
    HANDLER_INIT.call_once(|| {
        if unsafe { AddVectoredExceptionHandler(1, Some(veh_handler)) }.is_null() {
            panic!("Failed to set Vectored Exception Handler");
        }
    });

    let mut return_ctx = AlignedContext::default();
    unsafe {
        RtlCaptureContext(&mut return_ctx.0);
    } // This is where we jump back to after an error

    match RECOVER_ERR.with(|re| re.borrow_mut().take()) {
        Some(err) => Err(err),
        None => {
            // The error handler only jumps if there's a recover context so we want
            // to make sure to only set RECOVER_CTX when we're listening for errors
            RECOVER_CTX.with(|ctx| *ctx.borrow_mut() = Some(return_ctx.0));
            let data = std::hint::black_box(callback)();
            RECOVER_CTX.with(|ctx| *ctx.borrow_mut() = None);
            Ok(data)
        }
    }
}
