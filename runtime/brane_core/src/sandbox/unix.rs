use super::{
    AlignmentFault, ArithmeticFault, Fault, FaultKind, IllegalInstructionFault, MemoryFault,
    TrapFault,
};
use libc::{
    c_int, sigaction, sigaltstack, sigemptyset, siginfo_t, stack_t, strsignal, SA_ONSTACK,
    SA_SIGINFO, SIGSTKSZ,
};
use setjmp::{sigjmp_buf, siglongjmp, sigsetjmp};
use std::cell::RefCell;
use std::ffi::c_void;
use std::sync::Once;

// We do this so that when we have a stack overflow the handler
// can execute without issues. Aligned just to be safe.
const ALT_STACK_SIZE: usize = SIGSTKSZ;

static HANDLER_INIT: Once = Once::new();
thread_local! {
        // Statically allocated stack to use for the erorr handler
    static ALT_STACK_SET: Once = Once::new();
    static ALT_STACK: [u64; ALT_STACK_SIZE / 8] = [0u64; ALT_STACK_SIZE / 8];
    static RECOVER_CTX: RefCell<Option<sigjmp_buf>> = RefCell::new(None);
    static RECOVER_ERR: RefCell<Option<Fault>> = RefCell::new(None);
}

#[cfg(all(target_os = "linux", target_arch = "x86_64"))]
unsafe fn get_instruction_pointer(context: *mut c_void) -> Option<usize> {
    if context.is_null() {
        return None;
    }

    let ucontext = context as *const libc::ucontext_t;
    let mcontext = unsafe { &(*ucontext) }.uc_mcontext;
    Some(mcontext.gregs[libc::REG_RIP as usize] as usize)
}

#[cfg(any(
    all(target_os = "android", target_arch = "aarch64"),
    all(target_os = "linux", target_arch = "aarch64")
))]
unsafe fn get_instruction_pointer(context: *mut c_void) -> Option<usize> {
    if context.is_null() {
        return None;
    }

    let ucontext = context as *const libc::ucontext_t;
    let mcontext = unsafe { &(*ucontext) }.uc_mcontext;
    Some(mcontext.pc as usize)
}

// Default implementation for unsupported architectures
#[cfg(not(any(
    all(target_os = "linux", target_arch = "x86_64"),
    all(target_os = "android", target_arch = "aarch64"),
    all(target_os = "linux", target_arch = "aarch64")
)))]
unsafe fn get_instruction_pointer(_context: *mut c_void) -> Option<usize> {
    // Unsupported architecture - cannot extract instruction pointer - pull request welcome
    None
}

unsafe extern "C" fn sig_handler(sig: c_int, info: *const siginfo_t, context: *mut c_void) {
    unsafe {
        let Some(mut ctx) = RECOVER_CTX.with(|ctx| ctx.borrow_mut().take()) else {
            return;
        };

        let siginfo = &*info;

        let kind = match sig {
            libc::SIGSEGV => {
                // Memory access violation
                let fault_addr = siginfo.si_addr() as usize;
                Some(FaultKind::Memory(MemoryFault::UnixSigsegv, fault_addr))
            }

            libc::SIGBUS => {
                // Bus error - could be memory access or alignment
                let fault_addr = siginfo.si_addr() as usize;

                // Check si_code to distinguish between memory and alignment faults
                match siginfo.si_code {
                    libc::BUS_ADRALN => {
                        // Alignment error
                        Some(FaultKind::Alignment(AlignmentFault::UnixSigbus, fault_addr))
                    }
                    _ => {
                        // Other bus errors treated as memory faults
                        Some(FaultKind::Memory(MemoryFault::UnixSigbus, fault_addr))
                    }
                }
            }

            libc::SIGILL => {
                // Illegal instruction
                Some(FaultKind::IllegalInstruction(
                    IllegalInstructionFault::UnixSigill,
                ))
            }

            libc::SIGFPE => {
                // Arithmetic exception
                Some(FaultKind::Arithmetic(ArithmeticFault::UnixSigfpe))
            }

            libc::SIGTRAP => {
                // Trap/breakpoint
                Some(FaultKind::Trap(TrapFault::UnixSigtrap))
            }

            sig => {
                // Unknown signal
                unreachable!("Should not have installed handler for signal {}", sig);
                None
            }
        };

        if let Some(kind) = kind {
            let fault_source = get_instruction_pointer(context);
            let fault = Fault { kind, fault_source };
            RECOVER_ERR.with_borrow_mut(|e| *e = Some(fault));
            siglongjmp(&mut ctx, 1);
        }
    }
}

/// Try to catch exceptions thrown in the passed callback
///
/// If an exception can occur, it's the callers responsability to make sure that nothing with the
/// Drop trait is on the stack before the exception can occur, otherwise this will result in memory
/// leaks and various other undefined nasty things
///
/// On Unix based systems this will set perminant sigactions for a number of common flags, this is
/// because try_run is expected to run in multihreaded senarios and sig handlers are global.
/// Also setting sig and restoring handlers is a non-trival performance hit for the intended use case
/// of jit sandboxing.
///
/// Also if random errors start appearing around this code, you're not going insane, the jump
/// behaviour here is not supported by rust at the compiler level, so release optimizations may
/// break things in undefiend ways.
#[inline(never)]
pub unsafe fn try_run<F, T>(callback: &mut F) -> Result<T, super::Fault>
where
    F: FnMut() -> T,
{
    ALT_STACK_SET.with(|s| {
        s.call_once(|| unsafe {
            let mut ss: stack_t = std::mem::zeroed();
            ALT_STACK.with(|stack| {
                ss.ss_sp = stack.as_ptr().cast_mut().cast();
                ss.ss_size = ALT_STACK_SIZE;
            });
            if sigaltstack(&mut ss, std::ptr::null_mut()) != 0 {
                panic!("Failed to create alt stack for signal handler!");
            }
        });
    });

    HANDLER_INIT.call_once(|| unsafe {
        let mut sa: sigaction = std::mem::zeroed();
        sa.sa_sigaction = sig_handler as usize;
        sa.sa_flags = SA_SIGINFO | SA_ONSTACK;
        sigemptyset(&mut sa.sa_mask);

        for sig in [
            libc::SIGSEGV,
            libc::SIGBUS,
            libc::SIGILL,
            libc::SIGFPE,
            libc::SIGTRAP,
        ] {
            if libc::sigaction(sig, &sa, std::ptr::null_mut()) != 0 {
                panic!("Setting error handling action for signal {} failed", sig);
            }
            println!(
                "try_run listening for {}",
                std::ffi::CStr::from_ptr(strsignal(sig)).to_string_lossy()
            )
        }
    });

    unsafe {
        let mut new_jmp = std::mem::zeroed();
        let res = sigsetjmp(&mut new_jmp, 1);
        match (res, RECOVER_ERR.with_borrow_mut(|re| re.take())) {
            (1, Some(err)) => Err(err),
            (0, None) => {
                // The error handler only jumps if there's a recover context so we want
                // to make sure to only set RECOVER_CTX when we're listening for errors
                RECOVER_CTX.with(|ctx| *ctx.borrow_mut() = Some(new_jmp));
                let data = std::hint::black_box(callback)();
                RECOVER_CTX.with(|ctx| *ctx.borrow_mut() = None);
                Ok(data)
            }
            (sig, err) => unreachable!(
                "sigsetjmp returned {} and recover error was {:?}, this is an unexpected error state",
                sig, err
            ),
        }
    }
}
