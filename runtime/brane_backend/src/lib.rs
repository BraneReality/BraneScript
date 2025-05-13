pub type JitFunctionHandle = fn(*const *mut u8, i32, i32) -> ();
