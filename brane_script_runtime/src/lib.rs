pub mod backend;
pub use brane_script_common::ir;

pub type JitFunctionHandle = fn(*const *mut u8, i32, i32) -> ();
