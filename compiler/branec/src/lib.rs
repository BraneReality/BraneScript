use std::collections::HashSet;

use anyhow::{anyhow, bail};
pub mod types;
use types::{CompilerValue, Function, Object};

/// Interprets a script to create an IR module
struct Interpreter {
    pub intrinsics: HashSet<String, CompilerValue>,
}

impl Interpreter {
    pub fn constrain(
        &self,
        value: CompilerValue,
        constraints: impl Iterator<Item = CompilerValue>,
    ) {
    }

    /// Runs a script in object form
    pub fn call(&self, function: &Function, args: &[CompilerValue]) {
        match function.defintion {
            types::FunctionDefinition::Intrinsic(_) => todo!(),
            types::FunctionDefinition::Closure { operations, value } => {}
        }
    }
}
