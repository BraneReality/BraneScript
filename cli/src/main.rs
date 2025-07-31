use anyhow::anyhow;
//use brane_script_runtime::backend::llvm::LLVMJitBackend;
use branec::{
    types::{Array, Function, FunctionDefinition, Value},
    Interpreter,
};
use chumsky::{error::Rich, input::Input, Parser};
use std::{collections::HashMap, ops::Deref, sync::Arc};

#[derive(clap::Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Script to evaluate
    input: String,
}

fn main() -> anyhow::Result<()> {
    let cli = <Cli as clap::Parser>::parse();

    let intrinsics = branec::stdlib::define()
        .unpack()
        .collect::<HashMap<String, Value>>();

    let itpr = Interpreter { intrinsics };

    let document = std::fs::read_to_string(&cli.input)?;

    let parser = branec_parser::parser();
    {
        let input = document.map_span(move |span| span);
        let script_fn = parser.parse(input).into_result().map_err(|errs| {
            let mut message = String::new();
            for e in errs {
                let mut line = 1;
                let mut char_pos = 0;
                let mut count = 0;
                for c in document.chars() {
                    if count >= e.span().start {
                        break;
                    }
                    if c == '\n' {
                        line += 1;
                        char_pos = 0;
                    } else {
                        char_pos += 1;
                    }
                    count += 1;
                }
                message += format!(
                    "[{}:{}] {:#?} in contexts {:?}",
                    line,
                    char_pos,
                    e,
                    e.contexts()
                        .map(|c| c.0.to_string())
                        .reduce(|a, b| format!("{}, {}", a, b))
                        .unwrap_or_default()
                )
                .as_str();
            }
            anyhow!(message)
        })?;

        let result = itpr.call(&script_fn, []);
        println!("Result: {:#?}", result);
    }

    Ok(())
}
