use anyhow::anyhow;
//use brane_script_runtime::backend::llvm::LLVMJitBackend;
use branec::{
    types::{Array, CompilerValue, CompilerValueKind, Function, FunctionDefinition},
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

    let mut intrinsics = HashMap::new();

    intrinsics.insert(
        "Number".into(),
        CompilerValue {
            kind: CompilerValueKind::Function(Function {
                params: Array {
                    values: vec![CompilerValue::variant("None", None)],
                },
                defintion: FunctionDefinition::Intrinsic(Arc::new(|args| {
                    let arg = &args[0];
                    if !arg.is_number() {
                        CompilerValue::error(format!("Expected number but found {:?}", arg), "TODO")
                    } else {
                        arg.clone()
                    }
                })),
            }),
            share_info: None,
        },
    );

    intrinsics.insert(
        "add".into(),
        CompilerValue {
            kind: CompilerValueKind::Function(Function {
                params: Array {
                    values: vec![
                        CompilerValue::variant("Some", Some(CompilerValue::label("Number"))),
                        CompilerValue::variant("Some", Some(CompilerValue::label("Number"))),
                    ],
                },
                defintion: FunctionDefinition::Intrinsic(Arc::new(|args| {
                    let a = &args[0];
                    let b = &args[1];
                    if let (CompilerValueKind::Number(a), CompilerValueKind::Number(b)) =
                        (&a.kind, &b.kind)
                    {
                        CompilerValue::number(a.value + b.value)
                    } else {
                        unreachable!("Should be guarded by constraints");
                    }
                })),
            }),
            share_info: None,
        },
    );

    let itpr = Interpreter { intrinsics };

    let document = std::fs::read_to_string(&cli.input)?;

    let parser = branec_parser::parser();
    {
        let input = document.map_span(move |span| span);
        let script_fn = parser
            .parse(input)
            .into_result()
            .map_err(|e| anyhow!("{:#?}", e))?;

        let result = itpr.call(&script_fn, &[]);
        println!("Result: {:#?}", result);
    }

    Ok(())
}
