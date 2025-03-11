use anyhow::anyhow;
use brane_script_compiler::{compiler::Compiler, parsed_document::ParsedDocument};
use brane_script_runtime::backend::llvm::LLVMJitBackend;
use clap::Parser;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Documents to parse
    sources: Vec<String>,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    let mut ctx = Vec::new();

    for doc in cli.sources {
        let mut doc = ParsedDocument::new(doc.into(), None)?;
        let doc_ctx = doc.get_document_context()?;
        println!("Was able to parse out document context!");
        for module in doc_ctx.modules.iter() {
            println!("found module {}", module.0);
            println!("with pipelines:");
            for pipe in module.1.pipelines.iter() {
                println!("{}", pipe.0);
            }
        }
        ctx.push(doc_ctx.clone());
    }

    let mut compiler = Compiler::new();
    let compile_res = compiler.compile(ctx.as_slice());

    for m in compile_res.messages {
        println!("cm: {}", m.message);
    }

    let ir = match compile_res.data {
        Some(ir) => ir,
        None => return Err(anyhow!("Failed to compile module")),
    };

    let backend = LLVMJitBackend::new()?;

    for m in ir {
        println!("ir module:\n{}", m);
        backend.stage_module(m)?;
    }
    backend.process_modules()?;

    println!("Loaded functions:");
    let functions = backend.functions.read().unwrap();
    for f in functions.iter() {
        println!("{}", f.0)
    }

    let s0 = functions.get("add_multiple::s0").expect("expected");
    let s1 = functions.get("add_multiple::s1").expect("expected");

    let mut stack_page = [0u8; brane_script_common::BS_PAGE_SIZE as usize];
    let bindings_page = [stack_page.as_mut_ptr(); brane_script_common::BS_PAGE_SIZE as usize];

    #[derive(Debug)]
    struct Args {
        pub a: i32,
        pub b: i32,
        pub c: i32,
    }

    let stack_ptr = 4i32;
    let args = unsafe { (stack_page.as_mut_ptr().add(stack_ptr as usize)) as *mut Args };
    unsafe { std::ptr::write_unaligned(args, Args { a: 5, b: 3, c: 11 }) }

    print!("stack before call: ");
    for c in 0..40 {
        print!("{} ", stack_page[c]);
    }
    println!("");
    println!("");

    println!("running s0 with args {:?}", unsafe { &*args });
    s0(bindings_page.as_ptr(), stack_ptr);
    println!("s0 returned {:?}", unsafe { &*args });

    println!("");
    print!("stack after call: ");
    for c in 0..40 {
        print!("{} ", stack_page[c]);
    }
    println!("\n");
    println!("");

    println!("running s1 with args {:?}", unsafe { &*args });
    s1(bindings_page.as_ptr(), stack_ptr);
    println!("s1 returned {:?}", unsafe { &*args });

    println!("");
    print!("stack after call: ");
    for c in 0..40 {
        print!("{} ", stack_page[c]);
    }
    println!("\n");

    Ok(())
}
