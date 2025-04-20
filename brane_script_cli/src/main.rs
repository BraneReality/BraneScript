use anyhow::anyhow;
use brane_script_compiler::{
    ast::{self, ast::Ast},
    errors::console_emiter::ConsoleEmiter,
    hir::HirArena,
    source::{SourceManager, Uri},
    tokens,
};
use brane_script_runtime::backend::llvm::LLVMJitBackend;
use clap::Parser;
use std::sync::Arc;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Documents to parse
    sources: Vec<String>,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    let de = Arc::new(ConsoleEmiter::new());

    let mut sm = SourceManager::new();
    let sources: Vec<Uri> = cli
        .sources
        .into_iter()
        .map(|source| sm.load_from_file(source))
        .collect::<Result<Vec<Uri>, _>>()?;

    let lexer = tokens::lexer();
    for source in sources.iter() {
        let text = sm.get(source)?;
        match chumsky::Parser::parse(&lexer, text).into_result() {
            Ok(tokens) => {
                println!("printing tokens");
                for token in tokens {
                    println!("{:?}", token);
                }
            }
            Err(err) => println!("Failed to parse {:?}", err),
        }
    }

    let asts: Vec<_> = sources
        .into_iter()
        .map(|source| Ast::new(source, &sm))
        .collect::<Result<Vec<_>, _>>()?;

    let hir_arena = HirArena::new();
    let hir = ast::lowering::LoweringContext {
        diag_emitter: de,
        sources: &sm,
        current_origin: None,
    }
    .lower_asts(&asts, &hir_arena)?;

    for (id, module) in hir.modules.iter() {
        println!("found module {}", id);
        println!("with pipelines:");
        for pipe in module.pipelines.iter() {
            println!("{}", pipe.0);
        }
    }

    Ok(())
    /*

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

    let s0: fn(*const *mut u8, i32, i32, i32, i32) =
        unsafe { std::mem::transmute(*functions.get("add_multiple::s0").expect("expected")) };
    let s1: fn(*const *mut u8, i32, i32, i32) =
        unsafe { std::mem::transmute(*functions.get("add_multiple::s1").expect("expected")) };

    let mut stack_page = [0u32; brane_script_common::BS_PAGE_SIZE as usize / 4];
    let bindings_page =
        [stack_page.as_mut_ptr() as *mut u8; brane_script_common::BS_PAGE_SIZE as usize];

    #[derive(Debug)]
    struct Args {
        pub a: i32,
        pub b: i32,
        pub c: i32,
    }

    let stack_ptr = 20i32;
    let ret =
        unsafe { ((stack_page.as_mut_ptr() as *mut u8).add(stack_ptr as usize)) as *mut Args };

    print!("stack before call: ");
    for c in 0..40 {
        print!("{} ", stack_page[c]);
    }
    println!("");
    println!("");

    let s0_args = Args { a: 5, b: 3, c: 6 };
    println!("running s0 with args {:?}", s0_args);
    s0(
        bindings_page.as_ptr(),
        stack_ptr,
        s0_args.a,
        s0_args.b,
        s0_args.c,
    );
    println!("s0 returned {:?}", unsafe { &*ret });

    println!("");
    print!("stack after call: ");
    for c in 0..40 {
        print!("{} ", stack_page[c]);
    }
    println!("\n");
    println!("");

    unsafe {
        println!("running s1 with args a={}, b={}", (*ret).a, (*ret).b);
        s1(bindings_page.as_ptr(), stack_ptr, (*ret).a, (*ret).b);
    }
    println!("s1 returned {:?}", unsafe { &*ret });

    println!("");
    print!("stack after call: ");
    for c in 0..40 {
        print!("{} ", stack_page[c]);
    }
    println!("\n");

    Ok(())*/
}
