use branec_source::{SourceManager, Span, Uri};
use branec_tokens::{self, tree::TokenTree, TokenInput};
//use brane_script_runtime::backend::llvm::LLVMJitBackend;
use branec_emitter::{ConsoleEmiter, Diagnostic, DiagnosticBuilder, DiagnosticEmitter, Level};
use chumsky::{error::Rich, input::Input, Parser};
use std::{ops::Deref, sync::Arc};

#[derive(clap::Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Documents to parse
    sources: Vec<String>,
}

pub fn report_rich_error<T: std::fmt::Display>(
    error: &Rich<T, Span>,
    sources: &SourceManager,
    emitter: &impl DiagnosticEmitter,
    get_span: fn(&T) -> Span,
) {
    // Build the top-level message
    let mut span = error.span().clone();
    let message = match error.reason() {
        chumsky::error::RichReason::ExpectedFound { expected, found } => {
            let expected: Vec<_> = expected.iter().map(|e| format!("'{}'", e)).collect();
            let found = match found.as_ref() {
                Some(found) => {
                    span = get_span(found);
                    format!("'{}'", found.deref())
                }
                None => "nothing".into(),
            };

            if expected.is_empty() {
                format!("Unexpected {found}")
            } else {
                let expected = expected.join(", ");
                format!("Unexpected {found}, was expecting: {}", expected)
            }
        }
        chumsky::error::RichReason::Custom(msg) => msg.clone(),
    };

    DiagnosticBuilder::new(&message, Level::Error, sources)
        .err_at(span, "unexpected")
        .emit(emitter)
        .unwrap();
}

fn main() -> anyhow::Result<()> {
    let cli = <Cli as clap::Parser>::parse();

    let mut sm = SourceManager::new();
    let sources: Vec<Uri> = cli
        .sources
        .into_iter()
        .map(|source| sm.load_from_file(source))
        .collect::<Result<Vec<Uri>, _>>()?;

    let emitter = Arc::new(ConsoleEmiter::new());

    for source in sources.iter() {
        let text = sm.get(source)?;
        let lexer = branec_tokens::lexer::lexer();

        let char_soruce = Arc::new(source.clone());
        let input = text.map_span(move |span| Span {
            source: char_soruce.clone(),
            range: span.start..span.end,
        });
        let res = lexer.parse(input);
        let (tokens, errs) = &res.into_output_errors();
        if tokens.is_none() {
            println!("lexer errors:");
            for e in errs {
                println!("lexer error: {:#?}", e);
            }
            return Ok(());
        }
        let tokens = tokens.as_ref().unwrap();
        println!("parsed {} tokens", tokens.len());
        for token in tokens.iter() {
            println!("{:<10} {}", token.span.range.start, token);
        }

        let token_input = TokenInput(tokens);
        let tree_builder = branec_tokens::tree::tree_builder();
        let (tree, errs) = chumsky::Parser::parse(&tree_builder, &token_input).into_output_errors();
        if tree.is_none() {
            println!("tree building errors:");
            for e in errs {
                report_rich_error(&e, &sm, &*emitter, |t| t.span.clone());
                //println!("{:#?}", e);
            }
            return Ok(());
        }
        let tree = tree.unwrap();
        println!("built tree");
        println!("{}", tree.write_debug_tree(text));

        let tree = if let TokenTree::Group(group) = tree {
            group
        } else {
            unreachable!();
        };

        let ast_parser = branec_parser::ast_builder();
        let (ast, errs) = chumsky::Parser::parse(&ast_parser, &tree).into_output_errors();
        if ast.is_none() {
            println!("ast building errors:");
            for e in errs {
                report_rich_error(&e, &sm, &*emitter, |t| t.span().clone());
                //println!("{:#?}", e);
            }
            return Ok(());
        }
        let ast = ast.unwrap();
        println!("built ast");
        println!("{}", ast);

        //let hir_arena = HirArena::new();
        //let hir = ast::lowering::LoweringContext::lower(&ast, &hir_arena, emitter.clone())?;
        //println!("lowered ast to hir:");
        //println!("{}", hir);
    }

    Ok(())

    /*
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
