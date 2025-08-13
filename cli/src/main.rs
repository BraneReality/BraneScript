//use brane_script_runtime::backend::llvm::LLVMJitBackend;
use brane_backend_cranelift::CraneliftJitBackend;
use branec::CompileContext;
use branec_source::{SourceManager, Uri};

#[derive(clap::Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Script to evaluate
    input: String,
    func_arg_a: i64,
    func_arg_b: i64,
}

fn main() -> anyhow::Result<()> {
    let cli = <Cli as clap::Parser>::parse();

    let mut ctx = CompileContext {
        emitter: branec_emitter::ConsoleEmitter::new(),
        sources: SourceManager::new(),
        loaded_modules: Default::default(),
    };

    let module = ctx.emit_module(&Uri::File(cli.input.into()), vec!["test_mod".into()])?;

    println!("Emitted module:\n{}", module);
    println!("Jitting...");
    let mut jit = CraneliftJitBackend::default();
    let (fn_map, module) = jit.jit(module)?;
    println!("Jit compiled these functions:");
    for (label, id) in fn_map.iter() {
        println!("{}: {}", id, label);
    }

    let add_test = module.get_finalized_function(fn_map.first().unwrap().1);
    println!("Add test data: {:?}", add_test);

    let add_test = unsafe { std::mem::transmute::<_, fn(i64, i64) -> i64>(add_test) };

    let start = std::time::Instant::now();
    let res = add_test(cli.func_arg_a, cli.func_arg_b);
    let end = std::time::Instant::now() - start;

    println!("Add test result: {}", res);
    println!(
        "got result in {}nanoseconds {}microseconds {}miliseconds",
        end.as_nanos(),
        end.as_micros(),
        end.as_millis()
    );

    Ok(())
}
