use std::time::Instant;

//use brane_script_runtime::backend::llvm::LLVMJitBackend;
use brane_backend_cranelift::CraneliftJitBackend;
use branec::CompileContext;
use branec_source::{SourceManager, Uri};

#[derive(clap::Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Script to evaluate
    input: String,
    func_arg_a: Option<u32>,
    func_arg_b: Option<u32>,
}

fn main() -> anyhow::Result<()> {
    let cli = <Cli as clap::Parser>::parse();

    let compile_start = Instant::now();
    let mut ctx = CompileContext {
        emitter: branec_emitter::ConsoleEmitter::new(),
        sources: SourceManager::new(),
        loaded_modules: Default::default(),
    };

    let module = ctx.emit_module(&Uri::File(cli.input.into()), vec!["test_mod".into()])?;
    let jit_start = Instant::now();

    println!("Emitted module:\n{}", module);
    println!("Jitting...");
    let mut jit = CraneliftJitBackend::default();
    let (fn_map, module) = jit.jit(module)?;
    println!("Jit compiled these functions:");
    for (label, id) in fn_map.iter() {
        println!("{}: {}", id, label);
    }

    let add_test = module.get_finalized_function(fn_map["add_test"]);
    let jit_end = Instant::now();
    println!("Add test data: {:?}", add_test);

    let add_test =
        unsafe { std::mem::transmute::<_, fn(*const *mut u8, u32, u32) -> u32>(add_test) };

    let store = brane_core::memory::Store::new();
    println!("store initialized");
    let mut bindings = brane_core::memory::BindingSet::new();
    println!("bindings created");

    //let test_page = store.alloc_page();
    //println!("test page allocated");

    //let binding = bindings.bind_page_mutable(test_page).unwrap();
    //assert_eq!(binding, 0, "Our jank solution only works if this is 0");
    //println!("test page bound");

    let a = cli.func_arg_a.unwrap_or(40u32);
    let b = cli.func_arg_b.unwrap_or(40u32);
    println!("warming function");
    let mut r = add_test(bindings.bindings_ptr(), a, b);
    r += add_test(bindings.bindings_ptr(), a, b);
    r += add_test(bindings.bindings_ptr(), a, b);
    println!("profiling function (res {})", r);

    let start = std::time::Instant::now();
    let res = add_test(bindings.bindings_ptr(), a, b);
    let fn_end = std::time::Instant::now();

    println!("Add test result: {}", res);
    let end = fn_end - start;
    println!(
        "function took {}nanoseconds {}microseconds {}miliseconds",
        end.as_nanos(),
        end.as_micros(),
        end.as_millis()
    );
    let compile_time = jit_start - compile_start;
    println!(
        "compile took {}nanoseconds {}microseconds {}miliseconds",
        compile_time.as_nanos(),
        compile_time.as_micros(),
        compile_time.as_millis()
    );
    let end = jit_end - jit_start;
    println!(
        "jit took {}nanoseconds {}microseconds {}miliseconds",
        end.as_nanos(),
        end.as_micros(),
        end.as_millis()
    );
    let end = fn_end - compile_start;
    println!(
        "full program took {}nanoseconds {}microseconds {}miliseconds",
        end.as_nanos(),
        end.as_micros(),
        end.as_millis()
    );

    Ok(())
}
