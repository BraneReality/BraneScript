use std::{collections::HashMap, ffi::OsString, io::Write as _, time::Instant};

use anyhow::anyhow;
//use brane_script_runtime::backend::llvm::LLVMJitBackend;
use brane_backend_cranelift::CraneliftJitBackend;
use brane_core::runtime::{LoadedModule, Runtime};
use branec::CompileContext;
use branec_source::{SourceManager, Uri};
use clap::Subcommand;

mod c_abi_tests;

#[derive(clap::Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Script to evaluate
    #[command(subcommand)]
    kind: CommandKind,
}

#[derive(Subcommand)]
enum CommandKind {
    /// Exit the cli
    Exit,
    /// Build a module to a file
    Build {
        /// The module source file
        src: String,
    },
    /// Prints the current working directory
    Pwd,
    Print {
        #[command(subcommand)]
        kind: PrintKind,
    },
    /// Load a module into the cli runtime
    Load {
        /// The module source file
        src: String,
        /// The name of the module we're compiling (one day this might be contained in a mod config)
        #[arg(short, long)]
        module_name: Option<String>,
    },
    /// Call a function
    Call {
        /// Function to call
        function_name: String,
        /// Arguments to call the function with
        args: Option<Vec<String>>,
        /// Module that contains the function
        #[arg(short, long)]
        module_name: Option<String>,
    },
}

#[derive(Subcommand)]
enum PrintKind {
    Module {
        module_name: String,
        #[arg(short, long)]
        function_names: bool,
    },
}

pub fn split_args(program_name: Option<&str>, input: &str) -> Vec<OsString> {
    #[derive(Copy, Clone)]
    enum State {
        Normal,
        SingleQuote,
        DoubleQuote,
        Escape,
    }

    let mut args = Vec::new();
    if let Some(p) = program_name {
        args.push(p.into());
    }
    let mut current = String::new();
    let mut state = State::Normal;

    let mut chars = input.chars();

    while let Some(c) = chars.next() {
        match state {
            State::Normal => match c {
                '\\' => state = State::Escape,
                '\'' => state = State::SingleQuote,
                '"' => state = State::DoubleQuote,
                c if c.is_whitespace() => {
                    if !current.is_empty() {
                        args.push(OsString::from(&current));
                        current.clear();
                    }
                }
                _ => current.push(c),
            },

            State::SingleQuote => {
                if c == '\'' {
                    state = State::Normal;
                } else {
                    current.push(c);
                }
            }

            State::DoubleQuote => match c {
                '"' => state = State::Normal,
                '\\' => {
                    // In double quotes, backslash only escapes certain chars
                    if let Some(next) = chars.next() {
                        match next {
                            '\\' | '"' | '$' | '`' => current.push(next),
                            _ => {
                                current.push('\\');
                                current.push(next);
                            }
                        }
                    }
                }
                _ => current.push(c),
            },

            State::Escape => {
                current.push(c);
                state = State::Normal;
            }
        }
    }

    if !current.is_empty() {
        args.push(OsString::from(current));
    }

    args
}

fn main() -> anyhow::Result<()> {
    let prompt_str = "bs>>";
    print!("{} ", prompt_str);
    std::io::stdout().flush()?;
    let mut input = String::new();

    let mut ctx = CompileContext {
        emitter: branec_emitter::ConsoleEmitter::new(),
        sources: SourceManager::new(),
        loaded_modules: Default::default(),
    };
    ctx.load_std();
    let mut rt = Runtime::<CraneliftJitBackend>::new();

    while let Ok(input_len) = std::io::stdin().read_line(&mut input) {
        let args = split_args(Some(""), &input[0..input_len]);
        input.clear();
        let command = match <Cli as clap::Parser>::try_parse_from(args) {
            Err(err) => {
                println!("Command error {}", err);
                print!("{} ", prompt_str);
                std::io::stdout().flush()?;
                continue;
            }
            Ok(v) => v,
        };
        match || -> anyhow::Result<bool> {
            match command.kind {
                CommandKind::Exit => return Ok(true),
                CommandKind::Pwd => println!(
                    "{}",
                    std::env::current_dir()
                        .expect("Failed to get current dir")
                        .to_string_lossy()
                ),
                CommandKind::Print { kind } => match kind {
                    PrintKind::Module {
                        module_name,
                        function_names,
                    } => {
                        let module = rt
                            .get_module_by_name(&module_name)
                            .ok_or(anyhow!("Module {} not found", module_name))?;

                        if function_names {
                            let mut were_fn = false;
                            for f in module.get_function_names() {
                                println!("{}", f);
                                were_fn = true;
                            }

                            if !were_fn {
                                println!("No functions");
                            }
                        }
                    }
                },
                CommandKind::Build { src: _ } => todo!("We don't do that here"),
                CommandKind::Load { src, module_name } => {
                    let name = module_name.unwrap_or("default".into());
                    let module = ctx.emit_module(name.clone(), &Uri::File(src.into()))?;
                    let mod_id = rt.load_module(module)?;
                    println!("Loaded module with id {}", mod_id);
                }
                CommandKind::Call {
                    module_name,
                    function_name,
                    args: _,
                } => {
                    let module_name = module_name.unwrap_or("default".into());
                    let module = rt
                        .get_module_by_name(&module_name)
                        .ok_or(anyhow!("Module {} not found", module_name))?;

                    let func = module.get_fn(&function_name).ok_or(anyhow!(
                        "Function {} not found in {}",
                        function_name,
                        module_name
                    ))?;

                    println!(
                        "Found function {} in module {} with address {} but calling is not implemented yet",
                        function_name, module_name, func as usize
                    );
                    /*

                    let add_test = unsafe {
                        std::mem::transmute::<_, fn(*const *mut u8, u32, u32) -> u32>(add_test)
                    };

                    let store = brane_core::memory::Store::new();
                    println!("store initialized");
                    let mut bindings = brane_core::memory::BindingsCtx::new(std::ptr::null());
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
                    */
                }
            }
            Ok(false)
        }() {
            Err(err) => {
                println!("Command failed: {}", err);
            }
            Ok(true) => break,
            Ok(false) => {}
        }

        print!("{} ", prompt_str);
        std::io::stdout().flush()?;
    }

    /*
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

    let add_test =
        unsafe { std::mem::transmute::<_, fn(*const *mut u8, u32, u32) -> u32>(add_test) };

    let store = brane_core::memory::Store::new();
    println!("store initialized");
    let mut bindings = brane_core::memory::BindingsCtx::new(std::ptr::null());
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
    */

    Ok(())
}
