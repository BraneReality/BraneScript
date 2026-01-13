use std::ptr::NonNull;
use std::{collections::HashMap, ffi::OsString, io::Write as _, time::Instant};

use anyhow::{anyhow, bail, Context};
//use brane_script_runtime::backend::llvm::LLVMJitBackend;
use brane_backend_cranelift::CraneliftJitBackend;
use brane_core::ir::{self, Uri};
use brane_core::memory::BSPtr;
use brane_core::runtime::{LoadedModule, Runtime};
use brane_core::{sandbox, ExecutionCtx};
use branec::CompileContext;
use branec_source::SourceManager;
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
    Asm {
        module_name: String,
    },
    IR {
        module_name: String,
    },
    Memory {
        module_name: String,
        type_name: String,
        binding_index: u16,

        /// Offset to start printing from, rounds down to nearest 8 bytes
        #[arg(short, long)]
        offset: Option<usize>,

        /// How many groups of eight bytes to print, default 1
        #[arg(short, long)]
        eightbytes: Option<usize>,
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

    let mut exe_ctx = rt.new_execution_ctx();
    let mut vars_page = rt.store.alloc_page()?;
    let mut ret_page = rt.store.alloc_page()?;

    exe_ctx.bindings[1] = NonNull::new(vars_page.data.as_mut_ptr());
    exe_ctx.bindings[2] = NonNull::new(ret_page.data.as_mut_ptr());

    let user_code = ctx.sources.add_custom("".into())?;

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
                        let mod_id = rt
                            .find_module_by_name(&module_name)
                            .ok_or(anyhow!("Module {} not found", module_name))?;
                        let module = rt
                            .get_module(mod_id)
                            .ok_or(anyhow!("Module {} not found", module_name))?;

                        if function_names {
                            let mut were_fn = false;
                            for f in module.get_fn_names() {
                                println!("{}", f);
                                were_fn = true;
                            }

                            if !were_fn {
                                println!("No functions");
                            }
                        }
                    }
                    PrintKind::Memory {
                        module_name,
                        type_name,
                        binding_index,
                        offset,
                        eightbytes,
                    } => {
                        let binding = exe_ctx.bindings[binding_index as usize];
                        let Some(binding) = binding else {
                            bail!("Binding {} was unset", binding_index);
                        };

                        let offset = offset.unwrap_or(0);
                        let start = offset / 8;
                        let eightbytes = eightbytes.unwrap_or(1);
                        for o in start..(start + eightbytes) {
                            let index = o * 8;
                            if index >= u16::MAX as usize {
                                println!("PAGE END");
                                break;
                            }
                            unsafe {
                                println!(
                                    "({:>5}) {:X} {:X} {:X} {:X}  {:X} {:X} {:X} {:X}",
                                    index,
                                    binding.add(index).as_ref(),
                                    binding.add(index + 1).as_ref(),
                                    binding.add(index + 2).as_ref(),
                                    binding.add(index + 3).as_ref(),
                                    binding.add(index + 4).as_ref(),
                                    binding.add(index + 5).as_ref(),
                                    binding.add(index + 6).as_ref(),
                                    binding.add(index + 7).as_ref(),
                                )
                            }
                        }
                    }
                    PrintKind::Asm { module_name } => {
                        let mod_id = rt
                            .find_module_by_name(&module_name)
                            .ok_or(anyhow!("Module {} not found", module_name))?;

                        let module_src = rt
                            .module_src(mod_id)
                            .ok_or(anyhow!("Module source not found"))?;

                        let loaded_module =
                            rt.get_module(mod_id).ok_or(anyhow!("Module not loaded"))?;

                        println!("Assembly for module '{}':\n", module_name);

                        for function in &module_src.functions {
                            let fn_id = module_src
                                .find_function(&function.id)
                                .ok_or(anyhow!("Function {} not found", function.id))?;

                            println!("Function: {}", function.id);
                            println!("Signature: {}", function.sig);

                            if let Some(asm) = loaded_module.get_fn_asm(fn_id) {
                                println!("{}\n", asm);
                            } else {
                                println!("  (no assembly available)\n");
                            }
                        }
                    }
                    PrintKind::IR { module_name } => {
                        let mod_id = rt
                            .find_module_by_name(&module_name)
                            .ok_or(anyhow!("Module {} not found", module_name))?;

                        let module_src = rt
                            .module_src(mod_id)
                            .ok_or(anyhow!("Module source not found"))?;

                        println!("IR for module '{}':\n", module_name);
                        println!("{}", module_src);
                    }
                },
                CommandKind::Build { src: _ } => todo!("We don't do that here"),
                CommandKind::Load { src, module_name } => {
                    let name = module_name.unwrap_or("default".into());
                    let uri = Uri::File(src.into());
                    let module = ctx.emit_module(name.clone(), &uri)?;
                    println!("Compiled successfully");
                    let mod_id = rt.load_module(uri, module)?;
                    println!("Loaded module with id {}", mod_id);
                }
                CommandKind::Call {
                    module_name,
                    function_name,
                    args,
                } => {
                    let module_name = module_name.unwrap_or("default".into());
                    let mod_id = rt
                        .find_module_by_name(&module_name)
                        .ok_or(anyhow!("Module {} not found", module_name))?;

                    let module_uri = rt.module_uri(mod_id).unwrap();
                    let module = rt.get_module(mod_id).unwrap();
                    let module_src = rt.module_src(mod_id).unwrap();
                    let func_id = module_src.find_function(&function_name).ok_or(anyhow!(
                        "Function {} not found in source {}",
                        function_name,
                        module_name
                    ))?;
                    let func_src = module_src.get_function(func_id).unwrap();

                    let func_table_entry = module.get_fn_by_name(&function_name).ok_or(anyhow!(
                        "Function {} not found in compiled {}",
                        function_name,
                        module_name
                    ))?;

                    // TODO need an ir writer library that isn't string concatination
                    let jit_src = format!(
                        "using * from \"{}\";\
                        fn user_call(env: *any, result: *any) {{\
                            *result = {}({});\
                        }}",
                        module_uri.to_string(),
                        function_name,
                        if let Some(args) = args {
                            args.join(",")
                        } else {
                            "".into()
                        }
                    );
                    println!("jitting command fn\n{}", jit_src);
                    ctx.sources.update_custom(&user_code, jit_src)?;

                    let new_mod_src = ctx.emit_module("user_module".into(), &user_code)?;
                    let new_mod_id = rt.load_module(user_code.clone(), new_mod_src)?;
                    let new_mod = rt.get_module(new_mod_id).unwrap();
                    let user_call_index = new_mod.get_fn_by_name("user_call").unwrap();

                    unsafe {
                        let func = std::mem::transmute::<_, fn(*const ExecutionCtx, u32, u32)>(
                            exe_ctx.fn_bindings_ptr().add(user_call_index as usize),
                        );

                        func(&exe_ctx, BSPtr::new(1, 0).0, BSPtr::new(2, 0).0);
                        /*
                        let mut sb = || {
                            // The func
                            func(&exe_ctx, BSPtr::new(1, 0).0, BSPtr::new(2, 0).0);
                        };
                        sandbox::try_run(&mut sb)?;
                        */
                    }
                }
            }
            Ok(false)
        }() {
            Err(err) => {
                println!("Command failed: {}", err);
                println!("backtrace: {}", err.backtrace());
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
