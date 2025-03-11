use anyhow::{anyhow, bail};
use brane_script_common::{
    BS_PAGE_SIZE,
    ir::{IRFunction, IRModule, IROp, IRType, IRValue},
};
use defer::defer;
pub use inkwell::{
    builder::Builder as InkBuilder, context::Context as InkContext, module::Module as InkModule,
};
use inkwell::{
    types::{ArrayType, BasicTypeEnum, FunctionType, PointerType},
    values::{AnyValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue},
};
use llvm_sys::{
    core::{LLVMSetDataLayout, LLVMSetTarget},
    error::*,
    orc2::{lljit::*, *},
    target::LLVMSetModuleDataLayout,
};
use std::{
    collections::{HashMap, HashSet},
    ptr::{null_mut, slice_from_raw_parts},
    sync::{Mutex, RwLock, atomic::AtomicBool},
    u64,
};

pub type JitFunctionHandle = fn(*const *mut u8, i32) -> ();

pub struct LLVMJitBackend {
    lljit: LLVMOrcLLJITRef,

    staged_modules: Mutex<Vec<(IRModule, LLVMOrcThreadSafeModuleRef)>>,
    pub functions: RwLock<HashMap<String, JitFunctionHandle>>,
}

static LLVM_INITIALIZED: AtomicBool = AtomicBool::new(false);

unsafe fn map_llvm_res(error: llvm_sys::error::LLVMErrorRef) -> anyhow::Result<()> {
    if error.is_null() {
        return Ok(());
    }
    unsafe {
        let err_message = LLVMGetErrorMessage(error);
        defer!(LLVMDisposeErrorMessage(err_message));
        let err_str = std::ffi::CStr::from_ptr(err_message).to_str()?;
        let err = anyhow!("LLVM Error: {}", err_str);
        println!("{}", err);
        Err(err)
    }
}

extern "C" fn object_linking_layer_creator(
    _ctx: *mut std::ffi::c_void,
    es: LLVMOrcExecutionSessionRef,
    _triple: *const std::ffi::c_char,
) -> LLVMOrcObjectLayerRef {
    unsafe {
        let obj_layer = ee::LLVMOrcCreateRTDyldObjectLinkingLayerWithSectionMemoryManager(es);
        let gdb_events = llvm_sys::execution_engine::LLVMCreateGDBRegistrationListener();
        ee::LLVMOrcRTDyldObjectLinkingLayerRegisterJITEventListener(obj_layer, gdb_events);
        println!("registerd gdb listener");
        obj_layer
    }
}

impl LLVMJitBackend {
    pub fn new() -> anyhow::Result<LLVMJitBackend> {
        let should_init = !LLVM_INITIALIZED.swap(true, std::sync::atomic::Ordering::Relaxed);
        if should_init {
            unsafe {
                if llvm_sys::target::LLVM_InitializeNativeTarget() == 1 {
                    bail!("failed to initialize native llvm target")
                }

                if llvm_sys::target::LLVM_InitializeNativeAsmPrinter() == 1 {
                    bail!("failed to initialize native llvm asm printer")
                }
                if llvm_sys::target::LLVM_InitializeNativeAsmParser() == 1 {
                    bail!("failed to initialize native llvm asm parser")
                }
            }
        }

        //build
        let lljit = unsafe {
            let mut jtmb = null_mut();
            map_llvm_res(LLVMOrcJITTargetMachineBuilderDetectHost(&mut jtmb))?;

            let builder = LLVMOrcCreateLLJITBuilder();

            LLVMOrcLLJITBuilderSetJITTargetMachineBuilder(builder, jtmb);
            LLVMOrcLLJITBuilderSetObjectLinkingLayerCreator(
                builder,
                object_linking_layer_creator,
                null_mut(),
            );

            let mut lljit = null_mut();
            map_llvm_res(LLVMOrcCreateLLJIT(&mut lljit, builder))?;
            println!("Created lljit runtime");
            lljit
        };

        Ok(LLVMJitBackend {
            lljit,
            staged_modules: Mutex::new(Vec::new()),
            functions: RwLock::new(HashMap::new()),
        })
    }

    pub fn stage_module(&self, ir_module: IRModule) -> anyhow::Result<()> {
        let ts_ctx = unsafe { LLVMOrcCreateNewThreadSafeContext() };

        let context = unsafe { InkContext::new(LLVMOrcThreadSafeContextGetContext(ts_ctx)) };
        let err = {
            let module = LLVMModuleBuilder::build(&ir_module, &context);
            println!("built llvm ir for {}", &ir_module.id);

            match module {
                Ok(module) => unsafe {
                    LLVMSetTarget(module.as_mut_ptr(), LLVMOrcLLJITGetTripleString(self.lljit));
                    LLVMSetDataLayout(
                        module.as_mut_ptr(),
                        LLVMOrcLLJITGetDataLayoutStr(self.lljit),
                    );
                    let tsm = LLVMOrcCreateNewThreadSafeModule(module.as_mut_ptr(), ts_ctx);
                    println!("staged module:\n{}", module.print_to_string().to_string());

                    std::mem::forget(module);
                    self.staged_modules
                        .lock()
                        .expect("mutex corruption")
                        .push((ir_module, tsm));
                    println!("staged {}", tsm as usize);
                    None
                },
                Err(err) => Some(err),
            }
        };
        std::mem::forget(context);

        if let Some(err) = err {
            unsafe {
                println!("cleaning tsctx after error");
                LLVMOrcDisposeThreadSafeContext(ts_ctx);
                println!("cleaned");
            }
            Err(err)
        } else {
            Ok(())
        }
    }

    pub fn process_modules(&self) -> anyhow::Result<()> {
        let modules = {
            std::mem::take::<Vec<_>>(
                self.staged_modules
                    .lock()
                    .expect("mutex corruption")
                    .as_mut(),
            )
        };

        println!("{} modules to process", modules.len());

        let es = unsafe { LLVMOrcLLJITGetExecutionSession(self.lljit) };

        for staged in modules {
            unsafe {
                let lib_name = std::ffi::CString::new(staged.0.id.clone())?;
                let jit_lib = LLVMOrcExecutionSessionCreateBareJITDylib(es, lib_name.as_ptr());
                println!(
                    "created jit lib {}, addr {}",
                    lib_name.to_string_lossy(),
                    jit_lib as usize
                );
                map_llvm_res(LLVMOrcLLJITAddLLVMIRModule(self.lljit, jit_lib, staged.1))?;

                for f in staged.0.functions.iter() {
                    let f_id = std::ffi::CString::new(f.id.clone())?;
                    let symbol = LLVMOrcLLJITMangleAndIntern(self.lljit, f_id.as_ptr());

                    println!(
                        "searching for function {} (mangled = {})",
                        f_id.to_string_lossy(),
                        std::ffi::CStr::from_ptr(LLVMOrcSymbolStringPoolEntryStr(symbol))
                            .to_string_lossy()
                    );
                    let on_found: Box<dyn FnOnce(LLVMErrorRef, LLVMOrcCSymbolMapPairs, usize)> =
                        Box::new(
                            |err: LLVMErrorRef, symbols: LLVMOrcCSymbolMapPairs, size: usize| {
                                println!("Session lookup completed");
                                if let Err(_) = map_llvm_res(err) {
                                    return;
                                }

                                let mut functions = self.functions.write().unwrap();

                                let symbols = slice_from_raw_parts(symbols, size);
                                for symbol in (*symbols).iter() {
                                    let name = std::ffi::CStr::from_ptr(
                                        LLVMOrcSymbolStringPoolEntryStr(symbol.Name),
                                    )
                                    .to_string_lossy();
                                    println!("Found symbol: {}", &name);

                                    functions.insert(
                                        name.to_string(),
                                        std::mem::transmute(symbol.Sym.Address),
                                    );
                                }

                                LLVMOrcReleaseSymbolStringPoolEntry(symbol);
                            },
                        );
                    LLVMOrcExecutionSessionLookup(
                        es,
                        LLVMOrcLookupKind::LLVMOrcLookupKindStatic,
                        [LLVMOrcCJITDylibSearchOrderElement {
                            JD: jit_lib,
                            JDLookupFlags: LLVMOrcJITDylibLookupFlags::LLVMOrcJITDylibLookupFlagsMatchAllSymbols,
                        }]
                        .as_mut_ptr(),
                        1usize,
                        [LLVMOrcCLookupSetElement {
                            Name: symbol,
                            LookupFlags:
                                LLVMOrcSymbolLookupFlags::LLVMOrcSymbolLookupFlagsRequiredSymbol,
                        }]
                        .as_mut_ptr(),
                        1,
                        after_symbols_found,
                        Box::into_raw(Box::new(on_found)) as *mut std::ffi::c_void,
                    );
                }
            }
        }
        Ok(())
    }
}

extern "C" fn after_symbols_found(
    err: LLVMErrorRef,
    result: LLVMOrcCSymbolMapPairs,
    num_pairs: usize,
    ctx: *mut std::ffi::c_void,
) {
    let closure = unsafe {
        Box::from_raw(std::mem::transmute::<
            *mut std::ffi::c_void,
            *mut Box<dyn FnOnce(LLVMErrorRef, LLVMOrcCSymbolMapPairs, usize)>,
        >(ctx))
    };

    closure(err, result, num_pairs);
}

impl Drop for LLVMJitBackend {
    fn drop(&mut self) {
        unsafe {
            println!("Starting to dispose of lljit");
            LLVMOrcDisposeLLJIT(self.lljit);
            println!("Fully disposed lljit");
        }
    }
}

pub struct LLVMModuleBuilder<'ctx> {
    context: &'ctx InkContext,
    module: InkModule<'ctx>,
    builder: InkBuilder<'ctx>,

    function_type: FunctionType<'ctx>,
    functions: Vec<FunctionValue<'ctx>>,

    op_values: Vec<Option<BasicValueEnum<'ctx>>>,
    page_bindings_ptr: Option<PointerValue<'ctx>>,
    stack_ptr: Option<IntValue<'ctx>>,
    stack_offset: usize,
}

impl<'ctx> LLVMModuleBuilder<'ctx> {
    pub fn build(
        bs_module: &IRModule,
        context: &'ctx InkContext,
    ) -> anyhow::Result<InkModule<'ctx>> {
        let module = context.create_module(&bs_module.id);
        let builder = context.create_builder();

        let mut mb = LLVMModuleBuilder {
            function_type: context.void_type().fn_type(
                &[context.ptr_type(0.into()).into(), context.i32_type().into()],
                false,
            ),
            context,
            module,
            builder,
            functions: Vec::new(),
            op_values: Vec::new(),
            stack_ptr: None,
            page_bindings_ptr: None,
            stack_offset: 0,
        };

        // TODO generate debug info for struct types
        mb.build_module(bs_module)?;

        Ok(mb.module)
    }

    fn build_module(&mut self, bs_module: &IRModule) -> anyhow::Result<()> {
        self.functions.reserve(bs_module.functions.len());

        for f in bs_module.functions.iter() {
            self.functions.push(self.module.add_function(
                &f.id,
                self.function_type,
                Some(inkwell::module::Linkage::External),
            ));
        }

        for (ir_f, ll_f) in bs_module.functions.iter().zip(self.functions.clone()) {
            println!("building symbol: {}", ll_f.get_name().to_string_lossy());
            let entry = self.context.append_basic_block(ll_f, "entry");
            self.builder.position_at_end(entry);

            self.page_bindings_ptr = Some(ll_f.get_nth_param(0).unwrap().into_pointer_value());
            let stack_ptr_int = ll_f.get_nth_param(1).unwrap().into_int_value();

            // Push stack past input struct
            let stack_start = ir_f.input.size_layout(bs_module)?.0;
            let input_size = self.context.i32_type().const_int(stack_start as u64, false);
            self.stack_offset = stack_start;
            self.stack_ptr = Some(self.builder.build_int_add(
                stack_ptr_int,
                input_size,
                "stack-start",
            )?);

            self.op_values.clear();
            self.op_values.reserve(ir_f.operations.len());
            self.op_values
                .push(Some(BasicValueEnum::IntValue(stack_ptr_int)));

            for op in ir_f.operations.iter() {
                self.build_op(op, ir_f, bs_module)?;
            }
        }

        if let Err(err) = self.module.verify() {
            println!(
                "prininting failed module:\n{}",
                self.module.print_to_string()
            );
            return Err(anyhow!("generated module was invalid: {}", err));
        }

        Ok(())
    }

    fn build_op(&mut self, op: &IROp, func: &IRFunction, module: &IRModule) -> anyhow::Result<()> {
        let new_value = match op {
            IROp::NoOp => return Ok(()), // Return so we don't push a value
            IROp::ConstI32 { value: _ } => todo!(),
            IROp::ConstU32 { value } => Some(
                self.context
                    .i32_type()
                    .const_int(*value as u64, false)
                    .into(),
            ),
            IROp::ConstF32 { value: _ } => todo!(),
            IROp::AllocA { r#type } => {
                let (size, alignment) = r#type.size_layout(module)?;
                self.stack_offset += alignment - self.stack_offset % alignment;
                let offset_v = self
                    .context
                    .i32_type()
                    .const_int(self.stack_offset as u64, false);
                let aligned_i32_ptr =
                    self.builder
                        .build_int_add(self.stack_ptr.unwrap(), offset_v, "alloca")?;
                self.stack_offset += size;
                Some(aligned_i32_ptr.into())
            }
            IROp::Load { r#type, ptr } => {
                let int_ptr = self.get_value(*ptr)?;
                let int_ptr = match int_ptr {
                    BasicValueEnum::IntValue(int_ptr) => int_ptr,
                    _ => bail!("expected int_ptr but was passed {}", int_ptr),
                };

                let true_ptr = self.brane_ptr(int_ptr)?;
                let llvm_t = self.get_llvm_type(&IRType::Native(*r#type));
                let value = self.builder.build_load(llvm_t, true_ptr, "loaded-v")?;
                Some(BasicValueEnum::from(value))
            }
            IROp::Store { src, ptr } => {
                let int_ptr = self.get_value(*ptr)?;
                let int_ptr = match int_ptr {
                    BasicValueEnum::IntValue(int_ptr) => int_ptr,
                    _ => bail!("expected int_ptr but was passed {}", int_ptr),
                };

                let src_v = self.get_value(*src)?;

                let true_ptr = self.brane_ptr(int_ptr)?;
                self.builder.build_store(true_ptr, src_v)?;
                None
            }
            IROp::Add { left, right } => {
                let left = self.get_value(*left)?;
                let right = self.get_value(*right)?;
                Some(match (left, right) {
                    (BasicValueEnum::IntValue(left), BasicValueEnum::IntValue(right)) => {
                        self.builder.build_int_add(left, right, "")?.into()
                    }
                    (BasicValueEnum::FloatValue(left), BasicValueEnum::FloatValue(right)) => {
                        self.builder.build_float_add(left, right, "")?.into()
                    }
                    _ => bail!("Invalid arg pair for add! ({:?}, {:?})", left, right),
                })
            }
            IROp::Sub { left, right } => {
                let left = self.get_value(*left)?;
                let right = self.get_value(*right)?;
                Some(match (left, right) {
                    (BasicValueEnum::IntValue(left), BasicValueEnum::IntValue(right)) => {
                        self.builder.build_int_sub(left, right, "")?.into()
                    }
                    (BasicValueEnum::FloatValue(left), BasicValueEnum::FloatValue(right)) => {
                        self.builder.build_float_sub(left, right, "")?.into()
                    }
                    _ => bail!("Invalid arg pair for sub! ({:?}, {:?})", left, right),
                })
            }
            IROp::Mul { left, right } => {
                let left = self.get_value(*left)?;
                let right = self.get_value(*right)?;
                Some(match (left, right) {
                    (BasicValueEnum::IntValue(left), BasicValueEnum::IntValue(right)) => {
                        self.builder.build_int_mul(left, right, "")?.into()
                    }
                    (BasicValueEnum::FloatValue(left), BasicValueEnum::FloatValue(right)) => {
                        self.builder.build_float_mul(left, right, "")?.into()
                    }
                    _ => bail!("Invalid arg pair for mul! ({:?}, {:?})", left, right),
                })
            }
            IROp::Div { left, right } => {
                let left = self.get_value(*left)?;
                let right = self.get_value(*right)?;
                Some(match (left, right) {
                    (BasicValueEnum::IntValue(left), BasicValueEnum::IntValue(right)) => {
                        todo!("we need to separate the operands");
                    }
                    (BasicValueEnum::FloatValue(left), BasicValueEnum::FloatValue(right)) => {
                        self.builder.build_float_div(left, right, "")?.into()
                    }
                    _ => bail!("Invalid arg pair for div! ({:?}, {:?})", left, right),
                })
            }
            IROp::Rem { left, right } => todo!(),
            IROp::Eq { left, right } => todo!(),
            IROp::Ne { left, right } => todo!(),
            IROp::Gt { left, right } => todo!(),
            IROp::Ge { left, right } => todo!(),
            IROp::And { left, right } => todo!(),
            IROp::Or { left, right } => todo!(),
            IROp::Xor { left, right } => todo!(),
            IROp::Call {
                func,
                input,
                output,
            } => todo!(),
            IROp::NextStage { args, deps: _ } => {
                let return_value = self.brane_ptr(
                    self.get_value(*args)?
                        .try_into()
                        .map_err(|e| anyhow!("next stage args was not int"))?,
                )?;
                let return_dest = self.brane_ptr(self.stack_ptr.unwrap())?;

                let return_size = func.output.size_layout(module)?.0;

                self.fast_copy(return_value, return_dest, return_size)?;
                self.builder.build_return(None)?;
                None
            }
        };
        self.op_values.push(new_value);
        Ok(())
    }

    fn get_value(&self, ir_value: IRValue) -> anyhow::Result<BasicValueEnum<'ctx>> {
        Ok(self
            .op_values
            .get(ir_value.0 as usize)
            .cloned()
            .ok_or(anyhow!("op index {} not valid", ir_value.0))?
            .ok_or(anyhow!("op index {} has no value", ir_value.0))?)
    }

    fn brane_ptr(&mut self, int_ptr: IntValue<'ctx>) -> anyhow::Result<PointerValue<'ctx>> {
        let i32_t = self.context.i32_type();
        let mem_offset = self.builder.build_right_shift(
            int_ptr,
            i32_t.const_int(16, false),
            false,
            "offset-bits",
        )?;
        let binding_index =
            self.builder
                .build_and(int_ptr, i32_t.const_int(0x0000FFFF, false), "binding-bits")?;

        let ptr_t = self.context.ptr_type(0.into());
        let page_binding = unsafe {
            self.builder.build_gep(
                ptr_t,
                self.page_bindings_ptr.unwrap(),
                &[binding_index],
                "page-binding",
            )
        }?;
        let page_ptr = self
            .builder
            .build_load(ptr_t, page_binding, "page-ptr")?
            .into_pointer_value();
        Ok(unsafe {
            self.builder
                .build_gep(self.context.i8_type(), page_ptr, &[mem_offset], "brane-ptr")?
        })
    }

    fn get_llvm_type(&mut self, bs_type: &IRType) -> BasicTypeEnum<'ctx> {
        use brane_script_common::ir::IRNativeType::*;
        match bs_type {
            IRType::Native(native_type) => match native_type {
                U8 | I8 => BasicTypeEnum::IntType(self.context.i8_type()),
                U16 | I16 => BasicTypeEnum::IntType(self.context.i16_type()),
                U32 | I32 => BasicTypeEnum::IntType(self.context.i32_type()),
                F32 => BasicTypeEnum::FloatType(self.context.f32_type()),
                U64 | I64 => BasicTypeEnum::IntType(self.context.i64_type()),
                F64 => BasicTypeEnum::FloatType(self.context.f64_type()),
                U128 | I128 => BasicTypeEnum::IntType(self.context.i128_type()),
            },
            IRType::Struct(_) => BasicTypeEnum::IntType(self.context.i32_type()), // Referred to as int ptrs
        }
    }
    pub fn fast_copy(
        &self,
        src: PointerValue,
        dest: PointerValue,
        size: usize,
    ) -> anyhow::Result<()> {
        let i8_type = self.context.i8_type();
        let i16_type = self.context.i16_type();
        let i32_type = self.context.i32_type();
        let i64_type = self.context.i64_type();
        let i128_type = self.context.i128_type();

        let mut offset = self.context.i32_type().const_zero();

        // Step 1: Copy 16 bytes at a time (i128) — Looping
        let chunk_count = size / 16;
        for _ in 0..chunk_count {
            let src_ptr = unsafe {
                self.builder
                    .build_in_bounds_gep(i8_type, src, &[offset], "")?
            };
            let dest_ptr = unsafe {
                self.builder
                    .build_in_bounds_gep(i8_type, dest, &[offset], "")?
            };

            let value = self.builder.build_load(i128_type, src_ptr, "")?;
            self.builder.build_store(dest_ptr, value);

            offset = self.builder.build_int_add(
                offset,
                self.context.i32_type().const_int(16, false),
                "",
            )?;
        }

        // Step 2: Copy 8 bytes (i64)
        if size % 16 >= 8 {
            let src_ptr = unsafe {
                self.builder
                    .build_in_bounds_gep(i8_type, src, &[offset], "")?
            };
            let dest_ptr = unsafe {
                self.builder
                    .build_in_bounds_gep(i8_type, dest, &[offset], "")?
            };

            let value = self.builder.build_load(i64_type, src_ptr, "")?;
            self.builder.build_store(dest_ptr, value)?;

            offset = self.builder.build_int_add(
                offset,
                self.context.i32_type().const_int(8, false),
                "",
            )?;
        }

        // Step 3: Copy 4 bytes (i32)
        if size % 8 >= 4 {
            let src_ptr = unsafe {
                self.builder
                    .build_in_bounds_gep(i8_type, src, &[offset], "")?
            };
            let dest_ptr = unsafe {
                self.builder
                    .build_in_bounds_gep(i8_type, dest, &[offset], "")?
            };

            let value = self.builder.build_load(i32_type, src_ptr, "")?;
            self.builder.build_store(dest_ptr, value)?;

            offset = self.builder.build_int_add(
                offset,
                self.context.i32_type().const_int(4, false),
                "",
            )?;
        }

        // Step 4: Copy 2 bytes (i16)
        if size % 4 >= 2 {
            let src_ptr = unsafe {
                self.builder
                    .build_in_bounds_gep(i8_type, src, &[offset], "")?
            };
            let dest_ptr = unsafe {
                self.builder
                    .build_in_bounds_gep(i8_type, dest, &[offset], "")?
            };

            let value = self.builder.build_load(i16_type, src_ptr, "")?;
            self.builder.build_store(dest_ptr, value)?;

            offset = self.builder.build_int_add(
                offset,
                self.context.i32_type().const_int(2, false),
                "",
            )?;
        }

        // Step 5: Copy the final byte (i8)
        if size % 2 == 1 {
            let src_ptr = unsafe {
                self.builder
                    .build_in_bounds_gep(i8_type, src, &[offset], "")?
            };
            let dest_ptr = unsafe {
                self.builder
                    .build_in_bounds_gep(i8_type, dest, &[offset], "")?
            };

            let value = self.builder.build_load(i8_type, src_ptr, "")?;
            self.builder.build_store(dest_ptr, value)?;
        }
        Ok(())
    }
}
