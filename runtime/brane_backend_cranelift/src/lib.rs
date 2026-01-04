use brane_core::runtime::{JitBackend, LoadedModule as _, ModuleId, Runtime};
use cranelift_codegen::ir::immediates::V128Imm;
use std::collections::HashMap;
use std::ptr::NonNull;
use std::sync::atomic::AtomicUsize;
use std::sync::{Arc, RwLock};
use target_lexicon::{Architecture, CallingConvention};

use anyhow::{Result, anyhow, bail};
use brane_core::{ir, memory};
use cranelift_codegen::ir::{
    ArgumentPurpose, BlockArg, ConstantData, Endianness, Signature, StackSlotData, StackSlotKind,
};
use cranelift_codegen::{
    isa,
    settings::{self},
};
use cranelift_frontend::Switch;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, FuncId, Linkage, Module, ModuleError};

use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::types;
use cranelift_codegen::ir::{AbiParam, Block, InstBuilder, MemFlags, Type, Value};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};

pub use cranelift_jit;
pub use cranelift_module;

pub struct CraneliftJitBackend {
    pub isa: isa::OwnedTargetIsa,
    pub next_module_id: AtomicUsize,
    pub loaded_modules: RwLock<HashMap<ModuleId, Arc<LoadedModule>>>,
}

impl Default for CraneliftJitBackend {
    fn default() -> Self {
        let flag_builder = settings::builder();
        //flag_builder.set("use_colocated_libcalls", "false").unwrap();
        //flag_builder.set("is_pic", "true").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .expect("Unable to build ISA");

        Self {
            isa,
            next_module_id: AtomicUsize::new(0),
            loaded_modules: Default::default(),
        }
    }
}

pub struct LoadedModule {
    pub functions: Vec<memory::FnId>,
    /// Maps a function name to an index in functions
    pub name_to_function: HashMap<String, usize>,
    pub jit_module: cranelift_jit::JITModule,
}

impl JitBackend for CraneliftJitBackend {
    type ModuleTy = LoadedModule;

    fn load(
        &self,
        module: ir::Module,
        rt: &Runtime<Self>,
    ) -> anyhow::Result<brane_core::runtime::ModuleId> {
        let module = self.jit(module, rt)?;
        let id = self
            .next_module_id
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        self.loaded_modules
            .write()
            .unwrap()
            .insert(id, Arc::new(module));
        Ok(id)
    }

    fn get_module(&self, module: ModuleId) -> Option<Arc<Self::ModuleTy>> {
        self.loaded_modules.read().unwrap().get(&module).cloned()
    }
}

impl brane_core::runtime::LoadedModule for LoadedModule {
    fn get_fn(&self, id: ir::FnId) -> Option<memory::FnId> {
        self.functions.get(id as usize).cloned()
    }

    fn get_fn_by_name(&self, name: impl AsRef<str>) -> Option<memory::FnId> {
        let name = name.as_ref();
        self.functions
            .get(self.name_to_function.get(name).copied()?)
            .copied()
    }

    fn get_fn_names(&self) -> impl Iterator<Item = &str> {
        self.name_to_function.iter().map(|(name, _)| name.as_str())
    }
}

impl CraneliftJitBackend {
    pub fn jit(&self, module: ir::Module, rt: &Runtime<Self>) -> Result<LoadedModule> {
        let jit_module = JITModule::new(JITBuilder::with_isa(
            self.isa.clone(),
            cranelift_module::default_libcall_names(),
        ));

        let linked_modules = module
            .imports
            .iter()
            .map(|uri| {
                let id = rt
                    .find_module_by_uri(uri)
                    .ok_or_else(|| anyhow!("Could not link to module {}", uri))?;
                rt.get_module(id)
                    .ok_or_else(|| anyhow!("Could not link to module {}", uri))
            })
            .collect::<Result<Vec<_>>>()?;

        let linked_func_indicies = module
            .external_functions
            .iter()
            .map(|f| {
                let ext_module = linked_modules
                    .get(f.module as usize)
                    .ok_or_else(|| anyhow!("Invalid import id in {}", f.module))?;
                let id = ext_module
                    .get_fn_by_name(&f.id)
                    .ok_or_else(|| anyhow!("Invalid fn name in {}", f))?;
                Ok((id, f.sig.clone()))
            })
            .collect::<Result<Vec<_>>>()?;

        let mut codegen = jit_module.make_context();
        let mut builder_ctx = FunctionBuilderContext::new();
        let mut ctx = ModuleCtx {
            ptr_ty: jit_module.target_config().pointer_type(),
            ptr_bytes: jit_module.target_config().pointer_bytes(),
            module: jit_module,
            fn_map: Vec::new(),
            data_desc: DataDescription::new(),
            linked_func_indicies,
            rt,
        };

        let mut signatures = Vec::new();
        for func in module.functions.iter() {
            let (signature, has_ret_ptr) = self.construct_fn_sig(&func.sig, &module)?;
            let fn_id = ctx
                .module
                .declare_function(&func.id, Linkage::Export, &signature)
                .map_err(|e| anyhow!("Failed to declare function {}", e))?;
            //println!("Declared function `{}` with sig {}", func.id, signature);
            ctx.fn_map.push((func.id.clone(), fn_id));
            signatures.push((signature, has_ret_ptr));
        }

        for (index, (func, (sig, has_ret_ptr))) in
            module.functions.iter().zip(signatures).enumerate()
        {
            let fn_id = ctx.fn_map[index].1;
            self.jit_fn(
                func,
                sig,
                has_ret_ptr,
                &module,
                &mut ctx,
                &mut codegen,
                &mut builder_ctx,
            )?;
            ctx.module
                .define_function(fn_id, &mut codegen)
                .map_err(|e: ModuleError| {
                    match e {
                        ModuleError::Compilation(ce) => match ce {
                            cranelift_codegen::CodegenError::Verifier(verifier_errors) => {
                                anyhow!(
                                    "Verifier errors:\n{}",
                                    verifier_errors
                                        .0
                                        .iter()
                                        .map(|ve| ve.to_string())
                                        .collect::<Vec<_>>()
                                        .join("\n")
                                )
                            }
                            ce => anyhow!("Codegen error: {}", ce),
                        },
                        e => anyhow!("Failed to define function {}", e),
                    }
                    .context(codegen.func.clone())
                })?;
            let cs = self.isa.to_capstone()?;
            /* TODO enable with flags
             * println!(
                "Emitted function {} (in map as ({}, {})):\n{}",
                func.id,
                ctx.fn_map[index].0,
                ctx.fn_map[index].1,
                codegen
                    .compiled_code()
                    .unwrap()
                    .disassemble(Some(&codegen.func.params), &cs)?
            );*/
            ctx.module.clear_context(&mut codegen);
        }

        ctx.module.finalize_definitions()?;

        let (functions, name_to_function) = ctx.fn_map.into_iter().fold(
            (Vec::new(), HashMap::new()),
            |(mut functions, mut name_to_function), (name, func)| {
                let name_id = functions.len();
                unsafe {
                    let fn_ptr = NonNull::new_unchecked(std::mem::transmute(
                        ctx.module.get_finalized_function(func),
                    ));
                    let fn_index = rt.store.expose_fn(fn_ptr);
                    functions.push(fn_index);
                }
                name_to_function.insert(name, name_id);
                (functions, name_to_function)
            },
        );

        Ok(LoadedModule {
            functions,
            jit_module: ctx.module,
            name_to_function,
        })
    }

    fn jit_fn(
        &self,
        func: &ir::Function,
        sig: Signature,
        ret_ptr: bool,
        ir: &ir::Module,
        module: &mut ModuleCtx,
        codegen: &mut cranelift_codegen::Context,
        builder_ctx: &mut FunctionBuilderContext,
    ) -> Result<()> {
        let ptr_bytes = module.ptr_bytes;
        let fn_ctx = &mut codegen.func;
        let ptr_ty = module.module.target_config().pointer_type();

        // add memory binding table
        fn_ctx.collect_debug_info();

        let mut builder = FunctionBuilder::new(fn_ctx, builder_ctx);
        builder.func.signature = sig;

        let mut block_args = HashMap::new();
        let blocks: Vec<_> = func
            .blocks
            .iter()
            .map(|b| {
                let n = builder.create_block();
                let mut args = Vec::new();
                for phi in &b.phi_nodes {
                    let ty = Self::jit_ty(&phi.ty);
                    let arg_v = builder.append_block_param(n, ty);
                    args.push(FValue::new(arg_v, ty));
                }
                block_args.insert(n, args);
                n
            })
            .collect();

        if blocks.is_empty() {
            bail!("Function has no content!");
        }

        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        /*
        println!(
            "jitting fn {} with sig: {}",
            func.id, builder.func.signature
        );
        */

        let fn_params = builder.block_params(entry);
        let (ret_ptr, ctx_ptr, fn_params) = if ret_ptr {
            (Some(fn_params[0]), fn_params[1], fn_params[2..].to_owned())
        } else {
            (None, fn_params[0], fn_params[1..].to_owned())
        };

        let fn_args =
            { self.reconstruct_fn_args(&fn_params, ret_ptr.is_some(), func, ir, &mut builder)? };

        builder.ins().jump(blocks[0], &[]);
        builder.seal_block(entry);

        let mut ctx = FunctionCtx {
            mod_ctx: module,
            builder,
            entry_block_id: entry,
            block_values: Default::default(),
            blocks: blocks.clone(),
            ptr_ty,
            ptr_bytes,
            fn_args,
            ctx_ptr,
            ret_ptr,
            block_args,
        };

        for (block_id, (jit_block, ir_block)) in blocks.iter().zip(func.blocks.iter()).enumerate() {
            ctx.block_values.insert(
                Block::from_u32(block_id as u32),
                Vec::with_capacity(ir_block.ops.len()),
            );
            ctx.builder.switch_to_block(*jit_block);
            for op in &ir_block.ops {
                let new_result = match op {
                    ir::Op::Unary { op, value } => {
                        let (value, _, _) = self.jit_value(value, &mut ctx)?; /*TODO make this actually call an allocation callback*/
                        let res = match op {
                            ir::UnaryOp::INeg => ctx.builder.ins().ineg(value),
                            ir::UnaryOp::FNeg => ctx.builder.ins().fneg(value),
                            // TODO swap with call to runtime linked function from runtime
                            ir::UnaryOp::Alloc => ctx.builder.ins().iconst(types::I32, 032),
                        };
                        let ty = ctx.builder.func.dfg.value_type(res);
                        Some(FValue::new(res, ty))
                    }
                    ir::Op::Binary { op, left, right } => {
                        let (left, _, _) = self.jit_value(left, &mut ctx)?;
                        let (right, _, _) = self.jit_value(right, &mut ctx)?;
                        let res = match op {
                            ir::BinaryOp::IAdd => ctx.builder.ins().iadd(left, right),
                            ir::BinaryOp::FAdd => ctx.builder.ins().fadd(left, right),
                            ir::BinaryOp::ISub => ctx.builder.ins().isub(left, right),
                            ir::BinaryOp::FSub => ctx.builder.ins().fsub(left, right),
                            ir::BinaryOp::IMul => ctx.builder.ins().imul(left, right),
                            ir::BinaryOp::FMul => ctx.builder.ins().fmul(left, right),
                            ir::BinaryOp::SDiv => ctx.builder.ins().sdiv(left, right),
                            ir::BinaryOp::UDiv => ctx.builder.ins().udiv(left, right),
                            ir::BinaryOp::FDiv => ctx.builder.ins().fdiv(left, right),
                            ir::BinaryOp::URem => ctx.builder.ins().urem(left, right),
                            ir::BinaryOp::SRem => ctx.builder.ins().srem(left, right),
                            ir::BinaryOp::SCmp(cmp_ty) => ctx.builder.ins().icmp(
                                match cmp_ty {
                                    ir::CmpTy::Eq => IntCC::Equal,
                                    ir::CmpTy::Ne => IntCC::NotEqual,
                                    ir::CmpTy::Gt => IntCC::SignedGreaterThan,
                                    ir::CmpTy::Ge => IntCC::SignedGreaterThanOrEqual,
                                },
                                left,
                                right,
                            ),
                            ir::BinaryOp::UCmp(cmp_ty) => ctx.builder.ins().icmp(
                                match cmp_ty {
                                    ir::CmpTy::Eq => IntCC::Equal,
                                    ir::CmpTy::Ne => IntCC::NotEqual,
                                    ir::CmpTy::Gt => IntCC::UnsignedGreaterThan,
                                    ir::CmpTy::Ge => IntCC::UnsignedGreaterThanOrEqual,
                                },
                                left,
                                right,
                            ),
                            ir::BinaryOp::FCmp(cmp_ty) => ctx.builder.ins().fcmp(
                                match cmp_ty {
                                    ir::CmpTy::Eq => FloatCC::Equal,
                                    ir::CmpTy::Ne => FloatCC::NotEqual,
                                    ir::CmpTy::Gt => FloatCC::GreaterThan,
                                    ir::CmpTy::Ge => FloatCC::GreaterThanOrEqual,
                                },
                                left,
                                right,
                            ),
                            ir::BinaryOp::And => ctx.builder.ins().band(left, right),
                            ir::BinaryOp::Or => ctx.builder.ins().bor(left, right),
                            ir::BinaryOp::Xor => ctx.builder.ins().bxor(left, right),
                            ir::BinaryOp::ShiftL => ctx.builder.ins().ishl(left, right),
                            ir::BinaryOp::IShiftR => ctx.builder.ins().sshr(left, right),
                            ir::BinaryOp::UShiftR => ctx.builder.ins().ushr(left, right),
                        };
                        let ty = ctx.builder.func.dfg.value_type(res);
                        Some(FValue::new(res, ty))
                    }
                    ir::Op::Load { ty, ptr } => {
                        let jit_ty = Self::jit_ty(ty);
                        let (ptr, _, _) = self.jit_value(ptr, &mut ctx)?;
                        Some(FValue::new(Self::load(ptr, jit_ty, &mut ctx), jit_ty))
                    }
                    ir::Op::Store { src, ptr } => {
                        let (src, _, _) = self.jit_value(src, &mut ctx)?;
                        let (ptr, _, _) = self.jit_value(ptr, &mut ctx)?;
                        Self::store(ptr, src, &mut ctx);
                        None
                    }
                    ir::Op::Call { func, input } => {
                        if *func >= 0 {
                            self.call_fn(Callable::FnId(*func), &input, ir, &mut ctx)?
                        } else {
                            // Call from known index in the pointer table
                            let fn_index = func * -1 + 1;
                            let (fn_index, fn_sig) = ctx
                                .mod_ctx
                                .linked_func_indicies
                                .get(fn_index as usize)
                                .ok_or_else(|| {
                                    anyhow!("Invalid imported function index: {}", fn_index)
                                })?
                                .clone();

                            let entry = ctx.entry_block_id;
                            let exe_ctx = ctx.builder.block_params(entry)[0];
                            let fn_table_ptr = ctx.builder.ins().load(
                                ctx.ptr_ty,
                                MemFlags::trusted(),
                                exe_ctx,
                                ctx.ptr_ty.bytes() as i32,
                            );

                            let fn_ptr = ctx.builder.ins().load(
                                ctx.ptr_ty,
                                MemFlags::trusted(),
                                fn_table_ptr,
                                fn_index as i32 * ctx.ptr_ty.bytes() as i32,
                            );

                            self.call_fn(Callable::Pointer(fn_ptr, &fn_sig), &input, ir, &mut ctx)?
                        }
                    }
                    ir::Op::CallIndirect {
                        func_handle,
                        input,
                        sig,
                    } => {
                        let (func, _, _) = self.jit_value(func_handle, &mut ctx)?;

                        let entry = ctx.entry_block_id;
                        let exe_ctx = ctx.builder.block_params(entry)[0];
                        let fn_table_ptr = ctx.builder.ins().load(
                            ctx.ptr_ty,
                            MemFlags::trusted(),
                            exe_ctx,
                            ctx.ptr_ty.bytes() as i32,
                        );

                        let fn_ptr_offset =
                            ctx.builder.ins().imul_imm(func, ctx.ptr_ty.bytes() as i64);
                        let fn_ptr = ctx.builder.ins().iadd(fn_table_ptr, fn_ptr_offset);

                        let fn_ptr =
                            ctx.builder
                                .ins()
                                .load(ctx.ptr_ty, MemFlags::trusted(), fn_ptr, 0);
                        self.call_fn(Callable::Pointer(fn_ptr, sig), &input, ir, &mut ctx)?
                    }
                };
                ctx.block_values
                    .get_mut(&Block::from_u32(block_id as u32))
                    .unwrap()
                    .push(new_result);
            }
            match &ir_block.terminator {
                ir::TermOp::Jump { block } => {
                    let args = self.jump_args(block_id as u32, *block, func, &mut ctx)?;
                    let b = ctx.get_block(*block)?;
                    ctx.builder.ins().jump(b, args.as_slice());
                }
                ir::TermOp::JumpIf { cond, then, else_ } => {
                    let (cond, _, _) = self.jit_value(cond, &mut ctx)?;
                    let then_args = self.jump_args(block_id as u32, *then, func, &mut ctx)?;
                    let else_args = self.jump_args(block_id as u32, *else_, func, &mut ctx)?;
                    let then = ctx.get_block(*then)?;
                    let else_ = ctx.get_block(*else_)?;
                    ctx.builder.ins().brif(
                        cond,
                        then,
                        then_args.as_slice(),
                        else_,
                        else_args.as_slice(),
                    );
                }
                ir::TermOp::JumpMap {
                    cond,
                    branches,
                    default,
                } => {
                    let mut switch = Switch::new();
                    let default = ctx.get_block(*default)?;
                    for (cond, block) in branches {
                        let block = ctx.get_block(*block)?;
                        switch.set_entry(*cond, block);
                    }
                    let (cond, _, _) = self.jit_value(cond, &mut ctx)?;
                    switch.emit(&mut ctx.builder, cond, default);
                }
                ir::TermOp::Ret(value) => match value {
                    Some(values) => {
                        let values = values
                            .iter()
                            .map(|value| Ok(self.jit_value(value, &mut ctx)?.0))
                            .collect::<anyhow::Result<Vec<Value>>>()?;
                        self.jit_return(values, func, ir, &mut ctx)?;
                    }
                    None => {
                        ctx.builder.ins().return_(&[]);
                    }
                },
            };
        }
        ctx.builder.seal_all_blocks();
        ctx.builder.finalize();
        Ok(())
    }

    fn load(int_ptr: Value, ty: Type, ctx: &mut FunctionCtx) -> Value {
        let binding = ctx.builder.ins().ushr_imm(int_ptr, 16);
        let binding = ctx.builder.ins().uextend(ctx.ptr_ty, binding);
        let offset = ctx.builder.ins().band_imm(int_ptr, 0xFFFF);
        let offset = ctx.builder.ins().uextend(ctx.ptr_ty, offset);

        let entry = ctx.entry_block_id;
        let exe_ctx = ctx.builder.block_params(entry)[0];
        let bindings = ctx
            .builder
            .ins()
            .load(ctx.ptr_ty, MemFlags::trusted(), exe_ctx, 0);

        let mul_shift = match ctx.ptr_bytes {
            1 => 0,
            2 => 1,
            4 => 2,
            8 => 3,
            _ => unreachable!(),
        };
        let binding_offset = ctx.builder.ins().ishl_imm(binding, mul_shift);
        let binding_ptr = ctx.builder.ins().iadd(bindings, binding_offset);

        let page_ptr = ctx
            .builder
            .ins()
            .load(ctx.ptr_ty, MemFlags::trusted(), binding_ptr, 0);
        let value_ptr = ctx.builder.ins().iadd(page_ptr, offset);
        ctx.builder
            .ins()
            .load(ty, MemFlags::trusted(), value_ptr, 0)
    }

    fn store(int_ptr: Value, value: Value, ctx: &mut FunctionCtx) {
        let binding = ctx.builder.ins().ushr_imm(int_ptr, 16);
        let binding = ctx.builder.ins().uextend(ctx.ptr_ty, binding);
        let offset = ctx.builder.ins().band_imm(int_ptr, 0xFFFF);
        let offset = ctx.builder.ins().uextend(ctx.ptr_ty, offset);

        let entry = ctx.entry_block_id;
        let exe_ctx = ctx.builder.block_params(entry)[0];
        let bindings = ctx
            .builder
            .ins()
            .load(ctx.ptr_ty, MemFlags::trusted(), exe_ctx, 0);

        let mul_shift = match ctx.ptr_bytes {
            1 => 0,
            2 => 1,
            4 => 2,
            8 => 3,
            _ => unreachable!(),
        };
        let binding_offset = ctx.builder.ins().ishl_imm(binding, mul_shift);
        let binding_ptr = ctx.builder.ins().iadd(bindings, binding_offset);

        let page_ptr = ctx
            .builder
            .ins()
            .load(ctx.ptr_ty, MemFlags::trusted(), binding_ptr, 0);
        let value_ptr = ctx.builder.ins().iadd(page_ptr, offset);
        ctx.builder
            .ins()
            .store(MemFlags::trusted(), value, value_ptr, 0);
    }

    fn jump_args(
        &self,
        from: u32,
        to: u32,
        func: &ir::Function,
        ctx: &mut FunctionCtx,
    ) -> Result<Vec<BlockArg>> {
        let dest_block = func
            .blocks
            .get(to as usize)
            .ok_or_else(|| anyhow!("Invalid block id {} in jump from block {}", to, from))?;
        dest_block
            .phi_nodes
            .iter()
            .filter_map(|node| {
                node.variants
                    .iter()
                    .find(|v| v.block == from)
                    .map(|v| -> Result<BlockArg> {
                        Ok(BlockArg::Value(self.jit_value(&v.value, ctx)?.0))
                    })
            })
            .collect::<Result<Vec<_>>>()
    }

    fn jit_value(&self, value: &ir::Value, ctx: &mut FunctionCtx) -> Result<(Value, Type, usize)> {
        match value {
            ir::Value::PhiArg { block, arg } => ctx
                .block_args
                .get(&Block::from_u32(*block))
                .map(|args| args.get(*arg as usize))
                .flatten()
                .map(|fv| fv.values.get(0))
                .flatten()
                .cloned()
                .ok_or_else(|| anyhow!("invalid phi node {}", value)),
            ir::Value::FnArg(arg) => ctx
                .fn_args
                .get(*arg as usize)
                .map(|fv| fv.values.get(0))
                .flatten()
                .cloned()
                .ok_or_else(|| anyhow!("invalid function arg {}", value)),
            ir::Value::ObjFnArg(arg, index) => ctx
                .fn_args
                .get(*arg as usize)
                .map(|fv| fv.values.get(*index as usize))
                .flatten()
                .cloned()
                .ok_or_else(|| anyhow!("invalid function arg {}", arg)),
            ir::Value::BlockOp { block, op } => ctx
                .block_values
                .get(&Block::from_u32(*block))
                .map(|ops| ops.get(*op as usize).cloned())
                .flatten()
                .flatten()
                .map(|fv| fv.values.get(0).cloned())
                .flatten()
                .ok_or_else(|| anyhow!("Value is not a block op: {}", value)),
            ir::Value::ObjBlockOp { block, op, index } => ctx
                .block_values
                .get(&Block::from_u32(*block))
                .map(|ops| ops.get(*op as usize).cloned())
                .flatten()
                .flatten()
                .map(|fv| fv.values.get(*index as usize).cloned())
                .flatten()
                .ok_or_else(|| anyhow!("Value is not a block op: {}", value)),
            ir::Value::Const(const_value) => Ok(match const_value {
                ir::ConstValue::Bool(v) => {
                    (ctx.builder.ins().iconst(types::I8, *v as i64), types::I8, 0)
                }
                ir::ConstValue::U8(v) => {
                    (ctx.builder.ins().iconst(types::I8, *v as i64), types::I8, 0)
                }
                ir::ConstValue::I8(v) => {
                    (ctx.builder.ins().iconst(types::I8, *v as i64), types::I8, 0)
                }
                ir::ConstValue::U16(v) => (
                    ctx.builder.ins().iconst(types::I16, *v as i64),
                    types::I16,
                    0,
                ),
                ir::ConstValue::I16(v) => (
                    ctx.builder.ins().iconst(types::I16, *v as i64),
                    types::I16,
                    0,
                ),
                ir::ConstValue::U32(v) => (
                    ctx.builder.ins().iconst(types::I32, *v as i64),
                    types::I32,
                    0,
                ),
                ir::ConstValue::I32(v) => (
                    ctx.builder.ins().iconst(types::I32, *v as i64),
                    types::I32,
                    0,
                ),
                ir::ConstValue::U64(v) => (
                    ctx.builder.ins().iconst(types::I64, *v as i64),
                    types::I64,
                    0,
                ),
                ir::ConstValue::I64(v) => (
                    ctx.builder.ins().iconst(types::I64, *v as i64),
                    types::I64,
                    0,
                ),
                ir::ConstValue::F32(v) => (ctx.builder.ins().f32const(*v), types::F32, 0),
                ir::ConstValue::F64(v) => (ctx.builder.ins().f64const(*v), types::F64, 0),
            }),
        }
    }

    fn jit_ty(nt: &ir::NativeType) -> Type {
        match nt {
            ir::NativeType::Bool | ir::NativeType::U8 | ir::NativeType::I8 => types::I8,
            ir::NativeType::U16 | ir::NativeType::I16 => types::I16,
            ir::NativeType::U32 | ir::NativeType::I32 | ir::NativeType::Ptr(_, _) => types::I32,
            ir::NativeType::F32 => types::F32,
            ir::NativeType::U64 | ir::NativeType::I64 => types::I64,
            ir::NativeType::F64 => types::F64,
        }
    }

    fn construct_fn_sig(
        &self,
        sig_src: &ir::FnSig,
        ir: &ir::Module,
    ) -> anyhow::Result<(Signature, bool)> {
        let triple = self.isa.triple();
        let mut has_ret_ptr = false;

        match triple
            .default_calling_convention()
            .expect("No default calling convention for this platform")
        {
            CallingConvention::WindowsFastcall => {
                assert_eq!(
                    triple.architecture,
                    Architecture::X86_64,
                    "We only have X86_64 windows call convention support"
                );
                // Based off of: https://learn.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-170
                let mut sig = Signature::new(isa::CallConv::WindowsFastcall);

                if let Some(ret_ty) = &sig_src.ret_ty {
                    let mut ret_struct = |size: usize| {
                        //println!("sig for returning struct of size {}", size);
                        match size {
                            0 => unreachable!(),
                            1 => sig.returns.push(AbiParam::new(types::I8)),
                            2 => sig.returns.push(AbiParam::new(types::I16)),
                            3 | 4 => sig.returns.push(AbiParam::new(types::I32)),
                            5..=8 => sig.returns.push(AbiParam::new(types::I64)),
                            _ => {
                                let ret_param = AbiParam::new(self.isa.pointer_type());
                                // Windows wants us to pass a pointer, and then return
                                // that same pointer back when we return
                                sig.params.push(ret_param);
                                sig.returns.push(ret_param);
                                has_ret_ptr = true;
                            }
                        }
                    };

                    match ret_ty {
                        ir::Ty::Native(nt) => {
                            sig.returns.push(AbiParam::new(Self::jit_ty(nt)));
                        }
                        ir::Ty::Struct(id) => {
                            let layout = ir
                                .get_struct(*id)
                                .ok_or(anyhow!("Invalid struct id {}", id))?
                                .layout(ir)?;

                            ret_struct(layout.size)
                        }
                        ir::Ty::Enum(id) => {
                            let layout = ir
                                .get_enum(*id)
                                .ok_or(anyhow!("Invalid enum id {}", id))?
                                .layout(ir)?;

                            ret_struct(layout.size)
                        }
                    }
                }

                // ctx pointer
                sig.params.push(AbiParam::new(self.isa.pointer_type()));

                for param in &sig_src.param_tys {
                    let mut pass_struct = |size: usize| {
                        //println!("passing struct of size {} as arg", size);
                        match size {
                            0 => unreachable!(),
                            1 => sig.params.push(AbiParam::new(types::I8)),
                            2 => sig.params.push(AbiParam::new(types::I16)),
                            3 | 4 => sig.params.push(AbiParam::new(types::I32)),
                            5..=8 => sig.params.push(AbiParam::new(types::I64)),
                            _ => {
                                let struct_param = AbiParam::new(self.isa.pointer_type());
                                sig.params.push(struct_param);
                            }
                        }
                    };

                    match param {
                        ir::Ty::Native(native_type) => {
                            sig.params.push(AbiParam::new(Self::jit_ty(native_type)));
                        }
                        ir::Ty::Struct(id) => {
                            let layout = ir
                                .get_struct(*id)
                                .ok_or(anyhow!("Invalid struct id {}", id))?
                                .layout(ir)?;
                            pass_struct(layout.size)
                        }
                        ir::Ty::Enum(id) => {
                            let layout = ir
                                .get_enum(*id)
                                .ok_or(anyhow!("Invalid enum id {}", id))?
                                .layout(ir)?;
                            pass_struct(layout.size)
                        }
                    }
                }

                Ok((sig, has_ret_ptr))
            }
            CallingConvention::SystemV => match &triple.architecture {
                Architecture::X86_64 => {
                    // Based off of: https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf
                    let mut sig = Signature::new(isa::CallConv::SystemV);

                    // If the class is INTEGER, the next available register of the sequence %rdi,
                    // %rsi, %rdx, %rcx, %r8 and %r9 is used.
                    let mut rem_general = 6i32;
                    // If the class is SSE, the next available vector register is used, the registers
                    // are taken in the order from %xmm0 to %xmm7.
                    let mut rem_vector = 8i32;

                    if let Some(ret_ty) = &sig_src.ret_ty {
                        match ret_ty {
                            ir::Ty::Native(native_type) => {
                                sig.returns.push(AbiParam::new(Self::jit_ty(native_type)))
                            }
                            ir::Ty::Struct(_) | ir::Ty::Enum(_) => {
                                let (size, _) = ir.size_align(ret_ty)?;
                                let mut ret_as_ptr = || {
                                    let ret_ptr = AbiParam::new(self.isa.pointer_type());
                                    sig.returns.push(ret_ptr);
                                    sig.params.push(ret_ptr);
                                    rem_general = 0.max(rem_general - 1);
                                    has_ret_ptr = true;
                                };

                                if size > 16 {
                                    ret_as_ptr();
                                } else {
                                    let mut arg_classes = [ArgClass::NoClass, ArgClass::NoClass];
                                    ArgClass::ty_arg_class(ret_ty, ir, &mut arg_classes)?;
                                    if arg_classes.contains(&ArgClass::Memory) {
                                        ret_as_ptr();
                                    } else {
                                        for c in arg_classes {
                                            match c {
                                                ArgClass::NoClass => {}
                                                ArgClass::General => {
                                                    sig.returns.push(AbiParam::new(types::I64))
                                                }
                                                ArgClass::Vector => {
                                                    sig.returns.push(AbiParam::new(types::F64X2))
                                                }
                                                ArgClass::Memory => unreachable!(),
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // ctx pointer
                    sig.params.push(AbiParam::new(self.isa.pointer_type()));
                    rem_general -= 1;

                    for param in &sig_src.param_tys {
                        match param {
                            ir::Ty::Native(native_type) => {
                                let ty = Self::jit_ty(native_type);
                                sig.params.push(AbiParam::new(ty));
                                if ty.is_int() {
                                    rem_general = 0.max(rem_general - 1);
                                } else {
                                    rem_vector = 0.max(rem_vector - 1);
                                }
                            }
                            ir::Ty::Struct(_) | ir::Ty::Enum(_) => {
                                let (size, _) = ir.size_align(param)?;
                                let mut pass_as_mem = || {
                                    sig.params.push(AbiParam::special(
                                        self.isa.pointer_type(),
                                        ArgumentPurpose::StructArgument(size as u32),
                                    ));
                                };

                                if size > 16 {
                                    pass_as_mem();
                                } else {
                                    let mut arg_classes = [ArgClass::NoClass, ArgClass::NoClass];
                                    ArgClass::ty_arg_class(param, ir, &mut arg_classes)?;
                                    let mut general_consumed = 0;
                                    let mut vector_consumed = 0;
                                    for c in &arg_classes {
                                        match c {
                                            ArgClass::NoClass | ArgClass::Memory => {}
                                            ArgClass::General => general_consumed += 1,
                                            ArgClass::Vector => vector_consumed += 1,
                                        }
                                    }

                                    if general_consumed > rem_general
                                        || vector_consumed > rem_vector
                                        || arg_classes.contains(&ArgClass::Memory)
                                    {
                                        pass_as_mem();
                                    } else {
                                        for c in arg_classes {
                                            match c {
                                                ArgClass::NoClass => {}
                                                ArgClass::General => {
                                                    sig.params.push(AbiParam::new(types::I64))
                                                }
                                                ArgClass::Vector => {
                                                    sig.params.push(AbiParam::new(types::F64X2))
                                                }
                                                ArgClass::Memory => unreachable!(),
                                            }
                                        }
                                        if general_consumed >= rem_general {
                                            rem_general = 0;
                                        } else {
                                            rem_general -= general_consumed;
                                        }
                                        if vector_consumed >= rem_vector {
                                            rem_vector = 0;
                                        } else {
                                            rem_vector -= vector_consumed;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    Ok((sig, has_ret_ptr))
                }
                Architecture::Aarch64(_) => {
                    // Based off of: https://c9x.me/compile/bib/abi-arm64.pdf
                    let mut sig = Signature::new(isa::CallConv::SystemV);

                    // Next General-purpose Register Number

                    if let Some(ret_ty) = &sig_src.ret_ty {
                        match ret_ty {
                            ir::Ty::Native(native_type) => {
                                sig.returns.push(AbiParam::new(Self::jit_ty(native_type)));
                            }
                            ir::Ty::Struct(_) | ir::Ty::Enum(_) => {
                                if let Some((ty, fields)) =
                                    FValue::is_homogeneous_simd_aggregate(ret_ty, ir)
                                    && fields <= 4
                                {
                                    // Yes, amd64 can return up to four (4) vector/float registers
                                    for _ in 0..fields {
                                        sig.returns.push(AbiParam::new(ty));
                                    }
                                } else {
                                    let (size, _) = ir.size_align(ret_ty)?;
                                    match size {
                                        0..=8 => sig.returns.push(AbiParam::new(types::I64)),
                                        9..=16 => {
                                            sig.returns.push(AbiParam::new(types::I64));
                                            sig.returns.push(AbiParam::new(types::I64));
                                        }
                                        17.. => {
                                            let ret_ptr = AbiParam::special(
                                                self.isa.pointer_type(),
                                                ArgumentPurpose::StructReturn,
                                            );
                                            sig.params.push(ret_ptr);
                                            has_ret_ptr = true;
                                        }
                                    };
                                }
                            }
                        }
                    }

                    sig.params.push(AbiParam::new(self.isa.pointer_type()));
                    let mut ngrn = 1;
                    let mut nsrn = 0;

                    for param in &sig_src.param_tys {
                        match param {
                            ir::Ty::Native(native_type) => {
                                let ty = Self::jit_ty(native_type);
                                sig.params.push(AbiParam::new(ty));
                                if ty.is_int() {
                                    ngrn = 8.min(ngrn + 1);
                                } else {
                                    nsrn = 8.min(nsrn + 1);
                                }
                            }
                            ir::Ty::Struct(_) | ir::Ty::Enum(_) => {
                                if let Some((ty, fields)) =
                                    FValue::is_homogeneous_simd_aggregate(param, ir)
                                    && fields <= 4
                                {
                                    // Intentionally overflow vector registers to the stack
                                    if nsrn + fields > 8 {
                                        while nsrn != 8 {
                                            sig.params.push(AbiParam::new(types::F32));
                                            nsrn += 1;
                                        }
                                    }

                                    for _ in 0..fields {
                                        if nsrn < 8 || ty.bytes() >= 8 {
                                            sig.params.push(AbiParam::new(ty));
                                        } else {
                                            // Values passed on the stack must be a multiple of 8bytes wide
                                            // This is such a hack, but since we know at this point
                                            // all vector register things will be passed on the
                                            // stack, we're basically treating this as a general
                                            // register and bitcasting/packing to and from it.
                                            sig.params.push(AbiParam::new(types::F64))
                                        }
                                        nsrn = 8.min(nsrn + 1);
                                    }
                                } else {
                                    let (size, align) = ir.size_align(param)?;
                                    if size <= 16 {
                                        if ngrn == 7 && size > 8 {
                                            // Alignment padding
                                            sig.params.push(AbiParam::new(types::I64));
                                            ngrn = 8.min(ngrn + 1);
                                        }
                                        let use_128 = if align == 16 {
                                            // Align NGRN by 2
                                            if ngrn % 2 == 1 {
                                                ngrn += 1;
                                                true
                                            } else {
                                                true
                                            }
                                        } else {
                                            false
                                        };

                                        if size > 8 {
                                            if use_128 {
                                                sig.params.push(AbiParam::new(types::I128));
                                            } else {
                                                sig.params.push(AbiParam::new(types::I64));
                                                sig.params.push(AbiParam::new(types::I64));
                                            }
                                            ngrn = 8.min(ngrn + 2);
                                        } else {
                                            sig.params.push(AbiParam::new(types::I64));
                                            ngrn = 8.min(ngrn + 1);
                                        }
                                    } else {
                                        sig.params.push(AbiParam::new(self.isa.pointer_type()));
                                        ngrn = 8.min(ngrn + 1);
                                    }
                                }
                            }
                        }
                    }

                    Ok((sig, has_ret_ptr))
                }
                arch => unimplemented!(
                    "function call conv for SystemV on {:?} not implemented",
                    arch
                ),
            },
            CallingConvention::AppleAarch64 => {
                todo!("No apple calling convention suppport yet");
            }
            arch => {
                unimplemented!("No support for architecture: {:?}", arch)
            }
        }
    }

    /// Call a function using the current platform's ABI
    fn call_fn(
        &self,
        callable: Callable,
        args: &Vec<ir::ValueOrObj>,
        ir: &ir::Module,
        fn_ctx: &mut FunctionCtx,
    ) -> anyhow::Result<Option<FValue>> {
        let triple = self.isa.triple();

        let sig = match callable {
            Callable::FnId(id) => {
                &ir.get_function(id)
                    .ok_or(anyhow!("Invalid function id {}", id))?
                    .sig
            }
            Callable::Pointer(_value, fn_sig) => fn_sig,
        };

        let args = args
            .iter()
            .zip(&sig.param_tys)
            .map(|(arg, param)| match arg {
                ir::ValueOrObj::Value(value) => {
                    FValue::from_flattened(&[self.jit_value(value, fn_ctx)?.0], param, ir)
                }
                ir::ValueOrObj::Obj(values) => {
                    let values = values
                        .iter()
                        .map(|v| self.jit_value(v, fn_ctx).map(|v| v.0))
                        .collect::<anyhow::Result<Vec<_>>>()?;
                    FValue::from_flattened(&values, param, ir)
                }
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        let mut arg_values = Vec::new();
        let mut ret_alloc = None;

        // Construct args and return pointer memory acording to ABI
        match triple
            .default_calling_convention()
            .expect("No default calling convention for this platform")
        {
            CallingConvention::WindowsFastcall => {
                assert_eq!(
                    triple.architecture,
                    Architecture::X86_64,
                    "We only have X86_64 windows call convention support"
                );

                if let Some(ret_ty) = &sig.ret_ty {
                    let (size, align) = ir.size_align(ret_ty)?;
                    if size > 8 {
                        let stack_slot = StackSlotData::new(
                            StackSlotKind::ExplicitSlot,
                            size as u32,
                            align as u8,
                        );
                        let stack_slot = fn_ctx.builder.create_sized_stack_slot(stack_slot);
                        ret_alloc = Some(stack_slot);
                        arg_values.push(fn_ctx.builder.ins().stack_addr(
                            self.isa.pointer_type(),
                            stack_slot,
                            0,
                        ))
                    }
                }

                arg_values.push(fn_ctx.ctx_ptr);

                for arg in args {
                    if let FValueDesc::Value(_) = arg.desc {
                        arg_values.push(arg.values[0].0);
                    } else {
                        let size = arg.size();
                        if size > 8 {
                            let stack_slot = StackSlotData::new(
                                StackSlotKind::ExplicitSlot,
                                size as u32,
                                arg.align() as u8,
                            );
                            let stack_slot = fn_ctx.builder.create_sized_stack_slot(stack_slot);
                            let stack_ptr = fn_ctx.builder.ins().stack_addr(
                                self.isa.pointer_type(),
                                stack_slot,
                                0,
                            );
                            arg.store(stack_ptr, 0, ir, fn_ctx)?;
                            arg_values.push(stack_ptr)
                        } else {
                            let mut eightbyte = [fn_ctx.builder.ins().iconst(types::I64, 0)];

                            arg.to_packed_args(&mut eightbyte, &[ArgClass::General], ir, fn_ctx)?;
                            // Since this might overflow onto the stack, the size of this register
                            // might actually matter
                            let cast_int = match size {
                                1 => fn_ctx.builder.ins().ireduce(types::I8, eightbyte[0]),
                                2 => fn_ctx.builder.ins().ireduce(types::I16, eightbyte[0]),
                                4 => fn_ctx.builder.ins().ireduce(types::I32, eightbyte[0]),
                                8 => eightbyte[0],
                                _ => unreachable!(),
                            };
                            arg_values.push(cast_int);
                        }
                    }
                }
            }
            CallingConvention::SystemV => match &triple.architecture {
                Architecture::X86_64 => {
                    let mut rem_general = 6i32;
                    let mut rem_vector = 8i32;

                    if let Some(ret_ty) = &sig.ret_ty {
                        let (size, align) = ir.size_align(ret_ty)?;
                        if size > 16 {
                            let stack_slot = StackSlotData::new(
                                StackSlotKind::ExplicitSlot,
                                size as u32,
                                align as u8,
                            );
                            let stack_slot = fn_ctx.builder.create_sized_stack_slot(stack_slot);
                            ret_alloc = Some(stack_slot);
                            arg_values.push(fn_ctx.builder.ins().stack_addr(
                                self.isa.pointer_type(),
                                stack_slot,
                                0,
                            ));
                            rem_general = 0.max(rem_general - 1);
                        }
                    }

                    arg_values.push(fn_ctx.ctx_ptr);
                    rem_general = 0.max(rem_general - 1);

                    for (arg, arg_ty) in args.into_iter().zip(&sig.param_tys) {
                        if let FValueDesc::Value(_) = arg.desc {
                            arg_values.push(arg.values[0].0);
                            if arg.values[0].1.is_int() {
                                rem_general = 0.max(rem_general - 1);
                            } else {
                                rem_vector = 0.max(rem_vector - 1);
                            }
                        } else {
                            let mut pass_as_mem = || -> anyhow::Result<_> {
                                let stack_slot = StackSlotData::new(
                                    StackSlotKind::ExplicitSlot,
                                    arg.size() as u32,
                                    arg.align() as u8,
                                );
                                let stack_slot = fn_ctx.builder.create_sized_stack_slot(stack_slot);
                                let stack_ptr = fn_ctx.builder.ins().stack_addr(
                                    self.isa.pointer_type(),
                                    stack_slot,
                                    0,
                                );
                                arg.store(stack_ptr, 0, ir, fn_ctx)?;
                                arg_values.push(stack_ptr);
                                Ok(())
                            };

                            if arg.size() > 16 {
                                pass_as_mem()?;
                            } else {
                                let mut arg_classes = [ArgClass::NoClass, ArgClass::NoClass];
                                ArgClass::ty_arg_class(arg_ty, ir, &mut arg_classes)?;
                                let mut general_consumed = 0;
                                let mut vector_consumed = 0;

                                for c in &arg_classes {
                                    match c {
                                        ArgClass::NoClass | ArgClass::Memory => {}
                                        ArgClass::General => general_consumed += 1,
                                        ArgClass::Vector => vector_consumed += 1,
                                    }
                                }

                                if general_consumed > rem_general
                                    || vector_consumed > rem_vector
                                    || arg_classes.contains(&ArgClass::Memory)
                                {
                                    pass_as_mem()?;
                                } else {
                                    let arg_c = (general_consumed + vector_consumed) as usize;
                                    let current_arg_c = arg_values.len();
                                    arg_values.resize_with(current_arg_c + arg_c, || {
                                        fn_ctx.builder.ins().iconst(types::I64, 0)
                                    });
                                    arg_values.extend(arg_classes.iter().filter_map(|class| {
                                        match class {
                                            ArgClass::NoClass => None,
                                            ArgClass::General => {
                                                Some(fn_ctx.builder.ins().iconst(types::I64, 0))
                                            }
                                            ArgClass::Vector => Some({
                                                let constant =
                                                    fn_ctx.builder.func.dfg.constants.insert(
                                                        ConstantData::from(V128Imm([0; 16])),
                                                    );
                                                fn_ctx.builder.ins().vconst(types::F64X2, constant)
                                            }),
                                            ArgClass::Memory => unreachable!(),
                                        }
                                    }));
                                    arg.to_packed_args(
                                        &mut arg_values[current_arg_c..],
                                        &arg_classes,
                                        ir,
                                        fn_ctx,
                                    )?;
                                    if general_consumed >= rem_general {
                                        rem_general = 0;
                                    } else {
                                        rem_general -= general_consumed;
                                    }
                                    if vector_consumed >= rem_vector {
                                        rem_vector = 0;
                                    } else {
                                        rem_vector -= vector_consumed;
                                    }
                                }
                            }
                        }
                    }
                }
                Architecture::Aarch64(_) => {
                    if let Some(ret_ty) = &sig.ret_ty {
                        let (size, align) = ir.size_align(ret_ty)?;
                        if size > 16 {
                            let stack_slot = StackSlotData::new(
                                StackSlotKind::ExplicitSlot,
                                size as u32,
                                align as u8,
                            );
                            let stack_slot = fn_ctx.builder.create_sized_stack_slot(stack_slot);
                            ret_alloc = Some(stack_slot);
                            arg_values.push(fn_ctx.builder.ins().stack_addr(
                                self.isa.pointer_type(),
                                stack_slot,
                                0,
                            ))
                        }
                    }

                    arg_values.push(fn_ctx.ctx_ptr);
                    let mut ngrn = 1;
                    let mut nsrn = 0;

                    for (arg, arg_ty) in args.into_iter().zip(&sig.param_tys) {
                        if let FValueDesc::Value(_) = &arg.desc {
                            arg_values.push(arg.values[0].0);
                            if arg.values[0].1.is_int() {
                                ngrn = 8.min(ngrn + 1);
                            } else {
                                nsrn = 8.min(nsrn + 1);
                            }
                        } else {
                            if let Some((f_ty, fields)) =
                                FValue::is_homogeneous_simd_aggregate(arg_ty, ir)
                                && fields <= 4
                            {
                                // Intentionally overflow vector registers to the stack
                                if nsrn + fields > 8 {
                                    while nsrn != 8 {
                                        arg_values.push(fn_ctx.builder.ins().f32const(0f32));
                                        nsrn += 1;
                                    }
                                }

                                assert_eq!(fields, arg.values.len());
                                if nsrn < 8 || f_ty.bytes() >= 8 {
                                    for (value, _, _) in &arg.values {
                                        arg_values.push(*value);
                                        nsrn = 8.min(nsrn + 1);
                                    }
                                } else {
                                    let constant = fn_ctx
                                        .builder
                                        .func
                                        .dfg
                                        .constants
                                        .insert(ConstantData::from(V128Imm([0; 16])));
                                    let constant =
                                        fn_ctx.builder.ins().vconst(types::F64X2, constant);
                                    let mut packed = [constant, constant];
                                    arg.to_packed_args(
                                        &mut packed,
                                        &[ArgClass::Vector, ArgClass::Vector],
                                        ir,
                                        fn_ctx,
                                    )?;

                                    arg_values.push(fn_ctx.builder.ins().extractlane(packed[0], 0));
                                    if fields > 2 {
                                        arg_values
                                            .push(fn_ctx.builder.ins().extractlane(packed[1], 0));
                                    }
                                }
                            } else {
                                let (size, align) = ir.size_align(arg_ty)?;
                                if size <= 16 {
                                    // Cranelift does not implent ArgumentPurpose::StructArgument
                                    // for aarch64, so if we have a 1-8byte aligned struct with a
                                    // size of 16 bytes but only one general register left, we need
                                    // a way to force cranelift to pass all of it on the stack.
                                    // For this we use "padding arguments" to use up general
                                    // registers. Unfortunatly this does mean we will be forced to
                                    // pass zeroed args since cranelift doesn't allow for undefined
                                    // values.
                                    if ngrn == 7 && size > 8 {
                                        arg_values.push(fn_ctx.builder.ins().iconst(types::I64, 0));
                                        ngrn += 1;
                                    }
                                    let use_128 = if align == 16 {
                                        // Align NGRN by 2
                                        if ngrn % 2 == 1 {
                                            ngrn = 8.min(ngrn + 1);
                                            true
                                        } else {
                                            true
                                        }
                                    } else {
                                        false
                                    };

                                    let mut packed = [
                                        fn_ctx.builder.ins().iconst(types::I64, 0),
                                        fn_ctx.builder.ins().iconst(types::I64, 0),
                                    ];
                                    arg.to_packed_args(
                                        &mut packed,
                                        &[ArgClass::General, ArgClass::General],
                                        ir,
                                        fn_ctx,
                                    )?;

                                    if size > 8 {
                                        if use_128 {
                                            let lower = fn_ctx
                                                .builder
                                                .ins()
                                                .uextend(types::I128, packed[0]);
                                            let higher = fn_ctx
                                                .builder
                                                .ins()
                                                .uextend(types::I128, packed[1]);
                                            let higher = fn_ctx.builder.ins().ishl_imm(higher, 64);
                                            arg_values
                                                .push(fn_ctx.builder.ins().band(lower, higher));
                                        } else {
                                            arg_values.extend(packed);
                                        }
                                        ngrn = 8.min(ngrn + 2);
                                    } else {
                                        arg_values.push(packed[0]);
                                        ngrn = 8.min(ngrn + 1);
                                    }
                                } else {
                                    let stack_slot = StackSlotData::new(
                                        StackSlotKind::ExplicitSlot,
                                        size as u32,
                                        align as u8,
                                    );
                                    let stack_slot =
                                        fn_ctx.builder.create_sized_stack_slot(stack_slot);
                                    let stack_ptr = fn_ctx.builder.ins().stack_addr(
                                        self.isa.pointer_type(),
                                        stack_slot,
                                        0,
                                    );
                                    arg.store(stack_ptr, 0, ir, fn_ctx)?;
                                    ngrn = 8.min(ngrn + 1);
                                    arg_values.push(stack_ptr);
                                }
                            }
                        }
                    }
                }
                arch => unimplemented!(
                    "function call conv for SystemV on {:?} not implemented",
                    arch
                ),
            },
            CallingConvention::AppleAarch64 => {
                todo!("No apple calling convention suppport yet");
            }
            arch => {
                unimplemented!("No support for architecture: {:?}", arch)
            }
        }

        let call_inst = match callable {
            Callable::FnId(id) => {
                // Negative function ids refer to the (id * -1 - 1)th imported function
                assert!(
                    id >= 0,
                    "Though we do have negative function ids, they should be resolved and then passed as Pointer calls"
                );
                let func_id = fn_ctx
                    .mod_ctx
                    .fn_map
                    .get(id as usize)
                    .ok_or(anyhow!("Invalid function id {}", id))?
                    .1;
                let func_ref = fn_ctx
                    .mod_ctx
                    .module
                    .declare_func_in_func(func_id, fn_ctx.builder.func);
                fn_ctx.builder.ins().call(func_ref, &arg_values)
            }
            Callable::Pointer(value, fn_sig) => {
                let (sig, _ret_ptr) = self.construct_fn_sig(fn_sig, ir)?;
                let sig_ref = fn_ctx.builder.import_signature(sig);

                fn_ctx
                    .builder
                    .ins()
                    .call_indirect(sig_ref, value, &arg_values)
            }
        };

        if let Some(ret_ty) = &sig.ret_ty {
            match triple
                .default_calling_convention()
                .expect("No default calling convention for this platform")
            {
                CallingConvention::WindowsFastcall => {
                    if let Some(ret_alloc) = ret_alloc {
                        Ok(Some(FValue::load(
                            fn_ctx
                                .builder
                                .ins()
                                .stack_addr(self.isa.pointer_type(), ret_alloc, 0),
                            0,
                            ret_ty,
                            ir,
                            &mut fn_ctx.builder,
                        )?))
                    } else {
                        let ret_val = fn_ctx.builder.inst_results(call_inst)[0];
                        match ret_ty {
                            ir::Ty::Native(native_type) => {
                                Ok(Some(FValue::new(ret_val, Self::jit_ty(native_type))))
                            }
                            ir::Ty::Struct(_) => Ok(Some(FValue::from_packed_args(
                                &[ret_val],
                                &[ArgClass::General],
                                ret_ty,
                                ir,
                                &mut fn_ctx.builder,
                            )?)),
                            ir::Ty::Enum(_) => Ok(Some(FValue::from_packed_args(
                                &[ret_val],
                                &[ArgClass::General],
                                ret_ty,
                                ir,
                                &mut fn_ctx.builder,
                            )?)),
                        }
                    }
                }
                CallingConvention::SystemV => match &triple.architecture {
                    Architecture::X86_64 => match ret_ty {
                        ir::Ty::Native(nt) => {
                            let ret_val = fn_ctx.builder.inst_results(call_inst)[0];
                            let value_ty = Self::jit_ty(nt);
                            let value = FValue::new(ret_val, value_ty);
                            Ok(Some(value))
                        }
                        ir::Ty::Struct(_) | ir::Ty::Enum(_) => {
                            if let Some(ret_alloc) = ret_alloc {
                                let ptr = fn_ctx.builder.ins().stack_addr(
                                    self.isa.pointer_type(),
                                    ret_alloc,
                                    0,
                                );
                                let loaded = FValue::load(ptr, 0, ret_ty, ir, &mut fn_ctx.builder)?;
                                Ok(Some(loaded))
                            } else {
                                let ret_vals = fn_ctx.builder.inst_results(call_inst).to_vec();

                                let mut arg_classes = [ArgClass::NoClass, ArgClass::NoClass];
                                ArgClass::ty_arg_class(ret_ty, ir, &mut arg_classes)?;

                                let reconstructed = FValue::from_packed_args(
                                    &ret_vals,
                                    &arg_classes,
                                    ret_ty,
                                    ir,
                                    &mut fn_ctx.builder,
                                )?;
                                Ok(Some(reconstructed))
                            }
                        }
                    },
                    Architecture::Aarch64(_) => match ret_ty {
                        ir::Ty::Native(nt) => {
                            let ret_val = fn_ctx.builder.inst_results(call_inst)[0];
                            let value_ty = Self::jit_ty(nt);
                            let value = FValue::new(ret_val, value_ty);
                            Ok(Some(value))
                        }
                        ir::Ty::Struct(_) | ir::Ty::Enum(_) => {
                            if let Some(ret_alloc) = ret_alloc {
                                let ptr = fn_ctx.builder.ins().stack_addr(
                                    self.isa.pointer_type(),
                                    ret_alloc,
                                    0,
                                );
                                let loaded = FValue::load(ptr, 0, ret_ty, ir, &mut fn_ctx.builder)?;
                                Ok(Some(loaded))
                            } else {
                                let ret_vals = fn_ctx.builder.inst_results(call_inst).to_vec();

                                if let Some((_, fields)) =
                                    FValue::is_homogeneous_simd_aggregate(ret_ty, ir)
                                    && fields <= 4
                                {
                                    assert_eq!(fields, ret_vals.len());
                                    Ok(Some(FValue::from_flattened(&ret_vals, ret_ty, ir)?))
                                } else {
                                    let reconstructed = FValue::from_packed_args(
                                        &ret_vals,
                                        &[ArgClass::General, ArgClass::General],
                                        ret_ty,
                                        ir,
                                        &mut fn_ctx.builder,
                                    )?;
                                    Ok(Some(reconstructed))
                                }
                            }
                        }
                    },
                    arch => unimplemented!(
                        "function call conv for SystemV on {:?} not implemented",
                        arch
                    ),
                },
                CallingConvention::AppleAarch64 => {
                    todo!("No apple calling convention suppport yet");
                }
                arch => {
                    unimplemented!("No support for architecture: {:?}", arch)
                }
            }
        } else {
            Ok(None)
        }
    }

    fn reconstruct_fn_args(
        &self,
        params: &[Value],
        uses_ret_ptr: bool,
        func: &ir::Function,
        ir: &ir::Module,
        builder: &mut FunctionBuilder,
    ) -> anyhow::Result<Vec<FValue>> {
        let triple = self.isa.triple();
        let mut fn_args = Vec::new();
        match triple
            .default_calling_convention()
            .expect("No default calling convention for this platform")
        {
            CallingConvention::WindowsFastcall => {
                let mut consumed_params = 0;
                for arg in &func.sig.param_tys {
                    match arg {
                        ir::Ty::Native(nt) => {
                            let value = FValue::new(params[consumed_params], Self::jit_ty(nt));
                            consumed_params += 1;
                            fn_args.push(value);
                        }
                        ir::Ty::Struct(_) | ir::Ty::Enum(_) => {
                            let (size, _) = ir.size_align(arg)?;
                            if size <= 8 {
                                let mut eightbyte =
                                    [size_value(params[consumed_params], types::I64, builder)];
                                consumed_params += 1;
                                let rebuilt_arg = FValue::from_packed_args(
                                    &mut eightbyte,
                                    &[ArgClass::General],
                                    arg,
                                    ir,
                                    builder,
                                )?;
                                fn_args.push(rebuilt_arg);
                            } else {
                                let stack_ptr = params[consumed_params];
                                consumed_params += 1;
                                let loaded_arg = FValue::load(stack_ptr, 0, arg, ir, builder)?;
                                fn_args.push(loaded_arg);
                            }
                        }
                    }
                }
            }
            CallingConvention::SystemV => match &triple.architecture {
                Architecture::X86_64 => {
                    let mut rem_general = 5i32; // Only 5 to account for ctx ptr
                    let mut rem_vector = 8i32;

                    if uses_ret_ptr {
                        rem_general -= 1;
                    }

                    let mut params_consumed = 0;
                    for arg in &func.sig.param_tys {
                        match arg {
                            ir::Ty::Native(nt) => {
                                let value_ty = Self::jit_ty(nt);
                                let value = FValue::new(params[params_consumed], value_ty);
                                params_consumed += 1;
                                if value_ty.is_int() {
                                    rem_general = 0.max(rem_general - 1);
                                } else {
                                    rem_vector = 0.max(rem_vector - 1);
                                }
                                fn_args.push(value);
                            }
                            ir::Ty::Struct(_) | ir::Ty::Enum(_) => {
                                let (size, _) = ir.size_align(arg)?;
                                let mut load_as_mem = || -> anyhow::Result<_> {
                                    let ptr = params[params_consumed];
                                    params_consumed += 1;
                                    let loaded = FValue::load(ptr, 0, arg, ir, builder)?;
                                    Ok(loaded)
                                };

                                if size > 16 {
                                    fn_args.push(load_as_mem()?);
                                } else {
                                    let mut arg_classes = [ArgClass::NoClass, ArgClass::NoClass];
                                    ArgClass::ty_arg_class(arg, ir, &mut arg_classes)?;
                                    let mut general_consumed = 0;
                                    let mut vector_consumed = 0;

                                    for c in &arg_classes {
                                        match c {
                                            ArgClass::NoClass | ArgClass::Memory => {}
                                            ArgClass::General => general_consumed += 1,
                                            ArgClass::Vector => vector_consumed += 1,
                                        }
                                    }

                                    if general_consumed > rem_general
                                        || vector_consumed > rem_vector
                                        || arg_classes.contains(&ArgClass::Memory)
                                    {
                                        fn_args.push(load_as_mem()?);
                                    } else {
                                        let arg_c = (general_consumed + vector_consumed) as usize;
                                        let param_slice =
                                            &params[params_consumed..(arg_c + params_consumed)];
                                        let reconstructed = FValue::from_packed_args(
                                            param_slice,
                                            &arg_classes,
                                            arg,
                                            ir,
                                            builder,
                                        )?;
                                        params_consumed += arg_c;
                                        if general_consumed >= rem_general {
                                            rem_general = 0;
                                        } else {
                                            rem_general -= general_consumed;
                                        }
                                        if vector_consumed >= rem_vector {
                                            rem_vector = 0;
                                        } else {
                                            rem_vector -= vector_consumed;
                                        }
                                        fn_args.push(reconstructed);
                                    }
                                }
                            }
                        }
                    }
                }
                Architecture::Aarch64(_) => {
                    // Incremented by one to account for ctx ptr
                    let mut ngrn = 1;
                    let mut nsrn = 0;

                    if uses_ret_ptr {
                        ngrn += 1;
                    }

                    let mut params_consumed = 0;
                    for arg in &func.sig.param_tys {
                        match arg {
                            ir::Ty::Native(nt) => {
                                let value_ty = Self::jit_ty(nt);
                                let value = FValue::new(params[params_consumed], value_ty);
                                params_consumed += 1;
                                if value_ty.is_int() {
                                    ngrn = 8.min(ngrn + 1);
                                } else {
                                    nsrn = 8.min(nsrn + 1);
                                }
                                fn_args.push(value);
                            }
                            ir::Ty::Struct(_) | ir::Ty::Enum(_) => {
                                if let Some((f_ty, fields)) =
                                    FValue::is_homogeneous_simd_aggregate(arg, ir)
                                    && fields <= 4
                                {
                                    // Intentionally overflow vector registers to the stack
                                    if nsrn + fields > 8 {
                                        while nsrn != 8 {
                                            params_consumed += 1; // skip padding arg
                                            nsrn += 1;
                                        }
                                    }

                                    if nsrn < 8 || f_ty.bytes() >= 8 {
                                        fn_args.push(FValue::from_flattened(
                                            &params[params_consumed..(params_consumed + fields)],
                                            arg,
                                            ir,
                                        )?);
                                        params_consumed += fields;
                                        nsrn = 8.min(nsrn + fields);
                                    } else {
                                        let constant = builder
                                            .func
                                            .dfg
                                            .constants
                                            .insert(ConstantData::from(V128Imm([0; 16])));
                                        let constant = builder.ins().vconst(types::F64X2, constant);
                                        let mut packed = [constant, constant];

                                        packed[0] = builder.ins().insertlane(
                                            constant,
                                            params[params_consumed],
                                            0,
                                        );
                                        params_consumed += 1;
                                        if fields > 2 {
                                            packed[1] = builder.ins().insertlane(
                                                constant,
                                                params[params_consumed],
                                                0,
                                            );
                                            params_consumed += 1;
                                        }
                                        fn_args.push(FValue::from_packed_args(
                                            &packed,
                                            &[ArgClass::Vector, ArgClass::Vector],
                                            arg,
                                            ir,
                                            builder,
                                        )?);
                                    }
                                } else {
                                    let (size, align) = ir.size_align(arg)?;
                                    if size <= 16 {
                                        // Cranelift does not implent ArgumentPurpose::StructArgument
                                        // for aarch64, so if we have a 1-8byte aligned struct with a
                                        // size of 16 bytes but only one general register left, we need
                                        // a way to force cranelift to pass all of it on the stack.
                                        // For this we use "padding arguments" to use up general
                                        // registers. Unfortunatly this does mean we will be forced to
                                        // pass zeroed args since cranelift doesn't allow for undefined
                                        // values.
                                        if ngrn == 7 && size > 8 {
                                            params_consumed += 1; // skip padding arg
                                            ngrn += 1;
                                        }
                                        let packed = if align == 16 {
                                            // Align NGRN by 2
                                            if ngrn % 2 == 1 {
                                                ngrn += 1;
                                                params_consumed += 1;
                                            }

                                            let double = params[params_consumed];
                                            let low = builder.ins().ireduce(types::I64, double);
                                            let high = builder.ins().ushr_imm(double, 64);
                                            let high = builder.ins().ireduce(types::I64, high);
                                            params_consumed += 1;
                                            ngrn = 8.min(ngrn + 2);
                                            [low, high]
                                        } else if size > 8 {
                                            params_consumed += 2;
                                            ngrn = 8.min(ngrn + 2);
                                            [
                                                params[params_consumed - 2],
                                                params[params_consumed - 1],
                                            ]
                                        } else {
                                            params_consumed += 1;
                                            ngrn = 8.min(ngrn + 1);
                                            [
                                                params[params_consumed - 1],
                                                Value::from_bits(0xFFFFFFFF),
                                            ]
                                        };

                                        fn_args.push(FValue::from_packed_args(
                                            &packed,
                                            &[ArgClass::General, ArgClass::General],
                                            arg,
                                            ir,
                                            builder,
                                        )?);
                                    } else {
                                        let ptr = params[params_consumed];
                                        params_consumed += 1;
                                        fn_args.push(FValue::load(ptr, 0, arg, ir, builder)?);
                                    }
                                }
                            }
                        }
                    }
                }
                arch => unimplemented!(
                    "function call conv for SystemV on {:?} not implemented",
                    arch
                ),
            },
            CallingConvention::AppleAarch64 => {
                todo!("No apple calling convention suppport yet");
            }
            arch => {
                unimplemented!("No support for architecture: {:?}", arch)
            }
        }
        Ok(fn_args)
    }

    fn jit_return(
        &self,
        values: Vec<Value>,
        func: &ir::Function,
        ir: &ir::Module,
        ctx: &mut FunctionCtx,
    ) -> anyhow::Result<()> {
        let ret_ty = func.sig.ret_ty.as_ref().ok_or(anyhow!(
            "Tried to return value from function with no return value"
        ))?;
        match ret_ty {
            ir::Ty::Native(_) => {
                ctx.builder.ins().return_(&[values
                    .get(0)
                    .cloned()
                    .ok_or(anyhow!("Expected one return value"))?]);
            }
            ir::Ty::Struct(_) | ir::Ty::Enum(_) => {
                let ret_obj = FValue::from_flattened(&values, ret_ty, ir)?;
                let triple = self.isa.triple();
                match triple
                    .default_calling_convention()
                    .expect("No default calling convention for this platform")
                {
                    CallingConvention::WindowsFastcall => {
                        if let Some(ret_ptr) = ctx.ret_ptr.clone() {
                            ret_obj.store(ret_ptr, 0, ir, ctx)?;
                            ctx.builder.ins().return_(&[ret_ptr]);
                        } else {
                            assert!(ret_obj.size() <= 8);
                            let mut eightbyte = [ctx.builder.ins().iconst(types::I64, 0)];
                            ret_obj.to_packed_args(
                                &mut eightbyte,
                                &[ArgClass::General],
                                ir,
                                ctx,
                            )?;
                            let cast_int = match ret_obj.size() {
                                1 => ctx.builder.ins().ireduce(types::I8, eightbyte[0]),
                                2 => ctx.builder.ins().ireduce(types::I16, eightbyte[0]),
                                3 | 4 => ctx.builder.ins().ireduce(types::I32, eightbyte[0]),
                                5..=8 => eightbyte[0],
                                _ => {
                                    unreachable!("Windows Fastcall must have a defined return ptr")
                                }
                            };
                            ctx.builder.ins().return_(&[cast_int]);
                        }
                    }
                    CallingConvention::SystemV => match &triple.architecture {
                        Architecture::X86_64 => {
                            if let Some(ret_ptr) = ctx.ret_ptr.clone() {
                                ret_obj.store(ret_ptr, 0, ir, ctx)?;
                                ctx.builder.ins().return_(&[ret_ptr]);
                            } else {
                                assert!(ret_obj.size() <= 16);
                                let mut ret_classes = [ArgClass::NoClass, ArgClass::NoClass];
                                ArgClass::ty_arg_class(ret_ty, ir, &mut ret_classes)?;
                                let mut eightbyte =
                                    ret_classes
                                        .iter()
                                        .filter_map(|class| match class {
                                            ArgClass::NoClass => None,
                                            ArgClass::General => {
                                                Some(ctx.builder.ins().iconst(types::I64, 0))
                                            }
                                            ArgClass::Vector => Some({
                                                let constant =
                                                    ctx.builder.func.dfg.constants.insert(
                                                        ConstantData::from(V128Imm([0; 16])),
                                                    );
                                                ctx.builder.ins().vconst(types::F64X2, constant)
                                            }),
                                            ArgClass::Memory => unreachable!(),
                                        })
                                        .collect::<Vec<_>>();
                                ret_obj.to_packed_args(&mut eightbyte, &ret_classes, ir, ctx)?;
                                ctx.builder.ins().return_(&eightbyte);
                            }
                        }
                        Architecture::Aarch64(_) => {
                            if let Some((_, fields)) =
                                FValue::is_homogeneous_simd_aggregate(ret_ty, ir)
                                && fields <= 4
                            {
                                // Yes, amd64 can return up to four (4) vector/float registers
                                ctx.builder.ins().return_(&values);
                            } else {
                                let (size, _) = ir.size_align(ret_ty)?;
                                if let Some(ret_ptr) = ctx.ret_ptr {
                                    ret_obj.store(ret_ptr, 0, ir, ctx)?;
                                    ctx.builder.ins().return_(&[]);
                                } else {
                                    let mut packed = [
                                        ctx.builder.ins().iconst(types::I64, 0),
                                        ctx.builder.ins().iconst(types::I64, 0),
                                    ];
                                    ret_obj.to_packed_args(
                                        &mut packed,
                                        &[ArgClass::General, ArgClass::General],
                                        ir,
                                        ctx,
                                    )?;

                                    if size > 8 {
                                        ctx.builder.ins().return_(&packed);
                                    } else {
                                        ctx.builder.ins().return_(&[packed[0]]);
                                    }
                                }
                            }
                        }
                        arch => unimplemented!(
                            "function call conv for SystemV on {:?} not implemented",
                            arch
                        ),
                    },
                    CallingConvention::AppleAarch64 => {
                        todo!("No apple calling convention suppport yet");
                    }
                    arch => {
                        unimplemented!("No support for architecture: {:?}", arch)
                    }
                };
            }
        };
        Ok(())
    }
}

struct ModuleCtx<'a> {
    module: JITModule,
    data_desc: DataDescription,
    fn_map: Vec<(String, FuncId)>,
    ptr_ty: Type,
    ptr_bytes: u8,
    linked_func_indicies: Vec<(memory::FnId, ir::FnSig)>,
    rt: &'a Runtime<CraneliftJitBackend>,
}

struct FunctionCtx<'a, 'm, 's> {
    mod_ctx: &'m mut ModuleCtx<'s>,
    builder: FunctionBuilder<'a>,
    fn_args: Vec<FValue>,
    ctx_ptr: Value,
    ret_ptr: Option<Value>,
    blocks: Vec<Block>,
    entry_block_id: Block,
    block_values: HashMap<Block, Vec<Option<FValue>>>,
    block_args: HashMap<Block, Vec<FValue>>,
    ptr_ty: Type,
    ptr_bytes: u8,
}

impl<'a, 'm, 's> FunctionCtx<'a, 'm, 's> {
    pub fn get_block(&self, block: u32) -> Result<Block> {
        self.blocks
            .get(block as usize)
            .ok_or_else(|| anyhow!("Invalid block ID {}", block))
            .cloned()
    }
}

enum Callable<'a> {
    FnId(ir::FnId),
    // (Actual pointer (not Brane u32 pointer), fn sig)
    Pointer(Value, &'a ir::FnSig),
}

/// Describes where an argument will be stored
///
/// Inspired by the x86_64 SystemV arg class concept: https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum ArgClass {
    /// Padding, undefined data, etc
    NoClass,
    /// General/Int registers. Often other register types will be bitcast to this
    General,
    /// Usually used for float arguments not explicit vectors, but some ABIs pack struct floats as
    /// vectors
    Vector,
    /// For arguments that must be passed on the stack, some ABI require a pointer in it's place,
    /// others use implicit stack posiitons
    Memory,
}

impl ArgClass {
    /// Get the arg classification for an ir type
    ///
    /// # Arguments
    ///
    /// * `classes` - Should initialized to [ArgClass::NoClass], length should be the type's size / 8 rounded up
    pub fn ty_arg_class(
        ty: &ir::Ty,
        module: &ir::Module,
        classes: &mut [ArgClass],
    ) -> anyhow::Result<()> {
        fn class_merge(a: ArgClass, b: ArgClass) -> ArgClass {
            if a == b {
                a
            } else if a == ArgClass::Memory || b == ArgClass::Memory {
                ArgClass::Memory
            } else if a == ArgClass::General || b == ArgClass::General {
                ArgClass::General
            } else {
                ArgClass::Vector
            }
        }

        match ty {
            ir::Ty::Native(native_type) => {
                let class = classes
                    .get_mut(0)
                    .ok_or(anyhow!("Did not allocate enough arg classes"))?;
                *class = class_merge(
                    *class,
                    match native_type {
                        ir::NativeType::Bool
                        | ir::NativeType::U8
                        | ir::NativeType::I8
                        | ir::NativeType::U16
                        | ir::NativeType::I16
                        | ir::NativeType::U32
                        | ir::NativeType::I32
                        | ir::NativeType::U64
                        | ir::NativeType::I64
                        | ir::NativeType::Ptr(_, _) => ArgClass::General,
                        ir::NativeType::F32 | ir::NativeType::F64 => ArgClass::Vector,
                    },
                );
            }
            ir::Ty::Struct(id) => {
                let def = module
                    .get_struct(*id)
                    .ok_or(anyhow!("Invalid struct id: {}", id))?;
                let layout = def.layout(module)?;

                for (m, offset) in def.members.iter().zip(layout.byte_offsets) {
                    let eb_start = offset / 8;
                    let classes = classes
                        .get_mut(eb_start..)
                        .ok_or(anyhow!("Did not allocate enough arg classes"))?;
                    Self::ty_arg_class(&m.ty, module, classes)?;
                }
            }
            ir::Ty::Enum(id) => {
                let def = module
                    .get_enum(*id)
                    .ok_or(anyhow!("Invalid enum id: {}", id))?;
                let layout = def.layout(module)?;

                // We know the index ty will always be an int and in the first eightbyte
                let class = classes
                    .get_mut(0)
                    .ok_or(anyhow!("Did not allocate enough arg classes"))?;
                *class = class_merge(ArgClass::General, *class);

                for (v, (_, offset)) in def.variants.iter().zip(layout.variants) {
                    let eb_start = offset / 8;
                    let classes = classes
                        .get_mut(eb_start..)
                        .ok_or(anyhow!("Did not allocate enough arg classes"))?;
                    if let Some(ty) = &v.ty {
                        Self::ty_arg_class(ty, module, classes)?;
                    }
                }
            }
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct FStruct {
    pub fields: Vec<FValueDesc>,
    pub def_id: ir::StructId,
    pub size: usize,
}

#[derive(Clone)]
pub struct FEnum {
    /// Index of the value in the FValue values vec
    /// None in the cases of a 0 or 1 variant enum, we don't need a discriminant in those cases.
    pub index: Option<usize>,
    /// Represents a 1-1 mapping with all variants of the enum, with none values representing
    /// variants with no data
    pub variants: Vec<Option<FValueDesc>>,
    pub size: usize,
    pub def_id: ir::EnumId,
}

/// F values (or flattened values) are an abstraction over the flattened representation we use for structs and
/// enums to avoid needing to directly expose the stack.
#[derive(Clone)]
pub struct FValue {
    pub values: Vec<(Value, Type, usize)>,
    pub desc: FValueDesc,
}

#[derive(Clone)]
pub enum FValueDesc {
    /// Index of the value within the FValue values array
    Value(usize),
    Struct(FStruct),
    Enum(FEnum),
}

impl FValue {
    pub fn new(value: Value, ty: Type) -> Self {
        Self {
            values: vec![(value, ty, 0)],
            desc: FValueDesc::Value(0),
        }
    }

    fn new_auto_ty(value: Value, builder: &mut FunctionBuilder) -> Self {
        Self {
            values: vec![(value, builder.func.dfg.value_type(value), 0)],
            desc: FValueDesc::Value(0),
        }
    }

    pub fn size(&self) -> usize {
        match &self.desc {
            FValueDesc::Value(i) => self.values[*i].1.bytes() as usize,
            FValueDesc::Struct(obj) => obj.size,
            FValueDesc::Enum(obj) => obj.size,
        }
    }

    pub fn align(&self) -> u8 {
        self.values.iter().fold(1, |align, value| {
            // TODO account for vector types that may be fine with alignments less than their size
            align.max(value.1.bytes() as u8)
        })
    }

    pub fn zeroed_ty(
        ty: &ir::Ty,
        ir: &ir::Module,
        builder: &mut FunctionBuilder,
    ) -> anyhow::Result<Self> {
        let mut values = Vec::new();
        let desc = Self::zeroed_ty_internal(ty, ir, builder, 0, &mut values)?;
        Ok(Self { values, desc })
    }

    fn zeroed_ty_internal(
        ty: &ir::Ty,
        ir: &ir::Module,
        builder: &mut FunctionBuilder,
        struct_offset: usize,
        fvalues: &mut Vec<(Value, Type, usize)>,
    ) -> anyhow::Result<FValueDesc> {
        match ty {
            ir::Ty::Native(native_type) => {
                let ty = CraneliftJitBackend::jit_ty(native_type);
                let value = match native_type {
                    ir::NativeType::Bool | ir::NativeType::U8 | ir::NativeType::I8 => {
                        builder.ins().iconst(types::I8, 0)
                    }
                    ir::NativeType::U16 | ir::NativeType::I16 => {
                        builder.ins().iconst(types::I16, 0)
                    }
                    ir::NativeType::U32 | ir::NativeType::I32 | ir::NativeType::Ptr(_, _) => {
                        builder.ins().iconst(types::I32, 0)
                    }
                    ir::NativeType::U64 | ir::NativeType::I64 => {
                        builder.ins().iconst(types::I64, 0)
                    }
                    ir::NativeType::F32 => builder.ins().f32const(0f32),
                    ir::NativeType::F64 => builder.ins().f64const(0f64),
                };
                let index = fvalues.len();
                fvalues.push((value, ty, struct_offset));
                Ok(FValueDesc::Value(index))
            }
            ir::Ty::Struct(id) => {
                let def = ir
                    .get_struct(*id)
                    .ok_or(anyhow!("Invalid struct id: {}", id))?;
                let layout = def.layout(ir)?;

                let fields = def
                    .members
                    .iter()
                    .zip(&layout.byte_offsets)
                    .map(|(member, offset)| {
                        Self::zeroed_ty_internal(
                            &member.ty,
                            ir,
                            builder,
                            struct_offset + *offset,
                            fvalues,
                        )
                    })
                    .collect::<anyhow::Result<_>>()?;

                Ok(FValueDesc::Struct(FStruct {
                    fields,
                    def_id: *id,
                    size: layout.size,
                }))
            }
            ir::Ty::Enum(id) => {
                let def = ir.get_enum(*id).ok_or(anyhow!("Invalid enum id: {}", id))?;
                let layout = def.layout(ir)?;

                let index_ty = layout
                    .index_ty
                    .as_ref()
                    .map(|ty| CraneliftJitBackend::jit_ty(ty));

                if let Some(index_ty) = index_ty {
                    let index_value = builder.ins().iconst(index_ty, 0);
                    let idx = fvalues.len();
                    fvalues.push((index_value, index_ty, struct_offset));

                    let mut variants = Vec::new();
                    for (v, (_, v_offset)) in def.variants.iter().zip(layout.variants) {
                        let Some(v_ty) = &v.ty else {
                            variants.push(None);
                            continue;
                        };
                        variants.push(Some(Self::zeroed_ty_internal(
                            v_ty,
                            ir,
                            builder,
                            struct_offset + v_offset,
                            fvalues,
                        )?));
                    }

                    Ok(FValueDesc::Enum(FEnum {
                        index: Some(idx),
                        variants,
                        size: layout.size,
                        def_id: *id,
                    }))
                } else {
                    let variant = if def.variants.is_empty() {
                        None
                    } else {
                        let v = &def.variants[0];
                        v.ty.as_ref()
                            .map(|v_ty| {
                                Self::zeroed_ty_internal(
                                    v_ty,
                                    ir,
                                    builder,
                                    struct_offset + layout.variants[0].1,
                                    fvalues,
                                )
                            })
                            .transpose()?
                    };

                    Ok(FValueDesc::Enum(FEnum {
                        index: None,
                        variants: vec![variant],
                        size: layout.size,
                        def_id: *id,
                    }))
                }
            }
        }
    }

    pub fn load(
        ptr: Value,
        base_offset: i32,
        ty: &ir::Ty,
        ir: &ir::Module,
        builder: &mut FunctionBuilder,
    ) -> anyhow::Result<Self> {
        let mut fvalues = Vec::new();
        let desc = Self::load_internal(ptr, base_offset, ty, ir, builder, 0, &mut fvalues)?;
        Ok(Self {
            values: fvalues,
            desc,
        })
    }

    fn load_internal(
        ptr: Value,
        base_offset: i32,
        ty: &ir::Ty,
        ir: &ir::Module,
        builder: &mut FunctionBuilder,
        struct_offset: usize,
        fvalues: &mut Vec<(Value, Type, usize)>,
    ) -> anyhow::Result<FValueDesc> {
        match ty {
            ir::Ty::Native(native_type) => {
                let ty = CraneliftJitBackend::jit_ty(native_type);
                let value = builder.ins().load(
                    ty,
                    MemFlags::trusted(),
                    ptr,
                    base_offset + struct_offset as i32,
                );
                let index = fvalues.len();
                fvalues.push((value, ty, struct_offset));
                Ok(FValueDesc::Value(index))
            }
            ir::Ty::Struct(id) => {
                let def = ir
                    .get_struct(*id)
                    .ok_or(anyhow!("Invalid struct id: {}", id))?;
                let layout = def.layout(ir)?;

                let fields = def
                    .members
                    .iter()
                    .zip(&layout.byte_offsets)
                    .map(|(member, offset)| {
                        Self::load_internal(
                            ptr,
                            base_offset,
                            &member.ty,
                            ir,
                            builder,
                            struct_offset + *offset,
                            fvalues,
                        )
                    })
                    .collect::<anyhow::Result<_>>()?;

                Ok(FValueDesc::Struct(FStruct {
                    fields,
                    def_id: *id,
                    size: layout.size,
                }))
            }
            ir::Ty::Enum(id) => {
                let def = ir.get_enum(*id).ok_or(anyhow!("Invalid enum id: {}", id))?;
                let layout = def.layout(ir)?;

                let index_ty = layout
                    .index_ty
                    .as_ref()
                    .map(|ty| CraneliftJitBackend::jit_ty(ty));

                if let Some(index_ty) = index_ty {
                    let index_value = builder.ins().load(
                        index_ty,
                        MemFlags::trusted(),
                        ptr,
                        base_offset + struct_offset as i32,
                    );
                    let idx = fvalues.len();
                    fvalues.push((index_value, index_ty, struct_offset));

                    let variant_defaults = def
                        .variants
                        .iter()
                        .map(|v| {
                            Ok(if let Some(v_ty) = &v.ty {
                                Some(Self::zeroed_ty(v_ty, ir, builder)?)
                            } else {
                                None
                            })
                        })
                        .collect::<Result<Vec<_>>>()?;

                    let join_block = builder.create_block();
                    let default_block = builder.create_block();

                    for d in &variant_defaults {
                        let Some(d) = d else {
                            continue;
                        };
                        for (_, ty, _) in &d.values {
                            builder.append_block_param(join_block, *ty);
                        }
                    }

                    let mut switch = Switch::new();
                    let branches = def
                        .variants
                        .iter()
                        .enumerate()
                        .map(|(i, v)| {
                            if let Some(v_ty) = &v.ty {
                                let block = builder.create_block();
                                switch.set_entry(i as u128, block);
                                Some((block, v_ty))
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();

                    switch.emit(builder, index_value, default_block);

                    builder.switch_to_block(default_block);
                    builder.ins().jump(
                        join_block,
                        &variant_defaults
                            .iter()
                            .filter_map(|v| v.as_ref())
                            .flat_map(|v| &v.values)
                            .map(|(v, _, _)| BlockArg::Value(*v))
                            .collect::<Vec<_>>(),
                    );

                    for ((b_idx, b), (_, v_offset)) in
                        branches.into_iter().enumerate().zip(layout.variants)
                    {
                        let Some((block, v_ty)) = b else {
                            continue;
                        };
                        builder.switch_to_block(block);

                        let loaded =
                            Self::load(ptr, (struct_offset + v_offset) as i32, v_ty, ir, builder)?;

                        builder.ins().jump(
                            join_block,
                            &variant_defaults
                                .iter()
                                .enumerate()
                                .filter_map(|(i, v)| {
                                    if i != b_idx {
                                        v.as_ref()
                                    } else {
                                        Some(&loaded)
                                    }
                                })
                                .flat_map(|v| &v.values)
                                .map(|(v, _, _)| BlockArg::Value(*v))
                                .collect::<Vec<_>>(),
                        );
                    }

                    builder.switch_to_block(join_block);
                    let args = builder.block_params(join_block);
                    let mut used_args = 0;
                    let mut variants = Vec::new();
                    for v in &def.variants {
                        if let Some(v_ty) = &v.ty {
                            let arg_c = Self::value_count(v_ty, ir)?;
                            variants.push(Some(Self::from_flattened_internal(
                                &args[used_args..(used_args + arg_c)],
                                v_ty,
                                ir,
                                struct_offset,
                                fvalues,
                            )?));
                            used_args += arg_c;
                        } else {
                            variants.push(None);
                        }
                    }

                    Ok(FValueDesc::Enum(FEnum {
                        index: Some(idx),
                        variants,
                        size: layout.size,
                        def_id: *id,
                    }))
                } else {
                    let variant = if def.variants.is_empty() {
                        None
                    } else {
                        let v = &def.variants[0];
                        v.ty.as_ref()
                            .map(|v_ty| {
                                Self::load_internal(
                                    ptr,
                                    base_offset,
                                    v_ty,
                                    ir,
                                    builder,
                                    struct_offset + layout.variants[0].1,
                                    fvalues,
                                )
                            })
                            .transpose()?
                    };

                    Ok(FValueDesc::Enum(FEnum {
                        index: None,
                        variants: vec![variant],
                        size: layout.size,
                        def_id: *id,
                    }))
                }
            }
        }
    }

    pub fn store(
        &self,
        ptr: Value,
        base_offset: i32,
        ir: &ir::Module,
        ctx: &mut FunctionCtx,
    ) -> anyhow::Result<()> {
        self.store_internal(ptr, base_offset, ir, ctx, &self.desc)
    }

    fn store_internal(
        &self,
        ptr: Value,
        base_offset: i32,
        ir: &ir::Module,
        ctx: &mut FunctionCtx,
        desc: &FValueDesc,
    ) -> anyhow::Result<()> {
        match desc {
            FValueDesc::Value(index) => {
                let (value, _, offset) = self.values[*index];
                ctx.builder.ins().store(
                    MemFlags::trusted(),
                    value,
                    ptr,
                    base_offset + offset as i32,
                );
                Ok(())
            }
            FValueDesc::Struct(obj) => {
                for field_desc in &obj.fields {
                    self.store_internal(ptr, base_offset, ir, ctx, field_desc)?;
                }
                Ok(())
            }
            FValueDesc::Enum(obj) => {
                if let Some(index_idx) = obj.index {
                    let (index_value, _, offset) = self.values[index_idx];
                    ctx.builder.ins().store(
                        MemFlags::trusted(),
                        index_value,
                        ptr,
                        base_offset + offset as i32,
                    );

                    let mut switch = Switch::new();
                    let join_block = ctx.builder.create_block();
                    let branches = obj
                        .variants
                        .iter()
                        .enumerate()
                        .filter_map(|(i, v)| {
                            let Some(v_desc) = v else {
                                return None;
                            };
                            let block = ctx.builder.create_block();
                            switch.set_entry(i as u128, block);
                            Some((v_desc, block))
                        })
                        .collect::<Vec<_>>();

                    switch.emit(&mut ctx.builder, index_value, join_block);
                    for (v_desc, block) in branches {
                        ctx.builder.switch_to_block(block);

                        self.store_internal(ptr, base_offset, ir, ctx, v_desc)?;

                        ctx.builder.ins().jump(join_block, &[]);
                    }

                    ctx.builder.switch_to_block(join_block);
                } else {
                    // No discriminant, store the single variant
                    for variant_desc in &obj.variants {
                        if let Some(v_desc) = variant_desc {
                            self.store_internal(ptr, base_offset, ir, ctx, v_desc)?;
                        }
                    }
                }
                Ok(())
            }
        }
    }

    pub fn from_packed_args(
        arg_values: &[Value],
        arg_classes: &[ArgClass],
        ty: &ir::Ty,
        ir: &ir::Module,
        builder: &mut FunctionBuilder,
    ) -> anyhow::Result<Self> {
        let mut fvalues = Vec::new();
        let desc = Self::from_packed_args_internal(
            arg_values,
            arg_classes,
            0,
            ty,
            ir,
            builder,
            &mut fvalues,
        )?;
        Ok(Self {
            values: fvalues,
            desc,
        })
    }

    fn from_packed_args_internal(
        arg_values: &[Value],
        arg_classes: &[ArgClass],
        byte_offset: i64,
        ty: &ir::Ty,
        ir: &ir::Module,
        builder: &mut FunctionBuilder,
        fvalues: &mut Vec<(Value, Type, usize)>,
    ) -> anyhow::Result<FValueDesc> {
        match ty {
            ir::Ty::Native(native_type) => {
                let ty = CraneliftJitBackend::jit_ty(native_type);
                let eightbyte_idx = (byte_offset / 8) as usize;
                if eightbyte_idx >= arg_values.len() {
                    bail!(
                        "Not enough 64 bit ints to cast type. Found {} but needed {}",
                        arg_values.len(),
                        eightbyte_idx + 1
                    );
                }

                let arg_class = arg_classes[eightbyte_idx];
                match arg_class {
                    ArgClass::NoClass => bail!("Cannot pass an arg with no class"),
                    ArgClass::General => {
                        let bit_offset = (byte_offset % 8) * 8;

                        let value = builder
                            .ins()
                            .ushr_imm(arg_values[eightbyte_idx], bit_offset);
                        let value = size_value(value, ty.as_int(), builder);
                        let value = builder.ins().bitcast(ty, MemFlags::new(), value);

                        let index = fvalues.len();
                        fvalues.push((value, ty, byte_offset as usize));
                        Ok(FValueDesc::Value(index))
                    }
                    ArgClass::Vector => {
                        let value = match native_type {
                            ir::NativeType::Bool
                            | ir::NativeType::U8
                            | ir::NativeType::I8
                            | ir::NativeType::U16
                            | ir::NativeType::I16
                            | ir::NativeType::U32
                            | ir::NativeType::I32
                            | ir::NativeType::U64
                            | ir::NativeType::I64
                            | ir::NativeType::Ptr(_, _) => {
                                bail!("Packing ints in vector registers not supported")
                            }
                            ir::NativeType::F32 => {
                                let lane_offset = (byte_offset % 8) / 4;
                                let typed_vec = builder.ins().bitcast(
                                    types::F32X4,
                                    MemFlags::new().with_endianness(Endianness::Little),
                                    arg_values[eightbyte_idx],
                                );
                                builder.ins().extractlane(typed_vec, lane_offset as u8)
                            }
                            ir::NativeType::F64 => builder.ins().bitcast(
                                types::F64,
                                MemFlags::new(),
                                arg_values[eightbyte_idx],
                            ),
                        };

                        let index = fvalues.len();
                        fvalues.push((value, ty, byte_offset as usize));
                        Ok(FValueDesc::Value(index))
                    }
                    ArgClass::Memory => {
                        bail!("Cannot pack arguments that must be passed in memory")
                    }
                }
            }
            ir::Ty::Struct(id) => {
                let def = ir
                    .get_struct(*id)
                    .ok_or(anyhow!("Invalid struct id: {}", id))?;
                let layout = def.layout(ir)?;

                if (layout.size + byte_offset as usize) / 8 > arg_values.len() {
                    bail!(
                        "Cannot fit struct {} of size {} in provided {} eightbytes",
                        id,
                        layout.size,
                        arg_values.len()
                    );
                }

                let fields = def
                    .members
                    .iter()
                    .zip(&layout.byte_offsets)
                    .map(|(member, offset)| {
                        Self::from_packed_args_internal(
                            arg_values,
                            arg_classes,
                            byte_offset + *offset as i64,
                            &member.ty,
                            ir,
                            builder,
                            fvalues,
                        )
                    })
                    .collect::<anyhow::Result<_>>()?;

                Ok(FValueDesc::Struct(FStruct {
                    fields,
                    def_id: *id,
                    size: layout.size,
                }))
            }
            ir::Ty::Enum(id) => {
                let def = ir.get_enum(*id).ok_or(anyhow!("Invalid enum id: {}", id))?;
                let layout = def.layout(ir)?;

                if (layout.size + byte_offset as usize) / 8 > arg_values.len() {
                    bail!(
                        "Cannot fit enum {} of size {} in provided {} eightbytes",
                        id,
                        layout.size,
                        arg_values.len()
                    );
                }

                let index = if let Some(index_ty) = &layout.index_ty {
                    let FValueDesc::Value(index_idx) = Self::from_packed_args_internal(
                        arg_values,
                        arg_classes,
                        byte_offset,
                        &ir::Ty::Native(index_ty.clone()),
                        ir,
                        builder,
                        fvalues,
                    )?
                    else {
                        unreachable!("Index should be a single value");
                    };

                    let index_value = fvalues[index_idx].0;

                    let variant_defaults = def
                        .variants
                        .iter()
                        .map(|v| {
                            Ok(if let Some(v_ty) = &v.ty {
                                Some(Self::zeroed_ty(v_ty, ir, builder)?)
                            } else {
                                None
                            })
                        })
                        .collect::<Result<Vec<_>>>()?;

                    let join_block = builder.create_block();
                    let default_block = builder.create_block();

                    for d in &variant_defaults {
                        let Some(d) = d else {
                            continue;
                        };
                        for (_, ty, _) in &d.values {
                            builder.append_block_param(join_block, *ty);
                        }
                    }

                    let mut switch = Switch::new();
                    let branches = def
                        .variants
                        .iter()
                        .enumerate()
                        .map(|(i, v)| {
                            if let Some(v_ty) = &v.ty {
                                let block = builder.create_block();
                                switch.set_entry(i as u128, block);
                                Some((block, v_ty))
                            } else {
                                None
                            }
                        })
                        .collect::<Vec<_>>();

                    switch.emit(builder, index_value, default_block);

                    builder.switch_to_block(default_block);
                    builder.ins().jump(
                        join_block,
                        &variant_defaults
                            .iter()
                            .filter_map(|v| v.as_ref())
                            .flat_map(|v| &v.values)
                            .map(|(v, _, _)| BlockArg::Value(*v))
                            .collect::<Vec<_>>(),
                    );

                    for ((b_idx, b), (_, v_offset)) in
                        branches.into_iter().enumerate().zip(&layout.variants)
                    {
                        let Some((block, v_ty)) = b else {
                            continue;
                        };
                        builder.switch_to_block(block);

                        let mut variant_values = Vec::new();
                        Self::from_packed_args_internal(
                            arg_values,
                            arg_classes,
                            byte_offset + *v_offset as i64,
                            v_ty,
                            ir,
                            builder,
                            &mut variant_values,
                        )?;

                        builder.ins().jump(
                            join_block,
                            &variant_defaults
                                .iter()
                                .enumerate()
                                .filter_map(|(i, v)| v.as_ref().map(|v| (i, v)))
                                .flat_map(|(i, v)| {
                                    if i == b_idx {
                                        &variant_values
                                    } else {
                                        &v.values
                                    }
                                })
                                .map(|(v, _, _)| BlockArg::Value(*v))
                                .collect::<Vec<_>>(),
                        );
                    }

                    builder.switch_to_block(join_block);
                    let args = builder.block_params(join_block);
                    let mut used_args = 0;
                    let mut variants = Vec::new();
                    for v in &def.variants {
                        if let Some(v_ty) = &v.ty {
                            let arg_c = Self::value_count(v_ty, ir)?;
                            variants.push(Some(Self::from_flattened_internal(
                                &args[used_args..(used_args + arg_c)],
                                v_ty,
                                ir,
                                byte_offset as usize,
                                fvalues,
                            )?));
                            used_args += arg_c;
                        } else {
                            variants.push(None);
                        }
                    }
                    Some(index_idx)
                } else {
                    None
                };

                let variants = if index.is_none() {
                    def.variants
                        .iter()
                        .zip(layout.variants)
                        .map(|(v, (_, v_offset))| {
                            v.ty.as_ref()
                                .map(|v_ty| {
                                    Self::from_packed_args_internal(
                                        arg_values,
                                        arg_classes,
                                        byte_offset + v_offset as i64,
                                        v_ty,
                                        ir,
                                        builder,
                                        fvalues,
                                    )
                                })
                                .transpose()
                        })
                        .collect::<anyhow::Result<_>>()?
                } else {
                    vec![]
                };

                Ok(FValueDesc::Enum(FEnum {
                    index,
                    variants,
                    size: layout.size,
                    def_id: *id,
                }))
            }
        }
    }

    fn to_packed_args(
        &self,
        arg_values: &mut [Value],
        arg_classes: &[ArgClass],
        ir: &ir::Module,
        ctx: &mut FunctionCtx,
    ) -> anyhow::Result<()> {
        self.to_packed_args_internal(arg_values, arg_classes, ir, ctx, &self.desc)
    }

    fn to_packed_args_internal(
        &self,
        arg_values: &mut [Value],
        arg_classes: &[ArgClass],
        ir: &ir::Module,
        ctx: &mut FunctionCtx,
        desc: &FValueDesc,
    ) -> anyhow::Result<()> {
        match desc {
            FValueDesc::Value(index) => {
                let (value, ty, byte_offset) = self.values[*index];
                let eightbyte_idx = byte_offset / 8;
                if eightbyte_idx >= arg_values.len() {
                    bail!(
                        "Not enough 64 bit ints to cast type. Found {} but needed {}",
                        arg_values.len(),
                        eightbyte_idx + 1
                    );
                }

                let arg_class = arg_classes[eightbyte_idx];
                match arg_class {
                    ArgClass::NoClass => bail!("Cannot pass an arg with no class"),
                    ArgClass::General => {
                        let bit_offset = (byte_offset % 8) * 8;

                        let value = ctx
                            .builder
                            .ins()
                            .bitcast(ty.as_int(), MemFlags::new(), value);
                        let value = size_value(value, types::I64, &mut ctx.builder);
                        let value = ctx.builder.ins().ishl_imm(value, bit_offset as i64);
                        let current = arg_values[eightbyte_idx];
                        arg_values[eightbyte_idx] = ctx.builder.ins().bor(value, current);
                    }
                    ArgClass::Vector => {
                        arg_values[eightbyte_idx] = match ty {
                            types::F32 => {
                                let lane_offset = (byte_offset % 8) / 4;
                                let typed_vec = ctx.builder.ins().bitcast(
                                    types::F32X4,
                                    MemFlags::new().with_endianness(Endianness::Little),
                                    arg_values[eightbyte_idx],
                                );
                                let mutated_vec = ctx.builder.ins().insertlane(
                                    typed_vec,
                                    value,
                                    lane_offset as u8,
                                );
                                ctx.builder.ins().bitcast(
                                    types::F64X2,
                                    MemFlags::new().with_endianness(Endianness::Little),
                                    mutated_vec,
                                )
                            }
                            // No vector packing needed
                            types::F64 => value,
                            new_type => bail!(
                                "No support for packing value of type {} into vector args",
                                new_type
                            ),
                        };
                    }
                    ArgClass::Memory => {
                        bail!("Cannot unpack arguments that must be passed in memory")
                    }
                }

                Ok(())
            }
            FValueDesc::Struct(obj) => {
                for field_desc in &obj.fields {
                    self.to_packed_args_internal(arg_values, arg_classes, ir, ctx, field_desc)?;
                }
                Ok(())
            }
            FValueDesc::Enum(obj) => {
                if let Some(index_idx) = obj.index {
                    let (index_value, _index_ty, _struct_offset) = self.values[index_idx];

                    // Store discriminant
                    let discriminant_desc = FValueDesc::Value(index_idx);
                    self.to_packed_args_internal(
                        arg_values,
                        arg_classes,
                        ir,
                        ctx,
                        &discriminant_desc,
                    )?;

                    let mut switch = Switch::new();
                    let default_block = ctx.builder.create_block();
                    let join_block = ctx.builder.create_block();
                    for _ in arg_values.iter() {
                        ctx.builder.append_block_param(join_block, types::I64);
                    }

                    let branches = obj
                        .variants
                        .iter()
                        .enumerate()
                        .filter_map(|(i, v)| {
                            let Some(v_desc) = v else {
                                return None;
                            };
                            let block = ctx.builder.create_block();
                            switch.set_entry(i as u128, block);
                            Some((v_desc, block))
                        })
                        .collect::<Vec<_>>();

                    switch.emit(&mut ctx.builder, index_value, default_block);

                    ctx.builder.switch_to_block(default_block);
                    ctx.builder.ins().jump(
                        join_block,
                        &arg_values
                            .iter()
                            .map(|eb| BlockArg::Value(*eb))
                            .collect::<Vec<_>>(),
                    );

                    let mut v_eightbytes = arg_values.to_vec();
                    for (v_desc, block) in branches {
                        ctx.builder.switch_to_block(block);

                        // Reset to base eightbytes for this variant
                        for (dest, src) in v_eightbytes.iter_mut().zip(arg_values.iter()) {
                            *dest = *src;
                        }

                        self.to_packed_args_internal(
                            &mut v_eightbytes,
                            arg_classes,
                            ir,
                            ctx,
                            v_desc,
                        )?;

                        ctx.builder.ins().jump(
                            join_block,
                            &v_eightbytes
                                .iter()
                                .map(|eb| BlockArg::Value(*eb))
                                .collect::<Vec<_>>(),
                        );
                    }

                    ctx.builder.switch_to_block(join_block);

                    // Update eightbytes with PHI'd values
                    for (eb, value) in arg_values
                        .iter_mut()
                        .zip(ctx.builder.block_params(join_block))
                    {
                        *eb = *value;
                    }
                } else {
                    // No discriminant, handle single variant
                    for variant_desc in &obj.variants {
                        if let Some(v_desc) = variant_desc {
                            self.to_packed_args_internal(arg_values, arg_classes, ir, ctx, v_desc)?;
                        }
                    }
                }
                Ok(())
            }
        }
    }

    // If an agregate type only contains the same type of vector
    // register stored variables and none of the addressable fields overlap
    //
    // # Returns
    //
    // Returns the base type that constructs this agregate, as well as the number of uniquely
    // addressable fields
    pub fn is_homogeneous_simd_aggregate(ty: &ir::Ty, ir: &ir::Module) -> Option<(Type, usize)> {
        let (ty, fields) = match ty {
            ir::Ty::Native(nt) => {
                let ty = CraneliftJitBackend::jit_ty(nt);
                if !(ty.is_float() || ty.is_vector()) {
                    return None;
                }
                (ty, 1)
            }
            ir::Ty::Struct(id) => {
                let Some(def) = ir.get_struct(*id) else {
                    return None;
                };
                let Some(first) = def.members.first() else {
                    return None;
                };
                let Some((base_ty, mut fields)) =
                    Self::is_homogeneous_simd_aggregate(&first.ty, ir)
                else {
                    return None;
                };
                for m in &def.members[1..] {
                    let Some((m_ty, m_fields)) = Self::is_homogeneous_simd_aggregate(&m.ty, ir)
                    else {
                        return None;
                    };
                    if m_ty != base_ty {
                        return None;
                    }
                    fields += m_fields;
                }
                (base_ty, fields)
            }
            ir::Ty::Enum(id) => {
                let Some(def) = ir.get_enum(*id) else {
                    return None;
                };
                // if there's more then one variant, we have an int discriminant
                if def.variants.len() > 1 {
                    return None;
                }
                let Some(ir::EnumVariant {
                    id: _,
                    ty: Some(inner_ty),
                }) = def.variants.first()
                else {
                    return None;
                };

                return Self::is_homogeneous_simd_aggregate(inner_ty, ir);
            }
        };
        Some((ty, fields))
    }

    pub fn value_count(ty: &ir::Ty, module: &ir::Module) -> Result<usize> {
        match ty {
            ir::Ty::Native(_) => Ok(1usize),
            ir::Ty::Struct(id) => {
                let struct_def = module
                    .get_struct(*id)
                    .ok_or_else(|| anyhow!("Invalid struct ID: {}", id))?;
                let mut count = 0;
                for member in struct_def.members.iter() {
                    count += Self::value_count(&member.ty, module)?;
                }
                Ok(count)
            }
            ir::Ty::Enum(id) => {
                let enum_def = module
                    .get_enum(*id)
                    .ok_or_else(|| anyhow!("Invalid enum ID: {}", id))?;
                let mut count = 1;
                for variant in enum_def.variants.iter() {
                    if let Some(ty) = &variant.ty {
                        count += Self::value_count(&ty, module)?;
                    }
                }
                Ok(count)
            }
        }
    }

    pub fn from_flattened(values: &[Value], ty: &ir::Ty, ir: &ir::Module) -> anyhow::Result<Self> {
        let mut fvalues = Vec::new();
        fvalues.reserve_exact(values.len());
        let desc = Self::from_flattened_internal(values, ty, ir, 0, &mut fvalues)?;
        Ok(Self {
            values: fvalues,
            desc,
        })
    }

    pub fn from_flattened_internal(
        values: &[Value],
        ty: &ir::Ty,
        ir: &ir::Module,
        base_offset: usize,
        fvalues: &mut Vec<(Value, Type, usize)>,
    ) -> anyhow::Result<FValueDesc> {
        match ty {
            ir::Ty::Native(native_ty) => {
                if values.len() != 1 {
                    bail!(
                        "Expected a single value for native type {:?}, but got {} values",
                        native_ty,
                        values.len()
                    );
                }
                let index = fvalues.len();
                fvalues.push((
                    values[0],
                    CraneliftJitBackend::jit_ty(native_ty),
                    base_offset,
                ));
                Ok(FValueDesc::Value(index))
            }
            ir::Ty::Struct(def_id) => {
                let def_id = *def_id;
                let struct_def = ir
                    .get_struct(def_id)
                    .ok_or_else(|| anyhow!("Invalid struct ID: {}", def_id))?;
                let layout = struct_def.layout(ir)?;
                let mut fields = Vec::new();
                let mut offset = 0;

                for (member, byte_offset) in struct_def.members.iter().zip(layout.byte_offsets) {
                    let member_width = Self::value_count(&member.ty, ir)?;
                    let member_values = &values[offset..(offset + member_width)];
                    let member_desc = Self::from_flattened_internal(
                        member_values,
                        &member.ty,
                        ir,
                        base_offset + byte_offset,
                        fvalues,
                    )?;
                    fields.push(member_desc);
                    offset += member_width;
                }

                Ok(FValueDesc::Struct(FStruct {
                    fields,
                    def_id,
                    size: layout.size,
                }))
            }
            ir::Ty::Enum(def_id) => {
                let def_id = *def_id;
                let enum_def = ir
                    .get_enum(def_id)
                    .ok_or_else(|| anyhow!("Invalid enum ID: {}", def_id))?;
                let layout = enum_def.layout(ir)?;
                let index_ty = enum_def
                    .layout(ir)?
                    .index_ty
                    .map(|ty| CraneliftJitBackend::jit_ty(&ty));

                let mut offset = 0;

                let index = if index_ty.is_some() {
                    offset += 1;
                    let index = fvalues.len();
                    fvalues.push((
                        values
                            .get(0)
                            .ok_or(anyhow!("Expected discriminant value for enum"))?
                            .to_owned(),
                        index_ty.unwrap(),
                        base_offset,
                    ));
                    Some(index)
                } else {
                    None
                };
                let mut variants = Vec::new();

                for (variant, (_, v_offset)) in enum_def.variants.iter().zip(layout.variants) {
                    let variant_width = variant
                        .ty
                        .as_ref()
                        .map_or(Ok(0), |ty| Self::value_count(ty, ir))?;
                    let variant_values = &values[offset..(offset + variant_width)];
                    let variant_svalue = if let Some(ty) = &variant.ty {
                        Some(Self::from_flattened_internal(
                            variant_values,
                            ty,
                            ir,
                            v_offset + base_offset,
                            fvalues,
                        )?)
                    } else {
                        None
                    };
                    variants.push(variant_svalue);
                    offset += variant_width;
                }

                Ok(FValueDesc::Enum(FEnum {
                    variants,
                    index,
                    def_id,
                    size: layout.size,
                }))
            }
        }
    }
}

fn size_value(value: Value, ty: Type, builder: &mut FunctionBuilder) -> Value {
    let current_ty = builder.func.dfg.value_type(value);
    if current_ty.bytes() < ty.bytes() {
        builder.ins().uextend(ty, value)
    } else if current_ty.bytes() > ty.bytes() {
        builder.ins().ireduce(ty, value)
    } else {
        value
    }
}
