use std::collections::HashMap;
use target_lexicon::{Archetechture, Architecture, CallingConvention};

use anyhow::{Result, anyhow, bail};
use brane_core::ir;
use cranelift_codegen::ir::{
    ArgumentExtension, ArgumentPurpose, BlockArg, Signature, StackSlotData, StackSlotKind,
};
use cranelift_codegen::{
    isa,
    settings::{self, Configurable},
};
use cranelift_frontend::Switch;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, FuncId, Linkage, Module, ModuleError};

use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::types;
use cranelift_codegen::ir::{AbiParam, Block, InstBuilder, MemFlags, Type, Value};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};

pub struct CraneliftJitBackend {
    pub isa: isa::OwnedTargetIsa,
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

        Self { isa }
    }
}

impl CraneliftJitBackend {
    pub fn jit(&mut self, module: ir::Module) -> Result<(HashMap<String, FuncId>, JITModule)> {
        let jit_module = JITModule::new(JITBuilder::with_isa(
            self.isa.clone(),
            cranelift_module::default_libcall_names(),
        ));
        let mut ctx = ModuleCtx {
            ctx: jit_module.make_context(),
            ptr_ty: jit_module.target_config().pointer_type(),
            ptr_bytes: jit_module.target_config().pointer_bytes(),
            module: jit_module,
            fn_map: Vec::new(),
            data_desc: DataDescription::new(),
            function_builder_ctx: FunctionBuilderContext::new(),
        };

        for func in module.functions.iter() {
            let signature = self.construct_fn_sig(func, &module)?;
            let fn_id = ctx
                .module
                .declare_function(&func.id, Linkage::Export, &signature)
                .map_err(|e| anyhow!("Failed to declare function {}", e))?;
            ctx.fn_map.push((func.id.clone(), fn_id));
        }

        for (index, func) in module.functions.iter().enumerate() {
            let fn_id = ctx.fn_map[index].1;
            self.jit_fn(func, &module, &mut ctx)?;
            ctx.module
                .define_function(fn_id, &mut ctx.ctx)
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
                    .context(ctx.ctx.func.clone())
                })?;
            let cs = self.isa.to_capstone()?;
            println!(
                "Emitted function {}:\n{}",
                func.id,
                ctx.ctx
                    .compiled_code()
                    .unwrap()
                    .disassemble(Some(&ctx.ctx.func.params), &cs)?
            );
            ctx.module.clear_context(&mut ctx.ctx);
        }

        ctx.module.finalize_definitions()?;

        Ok((ctx.fn_map.into_iter().collect(), ctx.module))
    }

    fn jit_fn(
        &mut self,
        func: &ir::Function,
        ir: &ir::Module,
        module: &mut ModuleCtx,
    ) -> Result<()> {
        let ptr_ty = module.ptr_ty;
        let ptr_bytes = module.ptr_bytes;
        let fn_ctx = &mut module.ctx.func;
        let ptr_ty = module.module.target_config().pointer_type();

        // add memory binding table
        fn_ctx.collect_debug_info();

        let mut builder = FunctionBuilder::new(fn_ctx, &mut module.function_builder_ctx);

        let blocks: Vec<_> = func
            .blocks
            .iter()
            .map(|b| {
                let n = builder.create_block();
                for phi in &b.phi_nodes {
                    builder.append_block_param(n, Self::jit_ty(&phi.ty));
                }
                n
            })
            .collect();

        if blocks.is_empty() {
            bail!("Function has no content!");
        }

        let entry = *blocks.first().unwrap();
        builder.append_block_params_for_function_params(entry);
        builder.switch_to_block(entry);
        builder.seal_block(entry);

        let mut ctx = FunctionCtx {
            mod_ctx: module,
            builder,
            block_values: Default::default(),
            blocks: blocks.clone(),
            ptr_ty,
            ptr_bytes,
        };

        for (block_id, (jit_block, ir_block)) in blocks.iter().zip(func.blocks.iter()).enumerate() {
            ctx.block_values.insert(block_id, Vec::new());
            ctx.builder.switch_to_block(*jit_block);
            for op in &ir_block.ops {
                let new_result: Option<Value> = match op {
                    ir::Op::Unary { op, value } => {
                        let value = self.jit_value(value, &mut ctx)?; /*TODO make this actually call an allocation callback*/
                        match op {
                            ir::UnaryOp::INeg => todo!(),
                            ir::UnaryOp::FNeg => todo!(),
                            ir::UnaryOp::Alloc => Some(ctx.builder.ins().iconst(types::I32, 0)),
                        }
                    }
                    ir::Op::Binary { op, left, right } => {
                        let left = self.jit_value(left, &mut ctx)?;
                        let right = self.jit_value(right, &mut ctx)?;
                        match op {
                            ir::BinaryOp::IAdd => Some(ctx.builder.ins().iadd(left, right)),
                            ir::BinaryOp::FAdd => Some(ctx.builder.ins().fadd(left, right)),
                            ir::BinaryOp::ISub => Some(ctx.builder.ins().isub(left, right)),
                            ir::BinaryOp::FSub => Some(ctx.builder.ins().fsub(left, right)),
                            ir::BinaryOp::IMul => Some(ctx.builder.ins().imul(left, right)),
                            ir::BinaryOp::FMul => Some(ctx.builder.ins().fmul(left, right)),
                            ir::BinaryOp::SDiv => Some(ctx.builder.ins().sdiv(left, right)),
                            ir::BinaryOp::UDiv => Some(ctx.builder.ins().udiv(left, right)),
                            ir::BinaryOp::FDiv => Some(ctx.builder.ins().fdiv(left, right)),
                            ir::BinaryOp::URem => Some(ctx.builder.ins().urem(left, right)),
                            ir::BinaryOp::SRem => Some(ctx.builder.ins().srem(left, right)),
                            ir::BinaryOp::SCmp(cmp_ty) => Some(ctx.builder.ins().icmp(
                                match cmp_ty {
                                    ir::CmpTy::Eq => IntCC::Equal,
                                    ir::CmpTy::Ne => IntCC::NotEqual,
                                    ir::CmpTy::Gt => IntCC::SignedGreaterThan,
                                    ir::CmpTy::Ge => IntCC::SignedGreaterThanOrEqual,
                                },
                                left,
                                right,
                            )),
                            ir::BinaryOp::UCmp(cmp_ty) => Some(ctx.builder.ins().icmp(
                                match cmp_ty {
                                    ir::CmpTy::Eq => IntCC::Equal,
                                    ir::CmpTy::Ne => IntCC::NotEqual,
                                    ir::CmpTy::Gt => IntCC::UnsignedGreaterThan,
                                    ir::CmpTy::Ge => IntCC::UnsignedGreaterThanOrEqual,
                                },
                                left,
                                right,
                            )),
                            ir::BinaryOp::FCmp(cmp_ty) => Some(ctx.builder.ins().fcmp(
                                match cmp_ty {
                                    ir::CmpTy::Eq => FloatCC::Equal,
                                    ir::CmpTy::Ne => FloatCC::NotEqual,
                                    ir::CmpTy::Gt => FloatCC::GreaterThan,
                                    ir::CmpTy::Ge => FloatCC::GreaterThanOrEqual,
                                },
                                left,
                                right,
                            )),
                            ir::BinaryOp::And => Some(ctx.builder.ins().band(left, right)),
                            ir::BinaryOp::Or => Some(ctx.builder.ins().bor(left, right)),
                            ir::BinaryOp::Xor => Some(ctx.builder.ins().bxor(left, right)),
                            ir::BinaryOp::ShiftL => Some(ctx.builder.ins().ishl(left, right)),
                            ir::BinaryOp::IShiftR => Some(ctx.builder.ins().sshr(left, right)),
                            ir::BinaryOp::UShiftR => Some(ctx.builder.ins().ushr(left, right)),
                        }
                    }
                    ir::Op::Load { ty, ptr } => {
                        let jit_ty = Self::jit_ty(ty);
                        let ptr = self.jit_value(ptr, &mut ctx)?;
                        Some(Self::load(ptr, jit_ty, &mut ctx))
                    }
                    ir::Op::Store { src, ptr } => {
                        let src = self.jit_value(src, &mut ctx)?;
                        let ptr = self.jit_value(ptr, &mut ctx)?;
                        Self::store(ptr, src, &mut ctx);
                        None
                    }
                    ir::Op::Call { func, input } => {
                        let struct_value = Value;
                        ctx.builder.ins().call(todo!(), args);
                        todo!("Function calls not implemented yet");
                    }
                    ir::Op::CallIndirect { func_handle, input } => todo!(),
                };
                ctx.block_values
                    .get_mut(&block_id)
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
                    let cond = self.jit_value(cond, &mut ctx)?;
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
                    let cond = self.jit_value(cond, &mut ctx)?;
                    switch.emit(&mut ctx.builder, cond, default);
                }
                ir::TermOp::Ret(value) => match value {
                    Some(values) => {
                        let values = values
                            .iter()
                            .map(|value| self.jit_value(value, &mut ctx))
                            .collect::<anyhow::Result<Vec<Value>>>()?;
                        todo!("Implement return ABI");
                        ctx.builder.ins().return_(&values);
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

        let entry = ctx.blocks[0];
        let bindings = ctx.builder.block_params(entry)[0];

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

        let entry = ctx.blocks[0];
        let bindings = ctx.builder.block_params(entry)[0];

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
                        Ok(BlockArg::Value(self.jit_value(&v.value, ctx)?))
                    })
            })
            .collect::<Result<Vec<_>>>()
    }

    fn jit_value(&self, value: &ir::Value, ctx: &mut FunctionCtx) -> Result<Value> {
        match value {
            ir::Value::PhiArg { block, arg } => {
                let b = ctx.get_block(*block)?;
                ctx.builder
                    .block_params(b)
                    .get(*arg as usize)
                    .ok_or_else(|| anyhow!("invalid phi node (block: {}, arg: {})", block, arg))
                    .cloned()
            }
            ir::Value::FnArg(arg) => {
                let b = ctx.blocks[0];
                ctx.builder
                    .block_params(b)
                    .get(*arg as usize + 1)
                    .ok_or_else(|| anyhow!("invalid function arg {}", arg))
                    .cloned()
            }
            ir::Value::ObjFnArg(_, _) => todo!(),
            ir::Value::BlockOp { block, op } => ctx
                .block_values
                .get(&(*block as usize))
                .ok_or_else(|| anyhow!("Invalid block ID {}", block))?
                .get(*op as usize)
                .ok_or_else(|| anyhow!("Invalid op id {}", op))?
                .ok_or_else(|| anyhow!("Op {} does not return a value!", op)),
            ir::Value::ObjBlockOp { block, op, index } => todo!(),
            ir::Value::Const(const_value) => Ok(match const_value {
                ir::ConstValue::Bool(v) => ctx.builder.ins().iconst(types::I8, *v as i64),
                ir::ConstValue::U8(v) => ctx.builder.ins().iconst(types::I8, *v as i64),
                ir::ConstValue::I8(v) => ctx.builder.ins().iconst(types::I8, *v as i64),
                ir::ConstValue::U16(v) => ctx.builder.ins().iconst(types::I16, *v as i64),
                ir::ConstValue::I16(v) => ctx.builder.ins().iconst(types::I16, *v as i64),
                ir::ConstValue::U32(v) => ctx.builder.ins().iconst(types::I32, *v as i64),
                ir::ConstValue::I32(v) => ctx.builder.ins().iconst(types::I32, *v as i64),
                ir::ConstValue::U64(v) => ctx.builder.ins().iconst(types::I64, *v as i64),
                ir::ConstValue::I64(v) => ctx.builder.ins().iconst(types::I64, *v as i64),
                ir::ConstValue::F32(v) => ctx.builder.ins().f32const(*v),
                ir::ConstValue::F64(v) => ctx.builder.ins().f64const(*v),
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
            ir::NativeType::U128 | ir::NativeType::I128 => types::I128,
        }
    }

    fn construct_fn_sig(&self, func: &ir::Function, ir: &ir::Module) -> anyhow::Result<Signature> {
        let triple = self.isa.triple();

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
                let mut sig = Signature::new(isa::CallConv::WindowsFastcall);

                if let Some(ret_ty) = &func.sig.ret_ty {
                    let mut ret_struct = |size: usize| {
                        match size {
                            0 => unreachable!(),
                            1 => sig.returns.push(AbiParam::new(types::I8)),
                            2 => sig.returns.push(AbiParam::new(types::I16)),
                            3 | 4 => sig.returns.push(AbiParam::new(types::I32)),
                            5..=8 => sig.returns.push(AbiParam::new(types::I64)),
                            _ => {
                                let ret_param = AbiParam::special(
                                    self.isa.pointer_type(),
                                    ArgumentPurpose::StructReturn,
                                );
                                // Windows wants us to pass a pointer, and then return
                                // that same pointer back when we return
                                sig.params.push(ret_param);
                                sig.returns.push(ret_param);
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

                for param in &func.sig.param_tys {
                    let mut pass_struct = |size: usize| match size {
                        0 => unreachable!(),
                        1 => sig.returns.push(AbiParam::new(types::I8)),
                        2 => sig.returns.push(AbiParam::new(types::I16)),
                        3 | 4 => sig.returns.push(AbiParam::new(types::I32)),
                        5..=8 => sig.returns.push(AbiParam::new(types::I64)),
                        _ => {
                            let struct_param = AbiParam::special(
                                self.isa.pointer_type(),
                                ArgumentPurpose::StructArgument(size as u32),
                            );
                            sig.params.push(struct_param);
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

                Ok(sig)
            }
            CallingConvention::SystemV => match &triple.architecture {
                Architecture::X86_64 => {
                    todo!();
                }
                Architecture::Aarch64(_) => {
                    todo!();
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
        args: &[SValue],
        ir: &ir::Module,
        fn_ctx: &mut FunctionCtx,
    ) -> anyhow::Result<Option<SValue>> {
        let triple = self.isa.triple();

        let sig = match callable {
            Callable::FnId(id) => {
                &ir.get_function(id)
                    .ok_or(anyhow!("Invalid function id {}", id))?
                    .sig
            }
            Callable::Pointer(value, fn_sig) => fn_sig,
        };

        let mut arg_values = Vec::new();
        let mut ret_alloc = None;

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

                for arg in args {
                    match arg {
                        SValue::Value(value, _ty) => {
                            arg_values.push(*value);
                        }
                        SValue::Struct(ssastruct) => {
                            let (size, align) = ir.size_align(&ir::Ty::Struct(ssastruct.def_id))?;
                            if size > 8 {
                                let stack_slot = StackSlotData::new(
                                    StackSlotKind::ExplicitSlot,
                                    size as u32,
                                    align as u8,
                                );
                                let stack_slot = fn_ctx.builder.create_sized_stack_slot(stack_slot);
                                let stack_ptr = fn_ctx.builder.ins().stack_addr(
                                    self.isa.pointer_type(),
                                    stack_slot,
                                    0,
                                );
                                arg.store(stack_ptr, 0, ir, fn_ctx)?;
                                arg_values.push(stack_ptr);
                            } else {
                                let mut eightbyte = [fn_ctx.builder.ins().iconst(types::I64, 0)];
                                arg.as_int_eightbytes(&mut eightbyte, 0, ir, fn_ctx)?;
                                arg_values.push(eightbyte[0]);
                            }
                        }
                        SValue::Enum(ssaenum) => {
                            let (size, align) = ir.size_align(&ir::Ty::Enum(ssaenum.def_id))?;
                            if size > 8 {
                                let stack_slot = StackSlotData::new(
                                    StackSlotKind::ExplicitSlot,
                                    size as u32,
                                    align as u8,
                                );
                                let stack_slot = fn_ctx.builder.create_sized_stack_slot(stack_slot);
                                let stack_ptr = fn_ctx.builder.ins().stack_addr(
                                    self.isa.pointer_type(),
                                    stack_slot,
                                    0,
                                );
                                arg.store(stack_ptr, 0, ir, fn_ctx)?;
                                arg_values.push(stack_ptr);
                            } else {
                                let mut eightbyte = [fn_ctx.builder.ins().iconst(types::I64, 0)];
                                arg.as_int_eightbytes(&mut eightbyte, 0, ir, fn_ctx)?;
                                arg_values.push(eightbyte[0]);
                            }
                        }
                    }
                }
            }
            CallingConvention::SystemV => match &triple.architecture {
                Architecture::X86_64 => {
                    todo!();
                }
                Architecture::Aarch64(_) => {
                    todo!();
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
                todo!("Indirect function calls not implemented yet")
            }
        };

        if let Some(ret_ty) = &sig.ret_ty {
            match triple
                .default_calling_convention()
                .expect("No default calling convention for this platform")
            {
                CallingConvention::WindowsFastcall => {
                    if let Some(ret_alloc) = ret_alloc {
                        Ok(Some(SValue::load(
                            fn_ctx
                                .builder
                                .ins()
                                .stack_addr(self.isa.pointer_type(), ret_alloc, 0),
                            0,
                            ret_ty,
                            ir,
                            fn_ctx,
                        )?))
                    } else {
                        let ret_val = fn_ctx.builder.inst_results(call_inst)[0];
                        match ret_ty {
                            ir::Ty::Native(native_type) => {
                                Ok(Some(SValue::Value(ret_val, Self::jit_ty(native_type))))
                            }
                            ir::Ty::Struct(_) => Ok(Some(SValue::from_int_eightbytes(
                                &[ret_val],
                                0,
                                ret_ty,
                                ir,
                                fn_ctx,
                            )?)),
                            ir::Ty::Enum(_) => Ok(Some(SValue::from_int_eightbytes(
                                &[ret_val],
                                0,
                                ret_ty,
                                ir,
                                fn_ctx,
                            )?)),
                        }
                    }
                }
                CallingConvention::SystemV => match &triple.architecture {
                    Architecture::X86_64 => {
                        todo!();
                    }
                    Architecture::Aarch64(_) => {
                        todo!();
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
        } else {
            Ok(None)
        }
    }

    fn destruct_fn_args(&self, func: &ir::Function) -> Vec<Value> {
        todo!();
    }

    fn return_data(&self, value: ValueOrObj, fn_ctx: &mut FunctionCtx) {
        todo!()
    }
}

struct ModuleCtx {
    module: JITModule,
    data_desc: DataDescription,
    ctx: cranelift_codegen::Context,
    function_builder_ctx: FunctionBuilderContext,
    fn_map: Vec<(String, FuncId)>,
    ptr_ty: Type,
    ptr_bytes: u8,
}

struct FunctionCtx<'a> {
    mod_ctx: &'a mut ModuleCtx,
    builder: FunctionBuilder<'a>,
    blocks: Vec<Block>,
    block_values: HashMap<usize, Vec<Option<SValue>>>,
    ptr_ty: Type,
    ptr_bytes: u8,
}

impl<'a> FunctionCtx<'a> {
    pub fn get_block(&self, block: u32) -> Result<Block> {
        self.blocks
            .get(block as usize)
            .ok_or_else(|| anyhow!("Invalid block ID {}", block))
            .cloned()
    }
}

enum Callable<'a> {
    FnId(ir::FnId),
    Pointer(Value, &'a ir::FnSig),
}

#[derive(Clone)]
pub struct SSAStruct {
    pub fields: Vec<(SValue, usize)>,
    pub def_id: ir::StructId,
}

impl SSAStruct {
    pub fn width(&self) -> usize {
        self.fields.len()
    }

    pub fn walk_values(
        &self,
        base_offset: usize,
        callback: impl Fn(Value, Type, usize) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        for (value, offset) in &self.fields {
            value.walk_values(base_offset + offset, &callback)?;
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct SSAEnum {
    /// None in the cases of a 0 or 1 variant enum, we don't need a discriminant in those cases
    index: Option<Value>,
    /// Represents a 1-1 mapping with all variants of the enum, with none values representing
    /// variants with no data
    variants: Vec<Option<SValue>>,
    union_offset: usize,
    index_ty: Option<Type>,
    def_id: ir::EnumId,
}

impl SSAEnum {
    pub fn width(&self) -> usize {
        self.variants.len() + 1 // +1 for the index
    }

    pub fn walk_values(
        &self,
        base_offset: usize,
        callback: impl Fn(Value, Type, usize) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        if let (Some(index), Some(index_ty)) = (self.index, self.index_ty) {
            callback(index, index_ty, base_offset)?;
        }
        for variant in &self.variants {
            if let Some(svalue) = variant {
                svalue.walk_values(base_offset + self.union_offset, &callback)?;
            }
        }
        Ok(())
    }
}

/// Structured values are an abstraction over the flattened representation we use for structs and
/// enums, useful for easier construction of function arguments
#[derive(Clone)]
pub enum SValue {
    Value(Value, Type),
    Struct(SSAStruct),
    Enum(SSAEnum),
}

impl SValue {
    pub fn load(
        ptr: Value,
        base_offset: i32,
        ty: &ir::Ty,
        ir: &ir::Module,
        ctx: &mut FunctionCtx,
    ) -> anyhow::Result<Self> {
        Ok(match ty {
            ir::Ty::Native(native_type) => {
                let ty = CraneliftJitBackend::jit_ty(native_type);
                let value = ctx
                    .builder
                    .ins()
                    .load(ty, MemFlags::trusted(), ptr, base_offset);
                SValue::Value(value, ty)
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
                        Ok((
                            SValue::load(ptr, base_offset + *offset as i32, &member.ty, ir, ctx)?,
                            *offset,
                        ))
                    })
                    .collect::<anyhow::Result<_>>()?;

                SValue::Struct(SSAStruct {
                    fields,
                    def_id: *id,
                })
            }
            ir::Ty::Enum(id) => {
                let def = ir.get_enum(*id).ok_or(anyhow!("Invalid enum id: {}", id))?;
                let layout = def.layout(ir)?;

                let index_ty = layout.index_ty.map(|ty| CraneliftJitBackend::jit_ty(&ty));

                let mut variants = Vec::new();

                let index = if let Some(index_ty) = index_ty {
                    let index =
                        ctx.builder
                            .ins()
                            .load(index_ty, MemFlags::trusted(), ptr, base_offset);

                    let mut switch = Switch::new();

                    let join_block = ctx.builder.create_block();
                    let prev_block = ctx
                        .builder
                        .current_block()
                        .expect("Was expecting a current block");

                    for (i, v) in def.variants.iter().enumerate() {
                        let Some(v_ty) = &v.ty else {
                            variants.push(None);
                            continue;
                        };

                        let block = ctx.builder.create_block();
                        switch.set_entry(i as u128, block);
                        ctx.builder.switch_to_block(block);

                        variants.push(Some(SValue::load(
                            ptr,
                            layout.union_offset as i32 + base_offset,
                            v_ty,
                            ir,
                            ctx,
                        )?));

                        ctx.builder.ins().jump(join_block, &[]);
                    }

                    ctx.builder.switch_to_block(prev_block);
                    switch.emit(&mut ctx.builder, index, join_block);
                    ctx.builder.switch_to_block(join_block);

                    Some(index)
                } else {
                    None
                };

                SValue::Enum(SSAEnum {
                    index,
                    variants,
                    index_ty,
                    union_offset: layout.union_offset,
                    def_id: *id,
                })
            }
        })
    }

    pub fn store(
        &self,
        ptr: Value,
        base_offset: i32,
        ir: &ir::Module,
        ctx: &mut FunctionCtx,
    ) -> anyhow::Result<()> {
        match self {
            SValue::Value(value, _ty) => {
                ctx.builder
                    .ins()
                    .store(MemFlags::trusted(), *value, ptr, base_offset);
            }
            SValue::Struct(obj) => {
                let def = ir
                    .get_struct(obj.def_id)
                    .ok_or(anyhow!("Invalid struct id: {}", obj.def_id))?;
                let layout = def.layout(ir)?;

                for (field, offset) in obj.fields.iter().zip(&layout.byte_offsets) {
                    field.0.store(ptr, base_offset + *offset as i32, ir, ctx)?
                }
            }
            SValue::Enum(obj) => {
                let def = ir
                    .get_enum(obj.def_id)
                    .ok_or(anyhow!("Invalid enum id: {}", obj.def_id))?;
                let layout = def.layout(ir)?;

                if let Some(index) = obj.index {
                    ctx.builder
                        .ins()
                        .store(MemFlags::trusted(), index, ptr, base_offset);

                    let mut switch = Switch::new();

                    let join_block = ctx.builder.create_block();
                    let prev_block = ctx
                        .builder
                        .current_block()
                        .expect("Was expecting a current block");

                    for (i, v) in obj.variants.iter().enumerate() {
                        let Some(v) = v else {
                            continue;
                        };

                        let block = ctx.builder.create_block();
                        switch.set_entry(i as u128, block);
                        ctx.builder.switch_to_block(block);

                        v.store(ptr, layout.union_offset as i32 + base_offset, ir, ctx)?;

                        ctx.builder.ins().jump(join_block, &[]);
                    }

                    ctx.builder.switch_to_block(prev_block);
                    switch.emit(&mut ctx.builder, index, join_block);
                    ctx.builder.switch_to_block(join_block);

                    Some(index)
                } else {
                    None
                };
            }
        }
        Ok(())
    }

    pub fn from_int_eightbytes(
        eightbytes: &[Value],
        byte_offset: i64,
        ty: &ir::Ty,
        ir: &ir::Module,
        ctx: &mut FunctionCtx,
    ) -> anyhow::Result<Self> {
        Ok(match ty {
            ir::Ty::Native(native_type) => {
                let ty = CraneliftJitBackend::jit_ty(native_type);
                let eightbyte = byte_offset / 8;
                let value = ctx
                    .builder
                    .ins()
                    .ushr_imm(eightbytes[eightbyte as usize], byte_offset % 8);
                let value = ctx.builder.ins().ireduce(ty.as_int(), value);
                let value = ctx.builder.ins().bitcast(ty, MemFlags::trusted(), value);

                SValue::Value(value, ty)
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
                        Ok((
                            SValue::from_int_eightbytes(
                                eightbytes,
                                byte_offset + *offset as i64,
                                &member.ty,
                                ir,
                                ctx,
                            )?,
                            *offset,
                        ))
                    })
                    .collect::<anyhow::Result<_>>()?;

                SValue::Struct(SSAStruct {
                    fields,
                    def_id: *id,
                })
            }
            ir::Ty::Enum(id) => {
                let def = ir.get_enum(*id).ok_or(anyhow!("Invalid enum id: {}", id))?;
                let layout = def.layout(ir)?;

                let mut variants = Vec::new();

                let index = if let Some(index_ty) = &layout.index_ty {
                    let Self::Value(index, _) = Self::from_int_eightbytes(
                        eightbytes,
                        byte_offset,
                        &ir::Ty::Native(index_ty.clone()),
                        ir,
                        ctx,
                    )?
                    else {
                        unreachable!("Index should be a single value");
                    };

                    let mut switch = Switch::new();

                    let join_block = ctx.builder.create_block();
                    let prev_block = ctx
                        .builder
                        .current_block()
                        .expect("Was expecting a current block");

                    for (i, v) in def.variants.iter().enumerate() {
                        let Some(v_ty) = &v.ty else {
                            variants.push(None);
                            continue;
                        };

                        let block = ctx.builder.create_block();
                        switch.set_entry(i as u128, block);
                        ctx.builder.switch_to_block(block);

                        variants.push(Some(SValue::from_int_eightbytes(
                            eightbytes,
                            layout.union_offset as i64 + byte_offset,
                            v_ty,
                            ir,
                            ctx,
                        )?));

                        ctx.builder.ins().jump(join_block, &[]);
                    }

                    ctx.builder.switch_to_block(prev_block);
                    switch.emit(&mut ctx.builder, index, join_block);
                    ctx.builder.switch_to_block(join_block);

                    Some(index)
                } else {
                    None
                };

                SValue::Enum(SSAEnum {
                    index,
                    variants,
                    index_ty: layout.index_ty.map(|ty| CraneliftJitBackend::jit_ty(&ty)),
                    union_offset: layout.union_offset,
                    def_id: *id,
                })
            }
        })
    }

    /// Write this value into a set of general purpose registers preserving it's memory layout.
    ///
    /// eightbytes is expected to be a slice of values initialized to 0i64 large enough to contain
    /// the current value
    pub fn as_int_eightbytes(
        &self,
        eightbytes: &mut [Value],
        byte_offset: i64,
        ir: &ir::Module,
        ctx: &mut FunctionCtx,
    ) -> anyhow::Result<()> {
        match self {
            SValue::Value(value, ty) => {
                let eightbyte = byte_offset / 8;
                let value = ctx
                    .builder
                    .ins()
                    .bitcast(ty.as_int(), MemFlags::trusted(), *value);
                let value = ctx.builder.ins().uextend(types::I64, value);
                let value = ctx.builder.ins().ishl_imm(value, byte_offset % 8);
                let current = eightbytes[eightbyte as usize];
                eightbytes[eightbyte as usize] = ctx.builder.ins().band(value, current);
            }
            SValue::Struct(obj) => {
                let def = ir
                    .get_struct(obj.def_id)
                    .ok_or(anyhow!("Invalid struct id: {}", obj.def_id))?;
                let layout = def.layout(ir)?;

                for (field, offset) in obj.fields.iter().zip(&layout.byte_offsets) {
                    field
                        .0
                        .as_int_eightbytes(eightbytes, byte_offset + *offset as i64, ir, ctx)?
                }
            }
            SValue::Enum(obj) => {
                let def = ir
                    .get_enum(obj.def_id)
                    .ok_or(anyhow!("Invalid enum id: {}", obj.def_id))?;
                let layout = def.layout(ir)?;

                if let Some(index) = obj.index {
                    Self::Value(index, obj.index_ty.unwrap()).as_int_eightbytes(
                        eightbytes,
                        byte_offset,
                        ir,
                        ctx,
                    )?;

                    let mut switch = Switch::new();

                    let join_block = ctx.builder.create_block();
                    for _ in eightbytes.iter() {
                        ctx.builder.append_block_param(join_block, types::I64);
                    }
                    let prev_block = ctx
                        .builder
                        .current_block()
                        .expect("Was expecting a current block");

                    let mut v_eightbytes = eightbytes.to_vec();
                    for (i, v) in obj.variants.iter().enumerate() {
                        let Some(v) = v else {
                            continue;
                        };

                        let block = ctx.builder.create_block();
                        switch.set_entry(i as u128, block);
                        ctx.builder.switch_to_block(block);

                        for (dest, src) in v_eightbytes.iter_mut().zip(eightbytes.iter()) {
                            *dest = *src;
                        }

                        v.as_int_eightbytes(
                            &mut v_eightbytes,
                            layout.union_offset as i64 + byte_offset,
                            ir,
                            ctx,
                        )?;

                        ctx.builder.ins().jump(
                            join_block,
                            &v_eightbytes
                                .iter()
                                .map(|eb| BlockArg::Value(*eb))
                                .collect::<Vec<_>>(),
                        );
                    }

                    ctx.builder.switch_to_block(prev_block);
                    switch.emit(&mut ctx.builder, index, join_block);
                    ctx.builder.switch_to_block(join_block);

                    // Since SSA, need to return the PHI'd value
                    for (eb, value) in eightbytes
                        .iter_mut()
                        .zip(ctx.builder.block_params(join_block))
                    {
                        *eb = *value;
                    }

                    Some(index)
                } else {
                    None
                };
            }
        }
        Ok(())
    }

    pub fn width(&self) -> usize {
        match self {
            SValue::Value(_, _) => 1,
            SValue::Struct(ssa_struct) => ssa_struct.width(),
            SValue::Enum(ssa_enum) => ssa_enum.width(),
        }
    }

    /// Walk all fields of this object as if they were flattened. Callback is passed the Value,
    /// it's Type, and it's offset from the initial offset passed in as if it were stored in memory.
    pub fn walk_values(
        &self,
        base_offset: usize,
        callback: impl Fn(Value, Type, usize) -> anyhow::Result<()>,
    ) -> anyhow::Result<()> {
        match self {
            SValue::Value(value, ty) => callback(*value, *ty, base_offset),
            SValue::Struct(ssastruct) => ssastruct.walk_values(base_offset, callback),
            SValue::Enum(ssaenum) => ssaenum.walk_values(base_offset, callback),
        }
    }

    pub fn ty_width(ty: &ir::Ty, module: &ir::Module) -> Result<usize> {
        match ty {
            ir::Ty::Native(_) => Ok(1usize),
            ir::Ty::Struct(id) => {
                let struct_def = module
                    .get_struct(*id)
                    .ok_or_else(|| anyhow!("Invalid struct ID: {}", id))?;
                let mut count = 0;
                for member in struct_def.members.iter() {
                    count += Self::ty_width(&member.ty, module)?;
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
                        count += Self::ty_width(&ty, module)?;
                    }
                }
                Ok(count)
            }
        }
    }

    pub fn from_flattened(values: &[Value], ty: &ir::Ty, ir: &ir::Module) -> Result<SValue> {
        match ty {
            ir::Ty::Native(native_ty) => {
                if values.len() != 1 {
                    bail!(
                        "Expected a single value for native type {:?}, but got {} values",
                        native_ty,
                        values.len()
                    );
                }
                Ok(SValue::Value(
                    values[0],
                    CraneliftJitBackend::jit_ty(native_ty),
                ))
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
                    let member_width = Self::ty_width(&member.ty, ir)?;
                    let member_values = &values[offset..(offset + member_width)];
                    let member_svalue = Self::from_flattened(member_values, &member.ty, ir)?;
                    fields.push((member_svalue, byte_offset));
                    offset += member_width;
                }

                Ok(SValue::Struct(SSAStruct { fields, def_id }))
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
                    Some(
                        values
                            .get(0)
                            .ok_or(anyhow!("Expected discriminant value for enum"))?
                            .to_owned(),
                    )
                } else {
                    None
                };
                let mut variants = Vec::new();

                for variant in &enum_def.variants {
                    let variant_width = variant
                        .ty
                        .as_ref()
                        .map_or(Ok(0), |ty| Self::ty_width(ty, ir))?;
                    let variant_values = &values[offset..(offset + variant_width)];
                    let variant_svalue = if let Some(ty) = &variant.ty {
                        Some(Self::from_flattened(variant_values, ty, ir)?)
                    } else {
                        None
                    };
                    variants.push(variant_svalue);
                    offset += variant_width;
                }

                Ok(SValue::Enum(SSAEnum {
                    index_ty,
                    variants,
                    index,
                    def_id,
                    union_offset: layout.union_offset,
                }))
            }
        }
    }
}
