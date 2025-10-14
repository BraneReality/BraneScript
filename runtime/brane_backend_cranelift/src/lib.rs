use std::collections::HashMap;

use anyhow::{Result, anyhow, bail};
use brane_core::ir;
use cranelift_codegen::ir::BlockArg;
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
        let mut flag_builder = settings::builder();
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
            data_desc: DataDescription::new(),
            function_builder_ctx: FunctionBuilderContext::new(),
        };
        let mut fn_map = Vec::new();

        for func in module.functions.iter() {
            let fn_id = ctx
                .module
                .declare_function(&func.id, Linkage::Export, &ctx.ctx.func.signature)
                .map_err(|e| anyhow!("Failed to declare function {}", e))?;
            fn_map.push((func.id.clone(), fn_id));
        }

        for (index, func) in module.functions.iter().enumerate() {
            let fn_id = fn_map[index].1;
            self.jit_fn(func, &mut ctx)?;
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

        Ok((fn_map.into_iter().collect(), ctx.module))
    }

    fn jit_fn(&mut self, func: &ir::Function, module: &mut ModuleCtx) -> Result<()> {
        let ptr_ty = module.ptr_ty;
        let ptr_bytes = module.ptr_bytes;
        let fn_ctx = &mut module.ctx.func;
        let ptr_ty = module.module.target_config().pointer_type();

        // add memory binding table
        fn_ctx.collect_debug_info();
        fn_ctx.signature.params.push(AbiParam::new(ptr_ty));
        for p in &func.input {
            let p = match p {
                ir::Ty::Native(nt) => AbiParam::new(self.jit_ty(nt)),
                ir::Ty::Struct(_) => bail!("struct function params not supported"),
                ir::Ty::Enum(_) => bail!("enum function params not supported"),
            };
            fn_ctx.signature.params.push(p);
        }
        let ret = match func.output.as_slice() {
            [ir::Ty::Native(nt)] => AbiParam::new(self.jit_ty(nt)),
            [ir::Ty::Struct(_)] => bail!("struct function params not supported"),
            [ir::Ty::Enum(_)] => bail!("enum function params not supported"),
            _ => bail!("Ony one return value supported"),
        };
        fn_ctx.signature.returns.push(ret);

        let mut builder = FunctionBuilder::new(fn_ctx, &mut module.function_builder_ctx);

        let blocks: Vec<_> = func
            .blocks
            .iter()
            .map(|b| {
                let n = builder.create_block();
                for phi in &b.phi_nodes {
                    builder.append_block_param(n, self.jit_ty(&phi.ty));
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
                let new_value: Option<Value> = match op {
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
                        let jit_ty = self.jit_ty(ty);
                        let ptr = self.jit_value(ptr, &mut ctx)?;
                        Some(Self::load(ptr, jit_ty, &mut ctx))
                    }
                    ir::Op::Store { src, ptr } => {
                        let src = self.jit_value(src, &mut ctx)?;
                        let ptr = self.jit_value(ptr, &mut ctx)?;
                        Self::store(ptr, src, &mut ctx);
                        None
                    }
                    ir::Op::Call { func, input } => todo!(),
                    ir::Op::CallIndirect { func_handle, input } => todo!(),
                };
                ctx.block_values.get_mut(&block_id).unwrap().push(new_value);
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
                    Some(value) => {
                        let value = self.jit_value(value, &mut ctx)?;
                        ctx.builder.ins().return_(&[value]);
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
            ir::Value::VecFnArg(_, _) => todo!(),
            ir::Value::BlockOp { block, op } => ctx
                .block_values
                .get(&(*block as usize))
                .ok_or_else(|| anyhow!("Invalid block ID {}", block))?
                .get(*op as usize)
                .ok_or_else(|| anyhow!("Invalid op id {}", op))?
                .ok_or_else(|| anyhow!("Op {} does not return a value!", op)),
            ir::Value::VecBlockOp { block, op, index } => todo!(),
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

    fn jit_ty(&self, nt: &ir::NativeType) -> Type {
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
}

struct ModuleCtx {
    module: JITModule,
    data_desc: DataDescription,
    ctx: cranelift_codegen::Context,
    function_builder_ctx: FunctionBuilderContext,
    ptr_ty: Type,
    ptr_bytes: u8,
}

struct FunctionCtx<'a> {
    builder: FunctionBuilder<'a>,
    blocks: Vec<Block>,
    block_values: HashMap<usize, Vec<Option<Value>>>,
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
