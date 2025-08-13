use std::collections::HashMap;

use anyhow::{Result, anyhow, bail};
use brane_core::ir;
use cranelift::{codegen::ir::BlockArg, frontend::Switch, prelude::*};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, FuncId, Linkage, Module, ModuleError};

pub struct CraneliftJitBackend {
    pub isa: isa::OwnedTargetIsa,
}

impl Default for CraneliftJitBackend {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
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
    pub fn jit(&mut self, module: ir::Module) -> Result<(Vec<(String, FuncId)>, JITModule)> {
        let jit_module = JITModule::new(JITBuilder::with_isa(
            self.isa.clone(),
            cranelift_module::default_libcall_names(),
        ));
        let mut ctx = ModuleCtx {
            ctx: jit_module.make_context(),
            module: jit_module,
            data_desc: DataDescription::new(),
            function_builder_ctx: FunctionBuilderContext::new(),
        };
        let mut fn_map = Vec::new();
        for func in module.functions.iter() {
            self.jit_fn(func, &mut ctx)?;
            let fn_id = ctx
                .module
                .declare_function(&func.id, Linkage::Export, &ctx.ctx.func.signature)
                .map_err(|e| anyhow!("Failed to declare function {}", e))?;
            ctx.module
                .define_function(fn_id, &mut ctx.ctx)
                .map_err(|e: ModuleError| {
                    match e {
                        ModuleError::Compilation(ce) => match ce {
                            codegen::CodegenError::Verifier(verifier_errors) => {
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
            ctx.module.clear_context(&mut ctx.ctx);
            fn_map.push((func.id.clone(), fn_id));
        }

        ctx.module.finalize_definitions()?;

        Ok((fn_map, ctx.module))
    }

    fn jit_fn(&mut self, func: &ir::Function, module: &mut ModuleCtx) -> Result<()> {
        let fn_ctx = &mut module.ctx.func;
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
        };

        for (block_id, (jit_block, ir_block)) in blocks.iter().zip(func.blocks.iter()).enumerate() {
            ctx.block_values.insert(block_id, Vec::new());
            ctx.builder.switch_to_block(*jit_block);
            for op in &ir_block.ops {
                let new_value = match op {
                    ir::Op::AllocA { ty } => todo!(),
                    ir::Op::Load { ty, ptr } => todo!(),
                    ir::Op::Store { src, ptr } => todo!(),
                    ir::Op::INeg { arg } => todo!(),
                    ir::Op::FNeg { arg } => todo!(),
                    ir::Op::IAdd { left, right } => {
                        let left = self.jit_value(left, &mut ctx)?;
                        let right = self.jit_value(right, &mut ctx)?;
                        Some(ctx.builder.ins().iadd(left, right))
                    }
                    ir::Op::FAdd { left, right } => todo!(),
                    ir::Op::ISub { left, right } => todo!(),
                    ir::Op::FSub { left, right } => todo!(),
                    ir::Op::IMul { left, right } => todo!(),
                    ir::Op::FMul { left, right } => todo!(),
                    ir::Op::SDiv { left, right } => todo!(),
                    ir::Op::UDiv { left, right } => todo!(),
                    ir::Op::FDiv { left, right } => todo!(),
                    ir::Op::URem { left, right } => todo!(),
                    ir::Op::SRem { left, right } => todo!(),
                    ir::Op::FRem { left, right } => todo!(),
                    ir::Op::CmpEq { left, right } => todo!(),
                    ir::Op::CmpNe { left, right } => todo!(),
                    ir::Op::CmpGt { left, right } => {
                        let left = self.jit_value(left, &mut ctx)?;
                        let right = self.jit_value(right, &mut ctx)?;
                        Some(
                            ctx.builder
                                .ins()
                                .icmp(IntCC::SignedGreaterThan, left, right),
                        )
                    }
                    ir::Op::CmpGe { left, right } => {
                        let left = self.jit_value(left, &mut ctx)?;
                        let right = self.jit_value(right, &mut ctx)?;
                        Some(
                            ctx.builder
                                .ins()
                                .icmp(IntCC::SignedGreaterThanOrEqual, left, right),
                        )
                    }
                    ir::Op::And { left, right } => todo!(),
                    ir::Op::Or { left, right } => todo!(),
                    ir::Op::Xor { left, right } => todo!(),
                    ir::Op::ShiftL { left, right } => todo!(),
                    ir::Op::IShiftR { left, right } => todo!(),
                    ir::Op::UShiftR { left, right } => todo!(),
                    ir::Op::Call { func, input } => todo!(),
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
                    .get(*arg as usize)
                    .ok_or_else(|| anyhow!("invalid function arg {}", arg))
                    .cloned()
            }
            ir::Value::BlockOp { block, op } => ctx
                .block_values
                .get(&(*block as usize))
                .ok_or_else(|| anyhow!("Invalid block ID {}", block))?
                .get(*op as usize)
                .ok_or_else(|| anyhow!("Invalid op id {}", op))?
                .ok_or_else(|| anyhow!("Op {} does not return a value!", op)),
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
    ctx: codegen::Context,
    function_builder_ctx: FunctionBuilderContext,
}

struct FunctionCtx<'a> {
    builder: FunctionBuilder<'a>,
    blocks: Vec<Block>,
    block_values: HashMap<usize, Vec<Option<Value>>>,
}

impl<'a> FunctionCtx<'a> {
    pub fn get_block(&self, block: u32) -> Result<Block> {
        self.blocks
            .get(block as usize)
            .ok_or_else(|| anyhow!("Invalid block ID {}", block))
            .cloned()
    }
}
