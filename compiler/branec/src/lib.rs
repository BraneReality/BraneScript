use std::collections::{HashMap, LinkedList};

use anyhow::{anyhow, bail};
pub use brane_core::ir;
use brane_core::ir::{IRIDRef, IRModule, IRNativeType, IROp, IRType, IRValue};
pub use branec_hir::hir::{self, Graph, GraphId};
pub use branec_hir::queries;

fn resolve_value(
    value: hir::LocalValue,
    block: &hir::Block,
    writer: &mut IRWriter,
    proj: &hir::Project,
    resolved_values: &mut HashMap<hir::LocalValue, WriterIRValue>,
) -> anyhow::Result<WriterIRValue> {
    if let Some(value) = resolved_values.get(&value) {
        return Ok(value.clone());
    }

    match value {
        hir::LocalValue::BrokenRef => Err(anyhow!("Broken reference found!")),
        hir::LocalValue::BlockInput { index } => writer
            .get_labeled_value(index.to_string().as_str())
            .ok_or(anyhow!("ERROR WITH ARGS")),
        hir::LocalValue::NodeOutput { node, index } => {
            let node_data = block.nodes.get(&node).ok_or(anyhow!("Invalid node ref"))?;
            assert!(index == 0, "we don't account for multi-output yet!");

            let expr_item = proj
                .graph(node_data.expr.graph)
                .ok_or(anyhow!("Invalid graph ref"))?
                .item(node_data.expr.item)
                .ok_or(anyhow!("invalid local id"))?;
            match expr_item {
                hir::Item::Pipe(_) => todo!(),
                hir::Item::Fn(func) => {
                    let inputs: Vec<WriterIRValue> = func
                        .sig
                        .inputs
                        .iter()
                        .enumerate()
                        .map(|(input_index, _input)| -> anyhow::Result<WriterIRValue> {
                            let value = node_data
                                .inputs
                                .get(input_index)
                                .ok_or(anyhow!("arg not set"))?;
                            Ok(resolve_value(*value, block, writer, proj, resolved_values)?)
                        })
                        .collect::<anyhow::Result<Vec<WriterIRValue>>>()?;

                    match func.ident.as_str() {
                        "f32::add" => {
                            writer.add(IRNativeType::F32, inputs[0].value(), inputs[1].value())
                        }
                        "f32::sub" => {
                            writer.sub(IRNativeType::F32, inputs[0].value(), inputs[1].value())
                        }
                        _ => Err(anyhow!("Funciton {} not callable", func.ident)),
                    }
                }
                hir::Item::Trait(_) => todo!(),
                hir::Item::TraitImpl(_) => todo!(),
            }
        }
    }
}

pub fn lower_hir(project: &hir::Project) -> anyhow::Result<Vec<ir::IRModule>> {
    let mut modules = Vec::new();

    for (_, graph) in project.iter() {
        if graph.ident == "std" {
            continue;
        }

        let mut module = ir::IRModule {
            id: graph.ident.clone(),
            structs: Vec::new(),
            functions: Vec::new(),
            pipelines: Vec::new(),
        };

        for item in graph.iter() {
            match item {
                hir::Item::Pipe(_) => todo!(),
                hir::Item::Fn(func) => {
                    let input_ty = module.structs.len();
                    module.structs.push(ir::IRStruct {
                        id: Some("INPUT".into()),
                        members: func
                            .sig
                            .inputs
                            .iter()
                            .map(|input| ir::IRStructMember {
                                id: Some(input.ident.clone()),
                                r#type: ir::IRType::Native(ir::IRNativeType::F32),
                            })
                            .collect(),
                        packed: false,
                    });

                    let output_ty = module.structs.len();
                    module.structs.push(ir::IRStruct {
                        id: Some("OUTPUT".into()),
                        members: func
                            .sig
                            .outputs
                            .iter()
                            .map(|output| ir::IRStructMember {
                                id: Some(output.ident.clone()),
                                r#type: ir::IRType::Native(ir::IRNativeType::F32),
                            })
                            .collect(),
                        packed: false,
                    });

                    let block = graph.block(func.body).unwrap();
                    let mut writer = IRWriter::new();
                    writer.start_scope();
                    for (index, _) in func.sig.inputs.iter().enumerate() {
                        let value = writer
                            .write(
                                IROp::ArgValue {
                                    index: index as u32,
                                },
                                Some(IRValueCtx {
                                    r#type: IRType::Native(IRNativeType::F32),
                                    is_deref: true,
                                }),
                            )
                            .unwrap();
                        writer.set_labeled_value(index.to_string(), value)
                    }

                    let mut resolved_values = HashMap::new();
                    //let mut ret_args = Vec::new();

                    let ret_val = resolve_value(
                        *block
                            .outputs
                            .get(0)
                            .ok_or(anyhow::anyhow!("Need one return value"))?,
                        block,
                        &mut writer,
                        project,
                        &mut resolved_values,
                    )?;
                    /*
                    let ret_ty = ir::IRIDRef(output_ty as u32);
                    let ret_val = writer.alloca(ir::IRType::Struct(ret_ty.clone()));

                    for (index, arg) in ret_args.iter().enumerate() {
                        let mem = writer.get_struct_member_ptr(
                            &ret_ty,
                            ret_val.value(),
                            index,
                            &module,
                        )?;
                        writer.store(arg.value(), mem.value())?;
                    }
                    */
                    writer.next_stage(ir::IRIDRef(output_ty as u32), ret_val, None)?;

                    writer.end_scope()?;

                    module.functions.push(ir::IRFunction {
                        id: func.ident.clone(),
                        input: ir::IRIDRef(input_ty as u32),
                        output: ir::IRIDRef(output_ty as u32),
                        operations: writer.finish(),
                    });
                }
                hir::Item::Trait(_) => todo!(),
                hir::Item::TraitImpl(_) => todo!(),
            }
        }
        modules.push(module);
    }

    Ok(modules)
}

struct IRWriterScope {
    /// Operands to execute when the scope closes
    defered_ops: Vec<Box<dyn FnOnce(&mut IRWriter) -> anyhow::Result<()>>>,
    /// Values with labels
    labeled_values: HashMap<String, WriterIRValue>,
}

pub struct IRWriter {
    /// (operation, Option<type operation returns, is ptr to value>
    pub operations: Vec<(IROp, Option<IRValueCtx>)>,
    pub scopes: LinkedList<IRWriterScope>,
}

pub struct IROpCtx {}

pub enum IRCompilerValue {
    Address(IRValue),
    Value(IRValue),
}

#[derive(Clone)]
pub struct IRValueCtx {
    pub r#type: IRType,
    // if true the value holds the (type|struct address), if fase we have a ptr representing pointer to one of those
    // types
    pub is_deref: bool,
}

#[derive(Clone)]
pub struct WriterIRValue {
    pub ctx: IRValueCtx,
    pub value: IRValue,
}

impl WriterIRValue {
    pub fn value(&self) -> IRValue {
        self.value
    }
}

impl IRWriter {
    pub fn new() -> IRWriter {
        IRWriter {
            operations: Vec::new(),
            scopes: LinkedList::new(),
        }
    }

    pub fn finish(self) -> Vec<IROp> {
        self.operations.into_iter().map(|op| op.0).collect()
    }

    /// Write a new operation and record it's return value for future use
    pub fn write(&mut self, op: IROp, out_type: Option<IRValueCtx>) -> Option<WriterIRValue> {
        let op_index = self.operations.len() as u32;
        self.operations.push((op, out_type.clone()));
        if let Some(ctx) = out_type {
            Some(WriterIRValue {
                ctx,
                value: IRValue { 0: op_index },
            })
        } else {
            None
        }
    }

    /// Executes when end_scope() is called
    pub fn defer_scope(&mut self, f: impl FnOnce(&mut IRWriter) -> anyhow::Result<()> + 'static) {
        self.scopes
            .back_mut()
            .expect("must have one scope")
            .defered_ops
            .push(Box::new(f));
    }

    pub fn start_scope(&mut self) {
        self.scopes.push_back(IRWriterScope {
            defered_ops: Vec::new(),
            labeled_values: HashMap::new(),
        });
    }

    /// Runs all defered operations and pops a scope
    pub fn end_scope(&mut self) -> anyhow::Result<()> {
        if self.scopes.is_empty() {
            bail!("tried to pop nonexistant scope!")
        }
        let scope = self.scopes.pop_back().unwrap();
        for write in scope.defered_ops {
            write(self)?;
        }

        Ok(())
    }

    /// Get the stored type of a value
    pub fn get_value_ctx(&self, value: IRValue) -> anyhow::Result<IRValueCtx> {
        self.operations
            .get(value.0 as usize)
            .ok_or(anyhow!("no value {}", value.0))?
            .1
            .clone()
            .ok_or(anyhow!("op {} does not return a value", value.0))
    }

    pub fn set_labeled_value(&mut self, label: impl Into<String>, value: WriterIRValue) {
        self.scopes
            .back_mut()
            .expect("needs at least one scope")
            .labeled_values
            .insert(label.into(), value);
    }

    pub fn get_labeled_value(&self, label: &str) -> Option<WriterIRValue> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.labeled_values.get(label) {
                return Some(value.clone());
            }
        }
        None
    }

    // allocate stack memory of a type and get a ptr value representing a pointer back
    pub fn alloca(&mut self, r#type: IRType) -> WriterIRValue {
        self.write(
            IROp::AllocA {
                r#type: r#type.clone(),
            },
            Some(IRValueCtx {
                r#type,
                is_deref: false,
            }),
        )
        .expect("we should always get a value if we pass a ret type")
    }

    pub fn get_struct_member_ptr(
        &mut self,
        structure_type: &IRIDRef,
        ptr: IRValue,
        index: usize,
        module: &IRModule,
    ) -> anyhow::Result<WriterIRValue> {
        let structure = match module.get_struct(structure_type) {
            Some(s) => s,
            None => bail!("could not find struct for index"),
        };
        let layout = match structure.layout(module) {
            Ok(layout) => layout,
            Err(_) => bail!("Could not compute layout for struct"),
        };

        let ptr_offset = match layout.byte_offsets.get(index) {
            Some(po) => po,
            None => bail!("Tried to access non-existant struct member"),
        };

        if *ptr_offset == 0 {
            let m_type = structure.members[index].r#type.clone();
            return Ok(WriterIRValue {
                ctx: IRValueCtx {
                    is_deref: match &m_type {
                        IRType::Native(_) => false,
                        IRType::Struct(_) => true,
                    },
                    r#type: m_type,
                },
                value: ptr,
            });
        }

        let offset_value = self.get_const_i32(*ptr_offset as i32);
        let member_ptr_index = self.add(IRNativeType::I32, ptr, offset_value.value)?;

        Ok(WriterIRValue {
            ctx: IRValueCtx {
                r#type: structure.members[index].r#type.clone(),
                is_deref: false,
            },
            value: member_ptr_index.value,
        })
    }

    /// Operation that registers a const
    pub fn get_const_i32(&mut self, value: i32) -> WriterIRValue {
        self.write(
            IROp::ConstI32 { value },
            Some(IRValueCtx {
                r#type: IRType::Native(IRNativeType::U32),
                is_deref: true,
            }),
        )
        .expect("we should always get a value if we pass a ret type")
    }

    pub fn deref_value(&mut self, value: &WriterIRValue) -> anyhow::Result<WriterIRValue> {
        Ok(if !value.ctx.is_deref {
            match value.ctx.r#type {
                IRType::Native(nt) => self.load(nt, value.value)?,
                IRType::Struct(_) => self.load(IRNativeType::Ptr, value.value)?,
            }
        } else {
            value.clone()
        })
    }

    pub fn add(
        &mut self,
        r#type: IRNativeType,
        left: IRValue,
        right: IRValue,
    ) -> anyhow::Result<WriterIRValue> {
        Ok(match r#type {
            IRNativeType::I128
            | IRNativeType::U128
            | IRNativeType::I64
            | IRNativeType::U64
            | IRNativeType::I32
            | IRNativeType::U32
            | IRNativeType::Ptr
            | IRNativeType::I16
            | IRNativeType::U16
            | IRNativeType::U8
            | IRNativeType::I8 => self
                .write(
                    IROp::IAdd { left, right },
                    Some(IRValueCtx {
                        r#type: IRType::Native(r#type),
                        is_deref: true,
                    }),
                )
                .expect("we should always get a value if we pass a ret type"),
            IRNativeType::F32 | IRNativeType::F64 => self
                .write(
                    IROp::FAdd { left, right },
                    Some(IRValueCtx {
                        r#type: IRType::Native(r#type),
                        is_deref: true,
                    }),
                )
                .expect("we should always get a value if we pass a ret type"),
            IRNativeType::Bool => bail!("Cannot add to bool type"),
            IRNativeType::FnPtr => bail!("Cannot add to FnPtr type"),
        })
    }

    pub fn sub(
        &mut self,
        r#type: IRNativeType,
        left: IRValue,
        right: IRValue,
    ) -> anyhow::Result<WriterIRValue> {
        Ok(match r#type {
            IRNativeType::I128
            | IRNativeType::U128
            | IRNativeType::I64
            | IRNativeType::U64
            | IRNativeType::I32
            | IRNativeType::U32
            | IRNativeType::Ptr
            | IRNativeType::I16
            | IRNativeType::U16
            | IRNativeType::U8
            | IRNativeType::I8 => self
                .write(
                    IROp::ISub { left, right },
                    Some(IRValueCtx {
                        r#type: IRType::Native(r#type),
                        is_deref: true,
                    }),
                )
                .expect("we should always get a value if we pass a ret type"),
            IRNativeType::F32 | IRNativeType::F64 => self
                .write(
                    IROp::FSub { left, right },
                    Some(IRValueCtx {
                        r#type: IRType::Native(r#type),
                        is_deref: true,
                    }),
                )
                .expect("we should always get a value if we pass a ret type"),
            IRNativeType::Bool => bail!("Cannot sub bool type"),
            IRNativeType::FnPtr => bail!("Cannot sub FnPtr type"),
        })
    }

    pub fn mul(
        &mut self,
        r#type: IRNativeType,
        left: IRValue,
        right: IRValue,
    ) -> anyhow::Result<WriterIRValue> {
        Ok(match r#type {
            IRNativeType::I128
            | IRNativeType::I64
            | IRNativeType::I32
            | IRNativeType::I16
            | IRNativeType::I8
            | IRNativeType::Ptr
            | IRNativeType::U16
            | IRNativeType::U32
            | IRNativeType::U64
            | IRNativeType::U8
            | IRNativeType::U128 => self
                .write(
                    IROp::IMul { left, right },
                    Some(IRValueCtx {
                        r#type: IRType::Native(r#type),
                        is_deref: true,
                    }),
                )
                .expect("we should always get a value if we pass a ret type"),
            IRNativeType::F32 | IRNativeType::F64 => self
                .write(
                    IROp::FMul { left, right },
                    Some(IRValueCtx {
                        r#type: IRType::Native(r#type),
                        is_deref: true,
                    }),
                )
                .expect("we should always get a value if we pass a ret type"),
            IRNativeType::Bool => bail!("Cannot mul bool type"),
            IRNativeType::FnPtr => bail!("Cannot mul FnPtr type"),
        })
    }

    pub fn div(
        &mut self,
        r#type: IRNativeType,
        left: IRValue,
        right: IRValue,
    ) -> anyhow::Result<WriterIRValue> {
        Ok(match r#type {
            IRNativeType::I128
            | IRNativeType::I64
            | IRNativeType::I32
            | IRNativeType::I16
            | IRNativeType::I8 => self
                .write(
                    IROp::SDiv { left, right },
                    Some(IRValueCtx {
                        r#type: IRType::Native(r#type),
                        is_deref: true,
                    }),
                )
                .expect("we should always get a value if we pass a ret type"),
            IRNativeType::U16
            | IRNativeType::U32
            | IRNativeType::U64
            | IRNativeType::U8
            | IRNativeType::Ptr
            | IRNativeType::U128 => self
                .write(
                    IROp::UDiv { left, right },
                    Some(IRValueCtx {
                        r#type: IRType::Native(r#type),
                        is_deref: true,
                    }),
                )
                .expect("we should always get a value if we pass a ret type"),
            IRNativeType::F32 | IRNativeType::F64 => self
                .write(
                    IROp::FDiv { left, right },
                    Some(IRValueCtx {
                        r#type: IRType::Native(r#type),
                        is_deref: true,
                    }),
                )
                .expect("we should always get a value if we pass a ret type"),
            IRNativeType::Bool => bail!("Cannot div bool type"),
            IRNativeType::FnPtr => bail!("Cannot div FnPtr type"),
        })
    }

    pub fn rem(
        &mut self,
        r#type: IRNativeType,
        left: IRValue,
        right: IRValue,
    ) -> anyhow::Result<WriterIRValue> {
        Ok(match r#type {
            IRNativeType::I128
            | IRNativeType::I64
            | IRNativeType::I32
            | IRNativeType::I16
            | IRNativeType::I8 => self
                .write(
                    IROp::SRem { left, right },
                    Some(IRValueCtx {
                        r#type: IRType::Native(r#type),
                        is_deref: true,
                    }),
                )
                .expect("we should always get a value if we pass a ret type"),
            IRNativeType::U16
            | IRNativeType::U32
            | IRNativeType::U64
            | IRNativeType::U8
            | IRNativeType::Ptr
            | IRNativeType::U128 => self
                .write(
                    IROp::URem { left, right },
                    Some(IRValueCtx {
                        r#type: IRType::Native(r#type),
                        is_deref: true,
                    }),
                )
                .expect("we should always get a value if we pass a ret type"),
            IRNativeType::F32 | IRNativeType::F64 => self
                .write(
                    IROp::FRem { left, right },
                    Some(IRValueCtx {
                        r#type: IRType::Native(r#type),
                        is_deref: true,
                    }),
                )
                .expect("we should always get a value if we pass a ret type"),
            IRNativeType::Bool => bail!("Cannot get remainder of bool type"),
            IRNativeType::FnPtr => bail!("Cannot get remainder of FnPtr type"),
        })
    }

    fn neg(&mut self, value: &WriterIRValue) -> anyhow::Result<WriterIRValue> {
        Ok(match &value.ctx.r#type {
            IRType::Struct(_) => bail!("can't negate structs"),
            IRType::Native(nt) => match nt {
                IRNativeType::I8
                | IRNativeType::I16
                | IRNativeType::I32
                | IRNativeType::I64
                | IRNativeType::I128 => self
                    .write(
                        IROp::INeg { arg: value.value },
                        Some(IRValueCtx {
                            r#type: value.ctx.r#type.clone(),
                            is_deref: true,
                        }),
                    )
                    .unwrap(),
                IRNativeType::F32 | IRNativeType::F64 => self
                    .write(
                        IROp::FNeg { arg: value.value },
                        Some(IRValueCtx {
                            r#type: value.ctx.r#type.clone(),
                            is_deref: true,
                        }),
                    )
                    .unwrap(),
                IRNativeType::U8
                | IRNativeType::U16
                | IRNativeType::U32
                | IRNativeType::U64
                | IRNativeType::Ptr
                | IRNativeType::U128 => bail!("cannot negate unsigned type"),
                IRNativeType::Bool => bail!("Cannot negate bool type"),
                IRNativeType::FnPtr => bail!("Cannot negate FnPtr type"),
            },
        })
    }

    fn and(
        &mut self,
        r#type: IRNativeType,
        left: IRValue,
        right: IRValue,
    ) -> anyhow::Result<WriterIRValue> {
        if r#type == IRNativeType::F32 || r#type == IRNativeType::F64 {
            bail!("no bit operations for float types")
        }
        Ok(self
            .write(
                IROp::And {
                    left: left,
                    right: right,
                },
                Some(IRValueCtx {
                    r#type: IRType::Native(r#type),
                    is_deref: true,
                }),
            )
            .unwrap())
    }

    fn or(
        &mut self,
        r#type: IRNativeType,
        left: IRValue,
        right: IRValue,
    ) -> anyhow::Result<WriterIRValue> {
        if r#type == IRNativeType::F32 || r#type == IRNativeType::F64 {
            bail!("no bit operations for float types")
        }
        Ok(self
            .write(
                IROp::Or {
                    left: left,
                    right: right,
                },
                Some(IRValueCtx {
                    r#type: IRType::Native(r#type),
                    is_deref: true,
                }),
            )
            .unwrap())
    }

    fn xor(
        &mut self,
        r#type: IRNativeType,
        left: IRValue,
        right: IRValue,
    ) -> anyhow::Result<WriterIRValue> {
        if r#type == IRNativeType::F32 || r#type == IRNativeType::F64 {
            bail!("no bit operations for float types")
        }
        Ok(self
            .write(
                IROp::Xor {
                    left: left,
                    right: right,
                },
                Some(IRValueCtx {
                    r#type: IRType::Native(r#type),
                    is_deref: true,
                }),
            )
            .unwrap())
    }

    fn shift_left(
        &mut self,
        r#type: IRNativeType,
        left: IRValue,
        right: IRValue,
    ) -> anyhow::Result<WriterIRValue> {
        Ok(match r#type {
            IRNativeType::U8
            | IRNativeType::U16
            | IRNativeType::U32
            | IRNativeType::U64
            | IRNativeType::U128
            | IRNativeType::I8
            | IRNativeType::I16
            | IRNativeType::I32
            | IRNativeType::I64
            | IRNativeType::Bool
            | IRNativeType::I128 => self
                .write(
                    IROp::ShiftL { left, right },
                    Some(IRValueCtx {
                        r#type: IRType::Native(r#type),
                        is_deref: true,
                    }),
                )
                .unwrap(),
            IRNativeType::F32 | IRNativeType::F64 => bail!("no bit operations for float types"),
            IRNativeType::Ptr | IRNativeType::FnPtr => bail!("no bit operations for ptr types"),
        })
    }

    fn shift_right(
        &mut self,
        r#type: IRNativeType,
        left: IRValue,
        right: IRValue,
    ) -> anyhow::Result<WriterIRValue> {
        Ok(match r#type {
            IRNativeType::U8
            | IRNativeType::U16
            | IRNativeType::U32
            | IRNativeType::U64
            | IRNativeType::Bool
            | IRNativeType::U128 => self
                .write(
                    IROp::UShiftR { left, right },
                    Some(IRValueCtx {
                        r#type: IRType::Native(r#type),
                        is_deref: true,
                    }),
                )
                .unwrap(),
            IRNativeType::I8
            | IRNativeType::I16
            | IRNativeType::I32
            | IRNativeType::I64
            | IRNativeType::I128 => self
                .write(
                    IROp::IShiftR { left, right },
                    Some(IRValueCtx {
                        r#type: IRType::Native(r#type),
                        is_deref: true,
                    }),
                )
                .unwrap(),
            IRNativeType::F32 | IRNativeType::F64 => bail!("no bit operations for float types"),
            IRNativeType::Ptr | IRNativeType::FnPtr => bail!("no bit operations for ptr types"),
        })
    }

    fn load(&mut self, r#type: IRNativeType, ptr: IRValue) -> anyhow::Result<WriterIRValue> {
        Ok(self
            .write(
                IROp::Load { ptr, r#type },
                Some(IRValueCtx {
                    r#type: IRType::Native(r#type),
                    is_deref: true,
                }),
            )
            .expect("we should always get a value if we pass a ret type"))
    }

    fn store(&mut self, src: IRValue, dest: IRValue) -> anyhow::Result<()> {
        self.write(IROp::Store { src, ptr: dest }, None);
        Ok(())
    }

    /*fn call(&mut self, args: IRValue, ret: Option<IRIDRef>) -> anyhow::Result<Option<IRValue>> {
        todo!();
    }*/

    fn next_stage(
        &mut self,
        ret_t: IRIDRef,
        args: WriterIRValue,
        deps: Option<IRValue>,
    ) -> anyhow::Result<()> {
        assert!(deps.is_none(), "todo");
        self.write(
            IROp::NextStage {
                args_t: ret_t,
                args: args.value,
            },
            None,
        );
        Ok(())
    }

    fn copy_struct(
        &mut self,
        struct_type: &IRIDRef,
        src: IRValue,
        dest: IRValue,
        module: &IRModule,
    ) -> anyhow::Result<()> {
        let s = match module.get_struct(&struct_type) {
            Some(s) => s,
            None => bail!("Could not resolve struct type"),
        };
        for i in 0..s.members.len() {
            let src_mem_ptr = self.get_struct_member_ptr(struct_type, src, i, module)?;
            let dest_mem_ptr = self.get_struct_member_ptr(struct_type, dest, i, module)?;
            match &s.members[i].r#type {
                IRType::Native(r#type) => {
                    let value = self.load(r#type.clone(), src_mem_ptr.value)?;
                    self.store(value.value, dest_mem_ptr.value)?;
                }
                IRType::Struct(struct_type) => {
                    self.copy_struct(struct_type, src_mem_ptr.value, dest_mem_ptr.value, module)?;
                }
            }
        }
        Ok(())
    }
}
