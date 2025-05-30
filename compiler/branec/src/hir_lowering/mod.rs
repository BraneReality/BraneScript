use std::collections::{HashMap, LinkedList};

use anyhow::{anyhow, bail};
use brane_script_runtime::ir::{
    IRFunction, IRIDRef, IRModule, IRNativeType, IROp, IRPipeline, IRStruct, IRStructMember,
    IRType, IRValue,
};

use crate::{
    document_context::{
        AnonStructContext, AnonStructTypeContext, BinaryOperator, BlockContext, CallSigContext,
        ContextRef, DocumentContext, ExpressionContext, ModuleContext, PipelineContext,
        ProjectContext, ScopeSegment, StructContext, TextContext, TypeContext, TypeModifiers,
        UnaryOperator,
    },
    MessageType, ToolchainMessage,
};

pub struct CompileResult<T> {
    pub data: Option<T>,
    pub messages: Vec<ToolchainMessage>,
}

// TODO: add in any compiler settings here
pub struct Compiler {}

impl Compiler {
    pub fn compile(&mut self, documents: &[DocumentContext]) -> CompileResult<Vec<IRModule>> {
        let proj = match ProjectContext::new(documents) {
            Ok(proj) => proj,
            Err(messages) => {
                return CompileResult {
                    data: None,
                    messages,
                }
            }
        };

        GenerateIRPass::run(&proj)
        // We could add more passes here later now that we have IR
    }

    pub fn new() -> Compiler {
        Compiler {}
    }
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

impl IRWriter {
    pub fn new(arg_type: IRIDRef) -> IRWriter {
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
            IRType::Struct(idref) => bail!("can't negate structs"),
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

    fn call(&mut self, args: IRValue, ret: Option<IRIDRef>) -> anyhow::Result<Option<IRValue>> {
        todo!();
    }

    fn next_stage(&mut self, args: WriterIRValue, deps: Option<IRValue>) -> anyhow::Result<()> {
        let struct_t = match args.ctx.r#type {
            IRType::Native(_) => bail!("Can't return native type!"),
            IRType::Struct(idref) => idref,
        };
        assert!(deps.is_none(), "todo");
        self.write(
            IROp::NextStage {
                args_t: struct_t,
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

struct GenerateIRPass<'ctx> {
    messages: Vec<ToolchainMessage>,
    using_symbols: LinkedList<ContextRef<'ctx>>,
}

impl<'ctx> GenerateIRPass<'ctx> {
    fn record_err<E: Into<anyhow::Error>>(&mut self, err: E, ctx: TextContext) {
        self.messages.push(ToolchainMessage {
            message: format!(
                "[{}, {}] {}",
                ctx.range.start_point.row,
                ctx.range.start_point.column,
                err.into()
            ),
            r#type: MessageType::Error,
        });
    }

    fn record_log<T: Into<String>>(&mut self, text: T, ctx: TextContext) {
        self.messages.push(ToolchainMessage {
            message: format!(
                "[{}, {}] {}",
                ctx.range.start_point.row,
                ctx.range.start_point.column,
                text.into()
            ),
            r#type: MessageType::Log,
        });
    }

    fn search_up_stack(&self, id: &[ScopeSegment]) -> Option<ContextRef<'ctx>> {
        if id.is_empty() {
            return None;
        }
        for frame in self.using_symbols.iter().rev() {
            if let Some(found) = self.search_down_stack(id, *frame) {
                return Some(found);
            }
        }
        None
    }

    fn search_down_stack(
        &self,
        id: &[ScopeSegment],
        ctx: ContextRef<'ctx>,
    ) -> Option<ContextRef<'ctx>> {
        if id.is_empty() {
            return None;
        }

        let base = match &id[0] {
            ScopeSegment::Id(identifier) => identifier,
        };

        // Make sure that this type matches the base of the passed in id
        match ctx.label() {
            Some(label) => {
                if label.text != base.text {
                    return None;
                }
            }
            None => return None,
        }

        if id.len() == 1 {
            return Some(ctx);
        }

        let remainder = &id[1..id.len()];

        // Match the remainder against types that support searching children
        match ctx {
            ContextRef::Pipeline(pipe) => pipe.stages.iter().find_map(|stage| {
                match self.search_down_stack(remainder, ContextRef::PipelineStage(stage)) {
                    Some(res) => Some(res),
                    None => None,
                }
            }),
            ContextRef::Struct(structure) => structure.members.iter().find_map(|member| match self
                .search_down_stack(remainder, ContextRef::Value(member))
            {
                Some(res) => Some(res),
                None => None,
            }),
            ContextRef::Module(module) => {
                let id = &remainder[0];
                match id {
                    ScopeSegment::Id(id) => {
                        if let Some(func) = module.functions.get(&id.text) {
                            self.search_down_stack(remainder, ContextRef::Function(func))
                        } else if let Some(structure) = module.structs.get(&id.text) {
                            self.search_down_stack(remainder, ContextRef::Struct(structure))
                        } else if let Some(pipeline) = module.pipelines.get(&id.text) {
                            self.search_down_stack(remainder, ContextRef::Pipeline(pipeline))
                        } else {
                            None
                        }
                    } // Eventually we'll have generics
                }
            }
            _ => None,
        }
    }

    fn resolve_type_id(
        &mut self,
        ctx: &TypeContext,
        module: &mut IRModule,
    ) -> Result<IRType, ToolchainMessage> {
        for m in &ctx.modifiers {
            return match m {
                TypeModifiers::MutRef | TypeModifiers::ConstRef => {
                    Ok(IRType::Native(IRNativeType::Ptr))
                }
            };
        }

        assert!(ctx.base_type.scopes.len() > 0);
        if ctx.base_type.scopes.len() == 1 {
            match &ctx.base_type.scopes[0] {
                ScopeSegment::Id(id) => {
                    if let Ok(nt) = IRNativeType::from_str(id.text.as_str()) {
                        return Ok(IRType::Native(nt));
                    }
                }
            }
        }

        match self.search_up_stack(&ctx.base_type.scopes) {
            Some(found) => match found {
                ContextRef::Struct(structure) => {
                    for i in 0..module.structs.len() {
                        if let Some(sid) = &module.structs[i].id {
                            if sid == &structure.identifier.text {
                                return Ok(IRType::Struct(IRIDRef(i as u32)));
                            }
                        }
                    }

                    match self.compile_struct(structure, module) {
                        Some(id) => Ok(IRType::Struct(id)),
                        None => Err(ToolchainMessage {
                            message: format!(
                                "type {} was found, but could not be compiled",
                                ctx.base_type
                            ),
                            r#type: MessageType::Error,
                        }),
                    }
                }
                _ => Err(ToolchainMessage {
                    message: match found.label() {
                        Some(label) => format!("{} is not a type!", label),
                        None => format!("{} is not a type", ctx.base_type),
                    },
                    r#type: MessageType::Error,
                }),
            },
            None => Err(ToolchainMessage {
                message: format!("type {} not found", ctx.base_type),
                r#type: MessageType::Error,
            }),
        }
    }

    fn compile_struct(&mut self, ctx: &StructContext, module: &mut IRModule) -> Option<IRIDRef> {
        let struct_id = IRIDRef(module.structs.len() as u32);

        let mut members = Vec::new();
        for m in ctx.members.iter() {
            let member_type = match self.resolve_type_id(
                &m.r#type.as_ref().expect("Types should have been expanded"),
                module,
            ) {
                Ok(mt) => mt,
                Err(err) => {
                    self.messages.push(err);
                    continue;
                }
            };

            members.push(IRStructMember {
                id: m.label.as_ref().map(|i| i.text.clone()),
                r#type: member_type,
            });
        }

        if members.len() != ctx.members.len() {
            return None;
        }

        module.structs.push(IRStruct {
            id: Some(ctx.identifier.text.clone()),
            members,
            packed: false,
        });

        Some(struct_id)
    }

    fn resolve_anon_struct_type(
        &mut self,
        members: Vec<IRStructMember>,
        module: &mut IRModule,
    ) -> Result<IRIDRef, ToolchainMessage> {
        for i in 0..module.structs.len() {
            let s = &module.structs[i];
            if s.id.is_some() {
                continue;
            }
            if s.members.len() != members.len() {
                continue;
            }
            if members
                .iter()
                .zip(s.members.iter())
                .any(|(a, b)| a.id != b.id || a.r#type != b.r#type)
            {
                continue;
            }

            return Ok(IRIDRef(i as u32));
        }

        let new_id = IRIDRef(module.structs.len() as u32);
        module.structs.push(IRStruct {
            id: None,
            members,
            packed: false,
        });
        Ok(new_id)
    }

    fn compile_anon_struct_type(
        &mut self,
        ctx: &AnonStructTypeContext,
        module: &mut IRModule,
    ) -> Option<IRIDRef> {
        let mut members = Vec::new();
        for m in ctx.members.iter() {
            let r#type = m.r#type.as_ref().expect("type should have been resolved");
            let r#type = match self.resolve_type_id(&r#type, module) {
                Ok(t) => t,
                Err(err) => {
                    self.messages.push(err);
                    continue;
                }
            };
            members.push(IRStructMember {
                id: m.label.as_ref().map(|i| i.text.clone()),
                r#type,
            })
        }

        match self.resolve_anon_struct_type(members, module) {
            Ok(t) => Some(t),
            Err(err) => {
                self.messages.push(err);
                None
            }
        }
    }

    fn compile_anon_struct(
        &mut self,
        ctx: &AnonStructContext,
        writer: &mut IRWriter,
        module: &mut IRModule,
    ) -> anyhow::Result<WriterIRValue> {
        let mut member_values = Vec::new();
        for m in ctx.members.iter() {
            let m_value = self.compile_expression(&m.expression, writer, module)?;
            if let Some(m_value) = m_value {
                member_values.push(m_value);
            }
        }

        if member_values.len() != ctx.members.len() {
            bail!("error in members initialization")
        }

        let mut member_defs = Vec::new();
        for i in 0..ctx.members.len() {
            member_defs.push(IRStructMember {
                id: Some(ctx.members[i].id.text.clone()),
                r#type: member_values[i].ctx.r#type.clone(),
            })
        }
        let struct_type = self
            .resolve_anon_struct_type(member_defs.clone(), module)
            .map_err(|e| anyhow!(e.message))?;
        let root_ptr = writer.alloca(IRType::Struct(struct_type.clone()));
        for i in 0..member_values.len() {
            let mem_ptr = writer.get_struct_member_ptr(&struct_type, root_ptr.value, i, module)?;
            match &member_defs[i].r#type {
                IRType::Native(_) => {
                    let value = writer.deref_value(&member_values[i])?;
                    writer.store(value.value, mem_ptr.value)?;
                }
                IRType::Struct(struct_type) => {
                    writer.copy_struct(
                        struct_type,
                        member_values[i].value,
                        mem_ptr.value,
                        module,
                    )?;
                }
            }
        }

        Ok(root_ptr)
    }

    fn compile_expression(
        &mut self,
        expression: &ExpressionContext,
        writer: &mut IRWriter,
        module: &mut IRModule,
    ) -> anyhow::Result<Option<WriterIRValue>> {
        Ok(match expression {
            ExpressionContext::Scope(block) => self.compile_block(block.as_ref(), writer, module),
            ExpressionContext::Assignment(assignment) => {
                let src = self
                    .compile_expression(&assignment.src, writer, module)?
                    .ok_or(anyhow!("arg is not value!"))?;
                let src = writer.deref_value(&src)?;
                let dest = self
                    .compile_expression(&assignment.dest, writer, module)?
                    .ok_or(anyhow!("arg is not value!"))?;

                // TODO handle structs
                writer.store(src.value, dest.value)?;
                None
            }
            ExpressionContext::VariableDefinition(var_def) => {
                let var_label = var_def
                    .defined_value
                    .label
                    .as_ref()
                    .ok_or(anyhow!("need a label to define a var"))?;
                let var_type = self
                    .resolve_type_id(
                        var_def
                            .defined_value
                            .r#type
                            .as_ref()
                            .ok_or(anyhow!("need a type to define a var"))?,
                        module,
                    )
                    .map_err(|e| anyhow!(e.message))?;
                let mem = writer.alloca(var_type);

                writer.set_labeled_value(var_label.text.clone(), mem.clone());
                Some(mem)
            }
            ExpressionContext::ConstValue(_) => todo!(),
            ExpressionContext::ScopedId(scoped_identifier) => {
                if scoped_identifier.scopes.len() == 1 {
                    match &scoped_identifier.scopes[0] {
                        ScopeSegment::Id(id) => writer.get_labeled_value(&id.text),
                    }
                } else {
                    None
                }
            }
            ExpressionContext::MemberAccess(gep) => {
                let ptr = self
                    .compile_expression(&gep.base_expression, writer, module)?
                    .ok_or(anyhow!("expression is not value!"))?;
                let struct_type = match ptr.ctx.r#type {
                    IRType::Native(_) => bail!("Can't get member of non-struct type!"),
                    IRType::Struct(st) => st,
                };
                Some(writer.get_struct_member_ptr(&struct_type, ptr.value, gep.member, module)?)
            }
            ExpressionContext::UnaryOperator(uop) => {
                let expression = self
                    .compile_expression(&uop.expression, writer, module)?
                    .ok_or(anyhow!("arg is not value!"))?;

                // TODO make it so we don't continue evaluating logic comparisions if they're already resolved
                use UnaryOperator::*;
                Some(match expression.ctx.r#type {
                    IRType::Struct(_) => bail!("Structs don't support unary operators yet!"),
                    IRType::Native(r#type) => match uop.op_type {
                        Ref => todo!(),
                        Deref => todo!(),
                        Negate => {
                            let expression = writer.deref_value(&expression)?;
                            writer.neg(&expression)?
                        }
                        LogicNot => todo!(),
                        BitNot => todo!(),
                    },
                })
            }
            ExpressionContext::BinaryOperator(bop) => {
                let left = self
                    .compile_expression(&bop.left, writer, module)?
                    .ok_or(anyhow!("arg is not value!"))?;
                let left = writer.deref_value(&left)?;
                let right = self
                    .compile_expression(&bop.right, writer, module)?
                    .ok_or(anyhow!("arg is not value!"))?;
                let right = writer.deref_value(&right)?;

                // TODO make it so we don't continue evaluating logic comparisions if they're already resolved
                match left.ctx.r#type {
                    IRType::Struct(_) => bail!("Structs don't support binary operators yet!"),
                    IRType::Native(l_type) => {
                        let r_type = match right.ctx.r#type {
                            IRType::Struct(_) => bail!("Types were not the same!"),
                            IRType::Native(nt) => nt,
                        };

                        if l_type != r_type {
                            bail!("Operator args must be same type ({}, {})", l_type, r_type);
                        }

                        Some(match bop.op_type {
                            BinaryOperator::Add => writer.add(l_type, left.value, right.value)?,
                            BinaryOperator::Sub => writer.sub(l_type, left.value, right.value)?,
                            BinaryOperator::Mul => writer.mul(l_type, left.value, right.value)?,
                            BinaryOperator::Div => writer.div(l_type, left.value, right.value)?,
                            BinaryOperator::Mod => writer.rem(l_type, left.value, right.value)?,
                            BinaryOperator::Equal => todo!(),
                            BinaryOperator::NotEqual => todo!(),
                            BinaryOperator::Greater => todo!(),
                            BinaryOperator::GreaterEqual => todo!(),
                            BinaryOperator::Less => todo!(),
                            BinaryOperator::LessEqual => todo!(),
                            BinaryOperator::LogicAnd => todo!(),
                            BinaryOperator::LogicOr => todo!(),
                            BinaryOperator::BitwiseAnd => {
                                writer.and(l_type, left.value, right.value)?
                            }
                            BinaryOperator::BitwiseOr => {
                                writer.or(l_type, left.value, right.value)?
                            }
                            BinaryOperator::BitwiseXOr => {
                                writer.xor(l_type, left.value, right.value)?
                            }
                            BinaryOperator::BitshiftLeft => {
                                writer.shift_left(l_type, left.value, right.value)?
                            }
                            BinaryOperator::BitshiftRight => {
                                writer.shift_right(l_type, left.value, right.value)?
                            }
                        })
                    }
                }
            }
            ExpressionContext::AnonStruct(anon_struct) => {
                Some(self.compile_anon_struct(&anon_struct, writer, module)?)
            }
            ExpressionContext::Call(call) => {
                //TODO make this call through a pointer instead of manually enforcing an ID
                let id = match &call.callable {
                    ExpressionContext::ScopedId(id) => id,
                    _ => bail!("Cannot call functions on anything but scoped IDs"),
                };

                let arg_v = self.compile_anon_struct(&call.args, writer, module)?;

                //TODO make these more elegant
                match format!("{}", id).as_str() {
                    "next_stage" => {
                        writer.next_stage(arg_v, None)?;
                        None
                    }
                    _ => {
                        if let Some(ContextRef::Function(func)) =
                            self.search_up_stack(id.scopes.as_slice())
                        {
                            todo!()
                        } else {
                            bail!("Identifer {} isn't function", id)
                        }
                    }
                }
            }
        })
    }

    fn compile_block(
        &mut self,
        block: &BlockContext,
        writer: &mut IRWriter,
        module: &mut IRModule,
    ) -> Option<WriterIRValue> {
        let mut last_expression = None;
        for expr in block.expressions.iter() {
            match self.compile_expression(expr, writer, module) {
                Ok(expr) => last_expression = expr,
                Err(err) => self.record_err(err, expr.ctx().clone()),
            };
        }
        last_expression
    }

    fn compile_function(
        &mut self,
        id: String,
        call_sig: &CallSigContext,
        body: &BlockContext,
        module: &mut IRModule,
    ) -> Option<IRFunction> {
        let input = match self.compile_anon_struct_type(&call_sig.input, module) {
            Some(o) => o,
            None => return None,
        };

        let output = match self.compile_anon_struct_type(&call_sig.output, module) {
            Some(o) => o,
            None => return None,
        };

        let input_def = module.get_struct(&input).unwrap();
        let mut args: Vec<_> = input_def
            .members
            .iter()
            .filter_map(|m| {
                let r#type = match &m.r#type {
                    IRType::Native(nt) => IRType::Native(*nt),
                    IRType::Struct(_) => IRType::Native(IRNativeType::Ptr),
                };
                Some((m.id.clone(), r#type))
            })
            .collect();

        let mut writer = IRWriter::new(input.clone());
        {
            writer.start_scope();

            let mut skip_indicies = 0;
            for (index, arg) in args.into_iter().enumerate() {
                if let Some(id) = arg.0 {
                    let value = writer
                        .write(
                            IROp::ArgValue {
                                index: index as u32,
                            },
                            Some(IRValueCtx {
                                r#type: input_def.members[index - skip_indicies].r#type.clone(),
                                is_deref: match &input_def.members[index - skip_indicies].r#type {
                                    IRType::Native(_) => true,
                                    IRType::Struct(_) => false,
                                },
                            }),
                        )
                        .unwrap();
                    writer.set_labeled_value(id, value);
                } else {
                    writer
                        .write(
                            IROp::ArgValue {
                                index: index as u32,
                            },
                            Some(IRValueCtx {
                                r#type: arg.1,
                                is_deref: true,
                            }),
                        )
                        .unwrap();
                    skip_indicies += 1;
                }
            }

            self.compile_block(body, &mut writer, module);

            if let Err(err) = writer.end_scope() {
                self.record_err(err, body.ctx.clone());
            }
        }

        Some(IRFunction {
            id,
            input,
            output,
            operations: writer.finish(),
        })
    }

    fn compile_pipeline(
        &mut self,
        ctx: &PipelineContext,
        module: &mut IRModule,
    ) -> Option<IRPipeline> {
        let input = match self.compile_anon_struct_type(&ctx.call_sig.input, module) {
            Some(i) => i,
            None => return None,
        };

        let output = match self.compile_anon_struct_type(&ctx.call_sig.output, module) {
            Some(o) => o,
            None => return None,
        };

        let mut stages = Vec::new();
        for i in 0..ctx.stages.len() {
            let stage = &ctx.stages[i];

            if let Some(stage) = self.compile_function(
                format!(
                    "{}::{}",
                    ctx.identifier,
                    stage
                        .identifier
                        .as_ref()
                        .map(|i| i.text.clone())
                        .unwrap_or(format!("s{}", i))
                ),
                &stage.call_sig,
                &stage.body,
                module,
            ) {
                let stage_ref = IRIDRef(module.functions.len() as u32);
                module.functions.push(stage);
                stages.push(stage_ref);
            }
        }

        if stages.len() != ctx.stages.len() {
            return None;
        }

        Some(IRPipeline {
            id: ctx.identifier.text.clone(),
            input,
            output,
            stages,
        })
    }

    fn compile_module(&mut self, ctx: &ModuleContext) -> Option<IRModule> {
        let mut module = IRModule {
            id: ctx.identifier.text.clone(),
            structs: Vec::new(),
            functions: Vec::new(),
            pipelines: Vec::new(),
        };

        for pipe in ctx.pipelines.iter() {
            if let Some(pipe) = self.compile_pipeline(&pipe.1, &mut module) {
                module.pipelines.push(pipe);
            }
        }

        Some(module)
    }

    fn compile_project(&mut self, proj: &ProjectContext) -> Vec<IRModule> {
        let mut modules = Vec::new();
        for module in proj.modules.iter() {
            if let Some(module) = self.compile_module(&module.1) {
                modules.push(module);
            }
        }

        modules
    }

    pub fn run(proj: &'ctx ProjectContext) -> CompileResult<Vec<IRModule>> {
        let mut pass = GenerateIRPass {
            messages: Vec::new(),
            using_symbols: LinkedList::new(),
        };

        let modules = pass.compile_project(proj);

        CompileResult {
            data: Some(modules),
            messages: pass.messages,
        }
    }
}
