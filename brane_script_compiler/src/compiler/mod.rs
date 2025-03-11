use std::collections::{HashMap, LinkedList};

use anyhow::{anyhow, bail};
use brane_script_runtime::ir::{
    IDRef, IRFunction, IRModule, IRNativeType, IROp, IRPipeline, IRStruct, IRStructMember, IRType,
    IRValue,
};
use defer::defer;

use crate::{
    document_context::{
        AnonStructContext, AnonStructTypeContext, BinaryOperator, BlockContext, CallSigContext,
        ContextRef, DocumentContext, ExpressionContext, ModuleContext, PipelineContext,
        ProjectContext, ScopeSegment, StructContext, TextContext, TypeContext, TypeModifiers,
    },
    parsed_document::ParsedDocument,
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
    labeled_values: HashMap<String, IRValue>,
}

pub struct IRWriter {
    /// (operation, Option<type operation returns, is i32 ptr to value>
    pub operations: Vec<(IROp, Option<(IRType, bool)>)>,
    pub scopes: LinkedList<IRWriterScope>,
}

impl IRWriter {
    pub fn new(arg_type: IDRef) -> IRWriter {
        IRWriter {
            operations: vec![(IROp::NoOp, Some((IRType::Struct(arg_type), false)))],
            scopes: LinkedList::new(),
        }
    }

    pub fn finish(self) -> Vec<IROp> {
        self.operations.into_iter().map(|op| op.0).collect()
    }

    /// Write a new operation and record it's return value for future use
    pub fn write(
        &mut self,
        op: IROp,
        out_type: Option<IRType>,
        ret_is_ptr: bool,
    ) -> Option<IRValue> {
        let op_index = self.operations.len() as u32;
        self.operations
            .push((op, out_type.as_ref().map(|t| (t.clone(), ret_is_ptr))));
        if let Some(_) = out_type {
            Some(IRValue { 0: op_index })
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
    pub fn get_value_type(&self, value: IRValue) -> Option<(IRType, bool)> {
        self.operations
            .get(value.0 as usize)
            .as_ref()
            .map(|(_, t)| t.clone())
            .flatten()
    }

    pub fn set_labeled_value(&mut self, label: impl Into<String>, value: IRValue) {
        self.scopes
            .back_mut()
            .expect("needs at least one scope")
            .labeled_values
            .insert(label.into(), value);
    }

    pub fn get_labeled_value(&self, label: &str) -> Option<IRValue> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.labeled_values.get(label) {
                return Some(*value);
            }
        }
        None
    }

    // allocate stack memory of a type and get a i32 value representing a pointer back
    pub fn alloca(&mut self, r#type: IRType) -> IRValue {
        let is_shallow_ptr = match r#type {
            IRType::Native(_) => true,
            IRType::Struct(_) => false,
        };
        self.write(
            IROp::AllocA {
                r#type: r#type.clone(),
            },
            Some(r#type),
            is_shallow_ptr,
        )
        .expect("we should always get a value if we pass a ret type")
    }

    pub fn get_struct_member_ptr(
        &mut self,
        structure_type: &IDRef,
        ptr: IRValue,
        index: usize,
        module: &IRModule,
    ) -> anyhow::Result<IRValue> {
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

        let offset_value = self.get_const_u32(*ptr_offset as u32);
        let member_ptr_index = self.add(ptr, offset_value)?;

        // Correct output type
        let op = &mut self.operations[member_ptr_index.0 as usize];
        let m_type = &structure.members[index].r#type;
        op.1 = Some((
            m_type.clone(),
            match m_type {
                IRType::Native(_) => true,
                IRType::Struct(_) => false,
            },
        ));

        Ok(member_ptr_index)
    }

    /// Operation that registers a const
    pub fn get_const_u32(&mut self, value: u32) -> IRValue {
        self.write(
            IROp::ConstU32 { value },
            Some(IRType::Native(IRNativeType::U32)),
            false,
        )
        .expect("we should always get a value if we pass a ret type")
    }

    pub fn resolve_value(&mut self, value: IRValue) -> anyhow::Result<IRValue> {
        let storage = match self.get_value_type(value) {
            Some(s) => s,
            None => bail!("value not found!"),
        };

        Ok(if storage.1 {
            match storage.0 {
                IRType::Native(nt) => self.load(nt, value)?,
                IRType::Struct(_) => {
                    bail!("Cannot load struct types! Use get_struct_member_ptr() to access data")
                }
            }
        } else {
            value
        })
    }

    pub fn add(&mut self, left: IRValue, right: IRValue) -> anyhow::Result<IRValue> {
        let left = self.resolve_value(left)?;
        let right = self.resolve_value(right)?;

        Ok(self
            .write(
                IROp::Add { left, right },
                Some(self.get_value_type(left).unwrap().0),
                false,
            )
            .expect("we should always get a value if we pass a ret type"))
    }

    pub fn sub(&mut self, left: IRValue, right: IRValue) -> anyhow::Result<IRValue> {
        let left = self.resolve_value(left)?;
        let right = self.resolve_value(right)?;

        Ok(self
            .write(
                IROp::Sub { left, right },
                Some(self.get_value_type(left).unwrap().0),
                false,
            )
            .expect("we should always get a value if we pass a ret type"))
    }

    pub fn mul(&mut self, left: IRValue, right: IRValue) -> anyhow::Result<IRValue> {
        let left = self.resolve_value(left)?;
        let right = self.resolve_value(right)?;

        Ok(self
            .write(
                IROp::Mul { left, right },
                Some(self.get_value_type(left).unwrap().0),
                false,
            )
            .expect("we should always get a value if we pass a ret type"))
    }

    pub fn div(&mut self, left: IRValue, right: IRValue) -> anyhow::Result<IRValue> {
        let left = self.resolve_value(left)?;
        let right = self.resolve_value(right)?;

        Ok(self
            .write(
                IROp::Div { left, right },
                Some(self.get_value_type(left).unwrap().0),
                false,
            )
            .expect("we should always get a value if we pass a ret type"))
    }

    fn load(&mut self, r#type: IRNativeType, ptr: IRValue) -> anyhow::Result<IRValue> {
        Ok(self
            .write(
                IROp::Load { ptr, r#type },
                Some(IRType::Native(r#type)),
                false,
            )
            .expect("we should always get a value if we pass a ret type"))
    }

    fn store(&mut self, src: IRValue, dest: IRValue) -> anyhow::Result<()> {
        let src = self.resolve_value(src)?;
        self.write(IROp::Store { src, ptr: dest }, None, false);
        Ok(())
    }

    fn call(&mut self, args: IRValue, ret: Option<IDRef>) -> anyhow::Result<Option<IRValue>> {
        todo!();
    }

    fn next_stage(&mut self, args: IRValue, deps: Option<IRValue>) -> anyhow::Result<()> {
        let args = self.resolve_value(args)?;
        self.write(IROp::NextStage { args, deps }, None, false);
        Ok(())
    }

    fn copy_struct(
        &mut self,
        struct_type: &IDRef,
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
                    let value = self.load(r#type.clone(), src_mem_ptr)?;
                    self.store(value, dest_mem_ptr)?;
                }
                IRType::Struct(struct_type) => {
                    self.copy_struct(struct_type, src_mem_ptr, dest_mem_ptr, module)?;
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
                    Ok(IRType::Native(IRNativeType::I32))
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
                                return Ok(IRType::Struct(IDRef::Index(i as u32)));
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

    fn compile_struct(&mut self, ctx: &StructContext, module: &mut IRModule) -> Option<IDRef> {
        let struct_id = IDRef::Index(module.structs.len() as u32);

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
    ) -> Result<IDRef, ToolchainMessage> {
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

            return Ok(IDRef::Index(i as u32));
        }

        let new_id = IDRef::Index(module.structs.len() as u32);
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
    ) -> Option<IDRef> {
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
    ) -> anyhow::Result<IRValue> {
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
                r#type: writer.get_value_type(member_values[i]).unwrap().0,
            })
        }
        let struct_type = self
            .resolve_anon_struct_type(member_defs.clone(), module)
            .map_err(|e| anyhow!(e.message))?;
        let root_ptr = writer.alloca(IRType::Struct(struct_type.clone()));
        for i in 0..member_values.len() {
            let mem_ptr = writer.get_struct_member_ptr(&struct_type, root_ptr, i, module)?;
            match &member_defs[i].r#type {
                IRType::Native(r#type) => {
                    let value = writer.load(r#type.clone(), member_values[i])?;
                    writer.store(value, mem_ptr)?;
                }
                IRType::Struct(struct_type) => {
                    writer.copy_struct(struct_type, member_values[i], mem_ptr, module)?;
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
    ) -> anyhow::Result<Option<IRValue>> {
        Ok(match expression {
            ExpressionContext::Scope(block) => self.compile_block(block.as_ref(), writer, module),
            ExpressionContext::Assignment(assignment) => {
                let src = self
                    .compile_expression(&assignment.src, writer, module)?
                    .ok_or(anyhow!("arg is not value!"))?;
                let dest = self
                    .compile_expression(&assignment.dest, writer, module)?
                    .ok_or(anyhow!("arg is not value!"))?;
                writer.store(src, dest)?;
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

                writer.set_labeled_value(var_label.text.clone(), mem);
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
                let struct_type = writer
                    .get_value_type(ptr)
                    .ok_or(anyhow!("couldn't get value type!"))?
                    .0;
                let struct_type = match struct_type {
                    IRType::Native(_) => bail!("Can't get member of non-struct type!"),
                    IRType::Struct(st) => st,
                };
                Some(writer.get_struct_member_ptr(&struct_type, ptr, gep.member, module)?)
            }
            ExpressionContext::UnaryOperator(uop) => todo!(),
            ExpressionContext::BinaryOperator(bop) => {
                let left = self
                    .compile_expression(&bop.left, writer, module)?
                    .ok_or(anyhow!("arg is not value!"))?;
                let right = self
                    .compile_expression(&bop.right, writer, module)?
                    .ok_or(anyhow!("arg is not value!"))?;
                Some(match bop.op_type {
                    BinaryOperator::Add => writer.add(left, right)?,
                    BinaryOperator::Sub => writer.sub(left, right)?,
                    BinaryOperator::Mul => writer.mul(left, right)?,
                    BinaryOperator::Div => writer.div(left, right)?,
                    BinaryOperator::Mod => todo!(),
                    BinaryOperator::Equal => todo!(),
                    BinaryOperator::NotEqual => todo!(),
                    BinaryOperator::Greater => todo!(),
                    BinaryOperator::GreaterEqual => todo!(),
                    BinaryOperator::Less => todo!(),
                    BinaryOperator::LessEqual => todo!(),
                    BinaryOperator::LogicAnd => todo!(),
                    BinaryOperator::LogicOr => todo!(),
                    BinaryOperator::BitwiseAnd => todo!(),
                    BinaryOperator::BitwiseOr => todo!(),
                    BinaryOperator::BitwiseXOr => todo!(),
                    BinaryOperator::BitshiftLeft => todo!(),
                    BinaryOperator::BitshiftRight => todo!(),
                })
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

                //TODO make these more elant
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
    ) -> Option<IRValue> {
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
            Some(i) => i,
            None => return None,
        };

        let output = match self.compile_anon_struct_type(&call_sig.output, module) {
            Some(o) => o,
            None => return None,
        };

        let mut writer = IRWriter::new(input.clone());
        {
            writer.start_scope();

            let input_ptr = IRValue { 0: 0 };
            for i in 0..call_sig.input.members.len() {
                let arg = &call_sig.input.members[i];
                let label = match &arg.label {
                    Some(l) => l.text.clone(),
                    None => continue,
                };
                let value = match writer.get_struct_member_ptr(&input, input_ptr, i, module) {
                    Ok(v) => v,
                    Err(err) => {
                        self.record_err(err, arg.ctx.clone());
                        continue;
                    }
                };
                writer.set_labeled_value(label, value);
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
                let stage_ref = IDRef::Index(module.functions.len() as u32);
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
