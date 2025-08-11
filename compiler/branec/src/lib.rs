use anyhow::{anyhow, bail, Result};
use brane_core::ir::{self, Value};
use branec_emitter::{self as emt, Diagnostic, DiagnosticEmitter};
use branec_parser::ast::{self, Ty};
use branec_source::{SourceManager, Span, Uri};
use chumsky::input::Input;
use chumsky::span::Span as _;
use chumsky::Parser;
use std::collections::HashMap;
use std::ops::Range;
use std::rc::Rc;
use std::sync::Arc;

/// Information about how to compile brane script projects
pub struct CompileContext<E: DiagnosticEmitter> {
    pub emitter: E,
    pub sources: SourceManager,
    pub loaded_modules: HashMap<Uri, Vec<ast::Def>>,
}

impl<E: DiagnosticEmitter> CompileContext<E> {
    pub fn emit_module(
        &mut self,
        module_uri: &Uri,
        module_namespace: Vec<String>,
    ) -> anyhow::Result<ir::Module> {
        let defs = {
            self.sources.refresh(module_uri.clone())?;
            let source = self.sources.get(module_uri)?;
            let uri = Arc::new(module_uri.clone());
            let source =
                source.map_span(move |span| Span::new(uri.clone(), span.start()..span.end()));
            match branec_parser::parser().parse(source).into_result() {
                Ok(defs) => defs,
                Err(errors) => {
                    let err_msg = format!("Failed to parse {}:", module_uri);
                    let diag =
                        errors
                            .iter()
                            .fold(emt::error(&err_msg, &self.sources), |diag, error| {
                                // Build the top-level message
                                let span = error.span().clone();
                                let message = match error.reason() {
                                    chumsky::error::RichReason::ExpectedFound {
                                        expected,
                                        found: _,
                                    } => {
                                        let expected: Vec<_> =
                                            expected.iter().map(|e| format!("'{}'", e)).collect();

                                        if expected.is_empty() {
                                            format!("Unexpected")
                                        } else {
                                            let expected = expected.join(", ");
                                            format!("Was expecting {}", expected)
                                        }
                                    }
                                    chumsky::error::RichReason::Custom(msg) => msg.clone(),
                                };
                                diag.err_at(span, message)
                            });
                    diag.emit(&self.emitter)?;
                    bail!(err_msg);
                }
            }
        };

        self.loaded_modules.insert(module_uri.clone(), defs.clone());

        let mut module_ctx = ModuleCtx {
            module: ir::Module::new("DEFAULT"),
            uses: Default::default(), //TODO
            module_namespace,
            current_namespace: Vec::new(),
            module_uri: module_uri.clone(),
        };

        for def in defs {
            match &def.kind {
                ast::DefKind::Function(function) => {
                    let _ = self.emit_fn(function, &mut module_ctx)?;
                }
                //ast::DefKind::Pipeline(pipeline) => pipeline.ident.text.as_str(),
                _ => {}
            }
        }

        Ok(module_ctx.module)
    }

    pub fn resolve_def(
        &self,
        path: &ast::Path,
        module_ctx: &ModuleCtx,
    ) -> Result<Option<&ast::Def>> {
        //TODO account for namespaces and other modules!
        if let Some(defs) = self.loaded_modules.get(&module_ctx.module_uri) {
            for def in defs {
                if let ast::DefKind::Struct(s) = &def.kind {
                    if let Some(last) = path.segments.last() {
                        if last.ident == s.ident {
                            return Ok(Some(def));
                        }
                    }
                } else if let ast::DefKind::Enum(e) = &def.kind {
                    if let Some(last) = path.segments.last() {
                        if last.ident == e.ident {
                            return Ok(Some(def));
                        }
                    }
                }
            }
        }

        Err(anyhow!(
            "Failed to resolve path: {}",
            path.span.source.to_string()
        ))
    }

    pub fn emit_fn(&mut self, function: &ast::Function, module: &mut ModuleCtx) -> Result<u32> {
        let params = function
            .params
            .iter()
            .map(|(ident, ty)| Ok((ident, self.emit_ty(ty, module)?)))
            .collect::<Result<Vec<_>>>()?;

        let return_ty = match &function.ret_ty {
            Some(ty) => Some(self.emit_ty(ty, module)?),
            None => None,
        };

        let ir_fn = {
            let mut fn_ctx = FunctionCtx {
                function: ir::Function {
                    id: function.ident.text.clone(),
                    input: params.iter().map(|(_, v)| v.clone()).collect(),
                    output: return_ty.clone(),
                    blocks: vec![ir::Block {
                        ops: Vec::new(),
                        phi: Vec::new(),
                    }],
                },
                current_block: 0,
                variables: HashMap::new(),
                mod_ctx: module,
            };

            for (index, (ident, ty)) in params.iter().enumerate() {
                fn_ctx.variables.insert(
                    ident.text.clone(),
                    RValue::Value(ir::Value::FnArg(index as u32), ty.clone()),
                );
            }

            self.emit_block(&function.body, &mut fn_ctx)?;

            fn_ctx.function
        };

        let fn_id = module.module.functions.len();
        module.module.functions.push(ir_fn);
        Ok(fn_id as u32)
    }

    pub fn emit_block(&mut self, code_block: &ast::Block, ctx: &mut FunctionCtx) -> Result<()> {
        for stmt in code_block.statements.iter() {
            self.emit_stmt(stmt, ctx)?;
        }

        Ok(())
    }

    pub fn emit_stmt(&mut self, stmt: &ast::Stmt, ctx: &mut FunctionCtx) -> Result<()> {
        match &stmt.kind {
            ast::StmtKind::Expression(expr) => {
                self.emit_r_value(expr, ctx);
            }
            ast::StmtKind::Assign(dest, src) => {
                let d = self.emit_l_value(dest, ctx)?;
                let src = self.emit_r_value(src, ctx)?;
                self.store(d, src, ctx)?;
            }
            ast::StmtKind::VariableDef(ty, ident, expr) => todo!(),
            ast::StmtKind::If(expr, stmt, stmt1) => todo!(),
            ast::StmtKind::While(expr, stmt) => todo!(),
            ast::StmtKind::Match(span, expr, match_branchs) => todo!(),
            ast::StmtKind::Block(block) => todo!(),
            ast::StmtKind::Return(expr) => {
                todo!()
            }
        };
        Ok(())
    }

    /// Emit a temporary value possibly representing the current value of an lvalue
    pub fn emit_r_value(&mut self, stmt: &ast::Expr, ctx: &mut FunctionCtx) -> Result<RValue> {}

    /// Emit a value coresponding to the location of a value or variable
    pub fn emit_l_value(&mut self, stmt: &ast::Expr, ctx: &mut FunctionCtx) -> Result<LValue> {}

    pub fn emit_callable(&mut self, stmt: &ast::Expr, ctx: &mut FunctionCtx) -> Result<Callable> {}

    pub fn store(&mut self, lvalue: &LValue, rvalue: RValue, ctx: &mut FunctionCtx) -> Result<()> {
        match &lvalue {
            LValue::Ptr(ptr, _ty) => match rvalue {
                RValue::Value(src, _ty) => {
                    ctx.emit(ir::Op::Store { src, ptr: *ptr })?;
                }
                RValue::Struct(fields, ty) => {
                    for (i, (_, value)) in fields.iter().enumerate() {
                        let dest = self.get_struct_field(&lvalue, i, ctx)?;
                        self.store(&dest, value.as_ref().clone(), ctx)?;
                    }
                }
                RValue::Enum(data, variant, ty) => {
                    let layout = ty.layout(&ctx.mod_ctx.module)?;
                    /// emit map statment
                    if let Some(data) = data {
                        // This is technically reliant on us not typechecking store...
                        self.store(lvalue, data.as_ref().clone(), ctx);
                    }
                    let variant_ptr = ctx.emit(ir::Op::IAdd {
                        left: *ptr,
                        right: ir::Value::ConstI32(layout.index_offset as i32),
                    })?;
                    ctx.emit(ir::Op::Store {
                        src: variant,
                        ptr: variant_ptr,
                    })?;
                }
            },
            LValue::Variable(items) => {
                ctx.set_at_path(&items, rvalue)?;
            }
        };
        Ok(())
    }

    pub fn load(&mut self, lvalue: &LValue, ctx: &mut FunctionCtx) -> Result<RValue> {
        match lvalue {
            LValue::Ptr(ptr, ty) => match ty {
                ir::Ty::Struct(struct_id) => {
                    let struct_def = ctx
                        .mod_ctx
                        .module
                        .get_struct(*struct_id)
                        .ok_or_else(|| anyhow!("Invalid struct ID: {}", struct_id))?;
                    let fields = struct_def
                        .members
                        .iter()
                        .enumerate()
                        .map(|(i, member)| {
                            let field_lvalue = self.get_struct_field(lvalue, i, ctx)?;
                            let field_value = self.load(&field_lvalue, ctx)?;
                            Ok((member.id.clone().unwrap_or_default(), Box::new(field_value)))
                        })
                        .collect::<Result<Vec<_>>>()?;
                    Ok(RValue::Struct(fields, struct_def.clone()))
                }
                ir::Ty::Enum(enum_id) => {
                    let enum_def = ctx
                        .mod_ctx
                        .module
                        .get_enum(*enum_id)
                        .ok_or_else(|| anyhow!("Invalid enum ID: {}", enum_id))?;
                    let layout = enum_def.layout(&ctx.mod_ctx.module)?;
                    let variant_index_ptr = ctx.emit(ir::Op::IAdd {
                        left: *ptr,
                        right: ir::Value::ConstI32(layout.index_offset as i32),
                    })?;
                    let variant_index = ctx.emit(ir::Op::Load {
                        ptr: variant_index_ptr,
                        ty: layout.index_ty,
                    })?;
                    /// emit map statment
                    let data = match &variant.ty {
                        Some(data_ty) => {
                            let field_lvalue = LValue::Ptr(*ptr, data_ty.clone());
                            Some(Box::new(self.load(&field_lvalue, ctx)?))
                        }
                        None => None,
                    };

                    Ok(RValue::Enum(*ptr, variant_index, enum_def.clone()))
                }
                ir::Ty::Native(nt) => {
                    let value = ctx.emit(ir::Op::Load {
                        ptr: *ptr,
                        ty: nt.clone(),
                    })?;
                    Ok(RValue::Value(value, ty.clone()))
                }
                ir::Ty::Ptr(_, _) => {
                    let value = ctx.emit(ir::Op::Load {
                        ptr: *ptr,
                        // This needs to change with ptr bit size
                        ty: ir::NativeType::U32,
                    })?;
                    Ok(RValue::Value(value, ty.clone()))
                }
            },
            LValue::Variable(path) => ctx.get_variable_at_path(path),
        }
    }

    pub fn get_struct_field(
        &mut self,
        lvalue: &LValue,
        index: usize,
        ctx: &mut FunctionCtx,
    ) -> Result<LValue> {
        match lvalue {
            LValue::Ptr(ptr, ty) => {
                let ir::Ty::Struct(ty) = ty else {
                    bail!("Cannot get field of non struct value")
                };
                let s_ty = ctx
                    .mod_ctx
                    .module
                    .get_struct(*ty)
                    .ok_or_else(|| anyhow!("invalid struct"))?;
                let layout = s_ty.layout(&ctx.mod_ctx.module)?;
                let offset = layout
                    .byte_offsets
                    .get(index)
                    .ok_or_else(|| anyhow!("Struct did not have {} field", index))?;
                let field_ty = s_ty.members[index].ty.clone();
                Ok(LValue::Ptr(
                    ctx.emit(ir::Op::IAdd {
                        left: *ptr,
                        right: ir::Value::ConstI32(*offset as i32),
                    })?,
                    field_ty,
                ))
            }
            LValue::Variable(items) => match ctx.get_variable_at_path(&items)? {
                RValue::Struct(fields, _) => {
                    if let Some((ident, _)) = fields.get(index) {
                        let mut field_path = items.clone();
                        field_path.push(VariablePathSegment::StructField(ident.clone()));
                        Ok(LValue::Variable(field_path))
                    } else {
                        bail!("Struct did not have {} field", index);
                    }
                }
                _ => bail!("Expected struct value!"),
            },
        }
    }

    pub fn emit_ty(&self, ty: &Ty, ctx: &mut ModuleCtx) -> Result<ir::Ty> {
        match &ty.kind {
            ast::TyKind::Native(native_ty) => Ok(match native_ty {
                ast::NativeTy::I8 => ir::Ty::Native(ir::NativeType::I8),
                ast::NativeTy::I16 => ir::Ty::Native(ir::NativeType::I16),
                ast::NativeTy::I32 => ir::Ty::Native(ir::NativeType::I32),
                ast::NativeTy::I64 => ir::Ty::Native(ir::NativeType::I64),
                ast::NativeTy::U8 => ir::Ty::Native(ir::NativeType::U8),
                ast::NativeTy::U16 => ir::Ty::Native(ir::NativeType::U16),
                ast::NativeTy::U32 => ir::Ty::Native(ir::NativeType::U32),
                ast::NativeTy::U64 => ir::Ty::Native(ir::NativeType::U64),
                ast::NativeTy::F32 => ir::Ty::Native(ir::NativeType::F32),
                ast::NativeTy::F64 => ir::Ty::Native(ir::NativeType::F64),
                ast::NativeTy::Bool => ir::Ty::Native(ir::NativeType::Bool),
                ast::NativeTy::Char => ir::Ty::Native(ir::NativeType::U32),
                ast::NativeTy::String => {
                    let emitted_ptr_ty =
                        ir::Ty::Ptr(true, Some(Box::new(ir::Ty::Native(ir::NativeType::U32))));
                    let emitted_len_ty = ir::Ty::Native(ir::NativeType::U32);

                    let emitted_fields = vec![
                        ir::StructMember {
                            id: Some("ptr".to_string()),
                            ty: emitted_ptr_ty,
                        },
                        ir::StructMember {
                            id: Some("len".to_string()),
                            ty: emitted_len_ty,
                        },
                    ];

                    let struct_index = ctx.module.structs.len() as i32;
                    ctx.module.structs.push(ir::Struct {
                        id: None,
                        members: emitted_fields,
                        packed: false,
                    });

                    ir::Ty::Struct(struct_index)
                }
            }),
            ast::TyKind::Ptr(is_mut, inner_ty) => {
                let emitted_inner_ty = match inner_ty {
                    Some(inner) => Some(Box::new(self.emit_ty(inner.as_ref(), ctx)?)),
                    None => None,
                };
                Ok(ir::Ty::Ptr(*is_mut, emitted_inner_ty))
            }
            ast::TyKind::Tuple(elements) => {
                let emitted_fields = elements
                    .iter()
                    .enumerate()
                    .map(|(_index, elem)| {
                        let emitted_ty = self.emit_ty(elem.as_ref(), ctx)?;
                        Ok(ir::StructMember {
                            id: None,
                            ty: emitted_ty,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;

                if let Some((existing_index, _)) = ctx
                    .module
                    .structs
                    .iter()
                    .enumerate()
                    .find(|(_, s)| s.members == emitted_fields && s.id.is_none())
                {
                    return Ok(ir::Ty::Struct(existing_index as i32));
                }

                let struct_index = ctx.module.structs.len() as i32;
                ctx.module.structs.push(ir::Struct {
                    id: None,
                    members: emitted_fields,
                    packed: false,
                });

                Ok(ir::Ty::Struct(struct_index))
            }
            ast::TyKind::Struct(fields) => {
                let emitted_fields = fields
                    .iter()
                    .map(|(field_name, field_ty)| {
                        let emitted_ty = self.emit_ty(field_ty.as_ref(), ctx)?;
                        Ok(ir::StructMember {
                            id: Some(field_name.text.clone()),
                            ty: emitted_ty,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;

                if let Some((existing_index, _)) = ctx
                    .module
                    .structs
                    .iter()
                    .enumerate()
                    .find(|(_, s)| s.members == emitted_fields && s.id.is_none())
                {
                    return Ok(ir::Ty::Struct(existing_index as i32));
                }

                let struct_index = ctx.module.structs.len() as i32;
                ctx.module.structs.push(ir::Struct {
                    id: None,
                    members: emitted_fields,
                    packed: false,
                });

                Ok(ir::Ty::Struct(struct_index))
            }
            ast::TyKind::Slice(is_mut, inner_ty) => {
                let inner_ty = match inner_ty {
                    Some(inner_ty) => Some(Box::new(self.emit_ty(&inner_ty, ctx)?)),
                    None => None,
                };

                let emitted_ptr_ty = ir::Ty::Ptr(true, inner_ty);
                let emitted_len_ty = ir::Ty::Native(ir::NativeType::U32);

                let emitted_fields = vec![
                    ir::StructMember {
                        id: Some("ptr".to_string()),
                        ty: emitted_ptr_ty,
                    },
                    ir::StructMember {
                        id: Some("len".to_string()),
                        ty: emitted_len_ty,
                    },
                ];

                if let Some((existing_index, _)) = ctx
                    .module
                    .structs
                    .iter()
                    .enumerate()
                    .find(|(_, s)| s.members == emitted_fields && s.id.is_none())
                {
                    return Ok(ir::Ty::Struct(existing_index as i32));
                }

                let struct_index = ctx.module.structs.len() as i32;
                ctx.module.structs.push(ir::Struct {
                    id: None,
                    members: emitted_fields,
                    packed: false,
                });

                Ok(ir::Ty::Struct(struct_index))
            }
            ast::TyKind::Path(path) => {
                if let Some(def) = self.resolve_def(&path, ctx)? {
                    match &def.kind {
                        ast::DefKind::Struct(s) => {
                            let emitted_fields = s
                                .fields
                                .iter()
                                .map(|(field_name, field_ty)| {
                                    let emitted_ty = self.emit_ty(field_ty, ctx)?;
                                    Ok(ir::StructMember {
                                        id: Some(field_name.text.clone()),
                                        ty: emitted_ty,
                                    })
                                })
                                .collect::<Result<Vec<_>>>()?;

                            let id = Some(s.ident.text.clone());

                            if let Some((existing_index, _)) = ctx
                                .module
                                .structs
                                .iter()
                                .enumerate()
                                .find(|(_, s)| s.members == emitted_fields && s.id == id)
                            {
                                return Ok(ir::Ty::Struct(existing_index as i32));
                            }

                            let struct_index = ctx.module.structs.len() as i32;
                            ctx.module.structs.push(ir::Struct {
                                id,
                                members: emitted_fields,
                                packed: false,
                            });

                            Ok(ir::Ty::Struct(struct_index))
                        }
                        ast::DefKind::Enum(e) => {
                            let emitted_varaints = e
                                .variants
                                .iter()
                                .map(|(field_name, field_ty)| {
                                    Ok(ir::EnumVariant {
                                        id: field_name.text.clone(),
                                        ty: match field_ty {
                                            Some(ty) => Some(self.emit_ty(ty, ctx)?),
                                            None => None,
                                        },
                                    })
                                })
                                .collect::<Result<Vec<_>>>()?;

                            let id = Some(e.ident.text.clone());

                            if let Some((existing_index, _)) = ctx
                                .module
                                .enums
                                .iter()
                                .enumerate()
                                .find(|(_, s)| s.variants == emitted_varaints && s.id == id)
                            {
                                return Ok(ir::Ty::Enum(existing_index as i32));
                            }

                            let enum_index = ctx.module.structs.len() as i32;
                            ctx.module.enums.push(ir::Enum {
                                id: None,
                                variants: emitted_varaints,
                                packed: false,
                            });

                            Ok(ir::Ty::Enum(enum_index))
                        }
                        _ => bail!("Path does not reference a valid type"),
                    }
                } else {
                    bail!("Type not found: {}", path.span.source.to_string());
                }
            }
        }
    }

    pub fn mutated_value_labels(
        &mut self,
        stmt: &ast::Stmt,
        ctx: &mut FunctionCtx,
    ) -> Result<Vec<String>> {
        let mut mutated = Vec::new();
        match &stmt.kind {
            ast::StmtKind::Expression(expr) => {}
            ast::StmtKind::Assign(expr, expr1) => self.evalutate_expression(expr, ctx)?.as_ssa_id(),
            ast::StmtKind::VariableDef(ty, ident, expr) => todo!(),
            ast::StmtKind::If(expr, stmt, stmt1) => todo!(),
            ast::StmtKind::While(expr, stmt) => todo!(),
            ast::StmtKind::Match(span, expr, match_branchs) => todo!(),
            ast::StmtKind::Block(block) => todo!(),
            ast::StmtKind::Return(expr) => todo!(),
        }
        Ok(mutated)
    }

    pub fn evalutate_expression(
        &mut self,
        expr: &ast::Expr,
        ctx: &mut FunctionCtx,
    ) -> Result<Value> {
        match &expr.kind {
            ast::ExprKind::Literal(literal) => todo!(),
            ast::ExprKind::Array(exprs) => todo!(),
            ast::ExprKind::Tuple(exprs) => todo!(),
            ast::ExprKind::Struct(path, items) => todo!(),
            ast::ExprKind::Path(path) => {
                // Technically this should emit intrinsic functions too
                if path.segments.len() == 1 {
                    let ident = &path.segments.first().unwrap().ident;
                    if let Some(var) = ctx.variables.get(&ident.text) {
                        return Ok(var.clone());
                    }
                }
                bail!("Unable to find {}", path)
            }
            ast::ExprKind::Ref(expr) => todo!(),
            ast::ExprKind::Deref(expr) => todo!(),
            ast::ExprKind::Field(expr, path_segment) => todo!(),
            ast::ExprKind::Call(expr, exprs) => match &expr.kind {
                ast::ExprKind::Path(path) => {
                    if path.segments.len() == 1 {
                        let path = path.segments.first().unwrap();
                        match path.ident.text.as_str() {
                            "add" => {
                                let a = self.evalutate_expression(&exprs[0], ctx)?;
                                let b = self.evalutate_expression(&exprs[1], ctx)?;
                                return Ok(ctx.expr(
                                    Some(Ty::Native(ast::NativeTy::I32)),
                                    move |function, cache| {
                                        let a = a.value(function, cache)?.unwrap();
                                        let b = b.value(function, cache)?.unwrap();
                                        let lb = function.blocks.last_mut().unwrap();
                                        let op_id = lb.ops.len() as u32;
                                        lb.ops.push(ir::Op::IAdd { left: a, right: b });
                                        Ok(Some(ir::Value::BlockOp {
                                            block: (function.blocks.len() - 1) as u32,
                                            op: op_id,
                                        }))
                                    },
                                ));
                            }
                            name => bail!("Unknown intrinsic {}", name),
                        }
                    }
                    bail!("calls not implemented yet lol")
                }
                _ => todo!(),
            },
        }
    }
}

pub struct ModuleCtx {
    pub module: ir::Module,
    pub module_uri: Uri,
    pub uses: HashMap<String, String>,
    pub module_namespace: Vec<String>,
    pub current_namespace: Vec<String>,
}

pub struct FunctionCtx<'mc> {
    pub mod_ctx: &'mc mut ModuleCtx,
    pub function: ir::Function,
    pub current_block: usize,
    pub variables: HashMap<String, RValue>,
}

impl<'mc> FunctionCtx<'mc> {
    pub fn emit(&mut self, op: ir::Op) -> Result<ir::Value> {
        let b = self
            .function
            .blocks
            .get_mut(self.current_block)
            .ok_or_else(|| {
                anyhow!(
                    "no block at index {} when trying to emit {}",
                    self.current_block,
                    op
                )
            })?;
        let op_id = b.ops.len() as u32;
        b.ops.push(op);
        Ok(ir::Value::BlockOp {
            block: self.current_block as u32,
            op: op_id,
        })
    }

    pub fn get_variable_at_path(&mut self, path: &Vec<VariablePathSegment>) -> Result<RValue> {
        if path.is_empty() {
            bail!("variable path was empty");
        }
        let mut current_root: Option<RValue> = None;
        for seg in path.iter() {
            /*if let Some(root) = &mut dest {
                match root {
                    Variable::Value(value, ty) => todo!(),
                    Variable::Struct(items, _) => todo!(),
                    Variable::Enum(items, _) => todo!(),
                }
            }*/
            match seg {
                VariablePathSegment::Label(label) => {
                    if current_root.is_some() {
                        bail!("Label must only root value, but found path: {:?}", path)
                    }
                    current_root =
                        Some(self.variables.get(label).cloned().ok_or_else(|| {
                            anyhow!("could not resolve variable path {:?}", path)
                        })?);
                }
                VariablePathSegment::StructField(field) => {
                    let Some(root) = current_root else {
                        bail!(
                            "could not resolve variable path {:?}, must have label as root",
                            path
                        );
                    };
                    match root {
                        RValue::Struct(fields, _) => {
                            let new_root = fields.iter().find_map(|(label, value)| {
                                if label == field {
                                    Some(value.as_ref().clone())
                                } else {
                                    None
                                }
                            });
                            current_root = new_root;
                        }
                        RValue::Enum(_, _, _) => bail!("Expected struct but found enum"),
                        RValue::Value(_, _) => bail!("Expected struct but found value"),
                    }
                }
            }
        }
        Ok(current_root.unwrap())
    }

    pub fn set_at_path(&mut self, path: &Vec<VariablePathSegment>, variable: RValue) -> Result<()> {
        if path.is_empty() {
            bail!("variable path was empty");
        }
        let mut dest: Option<&mut RValue> = None;
        for seg in path.iter() {
            /*if let Some(root) = &mut dest {
                match root {
                    Variable::Value(value, ty) => todo!(),
                    Variable::Struct(items, _) => todo!(),
                    Variable::Enum(items, _) => todo!(),
                }
            }*/
            match seg {
                VariablePathSegment::Label(label) => {
                    if dest.is_some() {
                        bail!("Label must only root value, but found path: {:?}", path)
                    }
                    dest =
                        Some(self.variables.get_mut(label).ok_or_else(|| {
                            anyhow!("could not resolve variable path {:?}", path)
                        })?);
                }
                VariablePathSegment::StructField(field) => {
                    let Some(root) = dest else {
                        bail!(
                            "could not resolve variable path {:?}, must have label as root",
                            path
                        );
                    };
                    match root {
                        RValue::Struct(fields, _) => {
                            let new_root = fields.iter_mut().find_map(|(label, value)| {
                                if label == field {
                                    Some(value.as_mut())
                                } else {
                                    None
                                }
                            });
                            dest = new_root;
                        }
                        RValue::Enum(_, _, _) => bail!("Expected struct but found enum"),
                        RValue::Value(_, _) => bail!("Expected struct but found value"),
                    }
                }
            }
        }
        *dest.unwrap() = variable;
        Ok(())
    }
}

/// Values without a memory address
#[derive(Clone)]
enum RValue {
    Value(ir::Value, ir::Ty),
    Struct(Vec<(String, Box<RValue>)>, ir::Struct),
    /// enum data contains SSA values for all variants. All variants must be defined even if only some variants
    /// contain actual values. Non-defined variants will contain default values. Load instructions
    /// will be emitted as if all variants were defined simaltaniously.
    /// Then eliminated by dead code evaluation.
    /// TODO also have a optimization pass that moves values only used once into the blocks
    /// (enum data, variant index, enum def)
    Enum(Vec<RValue>, ir::Value, ir::Enum),
}

/// Values with a memory address or settable value
#[derive(Clone)]
enum LValue {
    /// Runtime memory address and inner type
    Ptr(ir::Value, ir::Ty),
    /// Compile time variable or emulated struct
    Variable(Vec<VariablePathSegment>),
}

#[derive(Debug, Clone)]
enum VariablePathSegment {
    Label(String),
    StructField(String),
}

#[derive(Clone)]
enum Callable {
    IntrinsicFn(Rc<dyn Fn(Vec<RValue>) -> Result<Option<RValue>>>),
}
