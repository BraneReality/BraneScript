use anyhow::{anyhow, bail, Result};
use brane_core::ir;
use branec_emitter::{self as emt, Diagnostic, DiagnosticEmitter};
use branec_parser::ast::{self, Ty};
use branec_source::{SourceManager, Span, Uri};
use chumsky::span::Span as _;
use chumsky::Parser;
use std::collections::HashMap;
use std::ops::Range;
use std::rc::Rc;
use std::sync::Arc;

mod passes;

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
            let source = <_ as chumsky::input::Input>::map_span(source.as_str(), move |span| {
                Span::new(uri.clone(), span.start()..span.end())
            });
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

        println!("pre-prune:\n{}", module_ctx.module);
        passes::prune_phi_nodes(&mut module_ctx.module)?;
        println!("post-prune:\n{}", module_ctx.module);

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

        Err(anyhow!("Failed to resolve path: {}", path))
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
                    blocks: vec![],
                },
                current_block: None,
                variables: Vec::new(),
                mod_ctx: module,
                block_phi_mappings: Vec::new(),
                scopes: Vec::new(),
            };

            let entry = fn_ctx.create_block()?;
            fn_ctx.start_block(entry)?;

            fn_ctx.start_scope();
            for (index, (ident, ty)) in params.iter().enumerate() {
                if let ir::Ty::Native(nt) = ty {
                    fn_ctx.define_variable(
                        ident.text.clone(),
                        RValue::Value(ir::Value::FnArg(index as u32), nt.clone()),
                    );
                } else {
                    bail!("Functions do not support enum or struct args yet! Use poiners.");
                    todo!("Need to flatten function parameters");
                }
            }

            self.emit_block(&function.body, &mut fn_ctx)?;

            fn_ctx.end_scope();

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
        if ctx.current_block.is_none() {
            emt::warning("Dead code", &self.sources)
                .warning_at(
                    stmt.span.clone(),
                    "Code after return, break, or continue won't be evaluated",
                )
                .emit(&self.emitter)?;
        }

        match &stmt.kind {
            ast::StmtKind::Expression(expr) => {
                self.emit_r_value(expr, ctx)?;
            }
            ast::StmtKind::Assign(dest, src) => {
                let d = self.emit_l_value(dest, ctx)?;
                let src = self
                    .emit_r_value(src, ctx)?
                    .ok_or_else(|| anyhow!("Expression does not return value!"))?;
                self.store(&d, src, ctx)?;
            }
            ast::StmtKind::VariableDef(ty, ident, expr) => {
                let src = self
                    .emit_r_value(expr, ctx)?
                    .ok_or_else(|| anyhow!("Expression does not return value!"))?;
                let ty = self.emit_ty(ty, &mut ctx.mod_ctx)?;
                if ty != src.ty() {
                    bail!("expected type {} but found {}", ty, src.ty());
                }
                ctx.define_variable(ident.text.clone(), src);
            }
            ast::StmtKind::If(cond, body, else_stmt) => {
                let cond = self
                    .emit_r_value(cond, ctx)?
                    .ok_or_else(|| anyhow!("Expression does not return value!"))?;
                let RValue::Value(cond, ir::NativeType::Bool) = cond else {
                    bail!("If condition must be a bool, but was {}", cond.ty());
                };
                let body_block = ctx.create_block()?;
                let else_block = match else_stmt {
                    Some(_) => Some(ctx.create_block()?),
                    None => None,
                };
                let join_block = ctx.create_block()?;
                ctx.end_block_jump_if(cond, body_block, else_block.unwrap_or(join_block));
                ctx.start_block(body_block)?;
                self.emit_stmt(&body, ctx)?;
                if ctx.current_block.is_some() {
                    ctx.end_block_jump(join_block);
                }
                if let (Some(else_block), Some(else_stmt)) = (else_block, else_stmt) {
                    ctx.start_block(else_block)?;
                    self.emit_stmt(&else_stmt, ctx)?;
                    if ctx.current_block.is_some() {
                        ctx.end_block_jump(join_block);
                    }
                }
                ctx.start_block(join_block)?;
            }
            ast::StmtKind::While(cond, body) => {
                let cond_block = ctx.create_block()?;
                let body_block = ctx.create_block()?;
                let finally_block = ctx.create_block()?;
                ctx.end_block_jump(cond_block);
                ctx.start_block(cond_block)?;
                let cond = self
                    .emit_r_value(cond, ctx)?
                    .ok_or_else(|| anyhow!("Expression does not return value!"))?;
                let RValue::Value(cond, ir::NativeType::Bool) = cond else {
                    bail!("While condition must be a bool, but was {}", cond.ty());
                };
                ctx.end_block_jump_if(cond, body_block, finally_block);
                ctx.start_block(body_block)?;
                self.emit_stmt(body, ctx)?;
                if ctx.current_block.is_some() {
                    ctx.end_block_jump(cond_block);
                }
                ctx.start_block(finally_block)?;
            }
            ast::StmtKind::Match(_span, expr, match_branches) => {
                let cond = self
                    .emit_r_value(expr, ctx)?
                    .ok_or_else(|| anyhow!("Expression does not return value!"))?;
                match cond {
                    RValue::Value(index, ty) => {
                        let branches =
                            match_branches
                                .iter()
                                .try_fold(Vec::new(), |mut indices, b| {
                                    let ast::CaseKind::Int(v) = &b.case else {
                                        bail!("Match cases must be int values!");
                                    };

                                    indices.push((*v as u128, ctx.create_block()?));
                                    Ok(indices)
                                })?;

                        let variables = ctx.record_variables();
                        let join_block = ctx.create_block()?;
                        let default_block = ctx.create_block()?;
                        ctx.end_block_jump_map(&ty, index, default_block, branches.clone())?;
                        for ((_cond, block), match_branch) in
                            branches.into_iter().zip(match_branches)
                        {
                            ctx.set_variables(variables.clone());
                            ctx.start_block(block)?;
                            ctx.start_scope();

                            self.emit_stmt(&match_branch.body, ctx)?;
                            ctx.end_scope();
                            if ctx.current_block.is_some() {
                                ctx.end_block_jump(join_block);
                            }
                        }
                        ctx.set_variables(variables.clone());
                        ctx.start_block(default_block)?;
                        ctx.end_block_jump(join_block);
                        ctx.start_block(join_block)?;
                    }
                    RValue::Enum(SSAEnum {
                        variants,
                        index,
                        index_ty: _,
                        def_id,
                    }) => {
                        let enum_def = ctx
                            .mod_ctx
                            .module
                            .get_enum(def_id)
                            .ok_or_else(|| anyhow!("Invalid enum ID: {}", def_id))?;
                        let layout = enum_def.layout(&ctx.mod_ctx.module)?;
                        let def_clone = enum_def.clone();

                        let (branches, variants) = match_branches.iter().try_fold(
                            (Vec::new(), Vec::new()),
                            |(mut branches, mut inner_data), b| {
                                let ast::CaseKind::EnumVariant(v_id, data_label) = &b.case else {
                                    bail!("Match cases must be enum variants!");
                                };

                                let Some(index) =
                                    def_clone.variants.iter().enumerate().find_map(|(i, v)| {
                                        if v.id == v_id.text {
                                            Some(i)
                                        } else {
                                            None
                                        }
                                    })
                                else {
                                    bail!(
                                        "Enum {} does not have a variant {}",
                                        def_clone.id.clone().unwrap_or_default(),
                                        v_id.text
                                    );
                                };

                                branches.push((index as u128, ctx.create_block()?));
                                inner_data.push((data_label.clone(), variants[index].clone()));
                                Ok((branches, inner_data))
                            },
                        )?;

                        let join_block = ctx.create_block()?;
                        let default_block = ctx.create_block()?;
                        let variables = ctx.record_variables();
                        ctx.end_block_jump_map(
                            &layout.index_ty,
                            index,
                            default_block,
                            branches.clone(),
                        )?;

                        ctx.start_block(default_block)?;
                        ctx.end_block_jump(join_block);
                        for (((_cond, block), match_branch), variant) in
                            branches.into_iter().zip(match_branches).zip(variants)
                        {
                            ctx.set_variables(variables.clone());
                            ctx.start_block(block)?;
                            ctx.start_scope();
                            if let (Some(data_label), Some(variant_data)) = variant {
                                ctx.define_variable(data_label.text, variant_data);
                            }

                            self.emit_stmt(&match_branch.body, ctx)?;

                            ctx.end_scope();
                            ctx.end_block_jump(join_block);
                        }
                        ctx.start_block(join_block)?;
                    }
                    RValue::Struct(_) => bail!("Cannot match on structs!"),
                }
            }
            ast::StmtKind::Block(block) => {
                ctx.start_scope();
                for stmt in block.statements.iter() {
                    self.emit_stmt(stmt, ctx)?;
                }
                ctx.end_scope();
            }
            ast::StmtKind::Return(expr) => {
                let body = match expr {
                    Some(expr) => Some(
                        match self
                            .emit_r_value(expr, ctx)?
                            .ok_or_else(|| anyhow!("Expression does not return value!"))?
                        {
                            RValue::Value(value, _) => {
                                //TODO typecheck
                                value
                            }
                            _ => {
                                bail!("We don't support returning structs or enums yet! Use pointers.")
                            }
                        },
                    ),
                    None => None,
                };
                ctx.end_block_ret(body);
            }
        };
        Ok(())
    }

    /// Emit a temporary value possibly representing the current value of an lvalue
    pub fn emit_r_value(
        &mut self,
        expr: &ast::Expr,
        ctx: &mut FunctionCtx,
    ) -> Result<Option<RValue>> {
        match &expr.kind {
            ast::ExprKind::Struct(_, _) | ast::ExprKind::Array(_) | ast::ExprKind::Tuple(_) => {
                todo!("struct, array, and tuple not implemented yet!");
            }
            ast::ExprKind::Literal(lit) => Ok(Some(match &lit.kind {
                ast::LiteralKind::Float(v) => {
                    RValue::Value(ir::Value::const_f64(*v as f64), ir::NativeType::F64)
                }
                ast::LiteralKind::Int(v) => {
                    RValue::Value(ir::Value::const_i64(*v as i64), ir::NativeType::I64)
                }
                ast::LiteralKind::String(_) => bail!("string literals not implemented yet!"),
            })),
            ast::ExprKind::Path(path) => {
                if path.segments.len() == 1 {
                    let ident = &path.segments.first().unwrap().ident;
                    let v_path = vec![VariablePathSegment::Label(ident.text.clone())];
                    if let Ok(var) = ctx.get_variable_at_path(&v_path) {
                        return Ok(Some(var));
                    }
                }
                bail!("path {} not found!", path);
            }
            ast::ExprKind::Ref(expr) => todo!(),
            ast::ExprKind::Deref(expr) => todo!(),
            ast::ExprKind::Field(expr, path_segment) => todo!(),
            ast::ExprKind::Call(callable, args) => {
                let callable = self.emit_callable(&callable, ctx)?;
                let args: Vec<RValue> =
                    args.iter()
                        .try_fold(Vec::new(), |mut args, arg| -> Result<Vec<_>> {
                            args.push(
                                self.emit_r_value(arg, ctx)?
                                    .ok_or_else(|| anyhow!("Expression does not return value!"))?,
                            );
                            Ok(args)
                        })?;

                match callable {
                    Callable::IntrinsicFn(callback) => (*callback)(args, ctx),
                }
            }
        }
    }

    /// Emit a value coresponding to the location of a value or variable
    fn emit_l_value(&mut self, expr: &ast::Expr, ctx: &mut FunctionCtx) -> Result<LValue> {
        match &expr.kind {
            ast::ExprKind::Struct(_, _)
            | ast::ExprKind::Literal(_)
            | ast::ExprKind::Array(_)
            | ast::ExprKind::Tuple(_) => {
                bail!("Temporary value does not have a memory address")
            }
            ast::ExprKind::Path(path) => {
                if path.segments.len() == 1 {
                    let ident = &path.segments.first().unwrap().ident;
                    let v_path = vec![VariablePathSegment::Label(ident.text.clone())];
                    if ctx.get_variable_at_path(&v_path).is_ok() {
                        return Ok(LValue::Variable(v_path));
                    }
                }
                bail!("path {} not found!", path);
            }
            ast::ExprKind::Ref(expr) => todo!(),
            ast::ExprKind::Deref(expr) => todo!(),
            ast::ExprKind::Field(expr, path_segment) => todo!(),
            ast::ExprKind::Call(expr, exprs) => todo!(),
        }
    }

    fn emit_callable(&mut self, expr: &ast::Expr, _ctx: &mut FunctionCtx) -> Result<Callable> {
        match &expr.kind {
            ast::ExprKind::Path(path) => {
                // Technically this should emit intrinsic functions too
                if path.segments.len() == 1 {
                    let path = path.segments.first().unwrap();
                    match path.ident.text.as_str() {
                        "add" => {
                            return Ok(Callable::IntrinsicFn(Rc::new(|args, ctx| {
                                Ok(match args.as_slice() {
                                    [RValue::Value(left, ir::NativeType::I32), RValue::Value(right, ir::NativeType::I32)] => {
                                        Some(RValue::Value(
                                            ctx.emit_op(ir::Op::IAdd {
                                                left: *left,
                                                right: *right,
                                            })?,
                                            ir::NativeType::I32,
                                        ))
                                    }
                                    [RValue::Value(left, ir::NativeType::I64), RValue::Value(right, ir::NativeType::I64)] => {
                                        Some(RValue::Value(
                                            ctx.emit_op(ir::Op::IAdd {
                                                left: *left,
                                                right: *right,
                                            })?,
                                            ir::NativeType::I64,
                                        ))
                                    }
                                    _ => bail!("Invalid args"),
                                })
                            })))
                        }
                        "cmp_gt" => {
                            return Ok(Callable::IntrinsicFn(Rc::new(|args, ctx| {
                                Ok(match args.as_slice() {
                                    [RValue::Value(left, ir::NativeType::I32), RValue::Value(right, ir::NativeType::I32)] => {
                                        Some(RValue::Value(
                                            ctx.emit_op(ir::Op::CmpGt {
                                                left: *left,
                                                right: *right,
                                            })?,
                                            ir::NativeType::Bool,
                                        ))
                                    }
                                    [RValue::Value(left, ir::NativeType::I64), RValue::Value(right, ir::NativeType::I64)] => {
                                        Some(RValue::Value(
                                            ctx.emit_op(ir::Op::CmpGt {
                                                left: *left,
                                                right: *right,
                                            })?,
                                            ir::NativeType::Bool,
                                        ))
                                    }
                                    _ => bail!("Invalid args"),
                                })
                            })))
                        }
                        name => bail!("Unknown intrinsic {}", name),
                    }
                }
                bail!("Unable to find {}", path)
            }
            _ => bail!("expression is not callable!"),
        }
    }

    fn store(&mut self, lvalue: &LValue, rvalue: RValue, ctx: &mut FunctionCtx) -> Result<()> {
        match &lvalue {
            LValue::Ptr(ptr, _ty) => match rvalue {
                RValue::Value(src, _ty) => {
                    ctx.emit_op(ir::Op::Store { src, ptr: *ptr })?;
                }
                RValue::Struct(SSAStruct { fields, def_id: _ }) => {
                    for (i, (_, value)) in fields.iter().enumerate() {
                        let dest = self.get_struct_field(&lvalue, i, ctx)?;
                        self.store(&dest, value.as_ref().clone(), ctx)?;
                    }
                }
                RValue::Enum(SSAEnum {
                    variants,
                    index,
                    index_ty: _,
                    def_id,
                }) => {
                    let enum_def = ctx
                        .mod_ctx
                        .module
                        .get_enum(def_id)
                        .ok_or_else(|| anyhow!("Invalid enum ID: {}", def_id))?;
                    let layout = enum_def.layout(&ctx.mod_ctx.module)?;
                    let variant_ptr = ctx.emit_op(ir::Op::IAdd {
                        left: *ptr,
                        right: ir::Value::const_u32(layout.index_offset as u32),
                    })?;
                    ctx.emit_op(ir::Op::Store {
                        src: index,
                        ptr: variant_ptr,
                    })?;

                    let branches = (0..variants.len())
                        .into_iter()
                        .map(|i| Ok((i as u128, ctx.create_block()?)))
                        .collect::<Result<Vec<_>>>()?;

                    let join_block = ctx.create_block()?;
                    ctx.end_block_jump_map(&layout.index_ty, index, join_block, branches.clone())?;
                    for ((_cond, block), inner) in branches.into_iter().zip(variants) {
                        ctx.start_block(block)?;
                        if let Some(inner) = inner {
                            self.store(&LValue::Ptr(*ptr, inner.ty()), inner, ctx)?;
                        }
                        ctx.end_block_jump(join_block);
                    }
                    ctx.start_block(join_block)?;
                }
            },
            LValue::Variable(items) => {
                ctx.set_at_path(&items, rvalue)?;
            }
        };
        Ok(())
    }

    fn load(&mut self, lvalue: &LValue, ctx: &mut FunctionCtx) -> Result<RValue> {
        match lvalue {
            LValue::Ptr(ptr, ty) => match ty {
                ir::Ty::Struct(def_id) => {
                    let def_id = *def_id as usize;
                    let struct_def = ctx
                        .mod_ctx
                        .module
                        .get_struct(def_id)
                        .cloned()
                        .ok_or_else(|| anyhow!("Invalid struct ID: {}", def_id))?;
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
                    Ok(RValue::Struct(SSAStruct { fields, def_id }))
                }
                ir::Ty::Enum(def_id) => {
                    let def_id = *def_id as usize;
                    let enum_def = ctx
                        .mod_ctx
                        .module
                        .get_enum(def_id)
                        .ok_or_else(|| anyhow!("Invalid enum ID: {}", def_id))?;
                    let variant_tys = enum_def
                        .variants
                        .iter()
                        .map(|v| v.ty.clone())
                        .collect::<Vec<_>>();
                    let layout = enum_def.layout(&ctx.mod_ctx.module)?;
                    let index_ty = layout.index_ty.clone();
                    let variant_ptr = ctx.emit_op(ir::Op::IAdd {
                        left: *ptr,
                        right: ir::Value::const_u32(layout.index_offset as u32),
                    })?;
                    let index = ctx.emit_op(ir::Op::Load {
                        ptr: variant_ptr,
                        ty: index_ty.clone(),
                    })?;

                    let branches = (0..variant_tys.len())
                        .into_iter()
                        .map(|i| Ok((i as u128, ctx.create_block()?)))
                        .collect::<Result<Vec<_>>>()?;

                    let join_block = ctx.create_block()?;
                    ctx.end_block_jump_map(&layout.index_ty, index, join_block, branches.clone())?;
                    let mut variants = Vec::new();
                    for ((_cond, block), variant) in branches.into_iter().zip(variant_tys) {
                        ctx.start_block(block)?;
                        variants.push(match &variant {
                            Some(inner) => Some(self.load(&LValue::Ptr(*ptr, inner.clone()), ctx)?),
                            None => None,
                        });
                        ctx.end_block_jump(join_block);
                    }
                    ctx.start_block(join_block)?;

                    Ok(RValue::Enum(SSAEnum {
                        variants,
                        index,
                        def_id,
                        index_ty,
                    }))
                }
                ir::Ty::Native(nt) => {
                    let value = ctx.emit_op(ir::Op::Load {
                        ptr: *ptr,
                        ty: nt.clone(),
                    })?;
                    Ok(RValue::Value(value, nt.clone()))
                }
            },
            LValue::Variable(path) => ctx.get_variable_at_path(path),
        }
    }

    fn get_struct_field(
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
                    .get_struct(*ty as usize)
                    .ok_or_else(|| anyhow!("invalid struct"))?;
                let layout = s_ty.layout(&ctx.mod_ctx.module)?;
                let offset = layout
                    .byte_offsets
                    .get(index)
                    .ok_or_else(|| anyhow!("Struct did not have {} field", index))?;
                let field_ty = s_ty.members[index].ty.clone();
                Ok(LValue::Ptr(
                    ctx.emit_op(ir::Op::IAdd {
                        left: *ptr,
                        right: ir::Value::const_u32(*offset as u32),
                    })?,
                    field_ty,
                ))
            }
            LValue::Variable(items) => match ctx.get_variable_at_path(&items)? {
                RValue::Struct(SSAStruct { fields, def_id: _ }) => {
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
                    let emitted_ptr_ty = ir::Ty::Native(ir::NativeType::Ptr(
                        true,
                        Some(Box::new(ir::Ty::Native(ir::NativeType::U32))),
                    ));
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

                    let struct_index = ctx.module.structs.len();
                    ctx.module.structs.push(ir::Struct {
                        id: None,
                        members: emitted_fields,
                        packed: false,
                    });

                    ir::Ty::Struct(struct_index as u64)
                }
            }),
            ast::TyKind::Ptr(is_mut, inner_ty) => {
                let emitted_inner_ty = match inner_ty {
                    Some(inner) => Some(Box::new(self.emit_ty(inner.as_ref(), ctx)?)),
                    None => None,
                };
                Ok(ir::Ty::Native(ir::NativeType::Ptr(
                    *is_mut,
                    emitted_inner_ty,
                )))
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
                    return Ok(ir::Ty::Struct(existing_index as u64));
                }

                let struct_index = ctx.module.structs.len();
                ctx.module.structs.push(ir::Struct {
                    id: None,
                    members: emitted_fields,
                    packed: false,
                });

                Ok(ir::Ty::Struct(struct_index as u64))
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
                    return Ok(ir::Ty::Struct(existing_index as u64));
                }

                let struct_index = ctx.module.structs.len();
                ctx.module.structs.push(ir::Struct {
                    id: None,
                    members: emitted_fields,
                    packed: false,
                });

                Ok(ir::Ty::Struct(struct_index as u64))
            }
            ast::TyKind::Slice(_is_mut, inner_ty) => {
                let inner_ty = match inner_ty {
                    Some(inner_ty) => Some(Box::new(self.emit_ty(&inner_ty, ctx)?)),
                    None => None,
                };

                let emitted_ptr_ty = ir::Ty::Native(ir::NativeType::Ptr(true, inner_ty));
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
                    return Ok(ir::Ty::Struct(existing_index as u64));
                }

                let struct_index = ctx.module.structs.len();
                ctx.module.structs.push(ir::Struct {
                    id: None,
                    members: emitted_fields,
                    packed: false,
                });

                Ok(ir::Ty::Struct(struct_index as u64))
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
                                return Ok(ir::Ty::Struct(existing_index as u64));
                            }

                            let struct_index = ctx.module.structs.len();
                            ctx.module.structs.push(ir::Struct {
                                id,
                                members: emitted_fields,
                                packed: false,
                            });

                            Ok(ir::Ty::Struct(struct_index as u64))
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
                                return Ok(ir::Ty::Enum(existing_index as u64));
                            }

                            let enum_index = ctx.module.structs.len() as i32;
                            ctx.module.enums.push(ir::Enum {
                                id: None,
                                variants: emitted_varaints,
                                packed: false,
                            });

                            Ok(ir::Ty::Enum(enum_index as u64))
                        }
                        _ => bail!("Path does not reference a valid type"),
                    }
                } else {
                    bail!("Type not found: {}", path.span.source.to_string());
                }
            }
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
    pub current_block: Option<usize>,
    /// Map ranges of block phi nodes, to the variables that they grab values from
    /// if a variable represents a flattened structs or enum, it's ID will be placed in multiple
    /// indices. This will be contigous and repeated the variable's "width" times
    pub block_phi_mappings: Vec<Vec<usize>>,
    /// Stack of a map from string labels in our document to actual variable ids.
    /// This allows scopes to share the same label for multiple values in ways that
    /// make sense in the document.
    pub scopes: Vec<HashMap<String, usize>>,
    /// SSA LValues
    pub variables: Vec<RValue>,
}

impl<'mc> FunctionCtx<'mc> {
    pub fn start_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn end_scope(&mut self) {
        self.scopes.pop().expect("Tried to pop nonexistant stack!");
    }

    pub fn define_variable(&mut self, label: impl Into<String>, value: RValue) {
        let new_id = self.variables.len();
        self.variables.push(value);

        let scope = self
            .scopes
            .last_mut()
            .expect("Tried to define variable with empty stack!");

        scope.insert(label.into(), new_id);
    }

    pub fn record_variables(&self) -> Vec<HashMap<String, usize>> {
        self.scopes.clone()
    }

    pub fn set_variables(
        &mut self,
        mut variables: Vec<HashMap<String, usize>>,
    ) -> Vec<HashMap<String, usize>> {
        std::mem::swap(&mut variables, &mut self.scopes);
        variables
    }
    pub fn emit_op(&mut self, op: ir::Op) -> Result<ir::Value> {
        let current_block = self.current_block.expect("no current block!");
        let b = self.function.blocks.get_mut(current_block).unwrap();
        let op_id = b.ops.len() as u32;
        b.ops.push(op);
        Ok(ir::Value::BlockOp {
            block: current_block as u32,
            op: op_id,
        })
    }

    pub fn end_block(&mut self, term: ir::TermOp) {
        match &term {
            ir::TermOp::Jump { block } => self.map_jump_vars(*block),
            ir::TermOp::JumpIf {
                cond: _,
                then,
                else_,
            } => {
                self.map_jump_vars(*then);
                self.map_jump_vars(*else_);
            }
            ir::TermOp::JumpMap {
                cond: _,
                default,
                branches,
            } => {
                for (_, b) in branches {
                    self.map_jump_vars(*b);
                }
                self.map_jump_vars(*default);
            }
            ir::TermOp::Ret(_) => {}
        }
        let current_block = self.current_block.expect("no current block!");
        self.function.blocks[current_block].terminator = term;
        /*
        let btrace = std::backtrace::Backtrace::capture();
        println!(
            "Closing block {}, with path {}",
            self.current_block.unwrap(),
            btrace
        );*/
        self.current_block = None;
    }

    pub fn end_block_ret(&mut self, value: Option<ir::Value>) {
        self.end_block(ir::TermOp::Ret(value));
    }

    pub fn end_block_jump(&mut self, block: u32) {
        // TODO handle SSA values and whatnot
        self.end_block(ir::TermOp::Jump {
            block: block as u32,
        });
    }

    pub fn end_block_jump_if(&mut self, cond: ir::Value, then: u32, else_: u32) {
        // TODO handle SSA values and whatnot
        self.end_block(ir::TermOp::JumpIf { cond, then, else_ });
    }

    /// Set the current block to terminate in a jump instruction that jumps based on an int value
    /// first branch is the default jump target
    pub fn end_block_jump_map(
        &mut self,
        cond_ty: &ir::NativeType,
        cond: ir::Value,
        default: u32,
        branches: Vec<(u128, u32)>,
    ) -> Result<()> {
        if !cond_ty.is_int() {
            bail!("condition must be an integer ty");
        }
        self.end_block(ir::TermOp::JumpMap {
            cond,
            branches,
            default,
        });
        Ok(())
    }

    /// Takes the current stack frame and unions it with a block's current set of phi nodes
    fn map_jump_vars(&mut self, block: u32) {
        assert!(block < self.function.blocks.len() as u32);
        let current_block = self.current_block.unwrap();
        let vars = self
            .scopes
            .iter()
            .map(|s| s.iter())
            .flatten()
            .map(|(_, id)| *id)
            .collect::<Vec<_>>();
        for var in vars {
            let v = &self.variables[var];
            let flattened = v.flattened();
            let phis = self.block_phis_for_var(block, var, &v.flattened_tys());
            for (phi, value) in phis.iter_mut().zip(flattened) {
                // TODO typecheck to make sure type stays the same
                phi.variants.push(ir::PhiValue {
                    block: current_block as u32,
                    value,
                })
            }
        }
    }

    /// Get a mutable list to the phi inputs for a certain variable on a block.
    /// If there is not currently a phi node for this variable, this will init a new one
    /// width is the size of the flattened type that the variable represents
    fn block_phis_for_var(
        &mut self,
        block: u32,
        var_id: usize,
        types: &[ir::NativeType],
    ) -> &mut [ir::PhiNode] {
        let block = block as usize;
        assert!(block < self.function.blocks.len());
        assert!(block < self.block_phi_mappings.len());
        let width = types.len();
        let mappings = &mut self.block_phi_mappings[block];
        let phi_indices = match mappings.iter().enumerate().find_map(|(index, var)| {
            if *var == var_id {
                Some(index)
            } else {
                None
            }
        }) {
            Some(index) => index..(index + width),
            None => {
                let new_index = mappings.len();
                assert_eq!(self.function.blocks[block].phi_nodes.len(), new_index);
                for ty in types {
                    self.function.blocks[block].phi_nodes.push(ir::PhiNode {
                        ty: ty.clone(),
                        variants: Vec::new(),
                    });
                    mappings.push(var_id);
                }
                new_index..(new_index + width)
            }
        };
        &mut self.function.blocks[block].phi_nodes[phi_indices]
    }

    /// Get a list of IR values representing a flattened representation of a variable
    fn block_phi_values_for_var(
        block_phi_mappings: &Vec<Vec<usize>>,
        block: usize,
        var_id: usize,
        width: usize,
    ) -> Option<Vec<ir::Value>> {
        assert!(block < block_phi_mappings.len());
        block_phi_mappings[block]
            .iter()
            .enumerate()
            .find_map(|(index, var)| if *var == var_id { Some(index) } else { None })
            .map(|index| {
                (index..(index + width))
                    .into_iter()
                    .map(|i| ir::Value::PhiArg {
                        block: block as u32,
                        arg: i as u32,
                    })
                    .collect()
            })
    }

    pub fn create_block(&mut self) -> Result<u32> {
        Ok(self.create_blocks(1)?.start)
    }

    pub fn create_blocks(&mut self, count: usize) -> Result<Range<u32>> {
        let start = self.function.blocks.len() as u32;
        for _ in 0..count {
            self.function.blocks.push(ir::Block {
                phi_nodes: Vec::new(),
                ops: Vec::new(),
                terminator: ir::TermOp::Ret(None),
            });
            self.block_phi_mappings.push(Vec::new());
        }
        Ok(start..(start + count as u32))
    }

    /// Call this when we first start emitting to a certain block to set up variables and whatnot
    pub fn start_block(&mut self, block: u32) -> Result<()> {
        let block = block as usize;
        assert!(block < self.function.blocks.len());
        assert!(
            self.current_block.is_none(),
            "Previous block must be be finished before starting a new one!"
        );
        self.current_block = Some(block);

        // Search the current scope of exposed values for any values that are mapped to a phi node
        // of this block, and if so, make our labels point to that.
        for scope in self.scopes.iter_mut() {
            for (_, var_id) in scope.iter() {
                let var = &self.variables[*var_id];
                let ty = var.ty();
                let width = var.width();
                if let Some(phis) =
                    Self::block_phi_values_for_var(&self.block_phi_mappings, block, *var_id, width)
                {
                    self.variables[*var_id] =
                        RValue::from_flattened(&phis, &ty, &self.mod_ctx.module)?;
                }
            }
        }
        Ok(())
    }

    fn get_variable_at_path(&mut self, path: &Vec<VariablePathSegment>) -> Result<RValue> {
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
                    current_root = Some(
                        self.variables[self
                            .scopes
                            .iter()
                            .rev()
                            .find_map(|scope| scope.get(label).cloned())
                            .ok_or_else(|| {
                                anyhow!("could not resolve variable path {:?}", path)
                            })?]
                        .clone(),
                    );
                }
                VariablePathSegment::StructField(field) => {
                    let Some(root) = current_root else {
                        bail!(
                            "could not resolve variable path {:?}, must have label as root",
                            path
                        );
                    };
                    match root {
                        RValue::Struct(SSAStruct { fields, def_id: _ }) => {
                            let new_root = fields.iter().find_map(|(label, value)| {
                                if label == field {
                                    Some(value.as_ref().clone())
                                } else {
                                    None
                                }
                            });
                            current_root = new_root;
                        }
                        RValue::Enum(_) => bail!("Expected struct but found enum"),
                        RValue::Value(_, _) => bail!("Expected struct but found value"),
                    }
                }
            }
        }
        Ok(current_root.unwrap())
    }

    fn set_at_path(&mut self, path: &Vec<VariablePathSegment>, variable: RValue) -> Result<()> {
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
                    dest = Some(
                        &mut self.variables[self
                            .scopes
                            .iter()
                            .rev()
                            .find_map(|scope| scope.get(label).cloned())
                            .ok_or_else(|| {
                                anyhow!("could not resolve variable path {:?}", path)
                            })?],
                    );
                }
                VariablePathSegment::StructField(field) => {
                    let Some(root) = dest else {
                        bail!(
                            "could not resolve variable path {:?}, must have label as root",
                            path
                        );
                    };
                    match root {
                        RValue::Struct(SSAStruct { fields, def_id: _ }) => {
                            let new_root = fields.iter_mut().find_map(|(label, value)| {
                                if label == field {
                                    Some(value.as_mut())
                                } else {
                                    None
                                }
                            });
                            dest = new_root;
                        }
                        RValue::Enum(_) => bail!("Expected struct but found enum"),
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
pub enum RValue {
    Value(ir::Value, ir::NativeType),
    Struct(SSAStruct),
    /// enum data contains SSA values for all variants. All variants must be defined even if only some variants
    /// contain actual values. Non-defined variants will contain default values. Load instructions
    /// will be emitted as if all variants were defined simaltaniously.
    /// Then eliminated by dead code evaluation.
    /// TODO also have a optimization pass that moves values only used once into the blocks
    /// (enum data, variant index, enum def)
    Enum(SSAEnum),
}

impl RValue {
    pub fn width(&self) -> usize {
        match self {
            RValue::Value(_, _) => 1,
            RValue::Struct(ssa_struct) => ssa_struct.width(),
            RValue::Enum(ssa_enum) => ssa_enum.width(),
        }
    }

    pub fn ty(&self) -> ir::Ty {
        match self {
            RValue::Value(_, ty) => ir::Ty::Native(ty.clone()),
            RValue::Struct(v) => ir::Ty::Struct(v.def_id as u64),
            RValue::Enum(v) => ir::Ty::Enum(v.def_id as u64),
        }
    }

    pub fn flattened(&self) -> Vec<ir::Value> {
        let mut values = Vec::new();
        self.flatten_internal(&mut values);
        values
    }

    fn flatten_internal(&self, values: &mut Vec<ir::Value>) {
        match self {
            RValue::Value(val, _) => values.push(*val),
            RValue::Struct(ssa_struct) => ssa_struct.flatten_internal(values),
            RValue::Enum(ssa_enum) => ssa_enum.flatten_internal(values),
        }
    }

    pub fn flattened_tys(&self) -> Vec<ir::NativeType> {
        let mut values = Vec::new();
        self.flatten_tys_internal(&mut values);
        values
    }

    fn flatten_tys_internal(&self, values: &mut Vec<ir::NativeType>) {
        match self {
            RValue::Value(_, nt) => values.push(nt.clone()),
            RValue::Struct(ssa_struct) => ssa_struct.flatten_tys_internal(values),
            RValue::Enum(ssa_enum) => ssa_enum.flatten_tys_internal(values),
        }
    }

    pub fn ty_width(ty: &ir::Ty, module: &ir::Module) -> Result<usize> {
        match ty {
            ir::Ty::Native(_) => Ok(1usize),
            ir::Ty::Struct(id) => {
                let struct_def = module
                    .get_struct(*id as usize)
                    .ok_or_else(|| anyhow!("Invalid struct ID: {}", id))?;
                let mut count = 0;
                for member in struct_def.members.iter() {
                    count += Self::ty_width(&member.ty, module)?;
                }
                Ok(count)
            }
            ir::Ty::Enum(id) => {
                let enum_def = module
                    .get_enum(*id as usize)
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

    pub fn from_flattened(value: &[ir::Value], ty: &ir::Ty, module: &ir::Module) -> Result<RValue> {
        match ty {
            ir::Ty::Native(native_ty) => {
                if value.len() != 1 {
                    bail!(
                        "Expected a single value for native type {:?}, but got {} values",
                        native_ty,
                        value.len()
                    );
                }
                Ok(RValue::Value(value[0], native_ty.clone()))
            }
            ir::Ty::Struct(def_id) => {
                let struct_def = module
                    .get_struct(*def_id as usize)
                    .ok_or_else(|| anyhow!("Invalid struct ID: {}", def_id))?;
                let mut fields = Vec::new();
                let mut offset = 0;

                for member in &struct_def.members {
                    let member_width = Self::ty_width(&member.ty, module)?;
                    let member_values = &value[offset..(offset + member_width)];
                    let member_rvalue = Self::from_flattened(member_values, &member.ty, module)?;
                    fields.push((
                        member.id.clone().unwrap_or_default(),
                        Box::new(member_rvalue),
                    ));
                    offset += member_width;
                }

                Ok(RValue::Struct(SSAStruct {
                    fields,
                    def_id: *def_id as usize,
                }))
            }
            ir::Ty::Enum(def_id) => {
                let enum_def = module
                    .get_enum(*def_id as usize)
                    .ok_or_else(|| anyhow!("Invalid enum ID: {}", def_id))?;
                let index_ty = enum_def.layout(module)?.index_ty;

                if value.len() < 1 {
                    bail!(
                        "Expected at least one value for enum {:?}, but got {} values",
                        def_id,
                        value.len()
                    );
                }

                let index = value[0];
                let mut variants = Vec::new();
                let mut offset = 1;

                for variant in &enum_def.variants {
                    let variant_width = variant
                        .ty
                        .as_ref()
                        .map_or(Ok(0), |ty| Self::ty_width(ty, module))?;
                    let variant_values = &value[offset..(offset + variant_width)];
                    let variant_rvalue = if let Some(ty) = &variant.ty {
                        Some(Self::from_flattened(variant_values, ty, module)?)
                    } else {
                        None
                    };
                    variants.push(variant_rvalue);
                    offset += variant_width;
                }

                Ok(RValue::Enum(SSAEnum {
                    variants,
                    index,
                    index_ty,
                    def_id: *def_id as usize,
                }))
            }
        }
    }
}

#[derive(Clone)]
pub struct SSAStruct {
    pub fields: Vec<(String, Box<RValue>)>,
    pub def_id: usize,
}

impl SSAStruct {
    pub fn def<'m>(&self, ctx: &'m ModuleCtx) -> &'m ir::Struct {
        ctx.module
            .get_struct(self.def_id)
            .expect("invalid struct id")
    }

    pub fn width(&self) -> usize {
        self.fields.len()
    }

    fn flatten_internal(&self, values: &mut Vec<ir::Value>) {
        for (_, rvalue) in &self.fields {
            rvalue.flatten_internal(values);
        }
    }

    fn flatten_tys_internal(&self, values: &mut Vec<ir::NativeType>) {
        for (_, rvalue) in &self.fields {
            rvalue.flatten_tys_internal(values);
        }
    }
}

#[derive(Clone)]
pub struct SSAEnum {
    /// Represents a 1-1 mapping with all variants of the enum, with non values representing
    /// variants with no data
    variants: Vec<Option<RValue>>,
    index: ir::Value,
    index_ty: ir::NativeType,
    def_id: usize,
}

impl SSAEnum {
    pub fn def<'m>(&self, ctx: &'m ModuleCtx) -> &'m ir::Enum {
        ctx.module.get_enum(self.def_id).expect("invalid struct id")
    }

    pub fn width(&self) -> usize {
        self.variants.len() + 1 // +1 for the index
    }

    fn flatten_internal(&self, values: &mut Vec<ir::Value>) {
        values.push(self.index);
        for variant in &self.variants {
            if let Some(rvalue) = variant {
                rvalue.flatten_internal(values);
            }
        }
    }

    fn flatten_tys_internal(&self, values: &mut Vec<ir::NativeType>) {
        values.push(self.index_ty.clone());
        for variant in &self.variants {
            if let Some(rvalue) = variant {
                rvalue.flatten_tys_internal(values);
            }
        }
    }
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
    IntrinsicFn(Rc<dyn Fn(Vec<RValue>, &mut FunctionCtx) -> Result<Option<RValue>>>),
}
