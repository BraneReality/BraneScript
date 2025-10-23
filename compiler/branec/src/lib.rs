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

mod intrinsics;
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
            let parser = branec_parser::parser();
            match parser.parse(source).into_result() {
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
            template_count: 0,
            type_instances: HashMap::new(),
            fn_instances: HashMap::new(),
        };

        let mut namespace = IdentScope::default();
        intrinsics::populate(&mut namespace, &mut module_ctx.template_count);

        let mut registration_error = false;
        for def in &defs {
            match &def.kind {
                ast::DefKind::Link(ident) => todo!("Have not implemented module links yet"),
                ast::DefKind::Use(path) => todo!("Have not implemented use statements yet"),
                ast::DefKind::Struct(object) => {
                    let id = module_ctx.template_count;
                    module_ctx.template_count += 1;

                    if let Some(existing) = namespace.defs.insert(
                        object.ident.text.clone(),
                        Template::Struct(object.clone(), id),
                    ) {
                        let message = emt::error("duplicate identifiers", &self.sources)
                            .err_at(object.ident.span.clone(), "already defined");
                        if let Some(ext_ident) = existing.first_ident() {
                            message
                                .warning_at(ext_ident.span.clone(), "was defined here")
                                .emit(&self.emitter)?;
                        } else {
                            message.emit(&self.emitter)?;
                        }
                        registration_error = true;
                    }
                }
                ast::DefKind::Enum(object) => {
                    let id = module_ctx.template_count;
                    module_ctx.template_count += 1;

                    if let Some(existing) = namespace.defs.insert(
                        object.ident.text.clone(),
                        Template::Enum(object.clone(), id),
                    ) {
                        let message = emt::error("duplicate identifiers", &self.sources)
                            .err_at(object.ident.span.clone(), "already defined");
                        if let Some(ext_ident) = existing.first_ident() {
                            message
                                .info_at(ext_ident.span.clone(), "was defined here")
                                .emit(&self.emitter)?;
                        } else {
                            message.emit(&self.emitter)?;
                        }
                        registration_error = true;
                    }
                }
                ast::DefKind::Function(_) | ast::DefKind::Pipeline(_) => {
                    // Do we want to pre-define these?
                }
                ast::DefKind::Namespace(ident, defs) => {
                    todo!("Haven't implemented namespaces yet")
                }
            }
        }

        if registration_error {
            bail!("Failed to register names");
        }

        for def in defs {
            match &def.kind {
                ast::DefKind::Function(function) => {
                    let _ = self.emit_fn(function, &namespace, &mut module_ctx)?;
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

    pub fn resolve_template<'a>(
        &self,
        path: &[ast::PathSegment],
        namespace: &'a IdentScope,
    ) -> Result<&'a Template> {
        let Some(segment) = path.first() else {
            unreachable!("unexpected end of path");
        };

        // TODO check if segment has template args, they're not allowed for modules
        if let Some(sub_namespace) = namespace.sub_scopes.get(&segment.ident.text) {
            if path.len() < 2 {
                bail!("Expected defintion not namespace");
            }
            return self.resolve_template(&path[1..path.len()], sub_namespace);
        }

        if let Some(temp) = namespace.defs.get(&segment.ident.text) {
            return Ok(temp);
        }
        let error = "Failed to resolve path";
        emt::error(&error, &self.sources)
            .err_at(segment.ident.span.clone(), "Not found")
            .emit(&self.emitter)?;
        Err(anyhow!(error))
    }

    pub fn emit_fn(
        &mut self,
        function: &ast::Function,
        namespace: &IdentScope,
        module: &mut ModuleCtx,
    ) -> Result<u32> {
        let params = function
            .params
            .iter()
            .map(|(ident, ty)| Ok((ident, self.emit_ty(ty, namespace, module)?)))
            .collect::<Result<Vec<_>>>()?;

        let return_ty = match &function.ret_ty {
            Some(ty) => Some(self.emit_ty(ty, namespace, module)?),
            None => None,
        };

        let ir_fn = {
            let mut fn_ctx = FunctionCtx {
                function: ir::Function {
                    id: function.ident.text.clone(),
                    sig: ir::FnSig {
                        param_tys: params.iter().map(|(_, v)| v.clone()).collect(),
                        ret_ty: return_ty.clone(),
                    },
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
            for (arg_index, (ident, ty)) in params.iter().enumerate() {
                fn_ctx.define_variable(
                    ident.text.clone(),
                    if let ir::Ty::Native(nt) = ty {
                        RValue::Value(ir::Value::FnArg(arg_index as u32), nt.clone())
                    } else {
                        let values = fn_ctx
                            .mod_ctx
                            .module
                            .flatten(&ty)?
                            .into_iter()
                            .enumerate()
                            .map(|(index, _ty)| ir::Value::ObjFnArg(arg_index as u32, index as u32))
                            .collect::<Vec<_>>();
                        RValue::from_flattened(&values, &ty, &fn_ctx.mod_ctx.module)?
                    },
                );
            }

            self.emit_block(&function.body, namespace, &mut fn_ctx)?;

            fn_ctx.end_scope();

            fn_ctx.function
        };

        let fn_id = module.module.functions.len();
        module.module.functions.push(ir_fn);
        Ok(fn_id as u32)
    }

    pub fn emit_block(
        &mut self,
        code_block: &ast::Block,
        namespace: &IdentScope,
        ctx: &mut FunctionCtx,
    ) -> Result<()> {
        for stmt in code_block.statements.iter() {
            self.emit_stmt(stmt, namespace, ctx)?;
        }

        Ok(())
    }

    pub fn emit_stmt(
        &mut self,
        stmt: &ast::Stmt,
        namespace: &IdentScope,
        ctx: &mut FunctionCtx,
    ) -> Result<()> {
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
                self.emit_r_value(expr, namespace, ctx)?;
            }
            ast::StmtKind::Assign(dest, src) => {
                let d = self.emit_l_value(dest, namespace, ctx)?;
                let src = self
                    .emit_r_value(src, namespace, ctx)?
                    .ok_or_else(|| anyhow!("Expression does not return value!"))?;
                self.store(&d, src, ctx)?;
            }
            ast::StmtKind::VariableDef(ty, ident, expr) => {
                let src = self
                    .emit_r_value(expr, namespace, ctx)?
                    .ok_or_else(|| anyhow!("Expression does not return value!"))?;
                let ty = self.emit_ty(ty, namespace, &mut ctx.mod_ctx)?;
                if ty != src.ty() {
                    bail!("expected type {} but found {}", ty, src.ty());
                }
                ctx.define_variable(ident.text.clone(), src);
            }
            ast::StmtKind::If(cond, body, else_stmt) => {
                let cond = self
                    .emit_r_value(cond, namespace, ctx)?
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
                self.emit_stmt(&body, namespace, ctx)?;
                if ctx.current_block.is_some() {
                    ctx.end_block_jump(join_block);
                }
                if let (Some(else_block), Some(else_stmt)) = (else_block, else_stmt) {
                    ctx.start_block(else_block)?;
                    self.emit_stmt(&else_stmt, namespace, ctx)?;
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
                    .emit_r_value(cond, namespace, ctx)?
                    .ok_or_else(|| anyhow!("Expression does not return value!"))?;
                let RValue::Value(cond, ir::NativeType::Bool) = cond else {
                    bail!("While condition must be a bool, but was {}", cond.ty());
                };
                ctx.end_block_jump_if(cond, body_block, finally_block);
                ctx.start_block(body_block)?;
                self.emit_stmt(body, namespace, ctx)?;
                if ctx.current_block.is_some() {
                    ctx.end_block_jump(cond_block);
                }
                ctx.start_block(finally_block)?;
            }
            ast::StmtKind::Match(_span, expr, match_branches) => {
                let cond = self
                    .emit_r_value(expr, namespace, ctx)?
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

                            self.emit_stmt(&match_branch.body, namespace, ctx)?;
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

                        // TODO handle the case for an actual default block (we need new grammar
                        // for that)
                        //
                        if let (Some(index_ty), Some(index)) = (&layout.index_ty, index) {
                            let join_block = ctx.create_block()?;
                            let default_block = ctx.create_block()?;
                            let variables = ctx.record_variables();
                            ctx.end_block_jump_map(
                                index_ty,
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

                                self.emit_stmt(&match_branch.body, namespace, ctx)?;

                                ctx.end_scope();
                                ctx.end_block_jump(join_block);
                            }
                            ctx.start_block(join_block)?;
                        } else if branches.len() == 1 {
                            // No index type, no branching
                            assert!(layout.index_ty.is_none() && index.is_none());
                            let match_branch = &match_branches[0];
                            self.emit_stmt(&match_branch.body, namespace, ctx)?
                        }
                    }
                    RValue::Struct(_) => bail!("Cannot match on structs!"),
                }
            }
            ast::StmtKind::Block(block) => {
                ctx.start_scope();
                for stmt in block.statements.iter() {
                    self.emit_stmt(stmt, namespace, ctx)?;
                }
                ctx.end_scope();
            }
            ast::StmtKind::Return(expr) => {
                let body = match expr {
                    Some(expr) => Some({
                        let value = self
                            .emit_r_value(expr, namespace, ctx)?
                            .ok_or_else(|| anyhow!("Expression does not return value!"))?;
                        let value_ty = value.ty();
                        if Some(&value_ty.clone()) != ctx.function.sig.ret_ty.as_ref() {
                            emt::error("incorrect return type", &self.sources)
                                .err_at(
                                    expr.span.clone(),
                                    &format!(
                                        "Expected {} but found {}",
                                        if let Some(output) = &ctx.function.sig.ret_ty {
                                            ctx.mod_ctx.module.ty_string(output)
                                        } else {
                                            "nothing".into()
                                        },
                                        ctx.mod_ctx.module.ty_string(&value_ty),
                                    ),
                                )
                                .emit(&self.emitter)?;
                            bail!("incorrect return type");
                        }
                        value.flattened()
                    }),
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
        namespace: &IdentScope,
        ctx: &mut FunctionCtx,
    ) -> Result<Option<RValue>> {
        match &expr.kind {
            ast::ExprKind::Struct(path, members) => {
                todo!("struct constructor grammar not implemented yet");
            }
            ast::ExprKind::Array(_) | ast::ExprKind::Tuple(_) => {
                todo!("array, and tuple not implemented yet!");
            }
            ast::ExprKind::Literal(lit) => Ok(Some(match &lit.kind {
                ast::LiteralKind::String(_) => bail!("string literals not implemented yet!"),
                ast::LiteralKind::I8(v) => {
                    RValue::Value(ir::Value::const_i8(*v), ir::NativeType::I8)
                }
                ast::LiteralKind::I16(v) => {
                    RValue::Value(ir::Value::const_i16(*v), ir::NativeType::I16)
                }
                ast::LiteralKind::I32(v) => {
                    RValue::Value(ir::Value::const_i32(*v), ir::NativeType::I32)
                }
                ast::LiteralKind::I64(v) => {
                    RValue::Value(ir::Value::const_i64(*v), ir::NativeType::I64)
                }
                ast::LiteralKind::U8(v) => {
                    RValue::Value(ir::Value::const_u8(*v), ir::NativeType::U8)
                }
                ast::LiteralKind::U16(v) => {
                    RValue::Value(ir::Value::const_u16(*v), ir::NativeType::U16)
                }
                ast::LiteralKind::U32(v) => {
                    RValue::Value(ir::Value::const_u32(*v), ir::NativeType::U32)
                }
                ast::LiteralKind::U64(v) => {
                    RValue::Value(ir::Value::const_u64(*v), ir::NativeType::U64)
                }
                ast::LiteralKind::F32(v) => {
                    RValue::Value(ir::Value::const_f32(*v), ir::NativeType::F32)
                }
                ast::LiteralKind::F64(v) => {
                    RValue::Value(ir::Value::const_f64(*v), ir::NativeType::F64)
                }
                ast::LiteralKind::Bool(v) => {
                    RValue::Value(ir::Value::const_bool(*v), ir::NativeType::Bool)
                }
                ast::LiteralKind::Char(v) => {
                    RValue::Value(ir::Value::const_u32(*v as u32), ir::NativeType::U32)
                }
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
            ast::ExprKind::Deref(expr) => {
                let ptr = self.emit_r_value(expr, namespace, ctx)?;
                let Some(RValue::Value(ptr, ir::NativeType::Ptr(_, inner_ty))) = ptr else {
                    bail!("Cannot dereference non-pointer value");
                };
                let Some(inner_ty) = inner_ty else {
                    bail!("Cannot dereference pointer with unknown type, cast it to a known type first");
                };
                let ptr = LValue::Ptr(ptr, *inner_ty);
                self.load(&ptr, ctx).map(|v| Some(v))
            }
            ast::ExprKind::Field(expr, path_segment) => todo!(),
            ast::ExprKind::Call(callable, args) => {
                let args: Vec<RValue> =
                    args.iter()
                        .try_fold(Vec::new(), |mut args, arg| -> Result<Vec<_>> {
                            args.push(
                                self.emit_r_value(arg, namespace, ctx)?
                                    .ok_or_else(|| anyhow!("Expression does not return value!"))?,
                            );
                            Ok(args)
                        })?;
                let arg_tys = args.iter().map(|rv| rv.ty()).collect::<Vec<_>>();
                let callable = self.emit_callable(&callable, &arg_tys, namespace, ctx)?;

                match callable {
                    Callable::IntrinsicFn(template_args, callback) => {
                        (*callback)(&template_args, &args, ctx)
                    }
                }
            }
        }
    }

    /// Emit a value coresponding to the location of a value or variable
    fn emit_l_value(
        &mut self,
        expr: &ast::Expr,
        namespace: &IdentScope,
        ctx: &mut FunctionCtx,
    ) -> Result<LValue> {
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
            ast::ExprKind::Deref(expr) => {
                let ptr = self.emit_r_value(expr, namespace, ctx)?;
                let Some(RValue::Value(ptr, ir::NativeType::Ptr(_, inner_ty))) = ptr else {
                    bail!("Cannot dereference non-pointer value");
                };
                let Some(inner_ty) = inner_ty else {
                    bail!("Cannot dereference pointer with unknown type, cast it to a known type first");
                };
                Ok(LValue::Ptr(ptr, *inner_ty))
            }
            ast::ExprKind::Field(expr, path_segment) => todo!(),
            ast::ExprKind::Call(expr, exprs) => todo!(),
        }
    }

    fn emit_callable(
        &mut self,
        expr: &ast::Expr,
        args: &[ir::Ty],
        namespace: &IdentScope,
        ctx: &mut FunctionCtx,
    ) -> Result<Callable> {
        match &expr.kind {
            ast::ExprKind::Path(path) => {
                let Template::Fn(fn_temp) = self.resolve_template(&path.segments, namespace)?
                else {
                    bail!("{} was not function", path);
                };

                let temp_args = path
                    .segments
                    .last()
                    .unwrap()
                    .template_args
                    .iter()
                    .map(|ast::TemplateArg(ty)| self.emit_ty(ty, namespace, &mut ctx.mod_ctx))
                    .collect::<Result<Vec<_>>>()?;

                for o in fn_temp.iter() {
                    if o.valid_for(&temp_args, args) {
                        match &o.kind {
                            FnOverrideKind::Ast(function) => todo!(),
                            FnOverrideKind::Intrinsic(callback) => {
                                return Ok(Callable::IntrinsicFn(temp_args, callback.clone()));
                            }
                        }
                    }
                }

                let error_msg = format!(
                    "No override for {}({})",
                    path,
                    args.iter()
                        .map(|a| a.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                emt::error(&error_msg, &self.sources)
                    .err_at(path.span.clone(), "here")
                    .emit(&self.emitter)?;
                bail!(error_msg);
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

                    if let Some(index) = index {
                        ctx.emit_op(ir::Op::Store {
                            src: index,
                            ptr: *ptr,
                        })?;
                    }

                    if let (Some(index_ty), Some(index)) = (&layout.index_ty, index) {
                        let mut variant_ptr = None;
                        let branches = (0..variants.len())
                            .into_iter()
                            .map(|i| Ok((i as u128, ctx.create_block()?)))
                            .collect::<Result<Vec<_>>>()?;

                        let join_block = ctx.create_block()?;
                        ctx.end_block_jump_map(index_ty, index, join_block, branches.clone())?;
                        for ((_cond, block), inner) in branches.into_iter().zip(variants) {
                            ctx.start_block(block)?;
                            if let Some(inner) = inner {
                                if variant_ptr.is_none() {
                                    variant_ptr = Some(ctx.emit_op(ir::Op::Binary {
                                        op: ir::BinaryOp::IAdd,
                                        left: *ptr,
                                        right: ir::Value::const_u32(layout.union_offset as u32),
                                    })?);
                                }
                                self.store(
                                    &LValue::Ptr(variant_ptr.unwrap(), inner.ty()),
                                    inner,
                                    ctx,
                                )?;
                            }
                            ctx.end_block_jump(join_block);
                        }
                        ctx.start_block(join_block)?;
                    } else if variants.len() == 1 {
                        if let Some(inner) = &variants[0] {
                            let variant_ptr = Some(ctx.emit_op(ir::Op::Binary {
                                op: ir::BinaryOp::IAdd,
                                left: *ptr,
                                right: ir::Value::const_u32(layout.union_offset as u32),
                            })?);
                            self.store(
                                &LValue::Ptr(variant_ptr.unwrap(), inner.ty()),
                                inner.clone(),
                                ctx,
                            )?;
                        }
                    }
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
                    let def_id = *def_id;
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
                    let def_id = *def_id;
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

                    let index = match &layout.index_ty {
                        Some(index_ty) => Some(ctx.emit_op(ir::Op::Load {
                            ptr: *ptr,
                            ty: index_ty.clone(),
                        })?),
                        None => None,
                    };

                    let mut variants = Vec::new();

                    if let (Some(index_ty), Some(index)) = (&layout.index_ty, &index) {
                        let mut variant_ptr = None;
                        let branches = (0..variant_tys.len())
                            .into_iter()
                            .map(|i| Ok((i as u128, ctx.create_block()?)))
                            .collect::<Result<Vec<_>>>()?;

                        let join_block = ctx.create_block()?;
                        ctx.end_block_jump_map(index_ty, *index, join_block, branches.clone())?;
                        for ((_cond, block), variant) in branches.into_iter().zip(variant_tys) {
                            ctx.start_block(block)?;
                            variants.push(match &variant {
                                Some(inner) => {
                                    if variant_ptr.is_none() {
                                        variant_ptr = Some(ctx.emit_op(ir::Op::Binary {
                                            op: ir::BinaryOp::IAdd,
                                            left: *ptr,
                                            right: ir::Value::const_u32(layout.union_offset as u32),
                                        })?);
                                    }
                                    Some(self.load(
                                        &LValue::Ptr(variant_ptr.unwrap(), inner.clone()),
                                        ctx,
                                    )?)
                                }
                                None => None,
                            });
                            ctx.end_block_jump(join_block);
                        }
                        ctx.start_block(join_block)?;
                    } else if variant_tys.len() == 1 {
                        variants.push(match &variant_tys[0] {
                            Some(inner) => {
                                let variant_ptr = ctx.emit_op(ir::Op::Binary {
                                    op: ir::BinaryOp::IAdd,
                                    left: *ptr,
                                    right: ir::Value::const_u32(layout.union_offset as u32),
                                })?;
                                Some(self.load(&LValue::Ptr(variant_ptr, inner.clone()), ctx)?)
                            }
                            None => None,
                        });
                    }

                    Ok(RValue::Enum(SSAEnum {
                        variants,
                        index,
                        def_id,
                        index_ty: layout.index_ty,
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
                    .get_struct(*ty)
                    .ok_or_else(|| anyhow!("invalid struct"))?;
                let layout = s_ty.layout(&ctx.mod_ctx.module)?;
                let offset = layout
                    .byte_offsets
                    .get(index)
                    .ok_or_else(|| anyhow!("Struct did not have {} field", index))?;
                let field_ty = s_ty.members[index].ty.clone();
                Ok(LValue::Ptr(
                    ctx.emit_op(ir::Op::Binary {
                        op: ir::BinaryOp::IAdd,
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

    pub fn emit_ty(&self, ty: &Ty, namespace: &IdentScope, ctx: &mut ModuleCtx) -> Result<ir::Ty> {
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
                        id: Some("slice".into()),
                        members: emitted_fields,
                        packed: false,
                    });

                    ir::Ty::Struct(struct_index as u64)
                }
            }),
            ast::TyKind::Ptr(is_mut, inner_ty) => {
                let emitted_inner_ty = match inner_ty {
                    Some(inner) => Some(Box::new(self.emit_ty(inner.as_ref(), namespace, ctx)?)),
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
                        let emitted_ty = self.emit_ty(elem.as_ref(), namespace, ctx)?;
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
                        let emitted_ty = self.emit_ty(field_ty.as_ref(), namespace, ctx)?;
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
                    Some(inner_ty) => Some(self.emit_ty(&inner_ty, namespace, ctx)?),
                    None => None,
                };

                SSAStruct::slice_ty(inner_ty, &mut ctx.module).map(|ty| ir::Ty::Struct(ty))
            }
            ast::TyKind::Path(path) => {
                let template = self.resolve_template(&path.segments, namespace)?;
                match &template {
                    Template::Struct(s, t_id) => {
                        if let Some(memoized) = ctx.type_instances.get(t_id) {
                            return Ok(memoized.clone());
                        }

                        let emitted_fields = s
                            .fields
                            .iter()
                            .map(|(field_name, field_ty)| {
                                let emitted_ty = self.emit_ty(field_ty, namespace, ctx)?;
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
                    Template::Enum(e, t_id) => {
                        if let Some(memoized) = ctx.type_instances.get(t_id) {
                            println!("returning memoized enum {}", memoized);
                            return Ok(memoized.clone());
                        }
                        let emitted_varaints = e
                            .variants
                            .iter()
                            .map(|(field_name, field_ty)| {
                                Ok(ir::EnumVariant {
                                    id: field_name.text.clone(),
                                    ty: match field_ty {
                                        Some(ty) => Some(self.emit_ty(ty, namespace, ctx)?),
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
                            println!("found existing enum: {}", existing_index);
                            return Ok(ir::Ty::Enum(existing_index as u64));
                        }

                        let enum_index = ctx.module.enums.len() as i32;
                        ctx.module.enums.push(ir::Enum {
                            id,
                            variants: emitted_varaints,
                            packed: false,
                        });

                        println!("emitted new enum: {}", enum_index);
                        Ok(ir::Ty::Enum(enum_index as u64))
                    }
                    _ => bail!("Path does not reference a valid type"),
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

    pub template_count: TemplateId,
    pub type_instances: HashMap<TemplateId, ir::Ty>,
    pub fn_instances: HashMap<TemplateId, ir::FnId>,
}

#[derive(Default)]
pub struct IdentScope {
    pub sub_scopes: HashMap<String, IdentScope>,
    pub defs: HashMap<String, Template>,
}

impl IdentScope {
    pub fn insert_override(
        &mut self,
        ident: impl Into<String>,
        fn_override: FnOverride,
    ) -> Result<()> {
        let ident = ident.into();
        if let Some(temp) = self.defs.get_mut(&ident) {
            let Template::Fn(overrides) = temp else {
                bail!("{} is already defined and is not a function!", ident);
            };
            // maybe pre-check for ambiquity here?
            overrides.push(fn_override);
        } else {
            self.defs.insert(ident, Template::Fn(vec![fn_override]));
        }

        Ok(())
    }
}

pub enum Template {
    Struct(ast::Struct, TemplateId),
    Enum(ast::Enum, TemplateId),
    Fn(Vec<FnOverride>),
    Pipeline(ast::Pipeline, TemplateId),
}

impl Template {
    /// Returns the ident for this template, or in the case of functions, the first function
    pub fn first_ident(&self) -> Option<&ast::Ident> {
        Some(match self {
            Template::Struct(obj, _) => &obj.ident,
            Template::Enum(obj, _) => &obj.ident,
            Template::Fn(fn_overrides) => match &fn_overrides
                .first()
                .expect("Expecting function overrides to contain at least one function")
                .kind
            {
                FnOverrideKind::Ast(function) => &function.ident,
                FnOverrideKind::Intrinsic(_) => return None,
            },
            Template::Pipeline(pipeline, _) => &pipeline.ident,
        })
    }
}

pub type TemplateId = usize;

pub enum TyPattern {
    Any,
    Exact(ir::Ty),
}

pub struct FnOverride {
    pub id: TemplateId,
    pub temp_args: Vec<TyPattern>,
    pub arg_tys: Vec<TyPattern>,
    pub kind: FnOverrideKind,
}

impl FnOverride {
    pub fn valid_for(&self, temp_args: &[ir::Ty], func_args: &[ir::Ty]) -> bool {
        if func_args.len() != self.arg_tys.len() || temp_args.len() != self.temp_args.len() {
            return false;
        }

        for (pat, ty) in self.arg_tys.iter().zip(func_args) {
            match pat {
                TyPattern::Any => {}
                TyPattern::Exact(pat_ty) => {
                    if pat_ty != ty {
                        return false;
                    }
                }
            }
        }

        for (pat, ty) in self.temp_args.iter().zip(temp_args) {
            match pat {
                TyPattern::Any => {}
                TyPattern::Exact(pat_ty) => {
                    if pat_ty != ty {
                        return false;
                    }
                }
            }
        }

        true
    }
}

pub enum FnOverrideKind {
    Ast(ast::Function),
    Intrinsic(IntrinsicFn),
}

/// Callback sig for directly generating ir.
///
/// Args (template types, args, ctx)
pub type IntrinsicFn = Rc<dyn Fn(&[ir::Ty], &[RValue], &mut FunctionCtx) -> Result<Option<RValue>>>;

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

    pub fn end_block_ret(&mut self, value: Option<Vec<ir::Value>>) {
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

    pub fn from_flattened(
        values: &[ir::Value],
        ty: &ir::Ty,
        module: &ir::Module,
    ) -> Result<RValue> {
        match ty {
            ir::Ty::Native(native_ty) => {
                if values.len() != 1 {
                    bail!(
                        "Expected a single value for native type {:?}, but got {} values",
                        native_ty,
                        values.len()
                    );
                }
                Ok(RValue::Value(values[0], native_ty.clone()))
            }
            ir::Ty::Struct(def_id) => {
                let def_id = *def_id;
                let struct_def = module
                    .get_struct(def_id)
                    .ok_or_else(|| anyhow!("Invalid struct ID: {}", def_id))?;
                let mut fields = Vec::new();
                let mut offset = 0;

                for member in &struct_def.members {
                    let member_width = Self::ty_width(&member.ty, module)?;
                    let member_values = &values[offset..(offset + member_width)];
                    let member_rvalue = Self::from_flattened(member_values, &member.ty, module)?;
                    fields.push((
                        member.id.clone().unwrap_or_default(),
                        Box::new(member_rvalue),
                    ));
                    offset += member_width;
                }

                Ok(RValue::Struct(SSAStruct { fields, def_id }))
            }
            ir::Ty::Enum(def_id) => {
                let def_id = *def_id;
                let enum_def = module
                    .get_enum(def_id)
                    .ok_or_else(|| anyhow!("Invalid enum ID: {}", def_id))?;
                let index_ty = enum_def.layout(module)?.index_ty;

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
                        .map_or(Ok(0), |ty| Self::ty_width(ty, module))?;
                    let variant_values = &values[offset..(offset + variant_width)];
                    let variant_rvalue = if let Some(ty) = &variant.ty {
                        Some(Self::from_flattened(variant_values, ty, module)?)
                    } else {
                        None
                    };
                    variants.push(variant_rvalue);
                    offset += variant_width;
                }

                Ok(RValue::Enum(SSAEnum {
                    index_ty,
                    variants,
                    index,
                    def_id,
                }))
            }
        }
    }
}

#[derive(Clone)]
pub struct SSAStruct {
    pub fields: Vec<(String, Box<RValue>)>,
    pub def_id: ir::StructId,
}

impl SSAStruct {
    pub fn new_slice(ptr: RValue, len: RValue, module: &mut ir::Module) -> Result<SSAStruct> {
        let RValue::Value(_, ir::NativeType::Ptr(true, inner_ty)) = &ptr else {
            bail!("must construct slice from pointer value");
        };
        let RValue::Value(_, ir::NativeType::U32) = &len else {
            bail!("slice length must be u32 value");
        };

        let def_id = Self::slice_ty(inner_ty.as_ref().map(|ty| (**ty).clone()), module)?;

        Ok(SSAStruct {
            fields: vec![
                ("ptr".to_string(), Box::new(ptr)),
                ("len".to_string(), Box::new(len)),
            ],
            def_id,
        })
    }

    pub fn slice_ty(inner_ty: Option<ir::Ty>, module: &mut ir::Module) -> Result<ir::StructId> {
        let emitted_ptr_ty =
            ir::Ty::Native(ir::NativeType::Ptr(true, inner_ty.map(|ty| Box::new(ty))));
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

        if let Some((existing_index, _)) = module
            .structs
            .iter()
            .enumerate()
            .find(|(_, s)| s.members == emitted_fields && s.id.is_none())
        {
            return Ok(existing_index as ir::StructId);
        }

        let struct_index = module.structs.len();
        let new_struct = ir::Struct {
            id: None,
            members: emitted_fields,
            packed: false,
        };
        println!("emitted slice ty Struct({}), {}", struct_index, new_struct);
        module.structs.push(new_struct);

        Ok(struct_index as ir::StructId)
    }

    /// Checks if this struct represents a slice, and if so returns (ptr, len)
    pub fn as_slice(&self) -> Result<(RValue, RValue)> {
        let [(ptr_label, ptr), (len_label, len)] = self.fields.as_slice() else {
            bail!("Incorrect field count for slice");
        };
        if ptr_label != "ptr" || len_label != "len" {
            bail!("struct has incorrect fields for slice");
        };

        let (RValue::Value(_, ir::NativeType::Ptr(_, _)), RValue::Value(_, ir::NativeType::U32)) =
            (ptr.as_ref(), len.as_ref())
        else {
            bail!("struct has incorrect field types for slice");
        };

        Ok((ptr.as_ref().clone(), len.as_ref().clone()))
    }

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
    /// None in the cases of a 0 or 1 variant enum, we don't need a discriminant in those cases
    index: Option<ir::Value>,
    /// Represents a 1-1 mapping with all variants of the enum, with none values representing
    /// variants with no data
    variants: Vec<Option<RValue>>,
    index_ty: Option<ir::NativeType>,
    def_id: ir::EnumId,
}

impl SSAEnum {
    pub fn def<'m>(&self, ctx: &'m ModuleCtx) -> &'m ir::Enum {
        ctx.module.get_enum(self.def_id).expect("invalid struct id")
    }

    pub fn width(&self) -> usize {
        self.variants.len() + 1 // +1 for the index
    }

    fn flatten_internal(&self, values: &mut Vec<ir::Value>) {
        if let Some(index) = self.index {
            values.push(index);
        }
        for variant in &self.variants {
            if let Some(rvalue) = variant {
                rvalue.flatten_internal(values);
            }
        }
    }

    fn flatten_tys_internal(&self, values: &mut Vec<ir::NativeType>) {
        if let Some(index_ty) = &self.index_ty {
            values.push(index_ty.clone());
        }
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
    IntrinsicFn(Vec<ir::Ty>, IntrinsicFn),
}
