use anyhow::{anyhow, bail, Result};
use brane_core::ir::{self, Module};
use branec_emitter::{self as emt, Diagnostic, DiagnosticEmitter};
use branec_parser::ast;
use branec_source::{SourceManager, Span, Uri};
use chumsky::input::Input;
use chumsky::span::Span as _;
use chumsky::Parser;
use std::cell::RefCell;
use std::collections::HashMap;
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

    pub fn resolve_ty(&self, ast_ty: &ast::Ty, ctx: &ModuleCtx) -> Result<Ty> {
        match &ast_ty.kind {
            ast::TyKind::Native(native_ty) => Ok(Ty::Native(*native_ty)),
            ast::TyKind::Ptr(is_mut, ty) => {
                let resolved_ty = match ty {
                    Some(t) => Some(Box::new(self.resolve_ty(t.as_ref(), ctx)?)),
                    None => None,
                };
                Ok(Ty::Ptr(*is_mut, resolved_ty))
            }
            ast::TyKind::Slice(is_mut, ty) => {
                let resolved_ty = match ty {
                    Some(t) => Some(Box::new(self.resolve_ty(t.as_ref(), ctx)?)),
                    None => None,
                };
                Ok(Ty::Slice(*is_mut, resolved_ty))
            }
            ast::TyKind::Tuple(elements) => {
                let resolved_elements = elements
                    .into_iter()
                    .map(|t| self.resolve_ty(t.as_ref(), ctx))
                    .collect::<Result<Vec<_>>>()?
                    .into_iter()
                    .map(|t| Box::new(t))
                    .collect();
                Ok(Ty::Tuple(resolved_elements))
            }
            ast::TyKind::Struct(fields) => {
                let resolved_fields = fields
                    .into_iter()
                    .map(|(l, t)| {
                        let resolved_t = self.resolve_ty(t.as_ref(), ctx)?;
                        Ok((l.text.clone(), Box::new(resolved_t)))
                    })
                    .collect::<Result<Vec<_>>>()?;
                Ok(Ty::Struct(None, resolved_fields))
            }
            ast::TyKind::Path(path) => {
                if let Some(def) = self.resolve_def(&path, ctx)? {
                    match &def.kind {
                        ast::DefKind::Struct(s) => Ok(Ty::Struct(
                            Some(s.ident.text.clone()),
                            s.fields
                                .iter()
                                .map(|(ident, ty)| {
                                    let resolved_ty = self.resolve_ty(ty, ctx)?;
                                    Ok((ident.text.clone(), Box::new(resolved_ty)))
                                })
                                .collect::<Result<Vec<_>>>()?,
                        )),
                        ast::DefKind::Enum(e) => Ok(Ty::Enum(
                            e.ident.text.clone(),
                            e.variants
                                .iter()
                                .map(|(ident, data)| {
                                    Ok((
                                        ident.text.clone(),
                                        match data {
                                            Some(ty) => Some(Box::new(self.resolve_ty(ty, ctx)?)),
                                            None => None,
                                        },
                                    ))
                                })
                                .collect::<Result<Vec<_>>>()?,
                        )),
                        _ => bail!("Path does not reference a valid type"),
                    }
                } else {
                    bail!("Type not found: {}", path.span.source.to_string());
                }
            }
        }
    }

    pub fn emit_fn(&mut self, function: &ast::Function, module: &mut ModuleCtx) -> Result<u32> {
        let params = function
            .params
            .iter()
            .map(|(ident, ty)| Ok((ident, self.resolve_ty(ty, module)?)))
            .collect::<Result<Vec<_>>>()?;

        let return_ty = match &function.ret_ty {
            Some(ty) => Some(self.resolve_ty(ty, module)?),
            None => None,
        };

        let ir_fn = {
            let mut fn_ctx = FunctionCtx {
                function: ir::Function {
                    id: function.ident.text.clone(),
                    input: params
                        .iter()
                        .map(|(_, ty)| Ok(self.emit_ty(ty, module)?))
                        .collect::<Result<Vec<_>>>()?,
                    output: match &return_ty {
                        Some(ty) => Some(self.emit_ty(ty, module)?),
                        None => None,
                    },
                    blocks: vec![ir::Block { ops: Vec::new() }],
                },
                values: HashMap::new(),
                expr_cache: Default::default(),
                mod_ctx: module,
                expr_count: 0,
            };
            for (index, (ident, ty)) in params.iter().enumerate() {
                let arg = fn_ctx.expr(Some(ty.clone()), move |_, _| {
                    Ok(Some(ir::Value::FnArg(index as u32)))
                });
                fn_ctx.values.insert(ident.text.clone(), arg);
            }

            self.emit_block(&function.body, &mut fn_ctx)?;

            fn_ctx.function
        };

        let fn_id = module.module.functions.len();
        module.module.functions.push(ir_fn);
        Ok(fn_id as u32)
    }

    pub fn emit_block(&mut self, code_block: &ast::Block, ctx: &mut FunctionCtx) -> Result<()> {
        for stmt in code_block.statements.iter() {}

        Ok(())
    }

    pub fn emit_stmt(&mut self, stmt: &ast::Stmt, ctx: &mut FunctionCtx) -> Result<()> {
        match &stmt.kind {
            ast::StmtKind::Expression(expr) => {
                let _ = self
                    .evalutate_expression(expr, ctx)?
                    .value(&mut ctx.function, &mut ctx.expr_cache)?;
            }
            ast::StmtKind::Assign(expr, expr1) => todo!(),
            ast::StmtKind::VariableDef(ty, ident, expr) => todo!(),
            ast::StmtKind::If(expr, stmt, stmt1) => todo!(),
            ast::StmtKind::While(expr, stmt) => todo!(),
            ast::StmtKind::Match(span, expr, match_branchs) => todo!(),
            ast::StmtKind::Block(block) => todo!(),
            ast::StmtKind::Return(expr) => {
                let ret = match expr {
                    Some(expr) => {
                        let ret = self.evalutate_expression(expr, ctx)?;

                        Some((
                            self.emit_ty(
                                ret.ty()
                                    .ok_or_else(|| anyhow!("Expression does not return type!"))?,
                                ctx.mod_ctx,
                            )?,
                            ret.value(&mut ctx.function, &mut ctx.expr_cache)?
                                .ok_or_else(|| anyhow!("Expression does not return value!"))?,
                        ))
                    }
                    None => None,
                };
                ctx.function
                    .blocks
                    .last_mut()
                    .unwrap()
                    .ops
                    .push(ir::Op::Ret(ret))
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
                    if let Some(var) = ctx.values.get(&ident.text) {
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
impl ModuleCtx {
    pub fn emit_ty(&mut self, ty: &Ty) -> Result<ir::Ty> {
        match ty {
            Ty::Native(native_ty) => Ok(match native_ty {
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
                    let emitted_fields = vec![
                        ir::StructMember {
                            id: Some("ptr".to_string()),
                            ty: ir::Ty::Ptr(
                                true,
                                Some(Box::new(ir::Ty::Native(ir::NativeType::U32))),
                            ),
                        },
                        ir::StructMember {
                            id: Some("len".to_string()),
                            ty: ir::Ty::Native(ir::NativeType::U64),
                        },
                    ];

                    let struct_index = self.module.structs.len() as i32;
                    self.module.structs.push(ir::Struct {
                        id: None,
                        members: emitted_fields,
                        packed: false,
                    });

                    ir::Ty::Struct(struct_index)
                }
            }),

            Ty::Ptr(is_mut, inner_ty) => {
                let emitted_inner_ty = match inner_ty {
                    Some(inner) => Some(Box::new(self.emit_ty(inner.as_ref())?)),
                    None => None,
                };
                Ok(ir::Ty::Ptr(*is_mut, emitted_inner_ty))
            }

            Ty::Tuple(elements) => {
                let emitted_fields = elements
                    .iter()
                    .enumerate()
                    .map(|(_index, elem)| {
                        let emitted_ty = self.emit_ty(elem.as_ref())?;
                        Ok(ir::StructMember {
                            id: None,
                            ty: emitted_ty,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;

                if let Some((existing_index, _)) = self
                    .module
                    .structs
                    .iter()
                    .enumerate()
                    .find(|(_, s)| s.members == emitted_fields && s.id.is_none())
                {
                    return Ok(ir::Ty::Struct(existing_index as i32));
                }

                let struct_index = self.module.structs.len() as i32;
                self.module.structs.push(ir::Struct {
                    id: None,
                    members: emitted_fields,
                    packed: false,
                });

                Ok(ir::Ty::Struct(struct_index))
            }

            Ty::Struct(name, fields) => {
                let emitted_fields = fields
                    .iter()
                    .map(|(field_name, field_ty)| {
                        let emitted_ty = self.emit_ty(field_ty.as_ref())?;
                        Ok(ir::StructMember {
                            id: Some(field_name.clone()),
                            ty: emitted_ty,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;

                if let Some((existing_index, _)) = self
                    .module
                    .structs
                    .iter()
                    .enumerate()
                    .find(|(_, s)| s.members == emitted_fields && s.id == *name)
                {
                    return Ok(ir::Ty::Struct(existing_index as i32));
                }

                let struct_index = self.module.structs.len() as i32;
                self.module.structs.push(ir::Struct {
                    id: name.clone(),
                    members: emitted_fields,
                    packed: false,
                });

                Ok(ir::Ty::Struct(struct_index))
            }

            Ty::Enum(name, variants) => {
                let emitted_variants = variants
                    .iter()
                    .map(|(variant_name, variant_ty)| {
                        let emitted_ty = match variant_ty {
                            Some(inner_ty) => Some(self.emit_ty(inner_ty.as_ref())?),
                            None => None,
                        };
                        Ok(ir::EnumVariant {
                            id: variant_name.clone(),
                            ty: emitted_ty,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;

                if let Some((existing_index, _)) = self
                    .module
                    .enums
                    .iter()
                    .enumerate()
                    .find(|(_, e)| e.variants == emitted_variants && e.id == Some(name.clone()))
                {
                    return Ok(ir::Ty::Enum(existing_index as i32));
                }

                let enum_index = self.module.enums.len() as i32;
                self.module.enums.push(ir::Enum {
                    id: Some(name.clone()),
                    variants: emitted_variants,
                    packed: false,
                });

                Ok(ir::Ty::Enum(enum_index))
            }

            Ty::Slice(is_mut, inner_ty) => {
                let ptr_ty = Ty::Ptr(*is_mut, inner_ty.clone());
                let len_ty = Ty::Native(ast::NativeTy::U64);

                let emitted_ptr_ty = self.emit_ty(&ptr_ty)?;
                let emitted_len_ty = self.emit_ty(&len_ty)?;

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

                if let Some((existing_index, _)) = self
                    .module
                    .structs
                    .iter()
                    .enumerate()
                    .find(|(_, s)| s.members == emitted_fields && s.id.is_none())
                {
                    return Ok(ir::Ty::Struct(existing_index as i32));
                }

                let struct_index = self.module.structs.len() as i32;
                self.module.structs.push(ir::Struct {
                    id: None,
                    members: emitted_fields,
                    packed: false,
                });

                Ok(ir::Ty::Struct(struct_index))
            }
        }
    }
}

pub struct FunctionCtx<'mc> {
    pub mod_ctx: &'mc mut ModuleCtx,
    pub output: ir::Function,
    pub values: HashMap<String, Value>,
    pub value_cache: HashMap<usize, Option<ir::Value>>,
    pub value_ptr_cache: HashMap<usize, ir::Value>,
    pub blocks: Vec<BasicBlock>,
    pub expr_count: usize,
}

impl<'mc> FunctionCtx<'mc> {
    pub fn emit(&mut self, op: ir::Op, block: usize) -> Result<ir::Value> {
        let b = self
            .function
            .blocks
            .get_mut(block)
            .ok_or_else(|| anyhow!("no block at index {} when trying to emit {}", block, op))?;
        let op_id = b.ops.len() as u32;
        b.ops.push(op);
        Ok(ir::Value::BlockOp {
            block: block as u32,
            op: op_id,
        })
    }
}

pub trait IRBuilderFn: Fn(&mut FunctionCtx) -> Result<Option<ir::Value>> {}
impl<T> IRBuilderFn for T where T: Fn(&mut FunctionCtx) -> Result<Option<ir::Value>> {}

pub enum BasicInstr {
    Expr(Rc<dyn IRBuilderFn>),
    Jump(u32),
    JumpIf(Rc<dyn IRBuilderFn>, u32),
    JumpMap(Rc<dyn IRBuilderFn>, Vec<(i128, u32)>),
    Ret(Option<Rc<dyn IRBuilderFn>>),
}

pub struct BasicBlock {
    instr: Vec<BasicInstr>,
    phi_nodes: HashMap<String, PhiNode>,
}

pub struct PhiNode {
    pub inner: Rc<RefCell<PhiNodeInner>>,
}

pub struct PhiNodeInner {
    pub args: Vec<Rc<dyn IRBuilderFn>>,
    pub block: u32,
    /// This will only be valid after the phi node culling pass, if None after that, args should
    /// only be one value, and that should be used instead of a phi value.
    pub index: Option<u32>,
}

impl PhiNode {
    pub fn value(&self, ctx: &mut FunctionCtx) -> Result<ir::Value> {
        let inner = self.inner.borrow();
        if let Some(index) = &inner.index {
            Ok(ir::Value::PhiArg {
                block: inner.block,
                arg: *index,
            })
        } else {
            assert_eq!(inner.args.len(), 1);
            (*inner.args.first().unwrap())(ctx)?.ok_or_else(|| anyhow!("Phi node expected value!"))
        }
    }
}

#[derive(Clone)]
enum ValueKind {
    /// Represents intention to retrieve a pointer to the inner value
    Reference(Value),
    /// Expressions can represent types, though this does not directly emit values
    TyDef(Ty),
    /// Constant SSA value, such as a function or block argument
    Const(ir::Value, Ty),
    /// Context dependent value that will be pulled from FunctionCtx when evaluated
    Variable(String),
    /// Callable reference to a function
    FnDef(u32),
    /// Callable reference to a pipline
    PipeDef(u32),
    /// Unallocated struct or enum
    SSAObject(Vec<(Option<String>, Value)>, Ty),
    /// (EmitIR callback, return_ty)
    IntrinsicFn(
        Rc<dyn Fn(&[Value], &mut FunctionCtx) -> Result<Option<ir::Value>>>,
        Option<Ty>,
    ),
    /// Evaluated member of either a pointer to an allocated struct, or SSAObject
    StructField(Value, String),
    /// (Callable value, args)
    Call(Value, Vec<Value>),
}
use std::fmt;

/// Implementing Display for ValueKind
impl fmt::Display for ValueKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueKind::Reference(value) => write!(f, "Reference({})", value),
            ValueKind::TyDef(ty) => write!(f, "TyDef({})", ty),
            ValueKind::Const(value, ty) => write!(f, "Const({}, {})", value, ty),
            ValueKind::Variable(label) => write!(f, "Variable({})", label),
            ValueKind::FnDef(fn_id) => write!(f, "FnDef({})", fn_id),
            ValueKind::PipeDef(pipe_id) => write!(f, "PipeDef({})", pipe_id),
            ValueKind::SSAObject(items, ty) => {
                let items_str: Vec<String> = items
                    .iter()
                    .map(|(name, value)| match name {
                        Some(name) => format!("{}: {}", name, value),
                        None => format!("{}", value),
                    })
                    .collect();
                write!(f, "SSAObject({}, [{}])", ty, items_str.join(", "))
            }
            ValueKind::IntrinsicFn(_, ty) => write!(
                f,
                "IntrinsicFn({})",
                ty.as_ref()
                    .map(|ty| format!("returns {}", ty))
                    .unwrap_or_default()
            ),
            ValueKind::Call(value, args) => {
                let args_str: Vec<String> = args.iter().map(|arg| format!("{}", arg)).collect();
                write!(f, "Call({}, [{}])", value, args_str.join(", "))
            }
            ValueKind::StructField(value, _) => todo!(),
        }
    }
}

/// Compiler values are lazy-emmiters of the IR required to access that value
#[derive(Clone)]
struct Value {
    inner: Rc<ValueInner>,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Value(id: {}, kind: {})", self.inner.id, self.inner.kind)
    }
}

struct ValueInner {
    id: usize,
    /// Which block should value operations be emitted into?
    block: usize,
    kind: ValueKind,
}

impl Value {
    /// Trys to emit the default set of instructions for this expression. May or may not return a
    /// value
    pub fn ssa_value(&self, ctx: &mut FunctionCtx) -> Result<Option<ir::Value>> {
        if let Some(cached_value) = ctx.value_cache.get(&self.inner.id) {
            return Ok(cached_value.clone());
        }

        let value = match &self.inner.kind {
            ValueKind::Reference(value) => Some(value.ssa_ptr(ctx)?),
            ValueKind::Variable(label) => {
                let val = ctx.values.get(label).cloned();
                if let Some(val) = val {
                    val.ssa_value(ctx)?
                } else {
                    bail!("Variable {} not found", label);
                }
            }
            ValueKind::TyDef(_ty) => bail!("Types do not have a runtime representation!"),
            ValueKind::Const(value, _ty) => Some(value.clone()),
            ValueKind::FnDef(_) => bail!("Function values not supported yet"),
            ValueKind::PipeDef(_) => bail!("Pipeline values not supported yet"),
            ValueKind::SSAObject(_, _) => bail!("Expected a single value but found struct"),
            ValueKind::IntrinsicFn(_index, _ty) => bail!("Function values not supported yet"),
            ValueKind::Call(value, values) => match value
                .as_callable(ctx)
                .ok_or_else(|| anyhow!("{} is not callable!", value))?
            {
                CallableValue::IntrinsicFn(callback, _ty) => callback(values, ctx)?,
            },
            ValueKind::StructField(value, field) => {
                if let ValueKind::SSAObject(fields, _ty) = &value.inner.kind {
                    if let Some(value) = fields.iter().find_map(|(label, value)| {
                        label
                            .as_ref()
                            .map(|label| if label == field { Some(value) } else { None })
                            .flatten()
                    }) {
                        value.ssa_value(ctx)?
                    } else {
                        bail!("Could not find field {}!", field);
                    }
                } else {
                    let ty = value
                        .ret_ty(ctx)
                        .ok_or_else(|| anyhow!("Cannot get field of unknown type"))?;
                    if let Ty::Ptr(_mut, inner_ty) = &ty {
                        if let Some(Ty::Struct(_, fields)) = inner_ty.as_ref().map(|b| b.as_ref()) {
                            let ir::Ty::Struct(ir_struct_id) =
                                ctx.mod_ctx.emit_ty(inner_ty.as_ref().unwrap())?
                            else {
                                bail!(
                                    "Type emitted for {} was not struct",
                                    inner_ty.as_ref().unwrap()
                                );
                            };
                            let ir_struct =
                                ctx.mod_ctx.module.get_struct(ir_struct_id).ok_or_else(|| {
                                    anyhow!(
                                        "Could not emit struct for ty {}",
                                        inner_ty.as_ref().unwrap()
                                    )
                                })?;
                            let (field_index, _) = fields
                                .iter()
                                .enumerate()
                                .find_map(|(index, (label, ty))| {
                                    if label == field {
                                        Some((index, ty))
                                    } else {
                                        None
                                    }
                                })
                                .ok_or_else(|| {
                                    anyhow!("Struct {} did not have field {}", ty, field)
                                })?;
                            let layout = ir_struct.layout(&ctx.mod_ctx.module)?;
                            let field_offset = layout.byte_offsets[field_index];
                            let base_addr = value.ssa_ptr(ctx)?;

                            /// TODO this should auto-dereference this value
                            Some(ctx.emit(
                                ir::Op::IAdd {
                                    left: base_addr,
                                    right: ir::Value::ConstI32(field_offset as i32),
                                },
                                self.inner.block,
                            )?)
                        } else {
                            bail!("Tried to access field of value that was not struct or pointer to struct {}", value);
                        }
                    } else {
                        bail!("Tried to access field of value that was not struct or pointer to struct {}", value);
                    }
                }
            }
        };

        ctx.value_cache.insert(self.inner.id, value.clone());
        Ok(value)
    }

    /// Trys to emit the instructions to produce a pointer to the value of this expression
    pub fn ssa_ptr(&self, ctx: &mut FunctionCtx) -> Result<ir::Value> {
        if let Some(cached_value) = ctx.value_ptr_cache.get(&self.inner.id) {
            return Ok(cached_value.clone());
        }

        let value = match &self.inner.kind {
            ValueKind::Reference(value) => value.ssa_ptr(ctx)?,
            ValueKind::Variable(label) => {
                let val = ctx.values.get(label).cloned();
                if let Some(val) = val {
                    val.ssa_ptr(ctx)?
                } else {
                    bail!("Variable {} not found", label);
                }
            }
            ValueKind::TyDef(_ty) => bail!("Types do not have a runtime representation!"),
            ValueKind::Const(value, ty) => {
                if let Ty::Ptr(_, _) = ty {
                    value.clone()
                } else {
                    bail!("Constants do not have a memory address!");
                }
            }
            ValueKind::FnDef(_) => bail!("Function values not supported yet"),
            ValueKind::PipeDef(_) => bail!("Pipeline values not supported yet"),
            ValueKind::SSAObject(_, _) => bail!("Expected a single value but found struct"),
            ValueKind::IntrinsicFn(_index, _ty) => bail!("Function values not supported yet"),
            ValueKind::Call(value, values) => {
                match value
                    .as_callable(ctx)
                    .ok_or_else(|| anyhow!("{} is not callable!", value))?
                {
                    CallableValue::IntrinsicFn(callback, ty) => {
                        if let Some(Ty::Ptr(_, _)) = ty {
                            callback(values, ctx)?.unwrap()
                        } else {
                            bail!("Function returns temporary value");
                        }
                    }
                }
            }
            ValueKind::StructField(value, field) => {
                if let ValueKind::SSAObject(fields, _ty) = &value.inner.kind {
                    bail!("Cannot get memory address for temporary objects!");
                } else {
                    let ty = value
                        .ret_ty(ctx)
                        .ok_or_else(|| anyhow!("Cannot get field of unknown type"))?;
                    if let Ty::Ptr(_mut, inner_ty) = &ty {
                        if let Some(Ty::Struct(_, fields)) = inner_ty.as_ref().map(|b| b.as_ref()) {
                            let ir::Ty::Struct(ir_struct_id) =
                                ctx.mod_ctx.emit_ty(inner_ty.as_ref().unwrap())?
                            else {
                                bail!(
                                    "Type emitted for {} was not struct",
                                    inner_ty.as_ref().unwrap()
                                );
                            };
                            let ir_struct =
                                ctx.mod_ctx.module.get_struct(ir_struct_id).ok_or_else(|| {
                                    anyhow!(
                                        "Could not emit struct for ty {}",
                                        inner_ty.as_ref().unwrap()
                                    )
                                })?;
                            let (field_index, _) = fields
                                .iter()
                                .enumerate()
                                .find_map(|(index, (label, ty))| {
                                    if label == field {
                                        Some((index, ty))
                                    } else {
                                        None
                                    }
                                })
                                .ok_or_else(|| {
                                    anyhow!("Struct {} did not have field {}", ty, field)
                                })?;
                            let layout = ir_struct.layout(&ctx.mod_ctx.module)?;
                            let field_offset = layout.byte_offsets[field_index];
                            let base_addr = value.ssa_ptr(ctx)?;

                            ctx.emit(
                                ir::Op::IAdd {
                                    left: base_addr,
                                    right: ir::Value::ConstI32(field_offset as i32),
                                },
                                self.inner.block,
                            )?
                        } else {
                            bail!("Tried to access field of value that was not struct or pointer to struct {}", value);
                        }
                    } else {
                        bail!("Tried to access field of value that was not struct or pointer to struct {}", value);
                    }
                }
            }
        };

        ctx.value_ptr_cache.insert(self.inner.id, value.clone());
        Ok(value)
    }

    /// If this value is stored in FuncitonCtx.values, return the path to get to it
    pub fn value_path(&self, ctx: &mut FunctionCtx) -> Option<ValuePath> {
        match &self.inner.kind {
            ValueKind::Variable(label) => Some(ValuePath {
                kind: ValuePathKind::Variable,
                label: label.clone(),
            }),
            ValueKind::StructField(value, field) => {
                if let Some(mut base_path) = value.value_path(ctx) {
                    base_path.kind = ValuePathKind::Field(field.clone(), Box::new(base_path.kind));
                    Some(base_path)
                } else {
                    None
                }
            }
            ValueKind::SSAObject(fields, _) => {
                for (_, value) in fields.iter() {
                    if let Some(mut base_path) = value.value_path(ctx) {
                        return Some(base_path);
                    }
                }
                None
            }
            _ => None,
        }
    }

    pub fn ret_ty(&self, ctx: &mut FunctionCtx) -> Option<Ty> {
        match &self.inner.kind {
            ValueKind::Reference(value) => match value.ret_ty(ctx) {
                Some(inner) => Some(Ty::Ptr(true, Some(Box::new(inner)))),
                _ => None,
            },
            ValueKind::TyDef(ty) => Some(ty.clone()),
            ValueKind::Const(_value, ty) => Some(ty.clone()),
            ValueKind::Variable(label) => ctx
                .values
                .get(label)
                .cloned()
                .map(|v| v.ret_ty(ctx))
                .flatten(),
            ValueKind::FnDef(_) => None,
            ValueKind::PipeDef(_) => None,
            ValueKind::SSAObject(_items, ty) => Some(ty.clone()),
            ValueKind::IntrinsicFn(_, ty) => ty.clone(),
            ValueKind::StructField(obj, field) => obj
                .ret_ty(ctx)
                .map(|obj_ty| match obj_ty {
                    Ty::Ptr(_mut, ty) => ty
                        .map(|inner| {
                            if let Ty::Struct(_, items) = inner.as_ref() {
                                items.iter().find_map(|(label, ty)| {
                                    if label == field {
                                        Some(ty.as_ref().clone())
                                    } else {
                                        None
                                    }
                                })
                            } else {
                                None
                            }
                        })
                        .flatten(),
                    Ty::Slice(_, _) => None,
                    Ty::Tuple(_) => None,
                    Ty::Enum(_, _) => None,
                    Ty::Struct(_, items) => items.iter().find_map(|(label, ty)| {
                        if label == field {
                            Some(ty.as_ref().clone())
                        } else {
                            None
                        }
                    }),
                    Ty::Native(_) => None,
                })
                .flatten(),
            ValueKind::Call(callback, _values) => callback.ret_ty(ctx),
        }
    }

    pub fn as_callable(&self, ctx: &mut FunctionCtx<'_>) -> Option<CallableValue> {
        Some(match &self.inner.kind {
            ValueKind::IntrinsicFn(callback, ty) => {
                CallableValue::IntrinsicFn(callback.clone(), ty.clone())
            }
            _ => return None,
        })
    }

    /// Without risking emitting IR, see if this value has a memory address
    pub fn has_value_ptr(&self) -> bool {
        self.emit_value_ptr.is_some()
    }

    /// Returns the ID of the expression value.
    pub fn id(&self) -> usize {
        self.id
    }
}

pub enum CallableValue {
    IntrinsicFn(
        Rc<dyn Fn(&[Value], &mut FunctionCtx) -> Result<Option<ir::Value>>>,
        Option<Ty>,
    ),
}

#[derive(Clone)]
pub enum ValuePathKind {
    /// Represents a root variable label in the values map
    Variable,
    /// Represents access to a field of an SSAObject by name
    Field(String, Box<ValuePathKind>),
    /// Represents access to a field of an SSAObject by index, must not be the root node
    Index(usize, Box<ValuePathKind>),
}

#[derive(Clone)]
pub struct ValuePath {
    pub kind: ValuePathKind,
    pub label: String,
}

#[derive(PartialEq, Clone)]
pub enum Ty {
    Native(ast::NativeTy),
    Ptr(bool, Option<Box<Ty>>),
    Slice(bool, Option<Box<Ty>>),
    Tuple(Vec<Box<Ty>>),
    Enum(String, Vec<(String, Option<Box<Ty>>)>),
    Struct(Option<String>, Vec<(String, Box<Ty>)>),
    // TODO function, pipe
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Native(native_ty) => match native_ty {
                ast::NativeTy::I8 => write!(f, "I8"),
                ast::NativeTy::I16 => write!(f, "I16"),
                ast::NativeTy::I32 => write!(f, "I32"),
                ast::NativeTy::I64 => write!(f, "I64"),
                ast::NativeTy::U8 => write!(f, "U8"),
                ast::NativeTy::U16 => write!(f, "U16"),
                ast::NativeTy::U32 => write!(f, "U32"),
                ast::NativeTy::U64 => write!(f, "U64"),
                ast::NativeTy::F32 => write!(f, "F32"),
                ast::NativeTy::F64 => write!(f, "F64"),
                ast::NativeTy::Bool => write!(f, "Bool"),
                ast::NativeTy::Char => write!(f, "Char"),
                ast::NativeTy::String => write!(f, "String"),
            },
            Ty::Ptr(is_mut, inner_ty) => {
                let mutability = if *is_mut { "mut" } else { "const" };
                match inner_ty {
                    Some(inner_ty) => write!(f, "Ptr({} {})", mutability, inner_ty),
                    None => write!(f, "Ptr({} None)", mutability),
                }
            }
            Ty::Slice(is_mut, inner_ty) => {
                let mutability = if *is_mut { "mut" } else { "const" };
                match inner_ty {
                    Some(inner_ty) => write!(f, "Slice({} {})", mutability, inner_ty),
                    None => write!(f, "Slice({} None)", mutability),
                }
            }
            Ty::Tuple(elements) => {
                let elements_str: Vec<String> =
                    elements.iter().map(|elem| format!("{}", elem)).collect();
                write!(f, "Tuple([{}])", elements_str.join(", "))
            }
            Ty::Enum(name, variants) => {
                let variants_str: Vec<String> = variants
                    .iter()
                    .map(|(variant_name, variant_ty)| match variant_ty {
                        Some(ty) => format!("{}({})", variant_name, ty),
                        None => variant_name.clone(),
                    })
                    .collect();
                write!(f, "Enum({}, [{}])", name, variants_str.join(", "))
            }
            Ty::Struct(name, fields) => {
                let fields_str: Vec<String> = fields
                    .iter()
                    .map(|(field_name, field_ty)| format!("{}: {}", field_name, field_ty))
                    .collect();
                match name {
                    Some(name) => write!(f, "Struct({}, [{}])", name, fields_str.join(", ")),
                    None => write!(f, "Struct([{}])", fields_str.join(", ")),
                }
            }
        }
    }
}
