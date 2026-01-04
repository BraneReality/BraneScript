use crate::{FnOverride, FnOverrideKind, FunctionCtx, IdentTree, RValue, SSAStruct, TyPattern};
use anyhow::{bail, Result};
use brane_core::ir;
use std::rc::Rc;

pub fn intrinsic_op(
    temp_args: impl IntoIterator<Item = TyPattern>,
    arg_tys: impl IntoIterator<Item = ir::NativeType>,
    callback: impl Fn(&[ir::Ty], &[RValue], &mut FunctionCtx) -> Result<Option<RValue>> + 'static,
) -> FnOverride {
    FnOverride {
        temp_args: temp_args.into_iter().collect(),
        arg_tys: arg_tys
            .into_iter()
            .map(|ty| TyPattern::Exact(ir::Ty::Native(ty)))
            .collect(),
        kind: FnOverrideKind::Intrinsic(Rc::new(callback)),
    }
}

pub fn define_bin_op_for<'a>(
    ident: impl Into<String> + Clone,
    tys: impl IntoIterator<Item = &'a ir::NativeType>,
    ret_ty: Option<ir::NativeType>,
    op: ir::BinaryOp,
    root: &mut IdentTree,
) {
    for ty in tys {
        let ty = ty.clone();
        let ret_ty = ret_ty.clone();
        root.insert_fn(
            ident.clone(),
            intrinsic_op([], [ty.clone(), ty.clone()], move |_, args, ctx| {
                let [RValue::Value(left, l_ty), RValue::Value(right, r_ty)] = args else {
                    unreachable!("incorrect binary intrinsic sig");
                };
                assert_eq!(ty, *l_ty);
                assert_eq!(ty, *r_ty);

                Ok(Some(RValue::Value(
                    ctx.emit_op(ir::Op::Binary {
                        op: op,
                        left: *left,
                        right: *right,
                    })?,
                    ret_ty
                        .as_ref()
                        .map(|ty| ty.clone())
                        .unwrap_or_else(|| ty.clone()),
                )))
            }),
        )
        .unwrap();
    }
}

pub fn define_ieq_op_for<'a>(
    tys: impl IntoIterator<Item = &'a ir::NativeType> + Clone,
    root: &mut IdentTree,
) {
    define_bin_op_for(
        "eq",
        tys.clone(),
        Some(ir::NativeType::Bool),
        ir::BinaryOp::SCmp(ir::CmpTy::Eq),
        root,
    );
    define_bin_op_for(
        "ne",
        tys.clone(),
        Some(ir::NativeType::Bool),
        ir::BinaryOp::SCmp(ir::CmpTy::Ne),
        root,
    );
}

pub fn define_scmp_op_for<'a>(
    tys: impl IntoIterator<Item = &'a ir::NativeType> + Clone,
    root: &mut IdentTree,
) {
    define_ieq_op_for(tys.clone(), root);
    define_bin_op_for(
        "ge",
        tys.clone(),
        Some(ir::NativeType::Bool),
        ir::BinaryOp::SCmp(ir::CmpTy::Ge),
        root,
    );
    define_bin_op_for(
        "gt",
        tys.clone(),
        Some(ir::NativeType::Bool),
        ir::BinaryOp::SCmp(ir::CmpTy::Gt),
        root,
    );
}

pub fn define_ucmp_op_for<'a>(
    tys: impl IntoIterator<Item = &'a ir::NativeType> + Clone,
    root: &mut IdentTree,
) {
    define_ieq_op_for(tys.clone(), root);
    define_bin_op_for(
        "ge",
        tys.clone(),
        Some(ir::NativeType::Bool),
        ir::BinaryOp::UCmp(ir::CmpTy::Ge),
        root,
    );
    define_bin_op_for(
        "gt",
        tys.clone(),
        Some(ir::NativeType::Bool),
        ir::BinaryOp::UCmp(ir::CmpTy::Gt),
        root,
    );
}

pub fn define_fcmp_op_for<'a>(
    tys: impl IntoIterator<Item = &'a ir::NativeType> + Clone,
    root: &mut IdentTree,
) {
    define_bin_op_for(
        "eq",
        tys.clone(),
        Some(ir::NativeType::Bool),
        ir::BinaryOp::FCmp(ir::CmpTy::Eq),
        root,
    );
    define_bin_op_for(
        "ne",
        tys.clone(),
        Some(ir::NativeType::Bool),
        ir::BinaryOp::FCmp(ir::CmpTy::Ne),
        root,
    );
    define_bin_op_for(
        "ge",
        tys.clone(),
        Some(ir::NativeType::Bool),
        ir::BinaryOp::FCmp(ir::CmpTy::Ge),
        root,
    );
    define_bin_op_for(
        "gt",
        tys.clone(),
        Some(ir::NativeType::Bool),
        ir::BinaryOp::FCmp(ir::CmpTy::Gt),
        root,
    );
}

pub fn define_as_op_for<'a>(root: &mut IdentTree) {
    use ir::NativeType::*;
    let tys = [
        U8,
        I8,
        U16,
        I16,
        U32,
        I32,
        U64,
        I64,
        F64,
        F32,
        Ptr(false, None),
    ];

    for from_ty in tys {
        root.insert_fn(
            "as",
            intrinsic_op([TyPattern::Any], [from_ty], |temp_tys, args, ctx| {
                let [to_ty] = temp_tys else {
                    unreachable!("incorrect as<>() template args");
                };
                let ir::Ty::Native(to_ty) = to_ty else {
                    bail!(
                        "as<T>() is only implemented for native types, not for {}",
                        to_ty
                    )
                };

                let [RValue::Value(from, from_ty)] = args else {
                    unreachable!("incorrect as<>() args");
                };

                Ok(Some(RValue::Value(
                    ctx.emit_op(ir::Op::Cast {
                        src: from.clone(),
                        from_ty: from_ty.clone(),
                        to_ty: to_ty.clone(),
                    })?,
                    to_ty.clone(),
                )))
            }),
        )
        .unwrap();
    }
}

pub fn populate(root: &mut IdentTree) {
    use ir::NativeType::*;
    let ints = [U8, I8, U16, I16, U32, I32, U64, I64];
    let unsigned = [U8, U16, U32, U64];
    let signed = [I8, I16, I32, I64];
    let floats = [F64, F32];

    define_bin_op_for("add", &ints, None, ir::BinaryOp::IAdd, root);
    define_bin_op_for("add", &floats, None, ir::BinaryOp::FAdd, root);

    define_bin_op_for("sub", &ints, None, ir::BinaryOp::ISub, root);
    define_bin_op_for("sub", &floats, None, ir::BinaryOp::FSub, root);

    define_bin_op_for("mul", &ints, None, ir::BinaryOp::IMul, root);
    define_bin_op_for("mul", &floats, None, ir::BinaryOp::FMul, root);

    define_bin_op_for("div", &signed, None, ir::BinaryOp::SDiv, root);
    define_bin_op_for("div", &unsigned, None, ir::BinaryOp::UDiv, root);
    define_bin_op_for("div", &floats, None, ir::BinaryOp::FDiv, root);

    define_bin_op_for("rem", &signed, None, ir::BinaryOp::SRem, root);
    define_bin_op_for("rem", &unsigned, None, ir::BinaryOp::URem, root);

    define_ieq_op_for(&[Bool], root);
    define_scmp_op_for(&signed, root);
    define_ucmp_op_for(&unsigned, root);
    define_fcmp_op_for(&floats, root);

    // TODO bitwise operators

    root.insert_fn(
        "slice",
        intrinsic_op([TyPattern::Any], [ir::NativeType::U32], |tys, args, ctx| {
            let [ty] = tys else {
                unreachable!("incorrect slice template args");
            };
            let [RValue::Value(len, ir::NativeType::U32)] = args else {
                unreachable!("incorrect slice args");
            };
            let (stride, _) = ctx.mod_ctx.module.size_align(ty)?;

            let bytes = ctx.emit_op(ir::Op::Binary {
                op: ir::BinaryOp::IMul,
                left: *len,
                right: ir::Value::const_u32(stride as u32),
            })?;

            let data = ctx.emit_op(ir::Op::Unary {
                op: ir::UnaryOp::Alloc,
                value: bytes,
            })?;

            Ok(Some(RValue::Struct(SSAStruct::new_slice(
                RValue::Value(data, ir::NativeType::Ptr(true, Some(Box::new(ty.clone())))),
                RValue::Value(*len, ir::NativeType::U32),
                &mut ctx.mod_ctx.module,
            )?)))
        }),
    )
    .unwrap();

    root.insert_fn("index", {
        FnOverride {
            temp_args: vec![],
            arg_tys: vec![
                TyPattern::Any,
                TyPattern::Exact(ir::Ty::Native(ir::NativeType::U32)),
            ],
            kind: FnOverrideKind::Intrinsic(Rc::new(|_, args, ctx| {
                let [RValue::Struct(slice), RValue::Value(index, ir::NativeType::U32)] = args
                else {
                    bail!("incorrect index args");
                };
                let (
                    RValue::Value(ptr, ir::NativeType::Ptr(_, Some(inner_ty))),
                    RValue::Value(_len, _),
                ) = slice.as_slice()?
                else {
                    unreachable!();
                };
                let (stride, _) = ctx.mod_ctx.module.size_align(inner_ty.as_ref())?;

                let offset = ctx.emit_op(ir::Op::Binary {
                    op: ir::BinaryOp::IMul,
                    left: *index,
                    right: ir::Value::const_u32(stride as u32),
                })?;

                // Add a range check here?

                let ptr = ctx.emit_op(ir::Op::Binary {
                    op: ir::BinaryOp::IAdd,
                    left: ptr,
                    right: offset,
                })?;

                Ok(Some(RValue::Value(
                    ptr,
                    ir::NativeType::Ptr(true, Some(inner_ty)),
                )))
            })),
        }
    })
    .unwrap();
}
