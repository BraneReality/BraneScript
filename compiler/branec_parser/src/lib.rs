use branec_ast::*;
use branec_source::Span;
use branec_symbols::Symbol;
use branec_tokens::tree::{self, Delimiter, Group, TokenTree};
use chumsky::{input::BorrowInput, label::LabelError, prelude::*, util::Maybe};

pub fn tree_group<'src>(
    delim: Option<Delimiter>,
) -> impl Clone + Parser<'src, &'src Group, &'src Group, extra::Err<Rich<'src, TokenTree, Span>>> {
    let label = match &delim {
        Some(delim) => delim.as_str(),
        None => "group",
    };

    any_ref()
        .labelled(label)
        .try_map(move |tree: &TokenTree, span| {
            if let TokenTree::Group(group) = tree {
                if group.delim == *delim.as_ref().unwrap_or(&group.delim) {
                    return Ok(group);
                }
            }
            Err(LabelError::<'src, &'src Group, _>::expected_found(
                [label],
                Some(Maybe::Ref(tree)),
                span,
            ))
        })
}

/// Match a token tree identifier
///
/// If ident is None then this will parse all identifiers, if it's None, then we only parse
/// identifiers that match that string
pub fn ident<'src, T>(
    symbol: Option<Symbol>,
) -> impl Clone + Parser<'src, T, Ident, extra::Err<Rich<'src, TokenTree, Span>>>
where
    T: BorrowInput<'src, Token = TokenTree, Span = Span>,
{
    let label = match &symbol {
        Some(label) => label.as_str(),
        None => "identifier",
    };

    any_ref()
        .labelled(label)
        .try_map(move |tree: &TokenTree, span| {
            if let TokenTree::Ident(ident) = tree {
                if ident.sym == *symbol.as_ref().unwrap_or(&ident.sym) {
                    return Ok(ident.clone());
                }
            }
            Err(LabelError::<'src, &'src Group, _>::expected_found(
                [label],
                Some(Maybe::Ref(tree)),
                span,
            ))
        })
}

pub fn punct<'src, T>(
    ch: char,
) -> impl Clone + Parser<'src, T, (), extra::Err<Rich<'src, TokenTree, Span>>>
where
    T: BorrowInput<'src, Token = TokenTree, Span = Span>,
{
    any_ref()
        .try_map(move |tree: &TokenTree, span| {
            if let TokenTree::Punct(punct) = tree {
                if punct.ch == ch {
                    return Ok(());
                }
            }
            Err(LabelError::<'src, &'src Group, _>::expected_found(
                [ch.to_string()],
                Some(Maybe::Ref(tree)),
                span,
            ))
        })
        .labelled(ch.to_string())
}

pub fn punct2<'src, T>(
    ops: &'static str,
) -> impl Clone + Parser<'src, T, (), extra::Err<Rich<'src, TokenTree, Span>>>
where
    T: BorrowInput<'src, Token = TokenTree, Span = Span>,
{
    let chars = ops.chars();

    let parser = chars
        .enumerate()
        .fold(
            None,
            move |parser: Option<Boxed<'src, '_, T, Result<(), &'src TokenTree>, _>>,
                  (index, ch)| {
                let must_join = index != ops.len() - 1;
                if let Some(parser) = parser {
                    Some(Parser::boxed(parser.then(any_ref()).map(
                        move |(r, tree)| match r {
                            Ok(_) => {
                                if let TokenTree::Punct(punct) = tree {
                                    if ch != punct.ch || must_join && !punct.joined {
                                        Err(tree)
                                    } else {
                                        Ok(())
                                    }
                                } else {
                                    Err(tree)
                                }
                            }
                            Err(err) => Err(err),
                        },
                    )))
                } else {
                    Some(Parser::boxed(any_ref().map(move |tree| {
                        if let TokenTree::Punct(punct) = tree {
                            if ch != punct.ch || must_join && !punct.joined {
                                Err(tree)
                            } else {
                                Ok(())
                            }
                        } else {
                            Err(tree)
                        }
                    })))
                }
            },
        )
        .expect("no input chars!");

    parser
        .try_map(move |result, span| {
            if let Err(tree) = result {
                Err(LabelError::<'src, &'src Group, _>::expected_found(
                    [ops],
                    Some(Maybe::Ref(tree)),
                    span,
                ))
            } else {
                Ok(())
            }
        })
        .labelled(ops)
}

pub fn ast_builder<'src>()
-> impl Parser<'src, &'src tree::Group, Ast, extra::Err<Rich<'src, TokenTree, Span>>> {
    let mut ty = Recursive::declare();
    let mut block = Recursive::declare();

    let mut expr = Recursive::declare();

    let path = ident(None)
        .map(|ident| PathSegment { ident })
        .separated_by(punct2("::").labelled("path"))
        .at_least(1)
        .collect()
        .map_with(|segments, e| Path {
            segments,
            span: e.span(),
        });

    let mut_sym = Symbol::intern("mut");
    let const_sym = Symbol::intern("const");

    let mut_ty = choice((ident(Some(mut_sym)), ident(Some(const_sym))))
        .or_not()
        .then(ty.clone())
        .map(move |(mutability, ty)| MutTy {
            ty,
            mutability: match mutability {
                Some(ident) => {
                    if ident.sym == mut_sym {
                        Mutability::Mut
                    } else if ident.sym == const_sym {
                        Mutability::Not
                    } else {
                        unreachable!()
                    }
                }
                None => Mutability::Not,
            },
        });

    let param = ident(None)
        .then_ignore(punct(':'))
        .then(ty.clone())
        .map_with(|(ident, ty), e| Param {
            ident,
            ty,
            span: e.span(),
        });
    let params = param.separated_by(punct(',')).allow_trailing().collect();

    let struct_ty = params.clone().nested_in(tree_group(Some(Delimiter::Brace)));

    let self_t_sym = Symbol::intern("Self");
    ty.define(
        choice((
            struct_ty.map(|params| TyKind::Struct(params)),
            ident(Some(self_t_sym)).to(TyKind::ImplicitSelf),
            path.clone().map(|path| TyKind::Path(path)),
            punct('&')
                .ignore_then(mut_ty.clone())
                .map(|ty| TyKind::Ref(ty)),
            punct('*')
                .ignore_then(mut_ty.clone())
                .map(|ty| TyKind::Ptr(ty)),
        ))
        .map_with(|kind, e| {
            Box::new(Ty {
                kind,
                span: e.span(),
            })
        })
        .labelled("type"),
    );

    let lit_expr = any_ref().labelled("literal").try_map(
        move |tree: &TokenTree, span: Span| -> Result<ast::LitKind, Rich<'src, TokenTree, Span>> {
            if let TokenTree::Literal(branec_tokens::tree::Literal { lit, span }) = tree {
                let sym = lit.symbol.as_str();
                return match lit.kind {
                    branec_tokens::LitKind::Bool => Ok(ast::LitKind::Bool(match sym {
                        "true" => true,
                        "false" => false,
                        _ => unreachable!("Non true/false bool literal symbol"),
                    })),
                    branec_tokens::LitKind::Char => {
                        if sym.len() != 1 {
                            unreachable!("Symbol for char was multiple characters?");
                        }
                        Ok(ast::LitKind::Char(sym.chars().nth(0).unwrap()))
                    }
                    branec_tokens::LitKind::Int => match sym.parse() {
                        Ok(value) => match lit.suffix {
                            Some(suffix) => {
                                let int_ty = match suffix.as_str() {
                                    "i32" => ast::LitIntType::Signed(ast::IntTy::I32),
                                    "i64" => ast::LitIntType::Signed(ast::IntTy::I64),
                                    "isize" => ast::LitIntType::Signed(ast::IntTy::Isize),
                                    "u32" => ast::LitIntType::Unsigned(ast::UintTy::U32),
                                    "u64" => ast::LitIntType::Unsigned(ast::UintTy::U64),
                                    "usize" => ast::LitIntType::Unsigned(ast::UintTy::Usize),
                                    _ => unreachable!(),
                                };
                                Ok(ast::LitKind::Int(value, int_ty))
                            }
                            None => Ok(ast::LitKind::Int(value, ast::LitIntType::Unsuffixed)),
                        },
                        Err(_) => Err(LabelError::<'src, &'src Group, _>::expected_found(
                            ["number"],
                            Some(Maybe::Ref(tree)),
                            span.clone(),
                        )),
                    },
                    branec_tokens::LitKind::Float => match lit.suffix {
                        None => Ok(ast::LitKind::Float(
                            lit.symbol,
                            ast::LitFloatType::Unsuffixed,
                        )),
                        Some(suffix) => {
                            let float_ty = match suffix.as_str() {
                                "f32" => ast::LitFloatType::Suffixed(ast::FloatTy::F32),
                                "f64" => ast::LitFloatType::Suffixed(ast::FloatTy::F64),
                                _ => unreachable!(),
                            };
                            Ok(ast::LitKind::Float(lit.symbol, float_ty))
                        }
                    },
                    branec_tokens::LitKind::Str => Ok(ast::LitKind::Str(lit.symbol)),
                };
            }
            Err(LabelError::<'src, &'src Group, _>::expected_found(
                ["literal"],
                Some(Maybe::Ref(tree)),
                span,
            ))
        },
    );

    let field_expr = ident(None)
        .then_ignore(punct(':'))
        .then(expr.clone())
        .map_with(|(ident, expr), e| FieldExpr {
            span: e.span(),
            ident,
            expr,
        });

    let fields_expr = field_expr.separated_by(punct(',')).collect();

    let struct_expr = path
        .clone()
        .or_not()
        .then(
            fields_expr
                .clone()
                .nested_in(tree_group(Some(Delimiter::Brace))),
        )
        .map_with(|(path, fields), e| {
            Box::new(StructExpr {
                span: e.span(),
                path,
                fields,
            })
        });

    let atom = choice((
        lit_expr.clone().map(|lit_kind| ExprKind::Lit(lit_kind)),
        path.clone().map(|path| ExprKind::Path(None, path)),
        block.clone().map(|block| ExprKind::Block(block)),
        struct_expr.map(|expr| ExprKind::Struct(expr)),
        expr.clone()
            .nested_in(tree_group(Some(Delimiter::Parenthesis)))
            .map(|paren| ExprKind::Paren(paren)),
    ))
    .map_with(|kind, e| {
        Box::new(Expr {
            kind,
            span: e.span(),
        })
    });

    // Method calls
    // fields

    let call = atom.foldl_with(
        fields_expr
            .nested_in(tree_group(Some(Delimiter::Parenthesis)))
            .repeated(),
        |expr, fields, e| {
            Box::new(Expr {
                kind: ExprKind::Call(expr, fields),
                span: e.span(),
            })
        },
    );

    // array indexing
    // ?
    let op = choice((
        punct('-').to(UnOpKind::Neg),
        punct('*').to(UnOpKind::Deref),
        punct('!').to(UnOpKind::Not),
    ))
    .labelled("operator")
    .map_with(|op, e| (e.span(), op));

    let un_op = op.repeated().foldr_with(call.clone(), |op, expr, e| {
        Box::new(Expr {
            kind: ExprKind::Unary(op, expr),
            span: e.span(),
        })
    });
    // as

    let ops = choice((
        punct('*').to(BinOpKind::Mul),
        punct('/').to(BinOpKind::Div),
        punct('%').to(BinOpKind::Rem),
    ))
    .labelled("operator")
    .map_with(|op, e| (e.span(), op));

    let product = un_op
        .clone()
        .foldl_with(
            ops.then(un_op.clone()).repeated(),
            |left, (op, right), e| {
                Box::new(Expr {
                    span: e.span(),
                    kind: ExprKind::Binary(op, left, right),
                })
            },
        )
        .boxed();

    let ops = choice((punct('+').to(BinOpKind::Add), punct('-').to(BinOpKind::Sub)))
        .labelled("operator")
        .map_with(|op, e| (e.span(), op));

    let sum = product.clone().foldl_with(
        ops.then(product.clone()).repeated(),
        |left, (op, right), e| {
            Box::new(Expr {
                span: e.span(),
                kind: ExprKind::Binary(op, left, right),
            })
        },
    );

    let ops = choice((
        punct2("<<").to(BinOpKind::Shl),
        punct2(">>").to(BinOpKind::Shl),
    ))
    .labelled("operator")
    .map_with(|op, e| (e.span(), op));

    let shift = sum
        .clone()
        .foldl_with(ops.then(sum.clone()).repeated(), |left, (op, right), e| {
            Box::new(Expr {
                span: e.span(),
                kind: ExprKind::Binary(op, left, right),
            })
        });

    let bit_and = shift
        .clone()
        .foldl_with(
            punct('&')
                .labelled("operator")
                .to(BinOpKind::BitAnd)
                .map_with(|op, e| (e.span(), op))
                .then(shift.clone())
                .repeated(),
            |left, (op, right), e| {
                Box::new(Expr {
                    span: e.span(),
                    kind: ExprKind::Binary(op, left, right),
                })
            },
        )
        .boxed();

    let bit_xor = bit_and.clone().foldl_with(
        punct('^')
            .labelled("operator")
            .to(BinOpKind::BitXor)
            .map_with(|op, e| (e.span(), op))
            .then(bit_and.clone())
            .repeated(),
        |left, (op, right), e| {
            Box::new(Expr {
                span: e.span(),
                kind: ExprKind::Binary(op, left, right),
            })
        },
    );

    let bit_or = bit_xor.clone().foldl_with(
        punct('|')
            .labelled("operator")
            .to(BinOpKind::BitOr)
            .map_with(|op, e| (e.span(), op))
            .then(bit_and.clone())
            .repeated(),
        |left, (op, right), e| {
            Box::new(Expr {
                span: e.span(),
                kind: ExprKind::Binary(op, left, right),
            })
        },
    );

    let ops = choice((
        punct2("==").to(BinOpKind::Eq),
        punct2("!=").to(BinOpKind::Ne),
        punct('<').to(BinOpKind::Lt),
        punct('>').to(BinOpKind::Gt),
        punct2("<=").to(BinOpKind::Gt),
        punct2(">=").to(BinOpKind::Gt),
    ))
    .labelled("operator")
    .map_with(|op, e| (e.span(), op));

    let cmp = bit_or
        .clone()
        .foldl_with(
            ops.then(bit_or.clone()).repeated(),
            |left, (op, right), e| {
                Box::new(Expr {
                    span: e.span(),
                    kind: ExprKind::Binary(op, left, right),
                })
            },
        )
        .boxed();

    let logic_and = cmp.clone().foldl_with(
        punct('|')
            .labelled("operator")
            .to(BinOpKind::And)
            .map_with(|op, e| (e.span(), op))
            .then(cmp.clone())
            .repeated(),
        |left, (op, right), e| {
            Box::new(Expr {
                span: e.span(),
                kind: ExprKind::Binary(op, left, right),
            })
        },
    );

    let logic_or = logic_and.clone().foldl_with(
        punct('|')
            .labelled("operator")
            .to(BinOpKind::Or)
            .map_with(|op, e| (e.span(), op))
            .then(logic_and.clone())
            .repeated(),
        |left, (op, right), e| {
            Box::new(Expr {
                span: e.span(),
                kind: ExprKind::Binary(op, left, right),
            })
        },
    );

    expr.define(logic_or.boxed().labelled("expression"));

    let let_sym = Symbol::intern("let");
    let local = ident(Some(let_sym))
        .ignore_then(ident(None))
        .then_ignore(punct(':'))
        .then(ty.clone())
        .then_ignore(punct('='))
        .then(expr.clone())
        .map_with(|((ident, ty), expr), e| {
            Box::new(Local {
                span: e.span(),
                ident,
                ty,
                kind: LocalKind::Init(expr),
            })
        });

    let stmt = choice((
        local.map(|local| StmtKind::Let(local)),
        expr.clone()
            .then(punct(';').or_not())
            .map(|(expr, is_final)| match is_final {
                Some(_) => StmtKind::Expr(expr),
                None => StmtKind::Final(expr),
            }),
        punct(';').to(StmtKind::Empty),
    ))
    .map_with(|kind, e| {
        Box::new(Stmt {
            kind,
            span: e.span(),
        })
    });

    block.define(
        stmt.repeated()
            .collect()
            .nested_in(tree_group(Some(Delimiter::Brace)))
            .map_with(|stmts, e| {
                Box::new(Block {
                    span: e.span(),
                    stmts,
                })
            }),
    );

    let mut item_parser = Recursive::declare();
    let mut group_parser = Recursive::declare();

    let call_sig = params
        .nested_in(tree_group(Some(Delimiter::Parenthesis)))
        .then(
            punct2("->")
                .ignore_then(ty)
                .or_not()
                .map_with(|ret, e| match ret {
                    Some(ty) => FnRetTy::Ty(ty),
                    None => FnRetTy::Default(e.span()),
                }),
        )
        .map_with(|(inputs, output), e| CallSig {
            span: e.span(),
            inputs,
            output,
        })
        .labelled("call signature");

    let pipe_stage = ident(None)
        .or_not()
        .then(call_sig.clone().or_not())
        .then(block)
        .map_with(|((ident, sig), body), e| PipeStage {
            span: e.span(),
            ident,
            sig,
            body,
        })
        .labelled("pipeline stage");

    let pipe_sym = Symbol::intern("pipe");
    let pipe = ident(Some(pipe_sym))
        .ignore_then(ident(None))
        .then(call_sig)
        .then(
            pipe_stage
                .repeated()
                .collect()
                .nested_in(tree_group(Some(Delimiter::Bracket))),
        )
        .map_with(|((ident, sig), stages), e| {
            Box::new(Pipe {
                ident,
                span: e.span(),
                sig,
                stages,
            })
        })
        .labelled("pipeline");

    item_parser.define(choice((
        group_parser.clone().map(|g| Item::Group(g)),
        pipe.map(|pipe| Item::Pipe(pipe)),
    )));

    let group_sym = Symbol::intern("group");

    group_parser.define(
        ident(Some(group_sym))
            .ignore_then(ident(None))
            .then(
                item_parser
                    .clone()
                    .repeated()
                    .collect()
                    .nested_in(tree_group(Some(Delimiter::Brace))),
            )
            .map_with(|(ident, items), e| {
                Box::new(ast::Group {
                    span: e.span(),
                    ident,
                    items,
                })
            })
            .labelled("group"),
    );
    Parser::boxed(
        item_parser
            .repeated()
            .collect()
            .map_with(|items, e| Ast {
                span: e.span(),
                items,
            })
            .labelled("ast"),
    )
}
