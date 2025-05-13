use super::ast::*;
use crate::source::Span;
use crate::symbols::Symbol;
use crate::tokens::tree::{self, Delimiter, Group, TokenTree};
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

pub fn ast_builder<'src>(
) -> impl Parser<'src, &'src tree::Group, Ast, extra::Err<Rich<'src, TokenTree, Span>>> {
    let mut ty = Recursive::declare();
    let mut block = Recursive::declare();

    let mut expr = Recursive::declare();

    let path = ident(None)
        .map(|ident| PathSegment { ident })
        .separated_by(punct2("::"))
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

    let struct_ty = params
        .clone()
        .nested_in(tree_group(Some(Delimiter::Parenthesis)));

    let self_t_sym = Symbol::intern("Self");
    ty.define(
        choice((
            struct_ty.map(|params| TyKind::Struct(params)),
            ident(Some(self_t_sym)).to(TyKind::ImplicitSelf),
            path.clone().map(|path| TyKind::Path(path)),
            punct('&')
                .ignore_then(mut_ty.clone())
                .map(|ty| TyKind::Ref(ty))
                .labelled("ref type"),
            punct('*')
                .ignore_then(mut_ty.clone())
                .map(|ty| TyKind::Ptr(ty))
                .labelled("pointer type"),
        ))
        .map_with(|kind, e| {
            Box::new(Ty {
                kind,
                span: e.span(),
            })
        }),
    );

    let lit_expr = any_ref()
        .labelled("literal")
        .try_map(move |tree: &TokenTree, span| {
            if let TokenTree::Literal(lit) = tree {
                return Ok(lit);
            }
            Err(LabelError::<'src, &'src Group, _>::expected_found(
                ["literal"],
                Some(Maybe::Ref(tree)),
                span,
            ))
        });

    let field_expr = ident(None)
        .then_ignore(punct(':'))
        .then(expr.clone())
        .map_with(|(ident, expr), e| FieldExpr {
            span: e.span(),
            ident,
            expr,
        });

    let fields_expr = field_expr
        .separated_by(punct(','))
        .collect()
        .nested_in(tree_group(Some(Delimiter::Parenthesis)));

    let struct_expr =
        path.clone()
            .or_not()
            .then(fields_expr.clone())
            .map_with(|(path, fields), e| {
                Box::new(StructExpr {
                    span: e.span(),
                    path,
                    fields,
                })
            });

    let atom = choice((
        lit_expr.clone().map(|literal| ExprKind::Lit(literal.kind)),
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

    let call = atom.foldl_with(fields_expr.repeated(), |expr, fields, e| {
        Box::new(Expr {
            kind: ExprKind::Call(expr, fields),
            span: e.span(),
        })
    });

    // array indexing
    // ?
    let op = choice((
        punct('-').to(UnOpKind::Neg),
        punct('*').to(UnOpKind::Deref),
        punct('!').to(UnOpKind::Not),
    ))
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
    let mut mod_parser = Recursive::declare();

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
        mod_parser.clone().map(|m| Item::Mod(m)),
        pipe.map(|pipe| Item::Pipe(pipe)),
    )));

    let mod_sym = Symbol::intern("mod");

    mod_parser.define(
        ident(Some(mod_sym))
            .ignore_then(ident(None))
            .then(
                item_parser
                    .repeated()
                    .collect()
                    .nested_in(tree_group(Some(Delimiter::Brace))),
            )
            .map_with(|(ident, items), e| {
                Box::new(Mod {
                    span: e.span(),
                    ident,
                    items,
                })
            })
            .labelled("module"),
    );
    Parser::boxed(
        mod_parser
            .repeated()
            .collect()
            .map_with(|modules, e| Ast {
                span: e.span(),
                modules,
            })
            .labelled("ast"),
    )
}
