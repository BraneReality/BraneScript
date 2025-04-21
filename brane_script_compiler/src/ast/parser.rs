use super::ast::*;
use crate::tokens::tokens::*;
use chumsky::{input::ValueInput, prelude::*};

pub fn ast_parser<'src, T>(
) -> impl Parser<'src, T, Ast<'src>, extra::Err<Rich<'src, TokenKind<'src>, SimpleSpan>>>
where
    T: ValueInput<'src, Token = TokenKind<'src>, Span = SimpleSpan>,
{
    let mut ty = Recursive::declare();
    let mut block = Recursive::declare();

    let mut expr = Recursive::declare();

    let ident = select! { TokenKind::Ident(ident) => ident }.map_with(|ident, e| Ident {
        span: e.span(),
        text: ident,
    });

    //let path_seg = ident.separated_by(just());
    //let path = ;

    ty.define(just(TokenKind::Whitespace).to(Box::new(Ty {
        kind: todo!(),
        span: todo!(),
    })));

    expr.define(
        choice((
            block.clone().map(|block| ExprKind::Block(block)),
            expr.clone()
                .delimited_by(just(TokenKind::OpenParen), just(TokenKind::CloseParen))
                .map(|paren| ExprKind::Paren(paren)),
        ))
        .map_with(|kind, e| {
            Box::new(Expr {
                kind,
                span: e.span(),
            })
        }),
    );

    let stmt = choice((
        expr.clone()
            .then_ignore(just(TokenKind::Semi))
            .map(|expr| StmtKind::Expr(expr)),
        just(TokenKind::Semi).to(StmtKind::Empty),
        expr.clone().map(|expr| StmtKind::Final(expr)),
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
            .delimited_by(just(TokenKind::OpenBrace), just(TokenKind::CloseBrace))
            .map_with(|stmts, e| {
                Box::new(Block {
                    span: e.span(),
                    stmts,
                })
            }),
    );

    let param = ident
        .then_ignore(just(TokenKind::Colon))
        .then(ty.clone())
        .map_with(|(ident, ty), e| Param {
            ident,
            ty,
            span: e.span(),
        });
    let params = param
        .separated_by(just(TokenKind::Comma))
        .allow_trailing()
        .collect();

    let mut item_parser = Recursive::declare();
    let mut mod_parser = Recursive::declare();

    let call_sig = params
        .delimited_by(just(TokenKind::OpenParen), just(TokenKind::CloseParen))
        .then(
            just(TokenKind::Gt)
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
        });

    let pipe_stage = ident
        .or_not()
        .then(call_sig.clone().or_not())
        .then(block)
        .map_with(|((ident, sig), body), e| PipeStage { ident, sig, body });

    let pipe = just(TokenKind::Ident("pipe"))
        .ignore_then(call_sig)
        .then(
            pipe_stage
                .repeated()
                .collect()
                .delimited_by(just(TokenKind::OpenBrace), just(TokenKind::CloseBrace)),
        )
        .map_with(|(sig, stages), e| Pipe { sig, stages });
    item_parser.define(choice((mod_parser.clone().map(|m| Item::Mod(m)),)));

    mod_parser.define(
        just(TokenKind::Ident("mod"))
            .ignore_then(ident)
            .then_ignore(just(TokenKind::OpenBrace))
            .then(item_parser.repeated().collect())
            .then_ignore(just(TokenKind::CloseBrace))
            .map_with(|(ident, items), e| Mod {
                span: e.span(),
                ident,
                items,
            }),
    );
    mod_parser.repeated().collect()
}

pub fn build_ast<'src, 'token>(
    tokens: &'token [Token<'src>],
) -> Result<Ast<'src>, Vec<chumsky::error::Rich<'src, TokenKind<'src>>>>
where
    'token: 'src,
{
    let tokens = tokens.map((tokens.len()..tokens.len()).into(), |t| (&t.kind, &t.span));
    ast_parser().parse(tokens).into_result()
}
