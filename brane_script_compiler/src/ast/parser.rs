use super::ast::*;
use crate::tokens::tokens::*;
use chumsky::{input::ValueInput, prelude::*};

pub fn ast_parser<'src, T>(
) -> impl Parser<'src, T, Ast<'src>, extra::Err<Rich<'src, TokenKind<'src>, SimpleSpan>>>
where
    T: ValueInput<'src, Token = TokenKind<'src>, Span = SimpleSpan>,
{
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
