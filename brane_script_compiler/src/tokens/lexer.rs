use super::tokens::*;
use chumsky::prelude::*;

pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<Token<'src>>, extra::Err<Rich<'src, char, SimpleSpan>>> {
    let line_comment = just("//")
        .ignore_then(any().and_is(just('\n').not()).repeated().to_slice())
        .map(TokenKind::LineComment);

    let ident = text::ascii::ident().map(|ident: &str| TokenKind::Ident(ident));

    let int = text::int(10)
        .to_slice()
        .from_str()
        .unwrapped()
        .map(|int: i64| LiteralKind::Int(int));

    let float = text::int(10)
        .then(just('.').then(text::digits(10)).or_not())
        .to_slice()
        .from_str()
        .unwrapped()
        .map(|f: f64| LiteralKind::Float(f));

    let string = just('"')
        .ignore_then(none_of('"').repeated().to_slice())
        .then_ignore(just('"'))
        .map(LiteralKind::String);

    let literal = choice((int, float, string)).map(TokenKind::Literal);

    let sym = choice((
        just(';').to(TokenKind::Semi),
        just(',').to(TokenKind::Comma),
        just('.').to(TokenKind::Dot),
        just('(').to(TokenKind::OpenParen),
        just(')').to(TokenKind::CloseParen),
        just('{').to(TokenKind::OpenBrace),
        just('}').to(TokenKind::CloseBrace),
        just('[').to(TokenKind::OpenBracket),
        just(']').to(TokenKind::CloseBracket),
        just('@').to(TokenKind::At),
        just('#').to(TokenKind::Pound),
        just('~').to(TokenKind::Tilde),
    ))
    .or(choice((
        just('?').to(TokenKind::Question),
        just(':').to(TokenKind::Colon),
        just('$').to(TokenKind::Dollar),
        just('=').to(TokenKind::Eq),
        just('!').to(TokenKind::Bang),
        just('<').to(TokenKind::Lt),
        just('>').to(TokenKind::Gt),
        just('-').to(TokenKind::Minus),
        just('&').to(TokenKind::And),
        just('|').to(TokenKind::Or),
        just('+').to(TokenKind::Plus),
        just('*').to(TokenKind::Star),
        just('/').to(TokenKind::Slash),
        just('^').to(TokenKind::Caret),
        just('%').to(TokenKind::Percent),
    )));

    let token = choice((literal, ident, sym))
        .map_with(|tk, e| Token {
            span: e.span(),
            kind: tk,
        })
        .padded_by(line_comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect();
    token
}
