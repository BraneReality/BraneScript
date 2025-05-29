use branec_source::Span;
use branec_symbols::Symbol;

use super::tokens::*;
use chumsky::{input::MappedSpan, label::LabelError, prelude::*, text::TextExpected};

macro_rules! escapable_char {
    ($delim:literal) => {
        choice((just('\\').ignore_then(any()), none_of($delim)))
    };
}
macro_rules! keyword {
    ($keyword:literal) => {
        text::ascii::ident()
            .try_map(|s, span| -> Result<(), Rich<'src, char, Span>> {
                if s == $keyword {
                    Ok(())
                } else {
                    Err(<Rich<'src, char, Span> as LabelError<
                        'src,
                        MappedSpan<Span, &'src str, M>,
                        _,
                    >>::expected_found(
                        [TextExpected::<MappedSpan<Span, &'src str, M>>::Identifier(
                            $keyword,
                        )],
                        None,
                        span,
                    ))
                }
            })
            .to_slice()
    };
}

pub fn lexer<'src, M>() -> impl Parser<
    'src,
    MappedSpan<Span, &'src str, M>,
    Vec<Token>,
    extra::Full<Rich<'src, char, Span>, (), ()>,
>
where
    M: 'static + Fn(SimpleSpan) -> Span,
{
    let line_comment = just("//")
        .ignore_then(any().and_is(just('\n').not()).repeated().to_slice())
        .to(TokenKind::LineComment);

    let block_comment = just("/*")
        .ignore_then(any().and_is(just("*/").not()).repeated().to_slice())
        .then_ignore(just("*/"))
        .to(TokenKind::BlockComment);

    let whitespace = text::whitespace().at_least(1).to(TokenKind::Whitespace);

    let ident = text::ascii::ident().map(|str| TokenKind::Ident(Symbol::intern(str)));

    let bool = text::ascii::ident()
        .to_slice()
        .try_map(move |s: &str, span| match s {
            "true" => Ok("true"),
            "false" => Ok("false"),
            _ => Err(<Rich<'src, char, Span> as LabelError<
                'src,
                MappedSpan<Span, &'src str, M>,
                _,
            >>::expected_found(
                [
                    TextExpected::<MappedSpan<Span, &'src str, M>>::Identifier("true"),
                    TextExpected::<MappedSpan<Span, &'src str, M>>::Identifier("false"),
                ],
                Option::None,
                span,
            )),
        })
        .map(|keyword| Lit {
            kind: LitKind::Bool,
            symbol: Symbol::intern(keyword),
            suffix: None,
        });
    /*let keyword_test =
    text::ascii::ident().try_map(|s, span| -> Result<(), Rich<'src, char, Span>> {
        if s == "text" {
            Ok(())
        } else {
            Err(Rich::<'src, char, Span>::expected_found(
                [TextExpected::Identifier("test")],
                Some(chumsky::util::Maybe::Ref(&s)),
                span,
            ))
        }
    });*/

    let float_suffix = choice((keyword!("f32"), keyword!("f64")))
        .to_slice()
        .map(|suffix: &str| (suffix, LitKind::Float));
    let int_suffix = choice((
        keyword!("i32"),
        keyword!("i64"),
        keyword!("u32"),
        keyword!("u64"),
        keyword!("isize"),
        keyword!("usize"),
    ))
    .to_slice()
    .map(|suffix: &str| (suffix, LitKind::Int));

    let number = choice((
        text::int(10)
            .then(just('.').then(text::digits(10)))
            .to_slice()
            .then(
                float_suffix
                    .clone()
                    .or_not()
                    .map(|suffix| (suffix.map(|s| s.0), LitKind::Float)),
            ),
        text::int(10)
            .to_slice()
            .then(int_suffix.or(float_suffix).or_not().map(|suffix| {
                (
                    suffix.map(|s| s.0),
                    suffix.map(|s| s.1).unwrap_or(LitKind::Int),
                )
            })),
    ))
    .map(|(number, (suffix, kind))| Lit {
        kind,
        symbol: Symbol::intern(number),
        suffix: suffix.map(|s| Symbol::intern(s)),
    });

    let char = escapable_char!('\'')
        .to_slice()
        .delimited_by(just('\''), just('\''))
        .map(|keyword| Lit {
            kind: LitKind::Char,
            symbol: Symbol::intern(keyword),
            suffix: None,
        });

    let string = just('"')
        .ignore_then(escapable_char!('"').repeated().to_slice())
        .then_ignore(just('"'))
        .map(|str| Lit {
            kind: LitKind::Str,
            symbol: Symbol::intern(str),
            suffix: None,
        });

    let literal = choice((char, string, number, bool)).map(|lit| TokenKind::Lit(lit));

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

    choice((line_comment, block_comment, literal, ident, sym, whitespace))
        .map_with(|tk, e| -> Token {
            Token {
                span: e.span(),
                kind: tk,
            }
        })
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}
