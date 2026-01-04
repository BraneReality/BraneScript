use chumsky::{input::StrInput, prelude::*};

use crate::ir::Uri;

type Span = SimpleSpan;

pub fn uri_parser<'src, I>(
) -> impl Parser<'src, I, Uri, extra::Full<Rich<'src, char, Span>, (), ()>>
where
    I: StrInput<'src, Token = char, Span = Span, Slice = &'src str>,
{
    choice((
        just("stdlib").to(Uri::StdLib),
        just("file://")
            .ignore_then(any().repeated().collect::<String>())
            .map(|path| Uri::File(path.into())),
        just("custom://")
            .ignore_then(hex_digit().repeated().at_least(1).at_most(16).to_slice())
            .try_map(|hex_str: &str, span| {
                Ok(Uri::Custom(usize::from_str_radix(hex_str, 16).map_err(
                    |_| Rich::custom(span, "Invalid hex escape sequence"),
                )?))
            }),
    ))
}

/// Parse a hexadecimal digit
fn hex_digit<'src, I>(
) -> impl Parser<'src, I, char, extra::Full<Rich<'src, char, Span>, (), ()>> + Clone
where
    I: StrInput<'src, Token = char, Span = Span, Slice = &'src str>,
{
    one_of("0123456789abcdefABCDEF")
}
