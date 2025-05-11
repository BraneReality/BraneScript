use chumsky::{input::BorrowInput, prelude::*};
use std::fmt;

use crate::source::Span;

#[derive(Clone, Debug, PartialEq)]
pub struct Token<'src> {
    pub span: Span,
    pub kind: TokenKind<'src>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LiteralKind<'src> {
    String(&'src str),
    Int(i64),
    Float(f64),
    True,
    False,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenKind<'src> {
    /// single line comment
    LineComment(&'src str),
    /// multi-line comment
    BlockComment(&'src str),
    /// spaces and newlines
    Whitespace,
    /// text-based identifiers
    Ident(&'src str),
    /// Literal constants
    Literal(LiteralKind<'src>),
    /// `(`
    OpenParen,
    /// `)`
    CloseParen,
    /// `{`
    OpenBrace,
    /// `}`
    CloseBrace,
    /// `[`
    OpenBracket,
    /// `]`
    CloseBracket,
    /// `;`
    Semi,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `@`
    At,
    /// `#`
    Pound,
    /// `~`
    Tilde,
    /// `?`
    Question,
    /// `:`
    Colon,
    /// `$`
    Dollar,
    /// `=`
    Eq,
    /// `!`
    Bang,
    /// `<`
    Lt,
    /// `>`
    Gt,
    /// `-`
    Minus,
    /// `&`
    And,
    /// `|`
    Or,
    /// `+`
    Plus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `^`
    Caret,
    /// `%`
    Percent,
}

impl<'src> fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl<'src> fmt::Display for LiteralKind<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralKind::String(s) => write!(f, "\"{}\"", s),
            LiteralKind::Int(i) => write!(f, "{}", i),
            LiteralKind::Float(fl) => write!(f, "{}", fl),
            LiteralKind::True => write!(f, "true"),
            LiteralKind::False => write!(f, "false"),
        }
    }
}

impl<'src> fmt::Display for TokenKind<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::LineComment(text) => write!(f, "//{}", text),
            TokenKind::BlockComment(text) => write!(f, "/*{}*/", text),
            TokenKind::Whitespace => write!(f, " "),
            TokenKind::Ident(name) => write!(f, "{}", name),
            TokenKind::Literal(lit) => write!(f, "{}", lit),
            TokenKind::OpenParen => write!(f, "("),
            TokenKind::CloseParen => write!(f, ")"),
            TokenKind::OpenBrace => write!(f, "{{"),
            TokenKind::CloseBrace => write!(f, "}}"),
            TokenKind::OpenBracket => write!(f, "["),
            TokenKind::CloseBracket => write!(f, "]"),
            TokenKind::Semi => write!(f, ";"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::At => write!(f, "@"),
            TokenKind::Pound => write!(f, "#"),
            TokenKind::Tilde => write!(f, "~"),
            TokenKind::Question => write!(f, "?"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Dollar => write!(f, "$"),
            TokenKind::Eq => write!(f, "="),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::Lt => write!(f, "<"),
            TokenKind::Gt => write!(f, ">"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::And => write!(f, "&"),
            TokenKind::Or => write!(f, "|"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Caret => write!(f, "^"),
            TokenKind::Percent => write!(f, "%"),
        }
    }
}

pub struct TokenInput<'src, 't>(pub &'t Vec<Token<'src>>);

impl<'src, 't, 'tree> Input<'tree> for &'tree TokenInput<'src, 't>
where
    'src: 'tree,
{
    type Cursor = usize;
    type Span = Span;
    type Token = Token<'src>;
    type MaybeToken = &'tree Token<'src>;
    type Cache = Self;

    fn begin(self) -> (Self::Cursor, Self::Cache) {
        (0, self)
    }

    fn cursor_location(cursor: &Self::Cursor) -> usize {
        *cursor
    }

    unsafe fn next_maybe(
        cache: &mut Self::Cache,
        cursor: &mut Self::Cursor,
    ) -> Option<Self::MaybeToken> {
        Self::next_ref(cache, cursor)
    }

    unsafe fn span(cache: &mut Self::Cache, range: std::ops::Range<&usize>) -> Self::Span {
        let get_span = |cursor: &Self::Cursor| {
            if *cursor >= cache.0.len() {
                let eof = cache.0[cache.0.len() - 1].span.end();
                Self::Span::new(cache.0[0].span.context(), eof..eof)
            } else {
                cache.0.get_unchecked(*cursor).span.clone()
            }
        };

        join_spans(&get_span(range.start), &get_span(range.end))
    }
}

fn join_spans<T: chumsky::span::Span<Offset = usize>>(a: &T, b: &T) -> T {
    T::new(a.context(), a.start().min(b.start())..a.end().max(b.end()))
}

impl<'src, 't, 'tree> BorrowInput<'tree> for &'tree TokenInput<'src, 't>
where
    'src: 'tree,
{
    unsafe fn next_ref(
        cache: &mut Self::Cache,
        cursor: &mut Self::Cursor,
    ) -> Option<&'tree Self::Token> {
        if *cursor < cache.0.len() {
            let index = *cursor;
            *cursor += 1;
            let token = &cache.0.get_unchecked(index);
            Some(token)
        } else {
            None
        }
    }
}
