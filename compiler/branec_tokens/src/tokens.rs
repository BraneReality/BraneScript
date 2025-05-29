use chumsky::{input::BorrowInput, prelude::*};
use std::fmt;

use branec_source::Span;
use branec_symbols::Symbol;

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LitKind {
    Bool,
    Char,
    Int,
    Float,
    Str,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Lit {
    pub kind: LitKind,
    pub symbol: Symbol,
    pub suffix: Option<Symbol>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TokenKind {
    /// single line comment
    LineComment,
    /// multi-line comment
    BlockComment,
    /// spaces and newlines
    Whitespace,
    /// text-based identifiers
    Ident(Symbol),
    /// Literal constants
    Lit(Lit),
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

pub struct TokenInput<'t>(pub &'t Vec<Token>);

impl<'t, 'tree> Input<'tree> for &'tree TokenInput<'t> {
    type Cursor = usize;
    type Span = Span;
    type Token = Token;
    type MaybeToken = &'tree Token;
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
        unsafe { Self::next_ref(cache, cursor) }
    }

    unsafe fn span(cache: &mut Self::Cache, range: std::ops::Range<&usize>) -> Self::Span {
        let get_span = |cursor: &Self::Cursor| {
            if *cursor >= cache.0.len() {
                let eof = cache.0[cache.0.len() - 1].span.end();
                Self::Span::new(cache.0[0].span.context(), eof..eof)
            } else {
                unsafe { cache.0.get_unchecked(*cursor).span.clone() }
            }
        };

        join_spans(&get_span(range.start), &get_span(range.end))
    }
}

fn join_spans<T: chumsky::span::Span<Offset = usize>>(a: &T, b: &T) -> T {
    T::new(a.context(), a.start().min(b.start())..a.end().max(b.end()))
}

impl<'t, 'tree> BorrowInput<'tree> for &'tree TokenInput<'t> {
    unsafe fn next_ref(
        cache: &mut Self::Cache,
        cursor: &mut Self::Cursor,
    ) -> Option<&'tree Self::Token> {
        if *cursor < cache.0.len() {
            let index = *cursor;
            *cursor += 1;
            let token = unsafe { &cache.0.get_unchecked(index) };
            Some(token)
        } else {
            None
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.suffix {
            Some(suffix) => write!(f, "{}{}", self.symbol.as_str(), suffix.as_str()),
            None => write!(f, "{}", self.symbol.as_str()),
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::LineComment => write!(f, "//comment"),
            TokenKind::BlockComment => write!(f, "/*comment*/"),
            TokenKind::Whitespace => write!(f, " "),
            TokenKind::Ident(sym) => write!(f, "{}", sym.as_str()),
            TokenKind::Lit(lit) => write!(f, "{}", lit),
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
