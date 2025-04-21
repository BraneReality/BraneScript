use chumsky::prelude::*;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub struct Token<'src> {
    pub span: SimpleSpan,
    pub kind: TokenKind<'src>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralKind<'src> {
    String(&'src str),
    Int(i64),
    Float(f64),
    True,
    False,
}

#[derive(Clone, Debug, PartialEq)]
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
