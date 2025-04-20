use chumsky::prelude::*;

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
    /// `;`
    Semi,
    /// `,`
    Comma,
    /// `.`
    Dot,
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
