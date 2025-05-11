pub use super::tokens::LiteralKind;
use super::{tokens::TokenKind, Token};
use crate::source::{Span, Uri};
use chumsky::{input::BorrowInput, label::LabelError, prelude::*, util::Maybe};

#[derive(Clone, PartialEq, Debug)]
pub enum TokenTree<'src> {
    Group(Group<'src>),
    Ident(Ident<'src>),
    Punct(Punct),
    Literal(Literal<'src>),
}

impl<'src> TokenTree<'src> {
    pub fn span(&self) -> &Span {
        match &self {
            TokenTree::Group(group) => &group.span,
            TokenTree::Ident(ident) => &ident.span,
            TokenTree::Punct(punct) => &punct.span,
            TokenTree::Literal(literal) => &literal.span,
        }
    }

    pub fn write_debug_tree(&self) -> String {
        let mut f = String::new();
        match &self {
            TokenTree::Group(group) => group.fmt(&mut f, 0, true).unwrap(),
            TokenTree::Ident(ident) => ident.fmt(&mut f, 0, true).unwrap(),
            TokenTree::Punct(punct) => punct.fmt(&mut f, 0, true).unwrap(),
            TokenTree::Literal(literal) => literal.fmt(&mut f, 0, true).unwrap(),
        };
        f
    }
}

impl<'src> Display for TokenTree<'src> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TokenTree::Group(group) => write!(f, "{}", group),
            TokenTree::Ident(ident) => write!(f, "{}", ident),
            TokenTree::Punct(punct) => write!(f, "{}", punct),
            TokenTree::Literal(literal) => write!(f, "{}", literal),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Delimiter {
    /// ()
    Parenthesis,
    /// {}
    Brace,
    /// []
    Bracket,
    None,
}

impl Delimiter {
    pub fn as_str(&self) -> &'static str {
        match self {
            Delimiter::Parenthesis => "( ... )",
            Delimiter::Brace => "{ ... }",
            Delimiter::Bracket => "[ ... ]",
            Delimiter::None => "non-delimited group",
        }
    }
}

impl Display for Delimiter {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Group<'src> {
    pub span: Span,
    pub delim: Delimiter,
    pub trees: Vec<TokenTree<'src>>,
    pub annotations: Vec<Annotation<'src>>,
}

impl<'src> Display for Group<'src> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.delim)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum AnnotationKind<'src> {
    Comment(&'src str),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Annotation<'src> {
    pub span: Span,
    pub kind: AnnotationKind<'src>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Ident<'src> {
    pub span: Span,
    pub ident: &'src str,
    pub annotations: Vec<Annotation<'src>>,
}

impl<'src> Display for Ident<'src> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ident)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Punct {
    pub span: Span,
    pub ch: char,
    pub joined: bool,
}

impl<'src> Display for Punct {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ch)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Literal<'src> {
    pub span: Span,
    pub kind: LiteralKind<'src>,
}

impl<'src> Display for Literal<'src> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

use std::{
    fmt::{self, Display, Formatter},
    ops::Range,
    sync::Arc,
};

impl<'src> Group<'src> {
    fn fmt(&self, f: &mut impl fmt::Write, indent: usize, last: bool) -> fmt::Result {
        let prefix = tree_prefix(indent, last);
        let delim = match self.delim {
            Delimiter::Parenthesis => ("(", ")"),
            Delimiter::Brace => ("{", "}"),
            Delimiter::Bracket => ("[", "]"),
            Delimiter::None => ("...", "..."),
        };
        writeln!(f, "{:<10}{}{}", self.span.start(), prefix, delim.0)?;

        let has_trees = !self.trees.is_empty();
        let has_anns = !self.annotations.is_empty();

        for (i, ann) in self.annotations.iter().enumerate() {
            let is_last = !has_trees && i == self.annotations.len() - 1;
            ann.fmt(f, indent + 1, is_last)?;
        }

        for (i, tree) in self.trees.iter().enumerate() {
            let is_last = i == self.trees.len() - 1;
            match &tree {
                TokenTree::Group(group) => group.fmt(f, indent + 1, is_last)?,
                TokenTree::Ident(ident) => ident.fmt(f, indent + 1, is_last)?,
                TokenTree::Punct(punct) => punct.fmt(f, indent + 1, is_last)?,
                TokenTree::Literal(literal) => literal.fmt(f, indent + 1, is_last)?,
            }
        }
        writeln!(f, "{:<10}{}{}", self.span.start(), prefix, delim.1)?;
        Ok(())
    }
}

impl<'src> Ident<'src> {
    fn fmt(&self, f: &mut impl fmt::Write, indent: usize, last: bool) -> fmt::Result {
        let prefix = tree_prefix(indent, last);
        writeln!(f, "{:<10}{}{}", self.span.start(), prefix, self.ident)?;
        for (i, ann) in self.annotations.iter().enumerate() {
            let is_last = i == self.annotations.len() - 1;
            ann.fmt(f, indent + 1, is_last)?;
        }
        Ok(())
    }
}

impl Punct {
    fn fmt(&self, f: &mut impl fmt::Write, indent: usize, last: bool) -> fmt::Result {
        let prefix = tree_prefix(indent, last);
        writeln!(f, "{:<10}{}{}", self.span.start(), prefix, self.ch)
    }
}

impl<'src> Literal<'src> {
    fn fmt(&self, f: &mut impl fmt::Write, indent: usize, last: bool) -> fmt::Result {
        let prefix = tree_prefix(indent, last);
        writeln!(f, "{:<10}{}{}", self.span.start(), prefix, self.kind)
    }
}

impl<'src> Annotation<'src> {
    fn fmt(&self, f: &mut impl fmt::Write, indent: usize, last: bool) -> fmt::Result {
        let prefix = tree_prefix(indent, last);
        match &self.kind {
            AnnotationKind::Comment(s) => {
                for line in s.split('\n') {
                    writeln!(f, "{:<10}{}//{}", self.span.start(), prefix, line)?;
                }
                Ok(())
            }
        }
    }
}

// Helpers
fn tree_prefix(indent: usize, last: bool) -> String {
    let mut s = String::new();
    for _ in 0..indent.saturating_sub(1) {
        s.push_str("│   ");
    }
    if indent > 0 {
        s.push_str(if last { "└── " } else { "├── " });
    }
    s
}

pub fn token_kind<'src, T>(
    kind: TokenKind<'src>,
) -> impl Clone + Parser<'src, T, Token<'src>, extra::Err<Rich<'src, Token<'src>, Span>>>
where
    T: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    any_ref()
        .try_map(move |token: &Token<'src>, span| {
            if token.kind == kind {
                Ok(token.clone())
            } else {
                Err(LabelError::<'src, T, _>::expected_found(
                    [kind.to_string()],
                    Some(Maybe::Ref(token)),
                    span,
                ))
            }
        })
        .labelled(kind.to_string())
}

fn join_spans(a: &Span, b: &Span) -> Span {
    return Span {
        range: a.start().min(b.start())..a.end().max(b.end()),
        source: a.source.clone(),
    };
}

pub fn tree_builder<'src, T>(
) -> impl Parser<'src, T, TokenTree<'src>, extra::Err<Rich<'src, Token<'src>, Span>>>
where
    T: BorrowInput<'src, Token = Token<'src>, Span = Span>,
{
    let spacing = select_ref! {
        Token { kind: TokenKind::Whitespace, span:_ } => None,
        Token { kind: TokenKind::LineComment(c), span } => Some((AnnotationKind::Comment(c), span)),
        Token { kind: TokenKind::BlockComment(c), span } => Some((AnnotationKind::Comment(c), span)),
    }.labelled("spacing token");

    let annotations = spacing
        .map(|a| match a {
            Some((kind, span)) => Some(Annotation {
                span: span.clone(),
                kind,
            }),
            None => None,
        })
        .repeated()
        .collect()
        .map(|annotations: Vec<_>| annotations.into_iter().filter_map(|a| a).collect());

    let mut tree = Recursive::declare();

    let group = annotations
        .then(choice((
            tree.clone()
                .repeated()
                .collect()
                .then_ignore(spacing.repeated())
                .delimited_by(
                    token_kind(TokenKind::OpenParen),
                    token_kind(TokenKind::CloseParen),
                )
                .map(|trees| (trees, Delimiter::Parenthesis))
                .map_err_with_state(|e, s, _| {
                    chumsky::error::Error::<'src, T>::merge(e, Rich::custom(s, "Unclosed ("))
                }),
            tree.clone()
                .repeated()
                .collect()
                .then_ignore(spacing.repeated())
                .delimited_by(
                    token_kind(TokenKind::OpenBrace),
                    token_kind(TokenKind::CloseBrace),
                )
                .map(|trees| (trees, Delimiter::Brace))
                .map_err_with_state(|e, s, _| {
                    chumsky::error::Error::<'src, T>::merge(e, Rich::custom(s, "Unclosed {"))
                }),
            tree.clone()
                .repeated()
                .collect()
                .then_ignore(spacing.repeated())
                .delimited_by(
                    token_kind(TokenKind::OpenBracket),
                    token_kind(TokenKind::CloseBracket),
                )
                .map(|trees| (trees, Delimiter::Bracket))
                .map_err_with_state(|e, s, _| {
                    chumsky::error::Error::<'src, T>::merge(e, Rich::custom(s, "Unclosed ["))
                }),
        )))
        .map_with(|(annotations, (trees, delim)), e| Group {
            span: e.span(),
            delim,
            trees,
            annotations,
        });
    fn fun_name<'src>(
        (annotations, (ident, span)): (Vec<Annotation<'src>>, (&&'src str, Span)),
    ) -> Ident<'src> {
        Ident {
            span,
            ident,
            annotations,
        }
    }

    let ident = annotations
        .then(select_ref! { Token {kind: TokenKind::Ident(ident), span } => (ident, span.clone()) })
        .map(fun_name)
        .labelled("identifier token");

    let punct = annotations
        .ignore_then(choice((
            token_kind(TokenKind::Semi).map(|span| (';', span)),
            token_kind(TokenKind::Comma).map(|span| (',', span)),
            token_kind(TokenKind::Dot).map(|span| ('.', span)),
            token_kind(TokenKind::At).map(|span| ('@', span)),
            token_kind(TokenKind::Pound).map(|span| ('#', span)),
            token_kind(TokenKind::Tilde).map(|span| ('~', span)),
            token_kind(TokenKind::Question).map(|span| ('?', span)),
            token_kind(TokenKind::Colon).map(|span| (':', span)),
            token_kind(TokenKind::Dollar).map(|span| ('$', span)),
            token_kind(TokenKind::Eq).map(|span| ('=', span)),
            token_kind(TokenKind::Bang).map(|span| ('!', span)),
            token_kind(TokenKind::Lt).map(|span| ('<', span)),
            token_kind(TokenKind::Gt).map(|span| ('>', span)),
            token_kind(TokenKind::Minus).map(|span| ('-', span)),
            token_kind(TokenKind::And).map(|span| ('&', span)),
            token_kind(TokenKind::Or).map(|span| ('|', span)),
            token_kind(TokenKind::Plus).map(|span| ('+', span)),
            token_kind(TokenKind::Star).map(|span| ('*', span)),
            token_kind(TokenKind::Slash).map(|span| ('/', span)),
            token_kind(TokenKind::Caret).map(|span| ('^', span)),
            token_kind(TokenKind::Percent).map(|span| ('%', span)),
        )))
        .then(token_kind(TokenKind::Whitespace).or_not())
        .map(|((ch, token), spacing)| Punct {
            span: token.span,
            ch,
            joined: spacing.is_none(),
        })
        .labelled("punct token");

    let literal =
        select_ref! { Token {kind: TokenKind::Literal(kind), span } => (*kind, span.clone()) }
            .map(|(kind, span)| Literal { span, kind })
            .labelled("literal token");

    tree.define(choice((
        group.map(|group| TokenTree::Group(group)),
        ident.map(|ident| TokenTree::Ident(ident)),
        punct.map(|punct| TokenTree::Punct(punct)),
        literal.map(|lit| TokenTree::Literal(lit)),
    )));
    tree.repeated()
        .collect()
        .map(|trees: Vec<TokenTree<'src>>| {
            let span = if trees.is_empty() {
                Span::new(Arc::new(Uri::Unknown), 0..0)
            } else {
                join_spans(trees[0].span(), trees[trees.len() - 1].span())
            };
            TokenTree::Group(Group {
                span,
                trees,
                delim: Delimiter::None,
                annotations: Vec::new(),
            })
        })
        .then_ignore(spacing.repeated())
}

impl<'src, 'tree> Input<'tree> for &'tree Group<'src>
where
    'src: 'tree,
{
    type Cursor = usize;
    type Span = Span;
    type Token = TokenTree<'src>;
    type MaybeToken = &'tree TokenTree<'src>;
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

    unsafe fn span(cache: &mut Self::Cache, range: Range<&Self::Cursor>) -> Self::Span {
        let get_span = |cursor: &Self::Cursor| {
            if *cursor >= cache.trees.len() {
                let eof = cache.span.end();
                Self::Span::new(cache.span.context(), eof..eof)
            } else {
                cache.trees.get_unchecked(*cursor).span().clone()
            }
        };

        join_spans(&get_span(range.start), &get_span(range.end))
    }
}

impl<'src, 'tree> BorrowInput<'tree> for &'tree Group<'src>
where
    'src: 'tree,
{
    unsafe fn next_ref(
        cache: &mut Self::Cache,
        cursor: &mut Self::Cursor,
    ) -> Option<&'tree Self::Token> {
        if *cursor < cache.trees.len() {
            let index = *cursor;
            *cursor += 1;
            let token = &cache.trees.get_unchecked(index);
            Some(token)
        } else {
            None
        }
    }
}
