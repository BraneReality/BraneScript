pub use super::tokens::LiteralKind;
use super::{tokens::TokenKind, Token};
use crate::{
    source::{Span, Uri},
    symbols::Symbol,
};
use chumsky::{input::BorrowInput, label::LabelError, prelude::*, util::Maybe};

#[derive(Clone, PartialEq, Debug)]
pub enum TokenTree {
    Group(Group),
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
}

impl TokenTree {
    pub fn span(&self) -> &Span {
        match &self {
            TokenTree::Group(group) => &group.span,
            TokenTree::Ident(ident) => &ident.span,
            TokenTree::Punct(punct) => &punct.span,
            TokenTree::Literal(literal) => &literal.span,
        }
    }

    pub fn write_debug_tree(&self, source: &str) -> String {
        let mut f = String::new();
        match &self {
            TokenTree::Group(group) => group.fmt(&mut f, 0, true, source).unwrap(),
            TokenTree::Ident(ident) => ident.fmt(&mut f, 0, true, source).unwrap(),
            TokenTree::Punct(punct) => punct.fmt(&mut f, 0, true).unwrap(),
            TokenTree::Literal(literal) => literal.fmt(&mut f, 0, true, source).unwrap(),
        };
        f
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

#[derive(Clone, PartialEq, Debug)]
pub struct Group {
    pub span: Span,
    pub delim: Delimiter,
    pub trees: Vec<TokenTree>,
    pub annotations: Vec<Annotation>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum AnnotationKind {
    Comment,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Annotation {
    pub span: Span,
    pub kind: AnnotationKind,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Ident {
    pub span: Span,
    pub sym: Symbol,
    pub annotations: Vec<Annotation>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Punct {
    pub span: Span,
    pub ch: char,
    pub joined: bool,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Literal {
    pub span: Span,
    pub kind: LiteralKind,
}

use std::{
    fmt::{self},
    ops::Range,
    sync::Arc,
};

impl Group {
    fn fmt(&self, f: &mut impl fmt::Write, indent: usize, last: bool, source: &str) -> fmt::Result {
        let prefix = tree_prefix(indent, last);
        let delim = match self.delim {
            Delimiter::Parenthesis => ("(", ")"),
            Delimiter::Brace => ("{", "}"),
            Delimiter::Bracket => ("[", "]"),
            Delimiter::None => ("...", "..."),
        };
        writeln!(f, "{:<10}{}{}", self.span.start(), prefix, delim.0)?;

        let has_trees = !self.trees.is_empty();

        for (i, ann) in self.annotations.iter().enumerate() {
            let is_last = !has_trees && i == self.annotations.len() - 1;
            ann.fmt(f, indent + 1, is_last, source)?;
        }

        for (i, tree) in self.trees.iter().enumerate() {
            let is_last = i == self.trees.len() - 1;
            match &tree {
                TokenTree::Group(group) => group.fmt(f, indent + 1, is_last, source)?,
                TokenTree::Ident(ident) => ident.fmt(f, indent + 1, is_last, source)?,
                TokenTree::Punct(punct) => punct.fmt(f, indent + 1, is_last)?,
                TokenTree::Literal(literal) => literal.fmt(f, indent + 1, is_last, source)?,
            }
        }
        writeln!(f, "{:<10}{}{}", self.span.start(), prefix, delim.1)?;
        Ok(())
    }
}

impl Ident {
    fn fmt(&self, f: &mut impl fmt::Write, indent: usize, last: bool, source: &str) -> fmt::Result {
        let prefix = tree_prefix(indent, last);
        writeln!(
            f,
            "{:<10}{}{}",
            self.span.start(),
            prefix,
            &source[self.span.range.clone()]
        )?;
        for (i, ann) in self.annotations.iter().enumerate() {
            let is_last = i == self.annotations.len() - 1;
            ann.fmt(f, indent + 1, is_last, source)?;
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

impl Literal {
    fn fmt(&self, f: &mut impl fmt::Write, indent: usize, last: bool, source: &str) -> fmt::Result {
        let prefix = tree_prefix(indent, last);
        writeln!(
            f,
            "{:<10}{}{} type={:?}",
            self.span.start(),
            prefix,
            &source[self.span.range.clone()],
            self.kind
        )
    }
}

impl Annotation {
    fn fmt(&self, f: &mut impl fmt::Write, indent: usize, last: bool, source: &str) -> fmt::Result {
        let prefix = tree_prefix(indent, last);
        match &self.kind {
            AnnotationKind::Comment => {
                for line in source[self.span.range.clone()].split('\n') {
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
    kind: TokenKind,
) -> impl Clone + Parser<'src, T, Token, extra::Err<Rich<'src, Token, Span>>>
where
    T: BorrowInput<'src, Token = Token, Span = Span>,
{
    any_ref()
        .try_map(move |token: &Token, span| {
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
) -> impl Parser<'src, T, TokenTree, extra::Err<Rich<'src, Token, Span>>>
where
    T: BorrowInput<'src, Token = Token, Span = Span>,
{
    let spacing = select_ref! {
        Token { kind: TokenKind::Whitespace, span:_ } => None,
        Token { kind: TokenKind::LineComment, span } => Some((AnnotationKind::Comment, span)),
        Token { kind: TokenKind::BlockComment, span } => Some((AnnotationKind::Comment, span)),
    }
    .labelled("spacing token");

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

    let ident = annotations
        .then(select_ref! { Token {kind: TokenKind::Ident(sym), span } => (*sym, span.clone()) })
        .map(|(annotations, (sym, span))| Ident {
            span,
            sym,
            annotations,
        })
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
        .map(|trees: Vec<TokenTree>| {
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

impl<'tree> Input<'tree> for &'tree Group {
    type Cursor = usize;
    type Span = Span;
    type Token = TokenTree;
    type MaybeToken = &'tree TokenTree;
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

impl<'tree> BorrowInput<'tree> for &'tree Group {
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
