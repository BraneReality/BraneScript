pub use super::tokens::LiteralKind;
use super::tokens::TokenKind;
use chumsky::{input::ValueInput, prelude::*};

type Span = SimpleSpan;

#[derive(Clone, PartialEq, Debug)]
pub enum TokenTreeKind<'src> {
    Group(Group<'src>),
    Ident(Ident<'src>),
    Punct(Punct),
    Literal(Literal<'src>),
}

#[derive(Clone, PartialEq, Debug)]
pub struct TokenTree<'src> {
    pub span: Span,
    pub kind: TokenTreeKind<'src>,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Delimiter {
    Parenthesis,
    Brace,
    Bracket,
    None,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Group<'src> {
    pub span: Span,
    pub delim: Delimiter,
    pub trees: Vec<TokenTree<'src>>,
    pub annotations: Vec<Annotation<'src>>,
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

#[derive(Clone, PartialEq, Debug)]
pub struct Punct {
    pub span: Span,
    pub ch: char,
    pub joined: bool,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Literal<'src> {
    pub span: Span,
    pub kind: LiteralKind<'src>,
}

use std::fmt::{self, Display, Formatter};

impl<'src> Display for TokenTree<'src> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.kind {
            TokenTreeKind::Group(group) => group.fmt(f, 0, true),
            TokenTreeKind::Ident(ident) => ident.fmt(f, 0, true),
            TokenTreeKind::Punct(punct) => punct.fmt(f, 0, true),
            TokenTreeKind::Literal(literal) => literal.fmt(f, 0, true),
        }
    }
}

impl<'src> Group<'src> {
    fn fmt(&self, f: &mut Formatter<'_>, indent: usize, last: bool) -> fmt::Result {
        let prefix = tree_prefix(indent, last);
        let delim = match self.delim {
            Delimiter::Parenthesis => "(...)",
            Delimiter::Brace => "{...}",
            Delimiter::Bracket => "[...]",
            Delimiter::None => "<...>",
        };
        writeln!(f, "{}{}", prefix, delim)?;

        let has_trees = !self.trees.is_empty();
        let has_anns = !self.annotations.is_empty();

        for (i, ann) in self.annotations.iter().enumerate() {
            let is_last = !has_trees && i == self.annotations.len() - 1;
            ann.fmt(f, indent + 1, is_last)?;
        }

        for (i, tree) in self.trees.iter().enumerate() {
            let is_last = i == self.trees.len() - 1;
            match &tree.kind {
                TokenTreeKind::Group(group) => group.fmt(f, indent + 1, is_last)?,
                TokenTreeKind::Ident(ident) => ident.fmt(f, indent + 1, is_last)?,
                TokenTreeKind::Punct(punct) => punct.fmt(f, indent + 1, is_last)?,
                TokenTreeKind::Literal(literal) => literal.fmt(f, indent + 1, is_last)?,
            }
        }
        Ok(())
    }
}

impl<'src> Ident<'src> {
    fn fmt(&self, f: &mut Formatter<'_>, indent: usize, last: bool) -> fmt::Result {
        let prefix = tree_prefix(indent, last);
        writeln!(f, "{}{}", prefix, self.ident)?;
        for (i, ann) in self.annotations.iter().enumerate() {
            let is_last = i == self.annotations.len() - 1;
            ann.fmt(f, indent + 1, is_last)?;
        }
        Ok(())
    }
}

impl Punct {
    fn fmt(&self, f: &mut Formatter<'_>, indent: usize, last: bool) -> fmt::Result {
        let prefix = tree_prefix(indent, last);
        writeln!(f, "{}{}", prefix, self.ch)
    }
}

impl<'src> Literal<'src> {
    fn fmt(&self, f: &mut Formatter<'_>, indent: usize, last: bool) -> fmt::Result {
        let prefix = tree_prefix(indent, last);
        writeln!(f, "{}{:?}", prefix, self.kind)
    }
}

impl<'src> Annotation<'src> {
    fn fmt(&self, f: &mut Formatter<'_>, indent: usize, last: bool) -> fmt::Result {
        let prefix = tree_prefix(indent, last);
        match &self.kind {
            AnnotationKind::Comment(s) => writeln!(f, "{}//{}", prefix, s),
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

pub fn tree_builder<'src, T>(
) -> impl Parser<'src, T, TokenTree<'src>, extra::Err<Rich<'src, TokenKind<'src>, SimpleSpan>>>
where
    T: ValueInput<'src, Token = TokenKind<'src>, Span = Span>,
{
    let spacing = select! {
        TokenKind::Whitespace => None,
        TokenKind::LineComment(c) => Some(AnnotationKind::Comment(c)),
        TokenKind::BlockComment(c) => Some(AnnotationKind::Comment(c)),
    }
    .labelled("spacing");

    let annotations = spacing
        .map_with(|kind, e| match kind {
            Some(kind) => Some(Annotation {
                span: e.span(),
                kind,
            }),
            None => None,
        })
        .repeated()
        .collect()
        .map(|annotations: Vec<_>| annotations.into_iter().filter_map(|a| a).collect())
        .labelled("annotation");

    let mut tree = Recursive::declare();

    let group = annotations
        .then(
            choice((
                just(TokenKind::OpenParen)
                    .then(tree.clone().repeated().collect())
                    .then_ignore(spacing.repeated())
                    .then_ignore(just(TokenKind::CloseParen)),
                just(TokenKind::OpenBrace)
                    .then(tree.clone().repeated().collect())
                    .then_ignore(spacing.repeated())
                    .then_ignore(just(TokenKind::CloseBrace)),
                just(TokenKind::OpenBracket)
                    .then(tree.clone().repeated().collect())
                    .then_ignore(spacing.repeated())
                    .then_ignore(just(TokenKind::CloseBracket)),
            ))
            .map_with(|group, e| (group, e.span())),
        )
        .map(|(annotations, ((open, trees), span))| Group {
            span,
            delim: match open {
                TokenKind::OpenParen => Delimiter::Parenthesis,
                TokenKind::OpenBracket => Delimiter::Bracket,
                TokenKind::OpenBrace => Delimiter::Brace,
                _ => unreachable!(),
            },
            trees,
            annotations,
        })
        .labelled("group");

    let ident = annotations
        .then(select! { TokenKind::Ident(ident) => ident }.map_with(|ident, e| (ident, e.span())))
        .map(|(annotations, (ident, span))| Ident {
            span,
            ident,
            annotations,
        })
        .labelled("identifier");

    let punct = annotations
        .ignore_then(select! {
            TokenKind::Semi => ';',
            TokenKind::Comma => ',',
            TokenKind::Dot => '.',
            TokenKind::At => '@',
            TokenKind::Pound => '#',
            TokenKind::Tilde => '~',
            TokenKind::Question => '?',
            TokenKind::Colon => ':',
            TokenKind::Dollar => '$',
            TokenKind::Eq => '=',
            TokenKind::Bang => '!',
            TokenKind::Lt => '<',
            TokenKind::Gt => '>',
            TokenKind::Minus => '-',
            TokenKind::And => '&',
            TokenKind::Or => '|',
            TokenKind::Plus => '+',
            TokenKind::Star => '*',
            TokenKind::Slash => '/',
            TokenKind::Caret => '^',
            TokenKind::Percent => '%',
        })
        .then(just(TokenKind::Whitespace).or_not().labelled("<space>"))
        .map_with(|(ch, spacing), e| Punct {
            span: e.span(),
            ch,
            joined: spacing.is_none(),
        })
        .labelled("punctuation");

    let literal = select! { TokenKind::Literal(lit) => lit }
        .map_with(|kind, e| Literal {
            span: e.span(),
            kind,
        })
        .labelled("literal");

    tree.define(
        choice((
            group.map(|group| (group.span.clone(), TokenTreeKind::Group(group))),
            ident.map(|ident| (ident.span.clone(), TokenTreeKind::Ident(ident))),
            punct.map(|punct| (punct.span.clone(), TokenTreeKind::Punct(punct))),
            literal.map(|lit| (lit.span.clone(), TokenTreeKind::Literal(lit))),
        ))
        .map(|(span, kind)| TokenTree { span, kind })
        .labelled("tree"),
    );
    tree.repeated()
        .collect()
        .map_with(|trees, e| TokenTree {
            span: e.span(),
            kind: TokenTreeKind::Group(Group {
                span: e.span(),
                trees,
                delim: Delimiter::None,
                annotations: Vec::new(),
            }),
        })
        .then_ignore(spacing.repeated())
        .labelled("ast")
}
