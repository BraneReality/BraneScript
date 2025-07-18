use std::collections::HashMap;

use branec::types::*;
use chumsky::{input::MappedSpan, prelude::*};

pub type Span = SimpleSpan;

pub fn parser<'src, M>() -> impl Parser<
    'src,
    MappedSpan<Span, &'src str, M>,
    Function,
    extra::Full<Rich<'src, char, Span>, (), ()>,
>
where
    M: 'static + Fn(SimpleSpan) -> Span,
{
    let mut compiler_value = Recursive::declare();

    let label_def = text::unicode::ident().to_slice().then(
        just(":")
            .padded()
            .ignore_then(compiler_value.clone())
            .or_not(),
    );

    let label_stmt = label_def
        .clone()
        .then_ignore(just("=").padded())
        .then(compiler_value.clone())
        .map(
            |((label, constraints), value): ((&str, Option<CompilerValue>), CompilerValue)| {
                LabelOperation::Label(label.to_string(), constraints, value)
            },
        );

    let destructure = text::unicode::ident()
        .to_slice()
        .padded()
        .then(
            just(":")
                .ignore_then(text::unicode::ident().to_slice().padded())
                .or_not(),
        )
        .map(|(member, label): (&str, Option<&str>)| {
            (member.to_string(), label.map(|l| l.to_string()))
        })
        .separated_by(just(','))
        .collect::<Vec<(String, Option<String>)>>()
        .delimited_by(just('{'), just('}'))
        .then_ignore(just("=").padded())
        .then(compiler_value.clone())
        .map(|(labels, object)| LabelOperation::Destructure(labels, object));

    let number = just('-')
        .or_not()
        .then(text::digits(10))
        .then(just(".").ignore_then(text::digits(10)).or_not())
        .to_slice()
        .try_map(|s: &str, span| s.parse::<f64>().map_err(|e| Rich::custom(span, e)))
        .map(|value| Number { value });

    let array_lit = ();
    let structure = ();

    let match_expr = just("match")
        .ignore_then(compiler_value.clone())
        .then(
            text::unicode::ident()
                .to_slice()
                .padded()
                .then_ignore(just("=>"))
                .then(compiler_value.clone())
                .map(|(label, branch): (&str, CompilerValue)| (label.to_string(), branch))
                .separated_by(just(','))
                .collect::<HashMap<String, CompilerValue>>(),
        )
        .map(|(value, branches)| CompilerValueKind::Match(Box::new(value), branches));

    let atom = choice((
        text::unicode::ident()
            .to_slice()
            .map(|label: &str| CompilerValueKind::Label(label.into())),
        number.map(|n| CompilerValueKind::Number(n)),
        match_expr,
    ))
    .padded()
    .map_with(|kind, _| CompilerValue {
        kind,
        share_info: None,
    });

    let mut call = Recursive::declare();
    call.define(
        atom.clone().foldl_with(
            call.clone()
                .separated_by(just(','))
                .collect()
                .delimited_by(just('('), just(')'))
                .repeated(),
            |function, args, _e| CompilerValue {
                kind: CompilerValueKind::Call(Box::new(function), args),
                share_info: None,
            },
        ),
    );

    let function = ();
    let pipeline = ();

    compiler_value.define(call.padded());

    let script_fn = choice((label_stmt, destructure))
        .padded()
        .repeated()
        //.separated_by(just(";"))
        .collect()
        .then(compiler_value)
        .map_with(|(operations, value), _e| Function {
            params: Array { values: vec![] },
            defintion: FunctionDefinition::Closure {
                operations,
                value: Box::new(value),
            },
        });

    script_fn.padded()
}
