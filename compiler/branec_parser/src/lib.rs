use std::collections::HashMap;

use branec::types::*;
use chumsky::prelude::*;

pub type Span = SimpleSpan;

pub fn token<'src, M>(
    text: &'static str,
) -> impl Parser<
    'src,
    chumsky::input::MappedSpan<Span, &'src str, M>,
    &'src str,
    extra::Full<Rich<'src, char, Span>, (), ()>,
> + Clone
where
    M: 'static + Fn(SimpleSpan) -> Span,
{
    just(text).padded().to_slice()
}

pub fn parser<'src, M>() -> impl Parser<
    'src,
    chumsky::input::MappedSpan<Span, &'src str, M>,
    Function,
    extra::Full<Rich<'src, char, Span>, (), ()>,
>
where
    M: 'static + Fn(SimpleSpan) -> Span,
{
    let mut compiler_value = Recursive::declare();
    let mut fn_def = Recursive::declare();

    let ident = text::unicode::ident().to_slice().padded();

    let label_def = ident
        .clone()
        .then(token(":").ignore_then(compiler_value.clone()).or_not());

    let label_stmt = label_def
        .clone()
        .then_ignore(token("="))
        .then(compiler_value.clone())
        .map(
            |((label, constraints), value): ((&str, Option<CompilerValue>), CompilerValue)| {
                LabelOperation::Label(label.to_string(), constraints, value)
            },
        );

    let destructure = ident
        .clone()
        .then(token(":").ignore_then(ident.clone()).or_not())
        .map(|(member, label): (&str, Option<&str>)| {
            (member.to_string(), label.map(|l| l.to_string()))
        })
        .separated_by(token(","))
        .collect::<Vec<(String, Option<String>)>>()
        .delimited_by(token("{"), token("}"))
        .then_ignore(token("="))
        .then(compiler_value.clone())
        .map_with(|(labels, object), _| LabelOperation::Destructure(labels, object));

    let number = token("-")
        .or_not()
        .then(text::digits(10))
        .then(just(".").ignore_then(text::digits(10)).or_not())
        .to_slice()
        .padded()
        .try_map(|s: &str, span| s.parse::<f64>().map_err(|e| Rich::custom(span, e)))
        .map_with(|value, _| Number { value });

    let array_lit = ();
    let structure = label_def
        .clone()
        .separated_by(token(","))
        .collect::<Vec<(&str, Option<CompilerValue>)>>()
        .delimited_by(token("{"), token("}"))
        .map_with(|members, _| {
            CompilerValueKind::Object(Object::new(members.iter().map(|(label, value)| {
                ObjectMember {
                    label: (*label).into(),
                    value: value
                        .as_ref()
                        .map(|v| v.clone())
                        .unwrap_or_else(|| CompilerValue::label(*label)),
                }
            })))
        });

    let match_expr = token("match")
        .ignore_then(compiler_value.clone())
        .then(
            ident
                .clone()
                .then_ignore(token("=>"))
                .then(compiler_value.clone())
                .labelled("match branch")
                .map(|(label, branch): (&str, CompilerValue)| (label.to_string(), branch))
                .separated_by(token(","))
                .collect::<HashMap<String, CompilerValue>>()
                .delimited_by(token("{"), token("}")),
        )
        .map_with(|(value, branches), _| CompilerValueKind::Match(Box::new(value), branches));

    let fn_expr = token("fn")
        .ignore_then(
            label_def
                .separated_by(token(","))
                .collect::<Vec<_>>()
                .delimited_by(token("("), token(")")),
        )
        .then(token("=>").ignore_then(compiler_value.clone()).or_not())
        .then(fn_def.clone().delimited_by(token("{"), token("}")))
        .map_with(|((params, returns), defintion), _| {
            CompilerValueKind::Function(Function {
                params: Array {
                    values: params
                        .into_iter()
                        .map(|(label, constraints)| {
                            CompilerValue::object([
                                ObjectMember {
                                    label: "label".into(),
                                    value: CompilerValue::string(label),
                                },
                                ObjectMember {
                                    label: "constraints".into(),
                                    value: CompilerValue::array(
                                        constraints.map(|c| vec![c]).unwrap_or_default(),
                                    ),
                                },
                            ])
                        })
                        .collect(),
                },
                returns: Array {
                    values: returns.map(|c| vec![c]).unwrap_or_default(),
                },
                defintion,
            })
        });

    let atom = choice((
        fn_expr,
        match_expr,
        structure,
        ident.map(|label: &str| CompilerValueKind::Label(label.into())),
        number.map_with(|n, _| CompilerValueKind::Number(n)),
    ))
    .map_with(|kind, _| CompilerValue {
        kind,
        share_info: None,
    });

    let mut call = Recursive::declare();
    call.define(
        atom.clone().foldl_with(
            call.clone()
                .separated_by(token(","))
                .collect()
                .delimited_by(token("("), token(")"))
                .repeated(),
            |function, args, _e| CompilerValue {
                kind: CompilerValueKind::Call(Box::new(function), args),
                share_info: None,
            },
        ),
    );

    let pipeline = ();
    compiler_value.define(call.padded());

    fn_def.define(
        choice((label_stmt, destructure))
            .repeated()
            //.separated_by(token(";"))
            .collect()
            .then(compiler_value)
            .map_with(|(operations, value), _e| FunctionDefinition::Closure {
                operations,
                value: Box::new(value),
            }),
    );

    fn_def.map(|defintion| Function {
        params: Array { values: Vec::new() },
        returns: Array { values: Vec::new() },
        defintion,
    })
}
