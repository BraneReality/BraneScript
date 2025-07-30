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
    let mut expression = Recursive::declare();
    let mut fn_def = Recursive::declare();

    let ident = text::unicode::ident().to_slice().padded();

    let label_def = ident
        .clone()
        .then(token(":").ignore_then(expression.clone()).or_not());

    let label_stmt = label_def
        .clone()
        .then_ignore(token("="))
        .then(expression.clone())
        .map(
            |((label, type_value), value): ((&str, Option<Expression>), Expression)| {
                LabelOperation::Label(label.to_string(), type_value, value)
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
        .then(expression.clone())
        .map_with(|(labels, object), _| LabelOperation::Destructure(labels, object));

    let number = token("-")
        .or_not()
        .then(text::digits(10))
        .then(just(".").ignore_then(text::digits(10)).or_not())
        .to_slice()
        .padded()
        .try_map(|s: &str, span| s.parse::<f64>().map_err(|e| Rich::custom(span, e)))
        .map_with(|value, _| Number {
            value: NumberState::Const(NumberValue::Float(value)),
            bit_width: NumberBitWidth::Unresolved,
            storage_type: StorageType::Unresolved,
        });

    let array_lit = ();
    let structure = label_def
        .clone()
        .separated_by(token(","))
        .collect::<Vec<(&str, Option<Expression>)>>()
        .delimited_by(token("{"), token("}"))
        .map_with(|members, _| {
            // TODO parse type_value + speculative(?)
            Expression::Structure(
                None,
                members
                    .iter()
                    .map(|(label, value)| {
                        (
                            (*label).into(),
                            false,
                            value
                                .as_ref()
                                .map(|v: &Expression| v.clone())
                                .unwrap_or_else(|| Expression::Label(label.to_string())),
                        )
                    })
                    .collect::<Vec<(String, bool, Expression)>>(),
            )
        });

    let match_expr = token("match")
        .ignore_then(expression.clone())
        .then(
            expression
                .clone()
                .clone()
                .then_ignore(token("=>"))
                .then(expression.clone())
                .labelled("match branch")
                .separated_by(token(","))
                .collect::<Vec<(Expression, Expression)>>()
                .delimited_by(token("{"), token("}")),
        )
        .map_with(|(value, branches), _| Expression::Match(Box::new(value), branches));

    let fn_expr = token("fn")
        .ignore_then(
            label_def
                .separated_by(token(","))
                .collect::<Vec<_>>()
                .delimited_by(token("("), token(")")),
        )
        .then(token("=>").ignore_then(expression.clone()).or_not())
        .then(fn_def.clone().delimited_by(token("{"), token("}")))
        .map_with(
            |((params, return_type_value), (label_ops, return_value)), _| {
                Expression::ConstructFunction(
                    params
                        .into_iter()
                        .map(|(label, type_value)| {
                            (
                                label.to_string(),
                                type_value.unwrap_or(Expression::Value(Value::Any)),
                            )
                        })
                        .collect(),
                    return_type_value
                        .map(|c| Box::new(c))
                        .unwrap_or(Box::new(Expression::Value(Value::Any))),
                    label_ops,
                    return_value,
                )
            },
        );

    let atom = choice((
        fn_expr,
        match_expr,
        structure,
        ident.map(|label: &str| Expression::Label(label.into())),
        number.map_with(|n, _| Expression::Value(Value::Number(n))),
    ));

    let mut call = Recursive::declare();
    call.define(
        atom.clone().foldl_with(
            call.clone()
                .separated_by(token(","))
                .collect()
                .delimited_by(token("("), token(")"))
                .repeated(),
            |function, args, _e| Expression::Call(Box::new(function), args),
        ),
    );

    let pipeline = ();
    expression.define(call.padded());

    // returns (operation, value)
    fn_def.define(
        choice((label_stmt, destructure))
            .repeated()
            //.separated_by(token(";"))
            .collect()
            .then(expression)
            .map_with(|(lo, r): (Vec<LabelOperation>, Expression), _| (lo, Box::new(r))),
    );

    fn_def.map(|(operations, value)| Function {
        params: vec![],
        returns: Box::new(Value::Any),
        defintion: FunctionDefinition::Closure { operations, value },
    })
}
