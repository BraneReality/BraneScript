pub mod ast;

use chumsky::{
    IterParser, Parser,
    error::Rich,
    extra,
    prelude::{Recursive, choice, just},
    span::SimpleSpan,
    text,
};

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

    let ident = text::unicode::ident().to_slice().padded().labelled("label");

    let label_def = ident
        .clone()
        .then(token(":").ignore_then(expression.clone()).or_not())
        .map(|v: (&str, Option<Expression>)| v);

    let label_stmt = label_def
        .clone()
        .then_ignore(token("="))
        .then(expression.clone())
        .map(
            |((label, type_value), value): ((&str, Option<Expression>), Expression)| {
                LabelOperation::Label(label.to_string(), type_value, value)
            },
        )
        .labelled("label stmt");

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
        .map_with(|(labels, object), _| LabelOperation::Destructure(labels, object))
        .labelled("destructure");

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
        })
        .labelled("number");

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
        })
        .labelled("structure");

    let match_expr = token("match")
        .ignore_then(expression.clone())
        .then(
            expression
                .clone()
                .separated_by(token(","))
                .collect::<Vec<Expression>>()
                .delimited_by(token("{"), token("}")),
        )
        .map_with(|(value, branches), _| Expression::Match(Box::new(value), branches))
        .labelled("match");

    let fn_expr = label_def
        .clone()
        .separated_by(token(","))
        .collect::<Vec<_>>()
        .delimited_by(token("("), token(")"))
        .then_ignore(token("=>"))
        .then(expression.clone().or_not())
        .then(fn_def.clone().delimited_by(token("{"), token("}")))
        .labelled("function definition")
        .map_with(
            |((params, return_type_value), (label_ops, return_value)), _| {
                Expression::ConstructFunction(
                    params
                        .into_iter()
                        .map(|(label, type_value)| {
                            (
                                label.to_string(),
                                type_value.unwrap_or(Expression::Ty(Ty::Any)),
                            )
                        })
                        .collect(),
                    return_type_value
                        .map(|c| Box::new(c))
                        .unwrap_or(Box::new(Expression::Ty(Ty::Any))),
                    label_ops,
                    return_value,
                )
            },
        );

    let pipe_expr = label_def
        .clone()
        .separated_by(token(","))
        .collect::<Vec<_>>()
        .delimited_by(token("("), token(")"))
        .then_ignore(token("=>"))
        .then(expression.clone().or_not())
        .then(
            expression
                .clone()
                .separated_by(token(","))
                .collect()
                .delimited_by(token("["), token("]")),
        )
        .labelled("pipeline definition")
        .map_with(|((params, return_type_value), segments), _| {
            Expression::ConstructPipeline(
                params
                    .into_iter()
                    .map(|(label, type_value)| {
                        (
                            label.to_string(),
                            type_value.unwrap_or(Expression::Ty(Ty::Any)),
                        )
                    })
                    .collect(),
                return_type_value
                    .map(|c| Box::new(c))
                    .unwrap_or(Box::new(Expression::Ty(Ty::Any))),
                segments,
            )
        });

    let atom = choice((
        fn_expr,
        pipe_expr,
        match_expr,
        structure,
        ident.map_with(|label: &str, _| Expression::Label(label.into())),
        number.map_with(|n, _| Expression::Ty(Ty::Number(n))),
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

    expression.define(call.labelled("expression").padded().map(|e: Expression| e));

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
        returns: Box::new(Ty::Any),
        defintion: FunctionDefinition::Closure { operations, value },
    })
}
