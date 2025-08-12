pub mod ast;
use ast::*;

use branec_source::Span;
use chumsky::prelude::*;
use chumsky::{IterParser, Parser, error::Rich, extra, span::SimpleSpan, text};

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
    just(text).labelled(text).padded().to_slice()
}

pub fn string_literal<'src, M>() -> impl Parser<
    'src,
    chumsky::input::MappedSpan<Span, &'src str, M>,
    String,
    extra::Full<Rich<'src, char, Span>, (), ()>,
> + Clone
where
    M: 'static + Fn(SimpleSpan) -> Span,
{
    just('"')
        .ignore_then(
            choice((
                escape_sequence(),
                none_of("\"\\").map(|c| c), // Any character except quote and backslash
            ))
            .repeated()
            .collect(),
        )
        .then_ignore(just('"'))
}

/// Parse all types of escape sequences
fn escape_sequence<'src, M>() -> impl Parser<
    'src,
    chumsky::input::MappedSpan<Span, &'src str, M>,
    char,
    extra::Full<Rich<'src, char, Span>, (), ()>,
> + Clone
where
    M: 'static + Fn(SimpleSpan) -> Span,
{
    just('\\').ignore_then(choice((
        // Single character escapes
        just('n').to('\n'),
        just('r').to('\r'),
        just('t').to('\t'),
        just('\\').to('\\'),
        just('/').to('/'),
        just('"').to('"'),
        just('0').to('\0'),
        just('a').to('\x07'), // Bell
        just('b').to('\x08'), // Backspace
        just('f').to('\x0C'), // Form feed
        just('v').to('\x0B'), // Vertical tab
        // Hexadecimal escape \xHH
        just('x')
            .ignore_then(hex_digit().repeated().exactly(2).collect())
            .try_map(|hex_str: String, span| {
                u8::from_str_radix(&hex_str, 16)
                    .map(|n| n as char)
                    .map_err(|_| Rich::custom(span, "Invalid hex escape sequence"))
            }),
        // Unicode escape \u{HHHHHH}
        just('u')
            .ignore_then(just('{'))
            .ignore_then(hex_digit().repeated().at_least(1).at_most(6).collect())
            .then_ignore(just('}'))
            .try_map(|hex_str: String, span| {
                char::from_u32(
                    u32::from_str_radix(&hex_str, 16).map_err(|_| {
                        Rich::custom(span.clone(), "Invalid unicode escape sequence")
                    })?,
                )
                .ok_or_else(|| Rich::custom(span, "Invalid unicode escape sequence"))
            }),
        // Unicode escape \uHHHH (4 digit form)
        just('u')
            .ignore_then(hex_digit().repeated().exactly(4).collect())
            .try_map(|hex_str: String, span| {
                char::from_u32(
                    u32::from_str_radix(&hex_str, 16).map_err(|_| {
                        Rich::custom(span.clone(), "Invalid unicode escape sequence")
                    })?,
                )
                .ok_or_else(|| Rich::custom(span, "Invalid unicode escape sequence"))
            }),
        // Unicode escape \UHHHHHHHH (8 digit form)
        just('U')
            .ignore_then(hex_digit().repeated().exactly(8).collect())
            .try_map(|hex_str: String, span| {
                char::from_u32(
                    u32::from_str_radix(&hex_str, 16).map_err(|_| {
                        Rich::custom(span.clone(), "Invalid unicode escape sequence")
                    })?,
                )
                .ok_or_else(|| Rich::custom(span, "Invalid unicode escape sequence"))
            }),
        // Octal escape \ooo (1-3 octal digits)
        octal_digit()
            .repeated()
            .at_least(1)
            .at_most(3)
            .collect()
            .try_map(|octal_str: String, span| {
                let n = u32::from_str_radix(&octal_str, 8)
                    .map_err(|_| Rich::custom(span.clone(), "Invalid octal escape sequence"))?;
                if n <= 0x10FFFF {
                    char::from_u32(n).ok_or_else(|| Rich::custom(span, "Invalid unicode codepoint"))
                } else {
                    Err(Rich::custom(span, "Octal value too large"))
                }
            }),
    )))
}

/// Parse a hexadecimal digit
fn hex_digit<'src, M>() -> impl Parser<
    'src,
    chumsky::input::MappedSpan<Span, &'src str, M>,
    char,
    extra::Full<Rich<'src, char, Span>, (), ()>,
> + Clone
where
    M: 'static + Fn(SimpleSpan) -> Span,
{
    one_of("0123456789abcdefABCDEF")
}

/// Parse an octal digit
fn octal_digit<'src, M>() -> impl Parser<
    'src,
    chumsky::input::MappedSpan<Span, &'src str, M>,
    char,
    extra::Full<Rich<'src, char, Span>, (), ()>,
> + Clone
where
    M: 'static + Fn(SimpleSpan) -> Span,
{
    one_of("01234567")
}

pub fn parser<'src, M>() -> impl Parser<
    'src,
    chumsky::input::MappedSpan<Span, &'src str, M>,
    Vec<Def>,
    extra::Full<Rich<'src, char, Span>, (), ()>,
>
where
    M: 'static + Fn(SimpleSpan) -> Span,
{
    let mut ty = Recursive::declare();
    let mut expr = Recursive::declare();

    let ident = text::unicode::ident()
        .to_slice()
        .padded()
        .labelled("ident")
        .map_with(|i: &str, e| Ident {
            text: i.to_string(),
            span: e.span(),
        });

    let number = token("-")
        .or_not()
        .then(text::digits(10))
        .then(just(".").ignore_then(text::digits(10)).or_not())
        .to_slice()
        .padded()
        .try_map(|s: &str, span| match s.parse() {
            Ok(f) => Ok(Literal {
                kind: LiteralKind::Int(f),
                span,
            }),
            Err(_) => match s.parse() {
                Ok(f) => Ok(Literal {
                    kind: LiteralKind::Float(f),
                    span,
                }),
                Err(e) => Err(Rich::custom(span, e)),
            },
        })
        .labelled("number");

    let literal = number.or(string_literal().map_with(|value, e| Literal {
        kind: LiteralKind::String(value),
        span: e.span(),
    }));

    let template_param = ident.clone().map(|ident| TemplateParam(ident));
    let template_arg = ty.clone().map(|t| TemplateArg(t));

    let path_segment = ident
        .then(
            template_arg
                .clone()
                .repeated()
                .collect()
                .delimited_by(token("<"), token(">"))
                .or_not(),
        )
        .map(|(ident, args)| PathSegment {
            ident,
            template_args: args.unwrap_or_default(),
        });

    let path = path_segment
        .clone()
        .separated_by(token("."))
        .collect()
        .map_with(|segments, e| Path {
            segments,
            span: e.span(),
        });

    let native_ty = choice((
        token("i8").map(|_| NativeTy::I8),
        token("i16").map(|_| NativeTy::I16),
        token("i32").map(|_| NativeTy::I32),
        token("i64").map(|_| NativeTy::I64),
        token("u8").map(|_| NativeTy::U8),
        token("u16").map(|_| NativeTy::U16),
        token("u32").map(|_| NativeTy::U32),
        token("u64").map(|_| NativeTy::U64),
        token("f32").map(|_| NativeTy::F32),
        token("f64").map(|_| NativeTy::F64),
        token("bool").map(|_| NativeTy::Bool),
        token("char").map(|_| NativeTy::Char),
        token("string").map(|_| NativeTy::String),
    ));

    let field_def = ident.clone().then_ignore(token(":")).then(ty.clone());

    ty.define(
        choice((
            native_ty.map(TyKind::Native),
            path.clone().map(|path| TyKind::Path(path)),
            token("*")
                .ignore_then(token("mut").or_not())
                .then(choice((
                    ty.clone().map(|ty| Some(Box::new(ty))),
                    token("void").map(|_| None),
                )))
                .map(|(is_mut, ty)| TyKind::Ptr(is_mut.is_some(), ty)),
            token("mut")
                .or_not()
                .then(choice((
                    ty.clone().map(|ty| Some(Box::new(ty))),
                    token("void").map(|_| None),
                )))
                .delimited_by(token("["), token("]"))
                .map(|(is_mut, ty)| TyKind::Slice(is_mut.is_some(), ty)),
            ty.clone()
                .map(|ty| Box::new(ty))
                .separated_by(token(","))
                .collect()
                .delimited_by(token("("), token(")"))
                .map(|fields| TyKind::Tuple(fields)),
            field_def
                .clone()
                .map(|(i, ty)| (i, Box::new(ty)))
                .separated_by(token(","))
                .collect()
                .delimited_by(token("{"), token("}"))
                .map(|fields| TyKind::Struct(fields)),
        ))
        .map_with(|kind, e| Ty {
            kind,
            span: e.span(),
        }),
    );

    let array_expr = expr
        .clone()
        .separated_by(token(","))
        .collect()
        .delimited_by(token("["), token("]"))
        .map(|fields| ExprKind::Array(fields))
        .labelled("tuple expression");

    let tuple_expr = expr
        .clone()
        .separated_by(token(","))
        .at_least(1)
        .allow_trailing()
        .collect()
        .delimited_by(token("("), token(")"))
        .map(|fields| ExprKind::Tuple(fields))
        .labelled("tuple expression");

    let struct_expr = path
        .clone()
        .or_not()
        .then(
            ident
                .clone()
                .then_ignore(token(":"))
                .then(expr.clone())
                .separated_by(token(","))
                .collect::<Vec<(Ident, Expr)>>()
                .delimited_by(token("{"), token("}")),
        )
        .map(|(ident, fields)| ExprKind::Struct(ident, fields))
        .labelled("struct expression");

    let atom = choice((
        literal.map(|l| ExprKind::Literal(l)),
        array_expr,
        tuple_expr,
        struct_expr,
        path.clone().map(|p| ExprKind::Path(p)),
        token("&")
            .ignore_then(expr.clone())
            .map(|expr| ExprKind::Ref(Box::new(expr))),
        token("*")
            .ignore_then(expr.clone())
            .map(|expr| ExprKind::Deref(Box::new(expr))),
    ))
    .map_with(|kind: ExprKind, e| Expr {
        span: e.span(),
        kind,
    })
    .or(expr.clone().delimited_by(token("("), token(")")));

    let mut field = Recursive::declare();
    field.define(atom.foldl_with(
        token(".").ignore_then(path_segment).repeated(),
        |expr, field, e| Expr {
            kind: ExprKind::Field(Box::new(expr), field),
            span: e.span(),
        },
    ));

    let mut call = Recursive::declare();
    call.define(
        field.clone().foldl_with(
            expr.clone()
                .separated_by(token(","))
                .collect()
                .delimited_by(token("("), token(")"))
                .repeated(),
            |function, args, e| Expr {
                kind: ExprKind::Call(Box::new(function), args),
                span: e.span(),
            },
        ),
    );

    expr.define(call.labelled("expression").padded());

    let mut stmt = Recursive::declare();
    let block = stmt
        .clone()
        .repeated()
        .collect()
        .delimited_by(token("{"), token("}"))
        .map_with(|statements, e| Block {
            statements,
            span: e.span(),
        });

    let assign = expr
        .clone()
        .then_ignore(token("="))
        .then(expr.clone())
        .then_ignore(token(";"))
        .map(|(dest, src)| StmtKind::Assign(dest, src));

    let variable_def = ty
        .clone()
        .then(ident.clone())
        .then_ignore(token("="))
        .then(expr.clone())
        .then_ignore(token(";"))
        .map(|((ty, ident), expr)| StmtKind::VariableDef(ty, ident, expr));

    let if_stmt = token("if")
        .ignore_then(expr.clone().delimited_by(token("("), token(")")))
        .then(stmt.clone())
        .then(token("else").ignore_then(stmt.clone()).or_not())
        .map(|((cond, body), else_stmt)| {
            StmtKind::If(cond, Box::new(body), else_stmt.map(|s: Stmt| Box::new(s)))
        });

    let while_stmt = token("while")
        .ignore_then(expr.clone().delimited_by(token("("), token(")")))
        .then(stmt.clone())
        .map(|(cond, body)| StmtKind::While(cond, Box::new(body)));

    let match_branch = choice((
        text::digits(10)
            .to_slice()
            .map(|c: &str| CaseKind::Int(c.parse().unwrap())),
        ident
            .clone()
            .then(ident.clone().delimited_by(token("("), token(")")).or_not())
            .map(|(enum_variant, data_label)| CaseKind::EnumVariant(enum_variant, data_label)),
    ))
    .then_ignore(token("=>"))
    .then(stmt.clone())
    .map_with(|(case, body), e| MatchBranch {
        span: e.span(),
        case,
        body,
    });

    let match_stmt = token("match")
        .ignore_then(expr.clone().delimited_by(token("("), token(")")))
        .then(
            match_branch
                .repeated()
                .collect()
                .delimited_by(token("{"), token("}")),
        )
        .map_with(|(cond, cases), e| StmtKind::Match(e.span(), cond, cases));

    stmt.define(
        choice((
            expr.clone()
                .then_ignore(token(";"))
                .map(|expr| StmtKind::Expression(expr)),
            assign,
            variable_def,
            if_stmt,
            token("return")
                .ignore_then(expr.or_not())
                .then_ignore(token(";"))
                .map(|expr| StmtKind::Return(expr)),
            while_stmt,
            match_stmt,
            block.clone().map(|b| StmtKind::Block(b)),
        ))
        .map_with(|kind, e| Stmt {
            kind,
            span: e.span(),
        }),
    );

    let template_params = template_param
        .separated_by(token(","))
        .collect()
        .delimited_by(token("<"), token(">"));

    let struct_def = token("struct")
        .ignore_then(ident.clone())
        .then(template_params.clone().or_not())
        .then(
            field_def
                .clone()
                .separated_by(token(","))
                .collect()
                .delimited_by(token("{"), token("}")),
        )
        .map_with(|((ident, template_params), fields), e| {
            DefKind::Struct(Struct {
                ident,
                span: e.span(),
                fields,
                template_params: template_params.unwrap_or_default(),
            })
        });

    let enum_def = token("enum")
        .ignore_then(ident.clone())
        .then(template_params.clone().or_not())
        .then(
            ident
                .clone()
                .then(ty.clone().delimited_by(token("("), token(")")).or_not())
                .separated_by(token(","))
                .collect()
                .delimited_by(token("{"), token("}")),
        )
        .map_with(|((ident, template_params), variants), e| {
            DefKind::Enum(Enum {
                ident,
                span: e.span(),
                variants,
                template_params: template_params.unwrap_or_default(),
            })
        });

    let function_def = token("fn")
        .ignore_then(ident.clone())
        .then(template_params.or_not())
        .then(
            field_def
                .clone()
                .separated_by(token(","))
                .collect()
                .delimited_by(token("("), token(")")),
        )
        .then(token("->").ignore_then(ty.clone()).or_not())
        .then(block.clone())
        .map_with(
            |((((ident, template_params), params), ret_ty), body), e| Function {
                span: e.span(),
                ident,
                params,
                ret_ty,
                body,
                template_params: template_params.unwrap_or_default(),
            },
        );

    let pipeline_stage = choice((
        block.clone().map(|b| PipelineStage::Block(b)),
        function_def.clone().map(|f| PipelineStage::Fn(f)),
    ));

    let pipeline_def = token("pipe")
        .ignore_then(ident.clone())
        .then(
            field_def
                .clone()
                .separated_by(token(","))
                .collect()
                .delimited_by(token("("), token(")")),
        )
        .then(token("->").ignore_then(ty.clone()).or_not())
        .then(
            pipeline_stage
                .separated_by(token(","))
                .collect()
                .delimited_by(token("["), token("]")),
        )
        .map_with(|(((ident, params), ret_ty), stages), e| {
            DefKind::Pipeline(Pipeline {
                span: e.span(),
                ident,
                params,
                ret_ty,
                stages,
            })
        });

    let mut def = Recursive::declare();

    let namespace = token("namespace")
        .ignore_then(ident.clone())
        .then(
            def.clone()
                .repeated()
                .collect()
                .delimited_by(token("{"), token("}")),
        )
        .map(|(ident, defs)| DefKind::Namespace(ident, defs));

    let link_def = token("link")
        .ignore_then(string_literal())
        .then_ignore(token(";"))
        .map_with(|link, e| {
            DefKind::Link(Ident {
                span: e.span(),
                text: link,
            })
        });

    let use_def = token("use")
        .ignore_then(path.clone())
        .then_ignore(token(";"))
        .map(|path| DefKind::Use(path));

    def.define(
        choice((
            struct_def,
            enum_def,
            function_def.map(|f| DefKind::Function(f)),
            pipeline_def,
            namespace,
            use_def,
            link_def,
        ))
        .map_with(|kind, e| Def {
            kind,
            span: e.span(),
        }),
    );

    def.repeated().collect()
}
