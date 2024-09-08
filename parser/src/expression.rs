#![allow(clippy::too_many_lines)]
use crate::ast::{BinaryOp, Block, ComparisonOp, Expression, For, If, Item, MathOp, Number, Value};
use crate::convenience_types::{Error, ParserInput, Span, Spanned};
use crate::util_parsers::{
    extra_delimited, ident_parser_fallback, name_parser, refutable_pattern, separator,
    type_ident_parser, type_ident_parser_fallback,
};
use crate::Token;
use chumsky::prelude::*;

pub fn expression<'tokens, 'src: 'tokens, T>(
    stmt: T,
) -> (impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<Expression>,        // Output
    Error<'tokens>,             // Error Type
> + Clone)
where
    T: Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Item>, Error<'tokens>> + Clone + 'tokens,
{
    let ident = ident_parser_fallback();
    let delim_block = extra_delimited(stmt.repeated().collect::<Vec<_>>())
        .map(|items| Expression::Block(Block(items)))
        .labelled("Code block");

    // The recursive expression Part
    recursive(|expression| {
        let struct_construction = type_ident_parser_fallback()
            .map_with(|a, ctx| (a, ctx.span()))
            .then(
                name_parser()
                    .map_with(|a, ctx| (a, ctx.span()))
                    .then_ignore(just(Token::Assign))
                    .then(expression.clone())
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::Colon), just(Token::Semicolon))
                    .map_with(|a, ctx| (a, ctx.span())),
            )
            .map(|(name, args)| Expression::Value(Value::Struct { name, fields: args }))
            .labelled("Object construction");
        let enum_construction = type_ident_parser()
            .clone()
            .map_with(|a, ctx| (a, ctx.span()))
            .then(
                expression
                    .clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::Lparen), just(Token::Rparen))
                    .map_with(|a, ctx| (a, ctx.span()))
                    .or_not(),
            )
            .map(|args| {
                Expression::Value(Value::Enum {
                    variant: args.0.clone(),
                    fields: args
                        .1
                        .unwrap_or((vec![], Span::new(args.0 .1.end, args.0 .1.end))),
                })
            })
            .labelled("Object construction");
        let inline_expression = {
            // let unary = choice((just(Token::Bang).then(expression), just()))
            // A list of expressions
            let items = expression
                .clone()
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .labelled("a list of expressions");

            // A list of expressions delimited by ()
            let list = items
                .clone()
                .delimited_by(just(Token::Lparen), just(Token::Rparen))
                .recover_with(via_parser(nested_delimiters(
                    Token::Lparen,
                    Token::Rparen,
                    [(Token::Lbracket, Token::Rbracket)],
                    |span| vec![(Expression::ParserError, span)],
                )))
                .labelled("list of expressions")
                .as_context();
            // Atom which is the smallest expression.
            let atom = choice((
                value(),
                struct_construction,
                enum_construction,
                ident.clone().map(Expression::Ident),
                delim_block,
                list.clone().map(Value::Tuple).map(Expression::Value),
            ))
            .labelled("Atom")
            .as_context()
            .map_with(|expr, span| (expr, span.span()))
            .or(
                extra_delimited(expression.clone())
                    .clone()
                    // Attempt to recover anything that looks like a parenthesised expression but contains errors
                    .recover_with(via_parser(nested_delimiters(
                        Token::Lparen,
                        Token::Rparen,
                        [(Token::Lbracket, Token::Rbracket)],
                        |span| (Expression::ParserError, span),
                    )))
                    .labelled("Expression Block")
                    .as_context(), // Atoms can also just be normal expressions, but surrounded with parentheses
            );

            // Function calls have very high precedence so we prioritise them
            let call = atom
                .clone()
                .foldl(
                    list.clone()
                        .map_with(|expr, span| (expr, span.span()))
                        .repeated(),
                    |func, args| {
                        let span = Span::new(func.1.start, args.1.end);
                        (Expression::FunctionCall(Box::new(func), args.0), span)
                    },
                )
                .labelled("Function call")
                .as_context();
            let method_call = call
                .clone()
                .foldl(
                    just(Token::Dot)
                        .ignore_then(ident.clone())
                        .then(list.clone().or_not())
                        .repeated(),
                    |func: Spanned<Expression>, (name, args)| {
                        let spanend = if let Some(arg) = args.clone() {
                            arg.last()
                                .unwrap_or_else(|| {
                                    panic!("[INTERNAL ERROR] method args span {arg:#?} is empty")
                                })
                                .1
                                .end
                        } else {
                            func.1.end
                        };

                        let span = func.1.start..spanend;
                        (
                            Expression::MethodCall(
                                Box::new(func),
                                name,
                                args.map_or_else(std::vec::Vec::new, |arguments| arguments),
                            ),
                            span.into(),
                        )
                    },
                )
                .labelled("method call");

            // Product ops (multiply and divide) have equal precedence
            let op = just(Token::Mul)
                .to(MathOp::Mul)
                .or(just(Token::Slash).to(MathOp::Div));
            let product = method_call
                .clone()
                .foldl(op.then(method_call).repeated(), |a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (
                        Expression::MathOp(Box::new(a), op, Box::new(b)),
                        span.into(),
                    )
                })
                .labelled("product")
                .as_context();

            // Sum ops (add and subtract) have equal precedence
            let op = just(Token::Plus)
                .to(MathOp::Add)
                .or(just(Token::Minus).to(MathOp::Sub));
            let sum = product
                .clone()
                .foldl(op.then(product).repeated(), |a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (
                        Expression::MathOp(Box::new(a), op, Box::new(b)),
                        span.into(),
                    )
                })
                .labelled("sum")
                .as_context();

            let else_expression = sum
                .clone()
                .foldl(
                    (just(Token::Else).ignore_then(expression.clone())).repeated(),
                    |expr, else_branch| {
                        let span = expr.1.start()..else_branch.1.end();
                        (
                            Expression::Else(Box::new(expr), Box::new(else_branch)),
                            span.into(),
                        )
                    },
                )
                .labelled("Else expression")
                .as_context();

            let logical = {
                let op = select! {
                    Token::And => BinaryOp::And,
                    Token::Or => BinaryOp::Or,
                };
                else_expression.clone().foldl(
                    op.then(sum).repeated(),
                    |lhs: Spanned<Expression>, (op, rhs): (_, Spanned<Expression>)| {
                        let span = Span::new(lhs.1.start, rhs.1.end);
                        (Expression::Binary(Box::new(lhs), op, Box::new(rhs)), span)
                    },
                )
            }
            .labelled("Equality")
            .as_context();

            let comp = {
                let op = select! {
                    Token::Eq => ComparisonOp::Eq,
                    Token::Neq => ComparisonOp::Neq,
                    Token::Gt => ComparisonOp::Gt,
                    Token::Lt => ComparisonOp::Lt,
                    Token::Gte => ComparisonOp::Gt,
                    Token::Lte => ComparisonOp::Lt,
                };
                logical.clone().foldl(
                    op.then(logical).repeated(),
                    |lhs: Spanned<Expression>, (op, rhs): (_, Spanned<Expression>)| {
                        let span = Span::new(lhs.1.start, rhs.1.end);
                        (
                            Expression::Comparison(Box::new(lhs), op, Box::new(rhs)),
                            span,
                        )
                    },
                )
            }
            .labelled("comparison")
            .as_context();

            // if => "if" expr "then" expr
            let if_ = just(Token::If)
                .ignore_then(expression.clone())
                .recover_with(via_parser(nested_delimiters(
                    Token::If,
                    Token::Then,
                    [
                        (Token::Lbracket, Token::Rbracket),
                        (Token::Lparen, Token::Semicolon),
                    ],
                    |span| (Expression::ParserError, span),
                )))
                .labelled("Condition")
                .as_context()
                .then_ignore(just(Token::Then))
                .then(
                    expression
                        .clone()
                        .labelled("If block")
                        .as_context()
                        .recover_with(via_parser(nested_delimiters(
                            Token::Lparen,
                            Token::Rparen,
                            [(Token::Lbracket, Token::Rbracket)],
                            |span| (Expression::ParserError, span),
                        ))),
                )
                .map_with(|(condition, code_block), ctx| {
                    (
                        Expression::If(Box::new(If {
                            condition,
                            code_block,
                        })),
                        ctx.span(),
                    )
                })
                .labelled("if *expression*");
            let r#match = just(Token::Match)
                .ignore_then(expression.clone())
                .recover_with(via_parser(nested_delimiters(
                    Token::Match,
                    Token::If,
                    [
                        (Token::Lbracket, Token::Rbracket),
                        (Token::Lparen, Token::Semicolon),
                    ],
                    |span| (Expression::ParserError, span),
                )))
                .then_ignore(just(Token::If).then(separator()))
                .then(
                    refutable_pattern()
                        .then_ignore(just(Token::Then))
                        .then(
                            expression
                                .clone()
                                .recover_with(via_parser(nested_delimiters(
                                    Token::Then,
                                    Token::Comma,
                                    [
                                        (Token::Then, Token::Newline),
                                        (Token::Lparen, Token::Semicolon),
                                    ],
                                    |span| (Expression::ParserError, span),
                                ))),
                        )
                        .separated_by(just(Token::Comma).then(separator()))
                        .collect(),
                )
                .map_with(|(condition, arms), ctx| {
                    (
                        Expression::Match {
                            condition: Box::new(condition),
                            arms,
                        },
                        ctx.span(),
                    )
                });
            // "for" <name> "in" <expression> "then" <expression>
            let for_loop = just(Token::For)
                .ignore_then(name_parser().map_with(|a, ctx| (a, ctx.span())))
                .then_ignore(just(Token::In))
                .then(expression.clone())
                .then_ignore(just(Token::Then))
                .then(expression.clone())
                .map_with(|((name, iterator), code_block), ctx| {
                    (
                        Expression::For(Box::new(For {
                            name,
                            iterator,
                            code_block,
                        })),
                        ctx.span(),
                    )
                });

            // Comparison ops (equal, not-equal) have equal precedence
            choice((
                comp.labelled("line expression").as_context(),
                if_,
                r#match,
                for_loop,
            ))
            .boxed()
        };
        inline_expression.boxed()
    })
}
pub fn value<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Expression,                 // Output
    Error<'tokens>,             // Error Type
> + Clone {
    let int = select! { Token::Integer(v) => v }.labelled("Whole AAh integer");
    let float = select! { Token::Float(v) => v }.labelled("Floating point");
    let number = int
        .map(|int| Expression::Value(Value::Number(Number::Int(int))))
        .or(float.map(|float| {
            Expression::Value(Value::Number(Number::Float(
                float.parse::<f64>().expect("float too big :()"),
            )))
        }));
    let bool = select! {
        Token::True=> Expression::Value(Value::Bool(true)),
        Token::False => Expression::Value(Value::Bool(false))
    }
    .labelled("Boolean");
    let string =
        select! {Token::LiteralString(s) => Expression::Value(Value::String(s))}.labelled("String");
    let span = select! {Token::Span(s) => Expression::Value(Value::Span(s.start, s.end))};
    choice((number, bool, string, span))
}
