#![allow(clippy::too_many_lines)]

use crate::ast::{self, BinaryOp, ComparisonOp, Expression, Item, MathOp, Number, Value};
use crate::convenience_types::{Error, ParserInput, Span, Spanned};
use crate::error::Pattern;
use crate::util_parsers::{
    generics_parser, ident_parser_fallback, in_paren_list, name_parser, newline,
    refutable_pattern, separator, type_ident_parser, type_ident_parser_fallback, type_parser,
    var_name,
};
use crate::{empty_span, Token, ParseError};
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

    // The recursive expression Part
    let x: Recursive<
        dyn Parser<
            chumsky::input::SpannedInput<Token, Span, &[(Token, Span)]>,
            Spanned<Expression>,
            extra::Full<crate::ParseError, (), &str>,
        >,
    > = recursive(|expression| {
        let delim_block = stmt
            .separated_by(newline())
            .collect::<Vec<_>>()
            .or_not()
            .map(|a| a.unwrap_or_default())
            .then(
                expression
                    .clone()
                    .or_not()
                    .map(|n| n.unwrap_or((Expression::Unit, empty_span()))),
            )
            .delimited_by(
                just(Token::Lbracket).delimited_by(separator(), separator()),
                separator().then(just(Token::Rbracket)),
            )
            .map(ast::extract_idents)
            .labelled("Code block");
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
        let struct_construction = type_ident_parser_fallback()
            .then(
                name_parser()
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
            .then(list.clone().map_with(|a, ctx| (a, ctx.span())).or_not())
            .map(|args| {
                Expression::Value(Value::Enum {
                    variant: args.0.clone(),
                    fields: args
                        .1
                        .unwrap_or((vec![], Span::new(args.0 .1.end, args.0 .1.end))),
                })
            })
            .labelled("Object construction");

        // Atom which is the smallest expression. Basically anything that a method can be called on
        let atom = choice((
            value(),
            struct_construction,
            enum_construction,
            delim_block,
            list.clone().map(Value::Tuple).map(Expression::Value),
            ident.clone().map(Expression::Ident),
        ))
        .labelled("Atom")
        .as_context()
        .map_with(|expr, span| (expr, span.span()));

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
                    .then(list.clone())
                    .repeated(),
                |func: Spanned<Expression>, (name, args)| {
                    let span = func.1.start..func.1.end;
                    (
                        Expression::MethodCall(Box::new(func), name, args),
                        span.into(),
                    )
                },
            )
            .boxed()
            .labelled("method call");
        let field_access = method_call
            .clone()
            .foldl(
                just(Token::Dot).ignore_then(ident.clone()).repeated(),
                |func, name| {
                    let span = func.1.start..func.1.end;
                    (Expression::FieldAccess(Box::new(func), name), span.into())
                },
            )
            .boxed()
            .labelled("method call");

        let unary = just(Token::Bang)
            .map_with(|a, ctx| (a, ctx.span()))
            .repeated()
            .foldr(field_access.clone(), |a, ctx| {
                let span = a.1.start..ctx.1.end;
                (Expression::UnaryBool(Box::new(ctx)), span.into())
            });
        let op = just(Token::Bang);
        let not = op
            .or_not()
            .then(unary)
            .clone()
            .map_with(|(op, a), ctx| {
                if let Some(_) = op {
                    (Expression::UnaryBool(Box::new(a)), ctx.span())
                } else {
                    a
                }
            })
            .labelled("not")
            .as_context();
        // Product ops (multiply and divide) have equal precedence
        let op = just(Token::Mul)
            .to(MathOp::Mul)
            .or(just(Token::Slash).to(MathOp::Div));
        let product = not
            .clone()
            .foldl(op.then(not).repeated(), |a, (op, b)| {
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
            .boxed()
            .labelled("sum")
            .as_context();

        let logical = {
            let op = select! {
                Token::And => BinaryOp::And,
                Token::Or => BinaryOp::Or,
            };
            sum.clone().foldl(
                op.then(sum.clone()).repeated(),
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
                Token::Gte => ComparisonOp::Gte,
                Token::Lte => ComparisonOp::Lte,
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
        .boxed()
        .labelled("comparison")
        .as_context();

        /*
        new[C](window, a) #(Window, int) -> C = {
            match a:
              Some(_) -> 4,
              Dude {bruv= x, _} -> {_ = println("69")},
              _ -> 1,
        }*/
        let r#match = just(Token::Match)
            .ignore_then(expression.clone())
            .recover_with(via_parser(nested_delimiters(
                Token::Match,
                Token::Colon,
                [
                    (Token::Lbracket, Token::Rbracket),
                    (Token::Lparen, Token::Semicolon),
                ],
                |span| (Expression::ParserError, span),
            )))
            .then_ignore(just(Token::Colon).then(separator()))
            .then(
                refutable_pattern()
                    .then_ignore(just(Token::Arrow))
                    .then(
                        expression
                            .clone()
                            .recover_with(via_parser(nested_delimiters(
                                Token::Arrow,
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
            .then_ignore(just(Token::Semicolon))
            .map_with(|(condition, arms), ctx| {
                (
                    Expression::Match {
                        condition: Box::new(condition),
                        arms,
                    },
                    ctx.span(),
                )
            });
        choice((
            comp.labelled("line expression").as_context().boxed(),
            r#match,
        ))
        .boxed()
    });
    x
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
    choice((number, bool, string))
}

// <Function> ::= <VarName> <Generics>? "(" <Names>? ")" "#" <FunctionType> "= " <Expression>
pub fn function_definition<'tokens, 'src: 'tokens, T>(
    block: T,
) -> (impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,         // Input
    Spanned<ast::FunctionDeclaration>, // Output
    Error<'tokens>,                     // Error Type
> + Clone)
where
    T: Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Expression>, Error<'tokens>> + Clone, // Statement
{
    var_name()
        .then(generics_parser().or_not())
        .then(in_paren_list(var_name()))
        .then_ignore(just(Token::Hashtag))
        .then(
            type_parser()
            .try_map(|a, em| {
                match a.0 {
                    ast::Type::FunctionType(_,_) => Ok(a),
                    b =>
                     Err(ParseError::expected_found_help(em, vec![Pattern::Label("function type")], Some(format!("{:?}",b)), "Functions can not be anotated by regular types. Try function type syntax (int, int) -> int".to_owned()))
                    
                }
                
            })
    )
        .then(block)
        .map_with(|((((name, generics), arguments), fn_type), body), c| {
            (
                ast::FunctionDeclaration {
                    name,
                    generics,
                    arguments,
                    type_: fn_type,
                    body,
                },
                c.span(),
            )
        })
}
