use crate::ast::{self, Ident, Name, Pattern, Type};
use crate::convenience_types::{Error, ParserInput, Spanned, StrId};
use crate::expression::value;
use crate::{ParseError, Token};
use chumsky::prelude::*;

pub fn name_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    StrId,                      // Output
    Error<'tokens>,             // Error Type
> + Clone {
    select! { Token::Ident(ident) => ident }.labelled("Name")
}
pub fn generic_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    ast::Generic,               // Output
    Error<'tokens>,             // Error Type
> + Clone {
    just(Token::Asterisc)
        .ignore_then(name_parser().map_with(|a, ctx| (a, ctx.span())))
        .then(
            just(Token::Hashtag)
                .ignore_then(name_parser().map_with(|a, ctx| (a, ctx.span())))
                .separated_by(just(Token::Plus))
                .collect(),
        )
        .map(|a| ast::Generic(a))
}
pub fn type_name_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    StrId,                      // Output
    Error<'tokens>,             // Error Type
> + Clone {
    select! { Token::Ident(ident) => ident }
        .validate(|v, ctx, emitter| {
            if !v.as_str().chars().next().is_some_and(char::is_uppercase) {
                emitter.emit(ParseError::expected_found_help(
                    ctx.span(),
                    vec![crate::error::Pattern::Label("Type name")],
                    Some("variable name".to_owned()),
                    "Consider making this Uppercase".to_owned(),
                ));
            }
            v
        })
        .labelled("Name")
}
pub fn ident_parser_fallback<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Ident,                      // Output
    Error<'tokens>,             // Error Type
> + Clone {
    select! { Token::Ident(ident) => ident }
        .validate(|v, ctx, emitter| {
            if !v.as_str().chars().next().is_some_and(char::is_lowercase) {
                emitter.emit(ParseError::expected_found_help(
                    ctx.span(),
                    vec![crate::error::Pattern::Label("Identifier")],
                    Some("Type ident".to_owned()),
                    "Consider making this lowercase".to_owned(),
                ));
            }
            v
        })
        .map_with(|a, ctx| (a, ctx.span()))
        .separated_by(just(Token::DoubleColon))
        .at_least(1)
        .collect()
        .map(|a| ast::Ident(a))
        .labelled("Identifier")
}
/// ONLY USE WHEN IDENT_PARSER IS NOT VALID
pub fn type_ident_parser_fallback<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Ident,                      // Output
    Error<'tokens>,             // Error Type
> + Clone {
    select! { Token::Ident(ident) => ident }
        .validate(|v, ctx, emitter| {
            if !v.as_str().chars().next().is_some_and(char::is_uppercase) {
                emitter.emit(ParseError::expected_found_help(
                    ctx.span(),
                    vec![crate::error::Pattern::Label("Type ident")],
                    Some("variable Ident".to_owned()),
                    "Consider making this uppercase".to_owned(),
                ));
            }
            v
        })
        .map_with(|a, ctx| (a, ctx.span()))
        .separated_by(just(Token::DoubleColon))
        .at_least(1)
        .collect()
        .map(ast::Ident)
        .labelled("Uppercase type ident")
}
pub fn type_ident_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Ident,                      // Output
    Error<'tokens>,             // Error Type
> + Clone {
    select! { Token::Ident(ident) if ident.as_str().chars().next().is_some_and(char::is_uppercase) => ident }
        .map_with(|a, ctx| (a, ctx.span()))
        .separated_by(just(Token::DoubleColon))
        .at_least(1)
        .collect()
        .map(ast::Ident)
        .labelled("Uppercase type ident")
}
pub fn separator<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    (),                         // Output
    Error<'tokens>,             // Error Type
> + Clone {
    just(Token::Newline).or_not().ignored()
}
pub fn type_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Type,                       // Output
    Error<'tokens>,             // Error Type
> + Clone {
    let int = select! { Token::Integer(v) => v }.labelled("Whole AAh integer");
    recursive(|r#type| {
        let path = type_ident_parser_fallback().map_with(|a, ctx| Type::Path((a, ctx.span())));
        let primitives = select! {Token::Type(x) => x,}.labelled("primitive type");
        let tuple = r#type
            .clone()
            .map_with(|b, ctx| (b, ctx.span()))
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::Lparen), just(Token::Rparen))
            .map(Type::Tuple)
            .labelled("Tuple");
        let array = r#type
            .clone()
            .map_with(|a, b| (a, b.span()))
            .clone()
            .then_ignore(just(Token::Comma))
            .then(int)
            .delimited_by(just(Token::Lbracket), just(Token::Rbracket))
            .map(|(r#type, len)| Type::Array(Box::new(r#type), len))
            .labelled("Array");
        let function_type = just(Token::Fn)
            .ignore_then(
                just(Token::Hashtag)
                    .ignore_then(r#type.clone().map_with(|a, ctx| Box::new((a, ctx.span()))))
                    .or_not(),
            )
            .then(
                name_parser()
                    .map_with(|a, ctx| (a, ctx.span()))
                    .then_ignore(just(Token::Hashtag))
                    .then(r#type.clone().map_with(|a, ctx| (a, ctx.span())))
                    .separated_by(just(Token::Comma).then(separator()))
                    .collect::<Vec<_>>()
                    .map_with(|a, ctx| (a, ctx.span()))
                    .delimited_by(just(Token::Colon), just(Token::Semicolon)),
            )
            .map(|(ret, args)| Type::FunctionType(args, ret))
            .labelled("function type");
        choice((
            tuple,
            primitives,
            array,
            path,
            function_type,
            just(Token::Self_).to(Type::Self_),
        ))
    })
    .labelled("Type")
}

pub fn extra_delimited<'tokens, 'src: 'tokens, T, U>(
    idk: T,
) -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    U,                          // Output
    Error<'tokens>,             // Error Type
> + Clone
where
    T: Parser<'tokens, ParserInput<'tokens, 'src>, U, Error<'tokens>> + Clone, // Statement
{
    idk.delimited_by(
        just(Token::Lparen).delimited_by(separator(), separator()),
        separator().then(just(Token::Rparen)),
    )
}
pub fn newline<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    (),                         // Output
    Error<'tokens>,             // Error Type
> + Clone {
    choice((
        just(Token::Newline).ignored(),
        end().labelled("EOI"),
        just(Token::Rparen).rewind().ignored(),
        just(Token::Lparen).rewind().ignored(),
    ))
    .labelled("Separator")
}
pub fn irrefutable_pattern<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<Pattern>,           // Output
    Error<'tokens>,             // Error Type
> + Clone {
    let pattern = recursive(|pat| pattern(pat).map_with(|pat, ctx| (pat, ctx.span())));
    pattern.labelled("irrefutable pattern")
}
pub fn refutable_pattern<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<Pattern>,           // Output
    Error<'tokens>,             // Error Type
> + Clone {
    let pattern = recursive(|pat| {
        pattern(pat)
            .or(value().map_with(|a, b| (a, b.span())).map(Pattern::Value))
            .map_with(|pat, ctx| (pat, ctx.span()))
    });
    pattern.labelled("Pattern")
}
pub fn pattern<'tokens, 'src: 'tokens, T>(
    pattern: T,
) -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Pattern,                    // Output
    Error<'tokens>,             // Error Type
> + Clone
where
    T: Parser<
            'tokens,
            ParserInput<'tokens, 'src>, // Input
            Spanned<Pattern>,           // Output
            Error<'tokens>,             // Error Type
        > + Clone,
{
    let nuthing = select! { Token::Ident(ident) if ident.as_str() == "_" =>
        Name::Underscore
    };
    let name_pattern = choice((nuthing, name_parser().map(|a| Name::Name(a))));
    let tuple_destructure = pattern
        .clone()
        .separated_by(just(Token::Comma))
        .collect()
        .delimited_by(just(Token::Lparen), just(Token::Rparen));
    // DO NOT use the fallback type parser here. It consumes to eagerly
    let enum_destructure = type_ident_parser().then(tuple_destructure.clone().or_not());
    let struct_destructure = type_ident_parser_fallback().then(
        name_pattern
            .clone()
            .map_with(|pat, ctx| (pat, ctx.span()))
            .then_ignore(just(Token::Hashtag))
            .then(pattern.clone())
            .separated_by(just(Token::Comma))
            .collect()
            .delimited_by(just(Token::Colon), just(Token::Semicolon)),
    );
    let array_destructure = pattern
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .then_ignore(just(Token::DoubleDot))
        .then(name_pattern.clone())
        .delimited_by(just(Token::Lbucket), just(Token::Rbucket));
    choice((
        struct_destructure
            .map(|(name, b)| Pattern::Struct(name, b))
            .labelled("Struct destructure"),
        enum_destructure
            .map(|(name, pat)| Pattern::Enum(name, pat.unwrap_or(vec![])))
            .labelled("Enum destructure"),
        tuple_destructure
            .map(Pattern::Tuple)
            .labelled("Tuple destructure"),
        array_destructure
            .map(|(pats, end)| Pattern::Array(pats, end))
            .labelled("Array destructure"),
        name_pattern.map(Pattern::Name).labelled("name"),
    ))
}
