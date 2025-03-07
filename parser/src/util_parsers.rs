use crate::ast::{self, Generics, Ident, Name, Pattern, Type};
use crate::convenience_types::{Error, ParserInput, Spanned, StrId};
use crate::expression::value;
use crate::{ParseError, Token};
use chumsky::prelude::*;

pub fn name_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<StrId>,             // Output
    Error<'tokens>,             // Error Type
> + Clone {
    select! { Token::Ident(ident) => ident,
    Token::TypeIdent(ident) => ident
     }
    .map_with(|a, ctx| (a, ctx.span()))
    .labelled("Name")
}
pub fn var_name<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<StrId>,             // Output
    Error<'tokens>,             // Error Type
> + Clone {
    select! { Token::Ident(ident) => ident}
        .map_with(|a, ctx| (a, ctx.span()))
        .labelled("Variable Name")
}
pub fn type_name<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<StrId>,             // Output
    Error<'tokens>,             // Error Type
> + Clone {
    select! { Token::TypeIdent(ident) => ident}
        .map_with(|a, ctx| (a, ctx.span()))
        .labelled("Type Name")
}
// <Generics> ::= "[" (<TypeName> ", ")+ "]"
pub fn generics_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<ast::Generics>,     // Output
    Error<'tokens>,             // Error Type
> + Clone {
    let generic = type_name()
        .then(
            just(Token::Hashtag)
                .ignore_then(
                    type_ident_parser_fallback()
                        .separated_by(just(Token::Plus))
                        .collect(),
                )
                .or_not()
                .map(|a| a.unwrap_or_default()),
        )
        .map_with(|a, c| (ast::Generic(a), c.span()));
    generic
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect()
        .delimited_by(just(Token::Lbucket), just(Token::Rbucket))
        .map_with(|a, c| (Generics(a), c.span()))
}
pub fn type_name_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<StrId>,             // Output
    Error<'tokens>,             // Error Type
> + Clone {
    name_parser()
        .validate(|v, ctx, emitter| {
            if !v.0.as_str().chars().next().is_some_and(char::is_uppercase) {
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
    Spanned<Ident>,             // Output
    Error<'tokens>,             // Error Type
> + Clone {
    name_parser()
        .validate(|v, ctx, emitter| {
            if !v.0.as_str().chars().next().is_some_and(char::is_lowercase) {
                emitter.emit(ParseError::expected_found_help(
                    ctx.span(),
                    vec![crate::error::Pattern::Label("Identifier")],
                    Some("Type ident".to_owned()),
                    "Consider making this lowercase".to_owned(),
                ));
            }
            v
        })
        .separated_by(just(Token::DoubleColon))
        .at_least(1)
        .collect()
        .map_with(|a, ctx| (ast::Ident(a), ctx.span()))
        .labelled("Identifier")
}
/// ONLY USE WHEN IDENT_PARSER IS NOT VALID
pub fn type_ident_parser_fallback<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<Ident>,             // Output
    Error<'tokens>,             // Error Type
> + Clone {
    name_parser()
        .validate(|v, ctx, emitter| {
            if !v.0.as_str().chars().next().is_some_and(char::is_uppercase) {
                emitter.emit(ParseError::expected_found_help(
                    ctx.span(),
                    vec![crate::error::Pattern::Label("Type ident")],
                    Some("variable Ident".to_owned()),
                    "Consider making this uppercase".to_owned(),
                ));
            }
            v
        })
        .separated_by(just(Token::DoubleColon))
        .at_least(1)
        .collect()
        .map(ast::Ident)
        .map_with(|a, c| (a, c.span()))
        .labelled("Uppercase type ident")
}
pub fn type_ident_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Ident,                      // Output
    Error<'tokens>,             // Error Type
> + Clone {
    type_name()
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
    Spanned<Type>,              // Output
    Error<'tokens>,             // Error Type
> + Clone {
    let int = select! { Token::Integer(v) => v }.labelled("Whole AAh integer");
    recursive(|r#type| {
        let path = type_ident_parser_fallback()
            .map(|a| Type::Path(a))
            .map_with(|b, ctx| (b, ctx.span()));
        let primitives = select! {Token::Type(x) => x,}
            .labelled("primitive type")
            .map_with(|b, ctx| (b, ctx.span()));
        let tuple = r#type
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::Lparen), just(Token::Rparen))
            .map(Type::Tuple)
            .map_with(|b, ctx| (b, ctx.span()))
            .labelled("Tuple");
        let array = r#type
            .clone()
            .clone()
            .then_ignore(just(Token::Comma))
            .then(int)
            .delimited_by(just(Token::Lbracket), just(Token::Rbracket))
            .map(|(r#type, len)| Type::Array(Box::new(r#type), len))
            .map_with(|b, ctx| (b, ctx.span()))
            .labelled("Array");

        // (<Type> | "(" <Types> ")" ) "->" <Type>
        // Type ::
        // | Boolean
        // | Fn (Type, Type)
        let function_type = in_paren_list(r#type.clone())
            .then_ignore(just(Token::Arrow))
            .then(r#type.clone().labelled("function return type"))
            .map(|a| ast::Type::FunctionType(a.0, (Box::new(a.1 .0), a.1 .1)))
            .map_with(|a, c| (a, c.span()))
            .labelled("function_type");

        let alt = r#type
            .clone()
            .then_ignore(just(Token::Arrow))
            .then(r#type.clone().labelled("function return type"))
            .delimited_by(just(Token::Lparen), just(Token::Rparen))
            .map(|a| ast::Type::FunctionType(vec![a.0], (Box::new(a.1 .0), a.1 .1)))
            .map_with(|a, c| (a, c.span()))
            .labelled("function_type");

        choice((
            just(Token::Self_)
                .to(Type::Self_)
                .map_with(|b, ctx| (b, ctx.span())),
            path,
            array,
            primitives,
            function_type.or(alt),
            tuple,
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
pub fn in_paren_list<'tokens, 'src: 'tokens, T, U>(
    idk: T,
) -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Vec<U>,                     // Output
    Error<'tokens>,             // Error Type
> + Clone
where
    T: Parser<'tokens, ParserInput<'tokens, 'src>, U, Error<'tokens>> + Clone, // Statement
{
    extra_delimited(
        idk.separated_by(just(Token::Comma).then(separator()))
            .allow_trailing()
            .collect(),
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
pub fn refutable_pattern<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<Pattern>,           // Output
    Error<'tokens>,             // Error Type
> + Clone {
    pattern()
        .or(value().map_with(|a, b| (Pattern::Value((a, b.span())), b.span())))
        .labelled("Pattern")
}
pub fn pattern<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<Pattern>,           // Output
    Error<'tokens>,             // Error Type
> + Clone {
    recursive(|pat| {
        let nuthing = select! { Token::Ident(ident) if ident.as_str() == "_" =>
            Name::Underscore
        };
        let name_pattern = choice((nuthing, name_parser().map(|a| Name::Name(a))));
        let tuple_destructure = pat
            .clone()
            .separated_by(just(Token::Comma))
            .collect()
            .delimited_by(just(Token::Lparen), just(Token::Rparen));
        // DO NOT use the fallback type parser here. It consumes too eagerly
        let enum_destructure = type_ident_parser().then(tuple_destructure.clone().or_not());
        let struct_destructure = type_ident_parser_fallback().then(
            name_pattern
                .clone()
                .map_with(|pat, ctx| (pat, ctx.span()))
                .then_ignore(just(Token::Hashtag))
                .then(pat.clone())
                .separated_by(just(Token::Comma))
                .collect()
                .delimited_by(just(Token::Colon), just(Token::Semicolon)),
        );
        let array_destructure = pat
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
        .map_with(|a, c| (a, c.span()))
    })
}
