use crate::ast::{
    EnumDeclaration, EnumVariantDeclaration, Expression, FunctionDeclaration, Impl, Import, Item,
    StructDeclaration, StructField, Trait, TraitFns, Type, VariableDeclaration,
};
use crate::convenience_parsers::{name_parser, separator, type_parser};
use crate::convenience_types::{Error, ParserInput, Span, Spanned, StrId};
use crate::lexer::Token;
use crate::util_parsers::{
    extra_delimited, generics_parser, ident_parser_fallback, in_paren_list, irrefutable_pattern,
    newline, type_ident_parser_fallback, type_name_parser,
};

use chumsky::prelude::*;

pub fn item<'tokens, 'src: 'tokens, T>(
    block: T,
) -> (impl Parser<'tokens, ParserInput<'tokens, 'src>, Item, Error<'tokens>> + Clone)
where
    T: Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Expression>, Error<'tokens>>
        + Clone
        + 'tokens,
{
    let declarations = choice((
        import().map(Item::Import).labelled("Import").as_context(),
        assingment(block.clone())
            .map(Item::Assingment)
            .labelled("Assignment")
            .as_context(),
        function_definition(block.clone())
            .map(Item::Function)
            .labelled("Function")
            .as_context(),
        crate::expression::function_definition(block.clone())
            .map(Item::AlternateSyntaxFunction)
            .labelled("Function")
            .as_context(),
        trait_parser()
            .map(Item::Trait)
            .labelled("Trait")
            .as_context(),
        enum_parser().map(Item::Enum).labelled("Enum").as_context(),
        struct_parser()
            .map(Item::Struct)
            .labelled("Struct")
            .as_context(),
        impl_parser(block.clone())
            .map_with(|a, ctx| (a, ctx.span()))
            .map(Item::ImplBlock)
            .labelled("Struct")
            .as_context(),
    ));
    declarations
}
pub fn function_definition<'tokens, 'src: 'tokens, T>(
    block: T,
) -> (impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,   // Input
    Spanned<FunctionDeclaration>, // Output
    Error<'tokens>,               // Error Type
> + Clone)
where
    T: Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Expression>, Error<'tokens>> + Clone, // Statement
{
    let block = extra_delimited(block).labelled("Code block").as_context();
    let arguments = choice((
        just(Token::Self_).map_with(|_, ctx| {
            (
                (StrId::from("self"), ctx.span()),
                Some((Type::Self_, ctx.span())),
            )
        }),
        name_parser().then(
            just(Token::Hashtag)
                .ignore_then(type_parser().map_with(|type_, ctx| (type_, ctx.span())))
                .or_not(),
        ),
    ))
    .map_with(|(name, b), ctx| ((b, name), ctx.span()))
    .separated_by(just(Token::Comma).then(separator()))
    .collect::<Vec<_>>()
    .labelled("arguments");
    let function = name_parser()
        .then(
            just(Token::Hashtag).ignore_then(
                type_parser()
                    .map_with(|r#type, ctx| -> (Type, Span) { (r#type, ctx.span()) })
                    .labelled("return type"),
            ),
        )
        .then_ignore(just(Token::Colon).then(separator()))
        .then(arguments)
        .then_ignore(just(Token::Semicolon).padded_by(separator()))
        .then(block.clone())
        .map_with(|(((name, return_type), arguments), block), ctx| {
            (
                FunctionDeclaration {
                    name,
                    return_type,
                    arguments,
                    body: block,
                },
                ctx.span(),
            )
        });
    function
}
pub fn struct_parser<'tokens, 'src: 'tokens>() -> (impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<StructDeclaration>, // Output
    Error<'tokens>,             // Error Type
> + Clone) {
    let struct_field = choice((
        just(Token::Self_)
            .map_with(|_, ctx| ((StrId::from("self"), ctx.span()), (Type::Self_, ctx.span()))),
        name_parser()
            .then_ignore(just(Token::Hashtag))
            .then(type_parser().map_with(|type_, ctx| (type_, ctx.span()))),
    ))
    .map_with(|(name, r#type), ctx| (StructField { name, r#type }, ctx.span()))
    .labelled("struct declaration field");
    let r#struct = just(Token::Struct)
        .ignore_then(type_name_parser())
        .then_ignore(just(Token::Colon))
        .then_ignore(separator())
        .then(
            struct_field
                .separated_by(just(Token::Comma).padded_by(separator()))
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(separator())
        .then_ignore(just(Token::Semicolon).padded_by(separator()))
        .map_with(|(struct_name, fields), ctx| {
            (
                StructDeclaration {
                    name: struct_name,
                    fields,
                    impl_blocks: vec![],
                },
                ctx.span(),
            )
        });
    r#struct
}
pub fn enum_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<EnumDeclaration>, Error<'tokens>> + Clone
{
    let enum_fields = name_parser()
        .then(in_paren_list(type_parser().map_with(|a, ctx| (a, ctx.span()))).or_not())
        .padded_by(separator())
        .map_with(|(name, fields), ctx| {
            (
                EnumVariantDeclaration {
                    name,
                    fields: fields.unwrap_or(vec![]),
                },
                ctx.span(),
            )
        })
        .labelled("Enum field");

    let r#enum = just(Token::Enum)
        .ignore_then(type_name_parser())
        .then_ignore(just(Token::Colon))
        .then_ignore(separator())
        .then(
            enum_fields
                .separated_by(just(Token::Comma).then_ignore(separator()))
                .allow_trailing()
                .collect::<Vec<(EnumVariantDeclaration, Span)>>(),
        )
        .then_ignore(separator())
        .then_ignore(just(Token::Semicolon).padded_by(separator()))
        .map_with(|(name, variants), ctx| (EnumDeclaration { name, variants }, ctx.span()));
    r#enum
}

pub fn import<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Import>, Error<'tokens>> + Clone {
    let import = name_parser()
        // FAT TODO: UTILIZE NAME GIVEN TO MODULE
        .then_ignore(just(Token::Assign))
        .then_ignore(just(Token::Import))
        .then(
            name_parser()
                .clone()
                .separated_by(just(Token::Slash))
                .collect(),
        )
        .map_with(|(name, module), ctx| (Import(name, module), ctx.span()));
    import
}
pub fn assingment<'tokens, 'src: 'tokens, T>(
    expr: T,
) -> (impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,   // Input
    Spanned<VariableDeclaration>, // Output
    Error<'tokens>,               // Error Type
> + Clone)
where
    T: Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Expression>, Error<'tokens>> + Clone,
{
    let assignment = irrefutable_pattern()
        .then(just(Token::Assign).ignore_then(expr))
        .map_with(|(name, val), ctx| -> (VariableDeclaration, Span) {
            (VariableDeclaration(name, val), ctx.span())
        });
    assignment
}
pub fn trait_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Trait>, Error<'tokens>> + Clone {
    let fns = name_parser()
        .then(
            just(Token::Hashtag)
                .ignore_then(type_parser().map_with(|ty, ctx| (ty, ctx.span())))
                .or_not(),
        )
        .then(
            type_parser()
                .map_with(|a, ctx| (a, ctx.span()))
                .separated_by(just(Token::Comma).then(separator()))
                .collect()
                .delimited_by(
                    just(Token::Colon).padded_by(separator()),
                    just(Token::Semicolon).padded_by(separator()),
                ),
        )
        .map_with(|((name, ret), args), ctx| (TraitFns(name, args, ret), ctx.span()));
    let r#trait = just(Token::Trait)
        .ignore_then(generics_parser().or_not())
        .then(type_name_parser())
        .then(
            fns.separated_by(just(Token::Comma).padded_by(separator()))
                .collect()
                .delimited_by(
                    just(Token::Colon).padded_by(separator()),
                    just(Token::Semicolon).padded_by(separator()),
                ),
        );
    return r#trait.map_with(|((g, n), fns), ctx| (Trait(g, n, fns), ctx.span()));
}
pub fn impl_parser<'tokens, 'src: 'tokens, T>(
    expr: T,
) -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Impl,                       // Output
    Error<'tokens>,             // Error Type
> + Clone
where
    T: Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Expression>, Error<'tokens>> + Clone,
{
    let impl_no_trait = just(Token::Impl)
        .ignore_then(type_ident_parser_fallback())
        .map(|a| ((a.clone(), None), a.1));
    let trait_impl = just(Token::Impl)
        .ignore_then(type_ident_parser_fallback())
        .then_ignore(just(Token::For))
        .then(ident_parser_fallback())
        .map_with(|(a, b), c| ((a, Some(b)), c.span()));
    let impl_block = choice((impl_no_trait, trait_impl))
        .then_ignore(just(Token::Colon))
        .then(
            function_definition(expr)
                .separated_by(newline())
                .allow_leading()
                .allow_trailing()
                .collect::<Vec<Spanned<FunctionDeclaration>>>(),
        )
        .then_ignore(just(Token::Semicolon).padded_by(separator()))
        .map(|(a, fns)| Impl {
            impl_for: a.0 .0,
            impl_what: a.0 .1,
            fns,
        })
        .labelled("Impl block");
    impl_block
}
