use crate::ast::{
    EnumDeclaration, EnumVariantDeclaration, Expression, FunctionDeclaration2, Impl, Import, Item,
    Name, StructDeclaration, StructField, Trait, TraitFns, Type, VariableDeclaration,
};
use crate::convenience_parsers::{name_parser, separator, type_parser};
use crate::convenience_types::{Error, ParserInput, Span, Spanned, StrId};
use crate::lexer::Token;
use crate::util_parsers::{
    extra_delimited, generics_parser, ident_parser_fallback, in_paren_list, newline, pattern,
    type_ident_parser_fallback, type_name_parser, var_name,
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
            .then(type_parser()),
    ))
    .map_with(|(name, r#type), ctx| (StructField { name, r#type }, ctx.span()))
    .labelled("struct declaration field");

    let r#struct = just(Token::Struct)
        .ignore_then(type_name_parser())
        .then(generics_parser().or_not())
        .then_ignore(just(Token::Colon))
        .then_ignore(separator())
        .then(
            struct_field
                .separated_by(just(Token::Comma).padded_by(separator()))
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(separator())
        .then_ignore(just(Token::Semicolon))
        .map_with(|((struct_name, generics), fields), ctx| {
            (
                StructDeclaration {
                    name: struct_name,
                    generics,
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
        .then(in_paren_list(type_parser()).or_not())
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
        .then_ignore(just(Token::Semicolon))
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
    let assignment = choice((
        var_name().map_with(|a, c| (crate::ast::Pattern::Name(Name::Name(a)), c.span())),
        just(Token::Let).ignore_then(pattern().labelled("irrefutable pattern")),
    ))
    .then(just(Token::Assign).ignore_then(expr))
    .map_with(|(name, val), ctx| -> (VariableDeclaration, Span) {
        (VariableDeclaration(name, val), ctx.span())
    });
    assignment
}
pub fn trait_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Trait>, Error<'tokens>> + Clone {
    let fns = name_parser()
        .then(just(Token::Hashtag).ignore_then(type_parser()).or_not())
        .then(
            type_parser()
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
            crate::expression::function_definition(expr)
                .separated_by(newline())
                .allow_leading()
                .allow_trailing()
                .collect::<Vec<Spanned<FunctionDeclaration2>>>(),
        )
        .then_ignore(just(Token::Semicolon))
        .map(|(a, fns)| Impl {
            impl_for: a.0 .0,
            impl_what: a.0 .1,
            fns,
        })
        .labelled("Impl block");
    impl_block
}
