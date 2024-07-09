use crate::ast::{
    EnumDeclaration, EnumVariantDeclaration, Expression, FunctionDeclaration, Import, Item, Module,
    StructDeclaration, StructField, Trait, TraitFns, Type, VariableDeclaration,
};
use crate::convenience_parsers::{name_parser, separator, type_parser};
use crate::convenience_types::{Error, ParserInput, Span, Spanned};
use crate::lexer::Token;
use crate::util_parsers::{extra_delimited, irrefutable_pattern, newline, parameter_parser};

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
        assingment(block.clone())
            .then_ignore(newline())
            .map(Item::Assingment)
            .labelled("Assignment")
            .as_context(),
        import().map(Item::Import).labelled("Import").as_context(),
        function_definition(block.clone())
            .then_ignore(separator())
            .map(Item::Function)
            .labelled("Function")
            .as_context(),
        trait_parser()
            .then_ignore(separator())
            .map(Item::Trait)
            .labelled("Trait")
            .as_context(),
        enum_parser(block.clone())
            .then_ignore(separator())
            .map(Item::Enum)
            .labelled("Enum")
            .as_context(),
        struct_parser(block)
            .then_ignore(separator())
            .map(Item::Struct)
            .labelled("Struct")
            .as_context(),
    ));
    let module = name_parser()
        .map_with(|a, c| (a, c.span()))
        .then_ignore(just(Token::Assign))
        .then_ignore(just(Token::Module))
        .then(
            declarations
                .clone()
                .map_with(|a, c| (a, c.span()))
                .separated_by(separator())
                .collect()
                .delimited_by(
                    just(Token::Colon).delimited_by(separator(), separator()),
                    just(Token::Semicolon).delimited_by(separator(), separator()),
                ),
        )
        .map_with(|(a, b), ctx| Item::Module((Module(a, b), ctx.span())));
    choice((declarations, module))
}
// fn = name ":" (ident "#" type ,)*; type block
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
    let arguments = parameter_parser()
        .map_with(|(name, b), ctx| ((b, name), ctx.span()))
        .separated_by(just(Token::Comma).then(separator()))
        .collect::<Vec<_>>()
        .labelled("arguments");
    let function = name_parser()
        .map_with(|name, ctx| (name, ctx.span()))
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
pub fn struct_parser<'tokens, 'src: 'tokens, T>(
    expr: T,
) -> (impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<StructDeclaration>, // Output
    Error<'tokens>,             // Error Type
> + Clone)
where
    T: Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Expression>, Error<'tokens>> + Clone, // Statement
{
    let struct_field = parameter_parser()
        .map_with(|(name, r#type), ctx| (StructField { name, r#type }, ctx.span()))
        .labelled("struct declaration field");
    let impl_block = just(Token::Impl)
        .ignore_then(name_parser().or_not())
        .then_ignore(just(Token::Colon))
        .then(
            function_definition(expr)
                .separated_by(newline())
                .allow_leading()
                .collect::<Vec<Spanned<FunctionDeclaration>>>(),
        )
        .then_ignore(just(Token::Semicolon).padded_by(separator()));
    let r#struct = name_parser()
        .map_with(|a, c| (a, c.span()))
        .then_ignore(just(Token::Assign))
        .then_ignore(just(Token::Struct))
        .then_ignore(just(Token::Colon))
        .then_ignore(separator())
        .then(
            struct_field
                .separated_by(just(Token::Comma).padded_by(separator()))
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(separator())
        // .then(
        //     (impl_block)
        //         .labelled("impl block")
        //         .as_context()
        //         .separated_by(separator())
        //         .collect::<Vec<(Option<String>, Vec<Spanned<FunctionDeclaration>>)>>(),
        // )
        .then_ignore(just(Token::Semicolon).padded_by(separator()))
        .map_with(|((struct_name, fields)), ctx| {
            // let fns = fns.into_iter().fold(
            //     Vec::new(),
            //     |acc: Vec<_>, (name, fns): (Option<String>, Vec<Spanned<FunctionDeclaration>>)| {
            //         acc.into_iter()
            //             .chain(vec![name].into_iter().cycle().zip(fns))
            //             .collect::<Vec<(Option<String>, Spanned<FunctionDeclaration>)>>()
            //     },
            // );
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
pub fn enum_parser<'tokens, 'src: 'tokens, T>(
    expr: T,
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<EnumDeclaration>, Error<'tokens>> + Clone
where
    T: Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Expression>, Error<'tokens>> + Clone,
{
    let enum_fields = name_parser()
        .then(
            type_parser()
                .map_with(|a, ctx| (a, ctx.span()))
                .separated_by(just(Token::Comma).then_ignore(separator()))
                .collect()
                .delimited_by(just(Token::Lparen), just(Token::Rparen))
                .or_not(),
        )
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

    let impl_block = just(Token::Impl)
        .ignore_then(name_parser().or_not())
        .then_ignore(just(Token::Colon))
        .then(
            function_definition(expr)
                .separated_by(newline())
                .allow_leading()
                .collect::<Vec<Spanned<FunctionDeclaration>>>(),
        )
        .then_ignore(just(Token::Semicolon).padded_by(separator()))
        .labelled("Impl block");
    let r#enum = name_parser()
        .map_with(|a, c| (a, c.span()))
        .then_ignore(just(Token::Assign))
        .then_ignore(just(Token::Enum))
        .then_ignore(just(Token::Colon))
        .then_ignore(separator())
        .then(
            enum_fields
                .separated_by(just(Token::Comma).then_ignore(separator()))
                .allow_trailing()
                .collect::<Vec<(EnumVariantDeclaration, Span)>>(),
        )
        .then_ignore(separator())
        // .then(
        //     impl_block
        //         .labelled("impl block")
        //         .as_context()
        //         .separated_by(separator())
        //         .collect::<Vec<(Option<String>, Vec<Spanned<FunctionDeclaration>>)>>(),
        // )
        .then_ignore(just(Token::Semicolon).padded_by(separator()))
        .map_with(|((name, variants)), ctx| {
            // let impl_blocks = fns.into_iter().fold(
            //     Vec::new(),
            //     |acc: Vec<_>, (name, fns): (Option<String>, Vec<Spanned<FunctionDeclaration>>)| {
            //         acc.into_iter()
            //             .chain(vec![name].into_iter().cycle().zip(fns))
            //             .collect::<Vec<(Option<String>, Spanned<FunctionDeclaration>)>>()
            //     },
            // );
            (
                EnumDeclaration {
                    name,
                    variants,
                    impl_blocks: vec![],
                },
                ctx.span(),
            )
        });
    r#enum
}

pub fn import<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Import>, Error<'tokens>> + Clone {
    let ident = name_parser();
    let import = name_parser()
        .map_with(|a, c| (a, c.span()))
        //. FAT TODO: UTILIZE NAME GIVEN TO MODULE
        .then_ignore(just(Token::Assign))
        .then_ignore(just(Token::Import))
        .then(
            ident
                .clone()
                .map_with(|module, ctx| (module, ctx.span()))
                .separated_by(just(Token::Slash))
                .collect(),
        )
        .then_ignore(newline())
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
    T: Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Expression>, Error<'tokens>> + Clone, // Statement
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
    let r#trait = name_parser()
        .then_ignore(just(Token::Assign))
        .then_ignore(just(Token::Trait))
        .then(
            fns.separated_by(just(Token::Comma).padded_by(separator()))
                .collect()
                .delimited_by(
                    just(Token::Colon).padded_by(separator()),
                    just(Token::Semicolon).padded_by(separator()),
                ),
        );
    return r#trait.map_with(|(a, b), ctx| (Trait(a, b), ctx.span()));
}
