use crate::ast::{Block, Expression, Item};
use crate::convenience_types::{Error, ParserInput, Span, Spanned};
use crate::error::{errors_to_diagnostics, Diagnostic, ParseError, Pattern};
use crate::expression::expression;
use crate::item::item;
use crate::lexer::{lex_sketchy_program, Lex, LexError};
use crate::span::SourceId;
use crate::Token;
use chumsky::prelude::*;
use thiserror::Error as DeriveError;

pub fn block<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<Item>,              // Output
    Error<'tokens>,             // Error Type)
> + Clone {
    // import, function, statement
    let x = recursive(|block| {
        let block_element = item(expression(block.clone()));
        block_element.map_with(|expr, ctx| (expr, ctx.span()))
    });
    x
}
pub fn programm<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>, // Input
    Spanned<Expression>,        // Output
    Error<'tokens>,             // Error Type)
> + Clone {
    block()
        .recover_with(via_parser(
            expression(block())
                .then_ignore(crate::util_parsers::newline())
                .map(|a| (Item::TopLevelExprError(a.0), a.1)),
        ))
        .validate(|it, ctx, emmit| {
            if let Item::TopLevelExprError(_) = it.0 {
                emmit.emit(ParseError::expected_found_help(
                    ctx.span(),
                    vec![Pattern::Label("Top level Item")],
                    Some("expression".to_owned()),
                    "Top Level expressions are not allowed".to_owned(),
                ));
            }
            it
        })
        .repeated()
        .collect::<Vec<_>>()
        .map_with(|items, ctx| (Expression::Block(Block(items)), ctx.span()))
}
// ----- STATES ----
#[derive(Default, Clone)]
pub struct NotInitialized;
#[derive(Default, Clone, Debug)]
pub struct Initialized(String);
#[derive(Default, Clone, Debug)]
pub struct Lexed(Vec<(Token, Span)>);

#[derive(Default, Clone)]
pub struct Parsed(Option<Spanned<Expression>>);
// ---- STATES ----

pub struct SketchyParser {
    input: String,
    parse_result: Spanned<Expression>,
}
#[derive(Default, Debug)]
pub struct SketchyParserBuilder<I, L, P> {
    name: Box<str>,
    input: I,
    tokens: L,
    parse_result: P,
}
impl<'i, I, L: Default, P: Default> SketchyParserBuilder<I, L, P> {
    pub fn input(
        self,
        inp: impl Into<String>,
        src_name: Box<str>,
    ) -> SketchyParserBuilder<Initialized, L, P> {
        SketchyParserBuilder {
            name: src_name,
            input: Initialized(inp.into()),
            tokens: self.tokens,
            parse_result: self.parse_result,
        }
    }
}
impl<L, P> SketchyParserBuilder<Initialized, L, P> {
    pub fn parenthesize_program(self) -> Self {
        let str = "(".to_owned() + &self.input.0 + ")";
        Self {
            input: Initialized(str),
            ..self
        }
    }
    pub fn dbg_print_input(self) -> Self {
        println!("{}", self.input.0);
        self
    }
    pub fn lex_sketchy_programm(self) -> LexResult<P> {
        let SketchyParserBuilder {
            name,
            input,
            parse_result,
            ..
        } = self;
        let lex = match lex_sketchy_program(&input.0).into_result() {
            Ok(lex) => Ok(SketchyParserBuilder {
                name,
                input,
                tokens: (Lexed(lex.into_iter().map(|(a, span)| (a, span.into())).collect())),
                parse_result,
            }),
            Err((recovered, err)) => Err(LexErr(recovered, err, input.0, name)),
        };
        LexResult(lex)
    }
}
impl<'a, P> SketchyParserBuilder<Initialized, Lexed, P> {
    pub fn dbg_print_tokens(self) -> Self {
        println!("{:#?}", self.tokens);
        self
    }
    pub fn remove_duplicate_newline(self) -> Self {
        let mut prev = Token::Newline;
        let toks = self
            .tokens
            .0
            .into_iter()
            .filter(|(tok, _)| {
                let result = tok != &Token::Newline || prev != Token::Newline;
                prev = tok.clone();
                result
            })
            .collect();
        Self {
            tokens: Lexed(toks),
            ..self
        }
    }
    pub fn wrap_programm_in_main_assignment(self) -> Self {
        let mut toks = vec![
            (Token::Ident("main".to_owned()), Span::new(0, 0)),
            (Token::Assign, Span::new(0, 0)),
            (Token::Lparen, Span::new(0, 0)),
        ];
        toks.extend(self.tokens.0);
        toks.extend(vec![(Token::Rparen, toks.last().unwrap().1)]);
        Self {
            tokens: Lexed(toks),
            ..self
        }
    }
    pub fn parse_sketchy_programm(self) -> ParserResult {
        let input = &self.tokens.0;
        let parse = programm().parse(
            input
                .as_slice()
                .spanned(Span::from(input.len()..input.len())),
        );
        let (ast, errs) = errors_to_diagnostics(parse, SourceId::INVALID);
        if let Some(ast) = ast {
            if errs.is_empty() {
                return ParserResult(Ok(SketchyParserBuilder {
                    name: self.name,
                    input: self.input,
                    tokens: self.tokens,
                    parse_result: Parsed(Some(ast)),
                }));
            }
            return ParserResult(Err(ParseErr(
                errs.into_iter().collect(),
                ast,
                self.input.0,
                self.name,
            )));
        }
        ParserResult(Err(ParseErr(
            errs.into_iter().collect(),
            (Expression::ParserError, Span::new(0, 0)),
            self.input.0,
            self.name,
        )))
    }
}
impl SketchyParserBuilder<Initialized, Lexed, Parsed> {
    pub fn finish(self) -> SketchyParser {
        SketchyParser {
            input: self.input.0,
            parse_result: self.parse_result.0.expect(
                "[INTERNAL ERROR] Fucked up the typestate. Called build on empty parse_result field",
            ),
        }
    }
}
impl SketchyParser {
    #[must_use]
    pub fn builder() -> SketchyParserBuilder<NotInitialized, NotInitialized, NotInitialized> {
        SketchyParserBuilder::default()
    }
}
impl SketchyParser {
    #[must_use]
    pub const fn ast(&self) -> &Expression {
        &self.parse_result.0
    }
    #[must_use]
    pub fn into_ast(self) -> Expression {
        self.parse_result.0
    }
    #[must_use]
    pub fn span_on_src(&self, span: Span) -> String {
        self.input[span.start()..span.end()].to_string()
    }
}
///Parser error type
#[derive(DeriveError, Debug, Clone)]
#[error("Error while Parsing")]
pub struct ParseErr(Vec<Diagnostic>, Spanned<Expression>, String, Box<str>);
/// Result Type of parse
pub struct ParserResult(anyhow::Result<SketchyParserBuilder<Initialized, Lexed, Parsed>, ParseErr>);
/// Lexer error type
#[derive(DeriveError, Debug)]
#[error("Error while Lexing")]
pub struct LexErr(Lex, LexError, String, Box<str>);
/// Lex result type
#[derive(DeriveError)]
pub struct LexResult<P>(anyhow::Result<SketchyParserBuilder<Initialized, Lexed, P>, LexErr>);
impl<P> LexResult<P> {
    /// Takes a function of fn('span', `erronious_token`, 'src', `src_name`)
    pub fn print_errors(
        self,
        formater: impl Fn(&std::ops::Range<usize>, &Token, &str, &Box<str>),
    ) -> Self {
        let Err(ref errors) = self.0 else {
            return self;
        };
        let name = &errors.3;
        for (span, err) in &errors.0 {
            formater(err, span, &errors.2[err.clone()], name);
        }
        self
    }
    pub fn dbg_panic(self) -> Self {
        let x = if let Err(ref error) = self.0 {
            error.to_string()
        } else {
            "paniced in lex Result".to_owned()
        };
        panic!("{x:?}");
    }
    pub fn into_result(
        self,
    ) -> anyhow::Result<SketchyParserBuilder<Initialized, Lexed, P>, LexErr> {
        self.0
    }
}
impl ParserResult {
    pub fn print_errors(
        self,
        formater: fn(&Diagnostic, &Spanned<Expression>, &str, &Box<str>),
    ) -> Self {
        let Err(ref error) = self.0 else {
            return self;
        };
        for err in &error.0 {
            formater(err, &error.1, &error.2, &error.3);
        }
        self
    }
    pub fn dbg_print_ast(self) -> Self {
        println!("{:?}", self.0.as_ref().map(|a| &a.parse_result.0));
        self
    }
    pub fn dbg_panic(self) -> Self {
        let x = if let Err(ref error) = self.0 {
            error.to_string()
        } else {
            "paniced in parserResult".to_owned()
        };
        panic!("{x:?}");
    }
    pub fn into_result(
        self,
    ) -> anyhow::Result<SketchyParserBuilder<Initialized, Lexed, Parsed>, ParseErr> {
        self.0
    }
}
