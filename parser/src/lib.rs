pub use lexer::Token;

mod ast;
mod error;
mod expression;
mod item;
mod lexer;
mod parser;
mod span;
mod util_parsers;

pub use parser::SketchyParser;
pub type OutputType = convenience_types::Spanned<ast::Expression>;
pub use crate::error::ParseError;
#[must_use]
pub const fn empty_span() -> span::Span {
    span::Span::new(0, 0)
}
pub mod convenience_types {
    use crate::{error::ParseError, span, Token};
    pub use span::Span;
    // pub type Error<'tokens> = chumsky::extra::Err<Rich<'tokens, Token, span::Span>>;
    pub type Error<'src> = chumsky::extra::Full<ParseError, (), &'src str>;
    pub type Spanned<T> = (T, span::Span);
    pub type ParserInput<'tokens, 'src> =
        chumsky::input::SpannedInput<Token, span::Span, &'tokens [(Token, span::Span)]>;
}
mod convenience_parsers {
    pub use super::util_parsers::{name_parser, separator, type_parser};
}
#[cfg(test)]
mod tests;
