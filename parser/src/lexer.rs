#![allow(clippy::unwrap_used)]
#![allow(clippy::expect_used)]
use crate::{ast, impl_display};
use logos::{Lexer, Logos};
pub type Lex = Vec<(Token, std::ops::Range<usize>)>;
pub type LexError = Vec<((), std::ops::Range<usize>, String)>;

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord, Copy)]
pub struct Span {
    pub start: i32,
    pub end: Option<i32>,
}
/// A Result type holding the successfully lexed tokens and any eventual errors
pub struct LexResult {
    tokens: Lex,
    errors: LexError,
}
impl LexResult {
    fn new(tokens: Lex, errors: Vec<((), std::ops::Range<usize>, String)>) -> Self {
        Self { tokens, errors }
    }
    pub fn is_ok(&self) -> bool {
        self.errors.is_empty()
    }
    /// Converts the lex to a result, consuming and returning the Lex if no errors exist and a tuple of lex and errors otherwise
    pub fn into_result(self) -> Result<Lex, (Lex, LexError)> {
        if self.is_ok() {
            Ok(self.tokens)
        } else {
            Err((self.tokens, self.errors))
        }
    }
}

#[derive(Logos, Debug, PartialEq, Clone, Hash)]
#[logos(skip r"[ \t\f]+|(?://.*\n|/\*[\s\S]*\*/)")]
// #[cfg_attr(test, visibility::make(pub(crate)))]
pub enum Token {
    #[token("+")]
    Plus,
    #[token("\n")]
    Newline,
    #[token("=")]
    Assign,
    #[token("!")]
    Bang,
    #[token(".")]
    Dot,
    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,
    #[token(",")]
    Comma,
    #[token("#")]
    Hashtag,
    #[token("/")]
    Slash,
    #[token("if")]
    If,
    #[token("use")]
    Import,
    #[token("else")]
    Else,
    #[token("trait")]
    Trait,
    #[token("then")]
    Then,
    #[token("Self")]
    Self_,
    #[token("fn")]
    Fn,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("mod")]
    Module,
    #[token("impl")]
    Impl,
    #[token("enum")]
    Enum,
    #[token("match")]
    Match,
    #[token("false")]
    False,
    #[token("struct")]
    Struct,
    #[token("<")]
    Gt,
    #[token("==")]
    Eq,
    #[token("<=")]
    Gte,
    #[token("..")]
    DoubleDot,
    #[token("'")]
    Asterisc,
    #[regex(r"(\d+)\.\.(\d+)?", |lex| parse_span(lex.slice()))]
    Span(Span),
    #[regex("[a-zA-Z_öäü][a-zA-Z0-9_öäü]*", |lex| lex.slice().to_string())]
    Ident(String),
    #[regex(r#""([^"\\]|\\t|\\u|\\n|\\")*""#, |lex| lex.slice().to_string())]
    LiteralString(String),
    #[token("{")]
    Lbracket,
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().unwrap(), priority=2)]
    Integer(i64),
    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().to_owned())]
    r#Float(String),
    #[token("(")]
    Lparen,
    #[token(">=")]
    Lte,
    #[token(">")]
    Lt,
    #[token("%")]
    Mod,
    #[token("?")]
    QuestionMark,
    #[token("*")]
    Mul,
    #[token("!=")]
    Neq,
    #[token("or")]
    #[token("||")]
    Or,
    #[token("and")]
    #[token("&&")]
    And,
    #[token("}")]
    Rbracket,
    #[token("]")]
    Rbucket,
    #[token("[")]
    Lbucket,
    #[token(")")]
    Rparen,
    #[token(";")]
    Semicolon,
    #[token("-")]
    Minus,
    #[token("true")]
    True,
    #[regex("bool|float|int|char|string", type_matcher)]
    Type(ast::Type),
    Nothing,
}

impl Eq for Token {}

fn type_matcher(lex: &Lexer<Token>) -> ast::Type {
    let slice = lex.slice();
    match slice {
        "bool" => ast::Type::Bool,
        "int" => ast::Type::Int,
        "float" => ast::Type::Float,
        "string" => ast::Type::String,
        _ => unreachable!(),
    }
}

/// # Panics
///
/// Panics if I misused unwrap here.
///
/// # Errors
///
/// This function will return an error if Lexing was unsuccessful.
#[must_use]
pub fn lex_sketchy_program(inp: &str) -> LexResult {
    let parse = Token::lexer(inp);
    let (token, err): (Vec<_>, Vec<_>) = parse.clone().spanned().partition(|token| token.0.is_ok());
    let result = LexResult::new(
        token
            .into_iter()
            .map(|token| (token.0.unwrap(), token.1))
            .collect(),
        err.iter()
            .map(|(error, span)| {
                (
                    error.clone().err().unwrap(),
                    span.clone(),
                    parse.slice().to_owned(),
                )
            })
            .collect::<Vec<((), std::ops::Range<usize>, String)>>(),
    );
    result
}
fn parse_span(inp: &str) -> Span {
    let nums = inp
        .split_once("..")
        .expect("Lexer Error: Span regex fucked");
    Span {
        start: nums.0.parse().expect("Lexer Error: Span regex fucked"),
        end: nums.1.parse().map_or(None, |x| Some(x)),
    }
}

impl_display!(Token, |s: &Token| {
    match s {
        Token::Plus => "+".to_owned(),
        Token::Newline => "Newline".to_owned(),
        Token::Assign => "=".to_owned(),
        Token::Bang => "!".to_owned(),
        Token::Dot => ".".to_owned(),
        Token::Colon => ":".to_owned(),
        Token::DoubleColon => "::".to_owned(),
        Token::Comma => ",".to_owned(),
        Token::Hashtag => "#".to_owned(),
        Token::Slash => "/".to_owned(),
        Token::Else => "else".to_owned(),
        Token::For => "for".to_owned(),
        Token::Fn => "fn".to_owned(),
        Token::In => "in".to_owned(),
        Token::Module => "mod".to_owned(),
        Token::Enum => "enum".to_owned(),
        Token::Eq => "==".to_owned(),
        Token::False => "false".to_owned(),
        Token::Gt => ">".to_owned(),
        Token::Span(Span { start, end }) => format!(
            "{start}..{}",
            end.map(|x| x.to_string()).unwrap_or_default()
        ),
        Token::Ident(ident) => format!("ident: {ident}"),
        Token::LiteralString(string) => format!(r#""{string}""#),
        Token::If => "if".to_owned(),
        Token::Import => "use".to_owned(),
        Token::Lbracket => "{".to_owned(),
        Token::Integer(int) => format!("{int}"),
        Token::Float(float) => float.to_string(),
        Token::Lparen => "(".to_owned(),
        Token::Lt => "<".to_owned(),
        Token::Mod => "%".to_owned(),
        Token::QuestionMark => "?".to_owned(),
        Token::Mul => "*".to_owned(),
        Token::Neq => "!=".to_owned(),
        Token::Or => "||".to_owned(),
        Token::And => "&&".to_owned(),
        Token::Rbracket => "}".to_owned(),
        Token::Rparen => ")".to_owned(),
        Token::Semicolon => ";".to_owned(),
        Token::Struct => "struct".to_owned(),
        Token::Minus => "-".to_owned(),
        Token::True => "true".to_owned(),
        Token::Type(type_) => format!("{type_:?}"),
        Token::Nothing => "Noting".to_owned(),
        Token::Lte => "<=".to_owned(),
        Token::Gte => ">=".to_owned(),
        Token::Match => "match".to_owned(),
        Token::Then => "then".to_owned(),
        Token::Impl => "impl".to_owned(),
        Token::DoubleDot => "..".to_owned(),
        Token::Rbucket => "]".to_owned(),
        Token::Asterisc => "'".to_owned(),
        Token::Lbucket => "[".to_owned(),
        Token::Self_ => "self".to_owned(),
        Token::Trait => "trait".to_owned(),
    }
});
