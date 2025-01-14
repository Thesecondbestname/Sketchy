use std::collections::HashSet;

use crate::convenience_types::Spanned;
use crate::format_join;
use crate::interner::StrId;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(pub Vec<Spanned<StrId>>);

#[derive(Debug, Clone)]
pub struct Programm(Vec<Spanned<Item>>, Symbols);

#[derive(Debug, Clone)]
pub struct Impl {
    pub(crate) impl_for: Spanned<Ident>,
    pub(crate) impl_what: Option<Spanned<Ident>>,
    pub(crate) fns: Vec<Spanned<FunctionDeclaration2>>,
}
#[derive(Debug, Clone)]
pub enum Pattern {
    Name(Name),
    Value(Spanned<Expression>),
    Enum(Ident, Vec<Spanned<Pattern>>),
    Struct(Spanned<Ident>, Vec<(Spanned<Name>, Spanned<Pattern>)>),
    Tuple(Vec<Spanned<Pattern>>),
    Array(Vec<Spanned<Pattern>>, Name),
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Name {
    Name(Spanned<StrId>),
    Underscore,
}
#[derive(Debug, Clone)]
pub struct Generics(pub Vec<Spanned<Generic>>);

#[derive(Debug, Clone)]
pub struct Generic(pub (Spanned<StrId>, Vec<Spanned<Ident>>));

#[derive(Debug, Clone)]
// #[cfg_attr(test, visibility::make(pub(crate)))]
pub enum Item {
    AlternateSyntaxFunction(Spanned<FunctionDeclaration2>),
    Import(Spanned<Import>),
    Enum(Spanned<EnumDeclaration>),
    Struct(Spanned<StructDeclaration>),
    Assingment(Spanned<VariableDeclaration>),
    Trait(Spanned<Trait>),
    ImplBlock(Spanned<Impl>),
    TopLevelExprError(Expression),
}

#[derive(Debug, Clone)]
pub struct Symbols {
    pub(crate) fns: HashSet<StrId>,
    pub(crate) traits: HashSet<Spanned<StrId>>,
    pub(crate) structs: HashSet<StrId>,
    pub(crate) enums: HashSet<StrId>,
    pub(crate) imports: HashSet<StrId>,
    pub(crate) vars: HashSet<StrId>,
}
#[derive(Debug, Clone)]
pub struct Trait(
    pub Option<Spanned<Generics>>,
    pub Spanned<StrId>,
    pub Vec<Spanned<TraitFns>>,
);
#[derive(Debug, Clone)]
pub struct Import(pub Spanned<StrId>, pub Vec<Spanned<StrId>>);

#[derive(Debug, Clone)]
pub struct TraitFns(
    pub Spanned<StrId>,
    pub Vec<Spanned<Type>>,
    pub Option<Spanned<Type>>,
);
#[derive(Debug, Clone)]
pub struct VariableDeclaration(pub Spanned<Pattern>, pub Spanned<Expression>);

#[derive(Debug, Clone)]
pub struct FunctionDeclaration2 {
    pub name: Spanned<StrId>,
    pub generics: Option<Spanned<Generics>>,
    pub arguments: Vec<Spanned<StrId>>,
    pub type_: Spanned<Type>,
    pub body: Spanned<Expression>,
}

#[derive(Debug, Clone)]
/// Where the enum is declared and not where it is constructed
pub struct EnumDeclaration {
    pub name: Spanned<StrId>,
    pub variants: Vec<Spanned<EnumVariantDeclaration>>,
}
/// A variant of an enum that is currently declared. I.e. an arm.
#[derive(Debug, Clone)]
pub struct EnumVariantDeclaration {
    pub name: Spanned<StrId>,
    pub fields: Vec<Spanned<Type>>,
}
#[derive(Debug, Clone)]
/// The one where the struct is firstly defined
pub struct StructDeclaration {
    pub name: Spanned<StrId>,
    pub generics: Option<Spanned<Generics>>,
    pub fields: Vec<Spanned<StructField>>,
    pub impl_blocks: Vec<(Option<StrId>, Spanned<FunctionDeclaration2>)>,
}
#[derive(Debug, Clone)]
pub struct StructField {
    pub name: Spanned<StrId>,
    pub r#type: Spanned<Type>,
}

// #[cfg_attr(test, visibility::make(pub(crate)))]
#[derive(Clone, Debug)]
pub enum Expression {
    ParserError,
    TopLvlExpr(Box<Spanned<Expression>>),
    If(Box<If>),
    For(Box<For>),
    Match {
        condition: Box<Spanned<Expression>>,
        arms: Vec<(Spanned<Pattern>, Spanned<Expression>)>,
    },
    Ident(Spanned<Ident>),
    FunctionCall(Box<Spanned<Self>>, Vec<Spanned<Self>>),
    MethodCall(Box<Spanned<Self>>, Spanned<Ident>, Vec<Spanned<Self>>),
    FieldAccess(Box<Spanned<Self>>, Spanned<Ident>),
    Block(Vec<Spanned<Item>>, Box<Spanned<Expression>>, Symbols),
    Value(Value),
    Else(Box<Spanned<Self>>, Box<Spanned<Self>>),
    UnaryBool(Box<Spanned<Self>>),
    MathOp(Box<Spanned<Self>>, MathOp, Box<Spanned<Self>>),
    Comparison(Box<Spanned<Self>>, ComparisonOp, Box<Spanned<Self>>),
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
    Unit,
}

#[derive(Debug, Clone)]
pub struct If {
    pub(crate) condition: Spanned<Expression>,
    pub(crate) code_block: Spanned<Expression>,
}
#[derive(Debug, Clone)]
pub struct For {
    pub(crate) name: Spanned<Pattern>,
    pub(crate) iterator: Spanned<Expression>,
    pub(crate) code_block: Spanned<Expression>,
}
#[derive(Clone, Debug)]
/// An enum of all possible values. A type can have
// #[cfg_attr(test, visibility::make(pub(crate)))]
pub enum Value {
    String(String),
    Number(Number),
    Tuple(Vec<Spanned<Expression>>),
    Bool(bool),
    Span(
        Option<Box<Spanned<Expression>>>,
        Option<Box<Spanned<Expression>>>,
    ),
    Struct {
        name: Spanned<Ident>,
        fields: Spanned<Vec<(Spanned<StrId>, Spanned<Expression>)>>,
    },
    Enum {
        variant: Spanned<Ident>,
        fields: Spanned<Vec<Spanned<Expression>>>,
    },
}
#[derive(Clone, Debug)]
pub enum MathOp {
    Add,
    Sub,
    Div,
    Mul,
}
#[derive(Clone, Debug)]
pub enum ComparisonOp {
    Lt,
    Gt,
    Lte,
    Gte,
    Neq,
    Eq,
}
#[derive(Clone, Debug)]
pub enum BinaryOp {
    And,
    Or,
}
/// The type of an expression
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
// #[cfg_attr(test, visibility::make(pub(crate)))]
pub enum Type {
    Self_,
    Int,
    Bool,
    Float,
    String,
    Array(Box<Spanned<Type>>, i64),
    Tuple(Vec<Spanned<Type>>),
    FunctionType(Vec<Spanned<Type>>, Spanned<Box<Type>>),
    Path(Spanned<Ident>),
}
#[derive(Debug, Clone)]
pub enum Number {
    Int(i64),
    Float(f64),
}
crate::impl_display!(BinaryOp, |s: &BinaryOp| match s {
    BinaryOp::And => "&&",
    BinaryOp::Or => "||",
});
crate::impl_display!(ComparisonOp, |s: &ComparisonOp| match s {
    ComparisonOp::Lt => "<",
    ComparisonOp::Gt => ">",
    ComparisonOp::Lte => "<=",
    ComparisonOp::Gte => ">=",
    ComparisonOp::Neq => "!=",
    ComparisonOp::Eq => "==",
});
crate::impl_display!(Value, |s: &Value| match s {
    Value::String(string) => string.to_string(),
    Value::Number(Number::Int(int)) => format!("{int}"),
    Value::Number(Number::Float(float)) => format!("{float}"),
    Value::Tuple(vals) => format!("({})", format_join(vals, ",").unwrap_or_default()),
    Value::Bool(bool) => format!("{bool}"),
    Value::Span(start, end) => format!(
        "{}..{}",
        start.as_ref().map(|x| x.0.to_string()).unwrap_or_default(),
        end.as_ref().map(|x| x.0.to_string()).unwrap_or_default()
    ),
    Value::Struct { name, fields } => format!(
        "{} {{{}}}",
        name.0,
        fields
            .0
            .iter()
            .map(|(name, field)| format!("{}: {}", name.0, field.0))
            .collect::<Vec<_>>()
            .join(",")
    ),
    Value::Enum { variant, fields } => format!(
        "{} {{{}}}",
        variant.0,
        format_join(&fields.0, ",").unwrap_or_default()
    ),
});
crate::impl_display!(MathOp, |s: &MathOp| match s {
    MathOp::Add => "+".to_string(),
    MathOp::Sub => "-".to_string(),
    MathOp::Div => "/".to_string(),
    MathOp::Mul => "*".to_string(),
});
crate::impl_display!(Expression, |s: &Expression| {
    match s {
        Expression::MathOp(a, b, c) => {
            format!("({} {} {})", a.0, b, c.0)
        }
        Expression::Value(a) => format!("{a}"),
        Expression::Ident((a, _)) => format!("{a}"),
        Expression::ParserError => "Error".to_string(),
        Expression::FunctionCall(called, args) => format!(
            "{{{}({})}}",
            called.0,
            format_join(args, ",").unwrap_or_default()
        ),
        Expression::MethodCall(on, name, args) => {
            format!(
                "{{{}.{}({})}}",
                on.0,
                name.0,
                format_join(args, ",").unwrap_or_default()
            )
        }
        Expression::Block(block, e, _) => format!(
            "{} \nreturn {};",
            block
                .iter()
                .fold(String::new(), |acc, elem| acc + &format!("{}", elem.0)),
            e.0
        ),
        Expression::If(if_) => format!("{if_}"),
        Expression::Comparison(lhs, op, rhs) => format!("({} {op} {})", lhs.0, rhs.0),
        Expression::Binary(a, op, b) => format!("({} {op} {})", a.0, b.0),
        Expression::Else(c, e) => format!("{} else ({})", c.0, e.0),
        Expression::UnaryBool(e) => format!("!{}", e.0),
        Expression::Unit => "Dis weird aah heal".to_owned(),
        Expression::Match { condition, arms } => format!(
            "match {} {{{}}}",
            condition.0,
            arms.iter().fold(String::new(), |acc, it| acc
                + &format!("{} => {},", it.0 .0, it.1 .0))
        ),
        Expression::TopLvlExpr(_) => "Illegal toplvl expresssion".to_string(),
        Expression::For(_for) => format!("{_for}"),
        Expression::FieldAccess(e, (f, _)) => format!("{}.{f}", e.0),
    }
});

crate::impl_display!(If, |s: &If| {
    let If {
        condition,
        code_block: blocc,
    } = s;
    format!("if {} {{{}}}", condition.0, blocc.0)
});
crate::impl_display!(For, |s: &For| {
    let For {
        code_block,
        name,
        iterator,
    } = s;
    format!("for {} in {} {{{}}}", name.0, iterator.0, code_block.0)
});
crate::impl_display!(Ident, |s: &Ident| {
    format_join(&s.0, "::").unwrap_or_default()
});
crate::impl_display!(Type, |s: &Type| {
    match s {
        Type::Self_ => "Self".to_owned(),
        Type::Int => "integer".to_owned(),
        Type::Bool => "bool".to_owned(),
        Type::Float => "float".to_owned(),
        Type::String => "String".to_owned(),
        Type::Array(ty, size) => format!("[{};{size}]", ty.0),
        Type::Tuple(t) => format!("({})", format_join(t, ",").unwrap_or_default()),
        Type::Path(p) => format!("{}", p.0),
        Type::FunctionType(b, c) => {
            format!(
                "fn({}) -> {}",
                format_join(&b, ",").unwrap_or_default(),
                c.0
            )
        }
    }
});
crate::impl_display!(StructDeclaration, |s: &StructDeclaration| {
    let fields = s
        .fields
        .iter()
        .map(|x| format!("{}: {}", x.0.name.0.clone(), x.0.r#type.0.clone()))
        .collect::<Vec<_>>()
        .join(",");
    format!(
        "struct {0} {{ {fields}}}{1}",
        s.name.0,
        s.impl_blocks.iter().fold(String::new(), |acc, a| acc
            + &format!(
                "impl {}{}{{{}}}",
                a.0.clone()
                    .map(|a| a.as_str().to_owned() + " for ")
                    .unwrap_or_default(),
                s.name.0,
                a.1 .0
            ))
    )
});
crate::impl_display!(FunctionDeclaration2, |s: &FunctionDeclaration2| {
    format!(
        "fn {}<{}>({}) -> {} {{{}}}",
        s.name.0,
        s.generics
            .as_ref()
            .map(|a| a.0.to_string())
            .unwrap_or_default(),
        format_join(&s.arguments, ",").unwrap_or_default(),
        s.type_.0,
        s.body.0
    )
});
crate::impl_display!(Programm, |s: &Programm| {
    format!(
        "{}",
        s.0.iter()
            .fold(String::new(), |acc, elem| acc + &format!("{}", elem.0))
    )
});
crate::impl_display!(Generics, |s: &Generics| {
    format_join(&s.0, ",")
        .map(|a| "<".to_owned() + &a + ">")
        .unwrap_or_default()
});
crate::impl_display!(Item, |s: &Item| {
    match s {
        Item::Import((imp, _)) => {
            format!(
                "use {} as {};",
                format_join(&imp.1, "::").unwrap_or_default(),
                imp.0 .0.as_str()
            )
        }
        Item::Enum((EnumDeclaration { name, variants }, _)) => {
            format!(
                "enum {} {{{}}}",
                name.0,
                variants.iter().fold(String::new(), |acc, (v, _)| acc
                    + &format!(
                        "{}({}),",
                        v.name.0,
                        format_join(&v.fields, ",").unwrap_or_default()
                    )),
            )
        }
        Item::Struct((struct_, _)) => format!("{struct_}"),
        Item::Assingment((decl, _)) => format!("let {} = {};", decl.0 .0, decl.1 .0),
        Item::Trait((Trait(c, (a, _), b), _)) => format!(
            "trait{} {a} {{{}}}",
            c.as_ref()
                .map_or("".to_string(), |a| a.0.clone().to_string()),
            format_join(
                &b.iter()
                    .map(|a| (
                        format!(
                            "fn {}({}) -> {}",
                            a.0 .0 .0,
                            format_join(&a.0 .1, ", ").unwrap_or_default(),
                            a.0 .2.as_ref().map(|x| x.0.to_string()).unwrap_or_default()
                        ),
                        a.1
                    ))
                    .collect::<Vec<_>>(),
                ","
            )
            .unwrap_or_default()
        ),
        Item::ImplBlock((bl, _)) => {
            let mut message = "impl".to_string();
            if let Some(tra) = &bl.impl_what {
                message.push_str(format!("{} for {} ", tra.0, bl.impl_for.0).as_str());
            } else {
                message.push_str(format!("{} ", bl.impl_for.0).as_str());
            }
            message.push_str(&format!(
                "{{{}}}",
                format_join(&bl.fns, ";").unwrap_or_default()
            ));
            message
        }
        Item::TopLevelExprError(a) => format!("Erronious Expression {a}"),
        _ => "".to_owned(),
    }
});
crate::impl_display!(Name, |s: &Name| {
    match s {
        Name::Name(name) => name.0.to_string(),
        Name::Underscore => "_".to_string(),
    }
});
crate::impl_display!(Generic, |s: &Generic| {
    format!(
        "{}{}",
        s.0 .0 .0,
        format_join(&s.0 .1, "+")
            .map(|a| ": ".to_owned() + &a)
            .unwrap_or_default()
    )
});
crate::impl_display!(Pattern, |s: &Pattern| {
    match s {
        Pattern::Enum(name, pattern) => {
            format!(
                "{} ({})",
                name,
                pattern
                    .iter()
                    .fold(String::new(), |acc, a| acc + &a.0.to_string())
            )
        }
        Pattern::Struct((name, _), pat) => format!(
            "{name}{{{}{}}}",
            format!(
                "{}:{}",
                pat.first().unwrap().0 .0,
                pat.first().unwrap().1 .0
            ),
            pat.iter()
                .skip(1)
                .fold(String::new(), |acc, (name, pat)| acc
                    + &format!(", {}: {}", name.0, pat.0))
        ),
        Pattern::Tuple(tuple) => format!("({})", format_join(tuple, ",").unwrap_or_default()),
        Pattern::Array(pats, end) => {
            format!("[{}..{end}]", format_join(pats, ",").unwrap_or_default())
        }
        Pattern::Name(name) => name.to_string(),
        Pattern::Value(e) => e.0.to_string(),
    }
});
impl Number {
    #[inline]
    pub fn from_i32(num: i32) -> Self {
        Number::Int(num.into())
    }
    #[inline]
    pub fn from_f32(num: f32) -> Self {
        Number::Float(num.into())
    }
}
impl Symbols {
    fn combine(&self) -> HashSet<StrId> {
        let mut fns = self.fns.clone();
        fns.extend(self.traits.iter().map(|a| a.0.clone()));
        fns.extend(self.structs.iter().map(|a| a.clone()));
        fns.extend(self.enums.iter().map(|a| a.clone()));
        fns.extend(self.imports.iter().map(|a| a.clone()));
        fns.extend(self.vars.iter().map(|a| a.clone()));
        fns
    }
}
impl Expression {
    pub fn get_idents(&self) -> HashSet<StrId> {
        match self {
            Self::Block(_, _, b) => b.combine(),
            _ => HashSet::new(),
        }
    }
}
impl Pattern {
    pub fn get_names(&self) -> Option<Vec<StrId>> {
        match self {
            Pattern::Name(Name::Name((n, _))) => Some(vec![*n]),
            Pattern::Name(_) => None,
            Pattern::Value(_) => panic!("[INTERNAL ERROR] Can not get name of a value pattern"),
            Pattern::Enum(_, vec) => {
                Some(vec.iter().flat_map(|a| a.0.get_names()).flatten().collect())
            }
            Pattern::Struct(_, vec) => Some(
                vec.iter()
                    .flat_map(|(a, b)| {
                        b.0.get_names().map(|mut v| {
                            if let Name::Name((c, _)) = &a.0 {
                                v.push(*c)
                            };
                            v
                        })
                    })
                    .flatten()
                    .collect(),
            ),
            Pattern::Tuple(vec) => {
                Some(vec.iter().flat_map(|a| a.0.get_names()).flatten().collect())
            }
            Pattern::Array(vec, Name::Name((g, _))) => {
                let mut x: Vec<_> = vec.iter().flat_map(|a| a.0.get_names()).flatten().collect();
                x.push(*g);
                Some(x)
            }
            Pattern::Array(vec, _) => {
                Some(vec.iter().flat_map(|a| a.0.get_names()).flatten().collect())
            }
        }
    }
}
#[macro_export]
/// Takes a struct name as first argument and a closure of it's Struct
/// Synopsys: (`struct_name`, |s: &`struct_name`| match s{...})
macro_rules! impl_display {
    ($struct_name:ident, $write_calls:expr) => {
        impl std::fmt::Display for $struct_name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "{}", &$write_calls(&self))
            }
        }
    };
}
#[macro_export]
macro_rules! display_inner {
    ($struct_name:ident) => {
        impl std::fmt::Display for $struct_name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "{:#?}", stringify!($struct_name))
            }
        }
    };
}
pub(crate) fn extract_idents(items: (Vec<Spanned<Item>>, Spanned<Expression>)) -> Expression {
    use crate::convenience_types::StrId;
    use std::collections::HashSet;
    let mut enums = HashSet::new();
    let mut structs = HashSet::new();
    let mut vars = HashSet::new();
    let mut fns = HashSet::new();
    let mut imports = HashSet::new();
    let mut traits = HashSet::new();
    for (item, _) in &items.0 {
        match item {
            Item::AlternateSyntaxFunction((a, _)) => {
                fns.insert(a.name.0.clone());
            }
            Item::Import(a) => {
                imports.insert(a.0 .0 .0.clone());
            }
            Item::Enum((e, _)) => {
                enums.insert(e.name.0.clone());
            }
            Item::Struct((s, _)) => {
                structs.insert(s.name.0.clone());
            }
            Item::Assingment((v, _)) => {
                for var in v.0 .0.get_names().unwrap_or_default() {
                    vars.insert(StrId::from(var));
                }
            }
            Item::Trait(a) => {
                traits.insert(a.0 .1.clone());
            }
            _ => (),
        }
    }
    Expression::Block(
        items.0,
        Box::new(items.1),
        Symbols {
            fns,
            traits,
            structs,
            enums,
            imports,
            vars,
        },
    )
}
