use crate::parser::SketchyParser;
use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
#[test]
fn basic_lex() -> anyhow::Result<()> {
    let lex = r#"
    x = xöla
    y = 69 / (56 - 0.45)
    _ = print::print(works)
    Print = use io/print
    enum Foo:
        baz
    ;
    trait[A,B#Add,C] Add: 
        add(int -> A, C)
        add(fn#A(B#int), C)
    ;    
    struct Baz:
        lmao# int,
        lmao2# int,
    ;
    impl Add:
        add(self) #(int -> Self) {
            self.lmao + self.lmao2
        }
    ;
    new(window)# Window-> int { 
         a-4 *3
    }  
    draw#int: 
        state, 
        frame, 
        window# Window; ( 
         a-4 *3
    )   
    // add(x, y) = 
    //     match x: 
    //         4 -> "four",
    //         _ -> x + y
    //      ;

    add#int: x#int, y#int; (
        match x: 
          4 -> "four",
          _ -> x + y
        ;
    )
    // Some kinda idk  
    _ = add (4,5).sqrt
    
    _ = match x:
      4 -> print ("oooops!"),
      _ -> print ("phew");
    "#;
    test(lex, "basic_lex")
}
#[test]
fn structs() -> anyhow::Result<()> {
    let input = r"struct Baz:
        lmao# int,
        lmao2# int
    ;";
    test(input, "structs")
}
#[test]
fn enums() -> anyhow::Result<()> {
    let input = r"enum Baz :
        lmao,
        lmao2(int, bool)
    ;";
    test(input, "enums")
}

#[test]
fn structs_with_impl() -> anyhow::Result<()> {
    let input = r"
    impl Add:
        draw(state ,frame ,window )#
            (SnekGame, Canvas, Window)-> int { 
             a-4 *3
        }   
    ;

    
    struct Baz:
        lmao# int,
        lmao2# int,
    ;
    impl Baz:
        new(window) #(Window -> int) { 
             a-4 *3
        }   
    ;
    ";
    test(input, "structs_with_impl")
}
#[test]
fn function_types() -> anyhow::Result<()> {
    let input = "trait Add: 
            add# ((int, int) -> int, int) -> int 
                             
        ;";
    test(input, "function_types")
}
#[test]
fn new_functions() -> anyhow::Result<()> {
    let input = r"new[C](window, a) #(Window, int) -> C { 
    a-4 *3
}";
    test(input, "new_functions")
}
#[test]
fn method_calls() -> anyhow::Result<()> {
    let input = "x = 500.sqrt()";
    test(input, "method_calls")
}
#[test]
fn multiple_stmts_in_block() -> anyhow::Result<()> {
    let input = "x = {x = 3\n y = 5}";
    test(input, "multiple_stmts_in_block")
}
#[test]
fn traits() -> anyhow::Result<()> {
    let input = "trait Add : add#int:int, int; ;";
    test(input, "traits")
}
#[test]
fn array_destructuring() -> anyhow::Result<()> {
    let input = "x = {let [a,b,c..d] = y}";
    test(input, "array_destructuring")
}
#[test]
fn paths() -> anyhow::Result<()> {
    let input = "x = std::core::rnd(crate::here::info)";
    test(input, "paths")
}
#[test]
fn struct_construction() -> anyhow::Result<()> {
    let input = r#"x = Dude: name= ("Kevin", 4), mood= Mood::Sadge;"#;
    test(input, "struct_construction")
}
#[test]
fn struct_destructuring() -> anyhow::Result<()> {
    let input = "let Dude: name#(name, _), mood#_ ; = x";
    test(input, "struct_destructuring")
}
#[test]
fn enum_construction() -> anyhow::Result<()> {
    let input = r#"x = Some("gerry", 24)"#;
    test(input, "enum_construction")
}
#[test]
fn enum_destructuring() -> anyhow::Result<()> {
    let input = "let Some((name, _)) = y";
    test(input, "enum_destructuring")
}
#[test]
#[should_panic]
fn top_level_expression() -> () {
    let input = "print(hello)";
    test(input, "top_level_expression").unwrap()
}
#[test]
fn function_definitions() -> anyhow::Result<()> {
    let input = "draw( 
    state,  
    frame , 
    window )# (State, Frame, Window) -> int {
         a-4 *3
    } 
    ";
    test(input, "function_definitions")
}

#[test]
fn calling_method_on_struct() -> anyhow::Result<()> {
    let input = "a = K: b=53;.sqrt()";
    test(input, "calling_method_on_struct")
}
#[test]
fn import() -> anyhow::Result<()> {
    let input = r"baz = use foo/bar/baz";
    test(input, "use")
}
#[test]
fn separator() -> anyhow::Result<()> {
    let input = r"x = 50
        g = print(ksjdfo) ";
    test(input, "separator")
}
#[test]
fn angery_case() -> anyhow::Result<()> {
    let input = r"x = 50.sqrt()
        y = ksjdfo
        _ = print(works)";
    test(input, "angery_case")
}
#[test]
fn assign() -> anyhow::Result<()> {
    let input = "\nx = 5 + 5 * (69 +420)";
    test(input, "assign")
}
#[test]
fn bool_expr() -> anyhow::Result<()> {
    let input = r"y = 4 == 4 and 5 <= (5 + 1)";
    test(input, "bool_expr")
}
#[test]
fn call() -> anyhow::Result<()> {
    let input = r"x = foo.bar(test)";
    test(input, "call")
}
#[test]
fn string() -> anyhow::Result<()> {
    let input = r#"g = "Hi!""#;
    test(input, "string")
}
#[test]
fn unary() -> anyhow::Result<()> {
    let input = r#"g = !(true or false)"#;
    test(input, "unary")
}
#[test]
fn r#match() -> anyhow::Result<()> {
    let input = "
    x = match Some(x):
            Some(e) -> e, 
            None -> panic();";
    test(input, "match")
}
#[test]
fn multiple_expressions() -> anyhow::Result<()> {
    let input = "z = {
     x = 4+5
        x = 32
    }";
    test(input, "multiple_expressions")
}
#[test]
fn precedence() -> anyhow::Result<()> {
    let input = "_ = {{{x()().q().q}.i().i}.i()}()()";
    test(input, "precedence")
}
#[test]
fn multiple_calls() -> anyhow::Result<()> {
    let input = r"m = lambda(3)(5).add(helo)";
    test(input, "multiple_calls")
}
#[test]
fn call_multiple_args() -> anyhow::Result<()> {
    let input = r#"m = print ("foo", 5, false)"#;
    test(input, "call_string")
}
#[test]
fn tuples() -> anyhow::Result<()> {
    let input = r#"m = ("foo", 5, false)"#;
    test(input, "tuples")
}
#[test]
fn alternate_tuples() -> anyhow::Result<()> {
    let input = r#"struct Pair[A,B]:
        first# A,
        second# B
    ;
    a = Pair{first:1, second: 4}"#;
    test(input, "tuples")
}
#[test]
fn math_operation() -> anyhow::Result<()> {
    let input = r"x = 2+7/(3+4)";
    test(input, "math_operation")
}
#[test]
fn hash_map() {
    let x = crate::interner::Interner::with_capacity(64);
    let id = x.intern("helo");
    let id2 = x.intern("helo");
    assert!(x.lookup(id) == x.lookup(id2) && x.lookup(id2) == "helo")
}

fn test(input: &str, name: &'static str) -> anyhow::Result<()> {
    let mut colors = ColorGenerator::new();
    let a = colors.next();
    let parse = SketchyParser::builder()
        .input(input.trim(), name)
        .inspect_input(|a| println!("{}", a))
        .lex_sketchy_programm()
        .print_errors(|span, token, input, name| {
            Report::build(ReportKind::Error, name, 12)
                .with_message(format!("Error while lexing test {input}"))
                .with_label(
                    Label::new((name, span.start..span.end))
                        .with_message(format!("Found unexpected Token {token}"))
                        .with_color(a),
                )
                .finish()
                .eprint((name, Source::from(input)))
                .expect("Falied to build report!");
        })
        .into_result()?
        .remove_duplicate_newline()
        .parse_sketchy_programm()
        .print_errors(|a, _ast, inp, name| {
            a.emit(std::io::stdout(), name, inp);
        })
        .into_result()?
        .inspect_ast(|x| x.clone().inspect(|a| println!("{:?}", a.0.get_idents())))
        // .inspect_ast(|x| {
        //     println!(
        //         "{}",
        //         x.as_ref()
        //             .map(|x| x.0.to_string())
        //             .unwrap_or("No ast was parsed!".to_string())
        //     )
        // })
        .finish();
    println!("\n\t{:?}", parse.ast());
    Ok(())
}
