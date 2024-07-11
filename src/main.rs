use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use log::debug;
use log::info;
use std::fs::read_to_string;
mod cli;
mod logger;
use cli::RunOptions;
mod bootstrap;
use parser::{self, SketchyParser};

use crate::bootstrap::create_env;
use crate::cli::Environment;

fn main() -> anyhow::Result<()> {
    let args = cli::parse_args();
    log::set_logger(&logger::LOGGER).expect("Logger already initialized");
    log::set_max_level(args.verbose.log_level_filter());
    info!("Logger initialized: {}", log::max_level());
    let contents;
    match &args.option {
        RunOptions::Run(file) => {
            contents = read_to_string(args.find_file().unwrap()).unwrap();
            let mut colors = ColorGenerator::new();
            let a = colors.next();
            SketchyParser::builder()
                .input(contents, (file.clone().get_name()).into())
                .inspect_input(|a| debug!("{:?}", a))
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
                .inspect_lex(|a| {
                    info!("Finished lexing");
                    debug!(
                        "{:?}",
                        a.iter()
                            .map(|a| &a.0)
                            .map(|a| format!("{a}"))
                            .collect::<Vec<_>>()
                    );
                })
                .remove_duplicate_newline()
                .parse_sketchy_programm()
                .print_errors(|a, _ast, inp, name| {
                    a.emit(std::io::stdout(), &name, inp);
                })
                .into_result()?
                .inspect_ast(|a| {
                    info!("Finished parsing");
                    debug!(
                        "{}",
                        a.as_ref()
                            .map_or("failed 2 parser :(".to_string(), |b| b.0.to_string())
                    );
                })
                .finish();

            Ok(())
        }
        RunOptions::Compiled(_) => todo!(),
        RunOptions::Assemble(_) => todo!(),
        RunOptions::Test(_) => todo!(),
        RunOptions::New(env) => {
            create_env(&env.name, env.graphics.unwrap_or(false));
            Ok(())
        }
    }
    // match parse {
    //     Ok(x) => x.for_each(|y| info!("{:?} \n", y.into_inner())),
    //     Err(err) => error!("{}", err),
    // }
}
