use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use colored::Colorize;
use log::LevelFilter;
use std::fs::read_to_string;
mod cli;
use cli::RunOptions;
mod bootstrap;
use anyhow::Result;
use parser::{self, SketchyParser};

// pub(crate) extern crate pest;
// #[macro_use]
// pub(crate) extern crate pest_derive;
// #[derive(Parser)]
// #[grammar = "arrow.pest"]
// struct ArrowParser;

pub struct SimpleLogger;
static LOGGER: SimpleLogger = SimpleLogger;

impl log::Log for SimpleLogger {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        metadata.level() <= log::Level::Trace
    }

    fn log(&self, record: &log::Record) {
        if self.enabled(record.metadata()) {
            let lvl = record.level();
            match lvl {
                log::Level::Error => {
                    println!("{} {}", lvl.as_str().bright_red().bold(), record.args());
                }
                log::Level::Warn => {
                    println!("{} {}", lvl.as_str().bright_yellow().bold(), record.args());
                }
                log::Level::Info => {
                    println!("{} {}", lvl.as_str().blue().bold(), record.args());
                }
                log::Level::Debug => {
                    println!(
                        "{} {}",
                        lvl.as_str().on_bright_yellow().black(),
                        record.args().to_string().blue()
                    );
                }
                log::Level::Trace => {
                    println!("{}", record.args());
                }
            };
        }
    }
    fn flush(&self) {}
}

pub fn init(level: LevelFilter) -> Result<(), log::SetLoggerError> {
    log::set_logger(&LOGGER).map(|()| log::set_max_level(level))
}

fn main() -> anyhow::Result<()> {
    let args = cli::parse_args();
    let loglevel = if args.is_verbose {
        LevelFilter::Debug
    } else {
        LevelFilter::Warn
    };
    match init(loglevel) {
        Ok(_) => println!(
            "{} {}",
            "info".on_green().black(),
            "Logger initialized".blue()
        ),
        Err(e) => println!("Oh nos! The logger failed to initialize. {e:?}"),
    };
    let contents;
    match &args.option {
        RunOptions::Run(file) => {
            contents = read_to_string(&args.find_file().unwrap()).unwrap();
            let mut colors = ColorGenerator::new();
            let a = colors.next();
            let inp = SketchyParser::builder().input(contents, (file.clone().get_name()).into());
            let lex = if loglevel == LevelFilter::Debug {
                inp.dbg_print_input()
            } else {
                inp
            }
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
            .into_result()?;
            let parse = if loglevel == LevelFilter::Debug {
                lex.dbg_print_tokens()
            } else {
                lex
            }
            .remove_duplicate_newline()
            .parse_sketchy_programm();
            if loglevel == LevelFilter::Debug {
                parse.dbg_print_ast()
            } else {
                parse
            }
            .print_errors(|a, _ast, inp, name| {
                a.emit(std::io::stdout(), &name, inp);
            })
            .into_result()?
            .finish();

            Ok(())
        }
        RunOptions::Compiled(_) => todo!(),
        RunOptions::Assemble(_) => todo!(),
        RunOptions::Test(_) => todo!(),
    }
    // match parse {
    //     Ok(x) => x.for_each(|y| info!("{:?} \n", y.into_inner())),
    //     Err(err) => error!("{}", err),
    // }
}
