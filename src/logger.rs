use colored::Colorize;
use terminal_size::terminal_size;

/// Logging flags to `#[command(flatten)]` into your CLI
#[derive(clap::Args, Debug, Clone, Default)]
#[command(about = None, long_about = None)]
pub struct Verbosity<L: LogLevel = ErrorLevel> {
    #[arg(
        long,
        short = 'v',
        action = clap::ArgAction::Count,
        global = true,
        help = L::verbose_help(),
        long_help = L::verbose_long_help(),
    )]
    verbose: u8,

    #[arg(
        long,
        short = 'q',
        action = clap::ArgAction::Count,
        global = true,
        help = L::quiet_help(),
        long_help = L::quiet_long_help(),
        conflicts_with = "verbose",
    )]
    quiet: u8,

    #[arg(skip)]
    phantom: std::marker::PhantomData<L>,
}
/// How many characters the logger's prefix takes
// Actually takes 21, this is for added flexibility. Just subtract 4 if shit breakes
const LOGGER_PREFIX_LEN: usize = 25;

impl<L: LogLevel> Verbosity<L> {
    /// Get the log level filter.
    pub fn log_level_filter(&self) -> log::LevelFilter {
        level_enum(self.verbosity()).map_or(log::LevelFilter::Off, |l| l.to_level_filter())
    }

    fn verbosity(&self) -> i8 {
        level_value(L::default()) - (self.quiet as i8) + (self.verbose as i8)
    }
}

const fn level_value(level: Option<log::Level>) -> i8 {
    match level {
        None => -1,
        Some(log::Level::Error) => 0,
        Some(log::Level::Warn) => 1,
        Some(log::Level::Info) => 2,
        Some(log::Level::Trace) => 3,
        Some(log::Level::Debug) => 4,
    }
}

const fn level_enum(verbosity: i8) -> Option<log::Level> {
    match verbosity {
        i8::MIN..=-1 => None,
        0 => Some(log::Level::Error),
        1 => Some(log::Level::Warn),
        2 => Some(log::Level::Info),
        3 => Some(log::Level::Trace),
        4..=i8::MAX => Some(log::Level::Debug),
    }
}

/// Customize the default log-level and associated help
pub trait LogLevel {
    fn default() -> Option<log::Level>;

    fn verbose_help() -> Option<&'static str> {
        Some("Increase logging verbosity")
    }

    fn verbose_long_help() -> Option<&'static str> {
        Some("Use -v to display warnings, -vv to display the info log, -vvv to display the programm trace and -vvvv to display the trace without the fancy formatting")
    }

    fn quiet_help() -> Option<&'static str> {
        Some("Decrease logging verbosity")
    }

    fn quiet_long_help() -> Option<&'static str> {
        Some("Use -q to silence all output that is not strictly nessecary for debugging")
    }
}

/// Default to [`log::Level::Error`]
#[derive(Copy, Clone, Debug, Default)]
pub struct ErrorLevel;

impl LogLevel for ErrorLevel {
    fn default() -> Option<log::Level> {
        Some(log::Level::Error)
    }
}

/// Default to [`log::Level::Warn`]
#[derive(Copy, Clone, Debug, Default)]
pub struct WarnLevel;

impl LogLevel for WarnLevel {
    fn default() -> Option<log::Level> {
        Some(log::Level::Warn)
    }
}

/// Default to [`log::Level::Info`]
#[derive(Copy, Clone, Debug, Default)]
pub struct InfoLevel;

impl LogLevel for InfoLevel {
    fn default() -> Option<log::Level> {
        Some(log::Level::Info)
    }
}
pub struct SimpleLogger;
pub static LOGGER: SimpleLogger = SimpleLogger;

impl log::Log for SimpleLogger {
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        metadata.level() <= log::Level::Trace && metadata.level() != log::LevelFilter::Off
    }

    fn log(&self, record: &log::Record) {
        fn insert_newlines(str: String) -> String {
            if let Some((w, _)) = terminal_size() {
                let len = str.chars().count();
                let chars = len.div_ceil(w.0 as usize);
                if chars <= 1 {
                    return str;
                }
                let mut tmp_msg = String::with_capacity(w.0 as usize);
                let units = str.split([',', '\n']);
                let mut lines = Vec::with_capacity(chars);
                for str in units {
                    if tmp_msg.chars().count() < w.0 as usize {
                        if tmp_msg.chars().count() + str.chars().count() + LOGGER_PREFIX_LEN
                            > w.0 as usize
                        {
                            lines.push(tmp_msg);
                            tmp_msg = str.to_owned();
                        } else {
                            tmp_msg.push_str(str);
                        }
                    }
                }
                lines.push(String::new());
                return lines.join(&format!(
                    "\n{ws:>len$}|",
                    ws = " ",
                    len = LOGGER_PREFIX_LEN - 6
                ));
            }
            str
        }
        if self.enabled(record.metadata()) {
            if log::max_level() == log::Level::Debug {
                println!("{}", record.args());
                return;
            }
            // ? Logging date and time.
            let date = chrono::offset::Local::now(); // Get the current date.
            let date = date.format("%H:%M:%S").to_string(); // Format the date.
            let date = date.dimmed();
            let message = record.args().to_string();

            // ? Logging message.
            let level = match record.level() {
                log::Level::Debug => format!("{}", "🔮 DBUG".bright_blue()),
                log::Level::Info => format!("{}", "📰 INFO".green()),
                log::Level::Warn => format!("{}", "💡 WARN".bright_yellow()),
                log::Level::Trace => format!("{}", "🔧 TRCE".bright_black()),
                log::Level::Error => {
                    format!("{}", format!("💥 {}", "F#CK".strikethrough()).bright_red())
                } // Help => format!("{}", "💭 HELP".normal()),
                  // Success => format!("{}", "🎉 YEEE".blink().bright_green()),
                  // Fatal => write!("{}", "😵 FATL".on_red()),
            };
            let message = insert_newlines(message);
            let message = if message.starts_with('\n') {
                normalize_message(&format!("\n{message}"), record.level())
            } else {
                normalize_message(&message, record.level())
            };
            println!("{} | {} | {}", date, level, message);
        }
    }
    fn flush(&self) {}
}

use colored::ColoredString;

/// Normalize the message level.
fn normalize_message(message: &str, level: log::Level) -> ColoredString {
    match level {
        log::Level::Debug | log::Level::Info => message.normal(),
        log::Level::Warn => message.bright_yellow().bold(),
        log::Level::Error => message.bright_red().bold(),
        log::Level::Trace => message.dimmed(),
        // log::Level::Help => message.italic(),
        // log::Level::Success => message.bright_green(),
        // log::Level::Fatal => message.bold(),
    }
}
