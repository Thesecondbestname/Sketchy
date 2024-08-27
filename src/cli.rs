use clap::{Args, Parser, Subcommand};
use log::{debug, trace, warn};
use std::path::{Path, PathBuf};
use std::{env, fs};

use crate::{abort, error, logger};
pub const PROJECT_FILE: &str = "project.toml";

/// Compiles, interprets, runs and manages sketchy programs!
#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about = None, propagate_version = true)]
pub struct Arguments {
    ///What to do with your sketchy program
    #[command(subcommand)]
    pub option: RunOptions,

    ///Debug the program in the current sketchy environment using the interpreter.
    #[arg(short, long)]
    repl: bool,

    ///initialize the compiler to the path variable
    #[arg(long)]
    init: bool,

    ///This will not have any effect if run from the normal binary.
    #[arg(long)]
    upgrade: bool,

    ///Set the verbosity.
    #[command(flatten)]
    pub verbose: logger::Verbosity,
}
impl Arguments {
    fn get_path_and_root(&self) -> (String, PathBuf) {
        let path = match &self.option {
            RunOptions::Compiled(file) => (file.path.clone(), file.root.clone()),
            RunOptions::Run(file) => (file.path.clone(), file.root.clone()),
            RunOptions::Assemble(file) => (file.path.clone(), file.root.clone()),
            RunOptions::Test(file) => (file.path.clone(), file.root.clone()),
            RunOptions::New(name) => todo!(),
        };

        let dir = path.1.as_ref().map_or_else(get_current_dir, PathBuf::from);
        trace!("looking for {:#?} in {:#?}", &path.0, dir);
        return (path.0, Path::new(&dir).to_path_buf());
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Subcommand, Debug)]
pub enum RunOptions {
    ///Run the code compiled and optimized.
    Compiled(File),
    ///Run the code interpreted. This will be slower but skip the compilation process.
    Run(File),
    ///Create a new sketchy environment
    New(Environment),
    ///Just compiles the code. Not much to be said.
    Assemble(File),
    ///Test all tests in the current sketchy environment
    Test(File),
}

#[derive(Args, PartialEq, Clone, Eq, PartialOrd, Ord, Debug)]
pub struct File {
    ///Path to the input file
    #[arg(default_value_t = String::from("main.ar"))]
    path: String,
    ///Path to the root directory
    root: Option<String>,
}
#[derive(Args, PartialEq, Clone, Eq, PartialOrd, Ord, Debug)]
pub struct Environment {
    ///The name of the environment being created
    pub name: String,
    ///Wether the package should have a window of not
    pub graphics: Option<bool>,
}
impl File {
    #[cfg(target_os = "linux")]
    pub fn get_name(&self) -> String {
        self.path
            .split('/')
            .last()
            .expect("cli::83 split does not work as expected")
            .to_string()
    }
    #[cfg(target_os = "windows")]
    pub fn get_name(&self) -> &str {
        self.path
            .split(r"\")
            .last()
            .expect("cli::87 split does not work as expected")
    }
}
impl Arguments {
    pub fn find_file(&self) -> Result<std::path::PathBuf, bool> {
        let fs = self.get_path_and_root();
        trace!("Looking in {:#?}", &fs.1);
        search_directory(&fs.1, fs.0.as_str())
    }
}
/// first searches src/ directory then the current one for a specified file.
/// Returns a Result of a path or a bool that indicates wether it even found an initialized environment
fn search_directory(path: &PathBuf, file_name: &str) -> Result<std::path::PathBuf, bool> {
    let mut is_init = false;
    if !path.is_dir() {
        return Err(is_init);
    }
    // check if the directory has a "src" subdirectory and search it first
    let src_path = path.join("src");
    if src_path.is_dir() {
        trace!(
            "Searching in subdirectory 'src': {}",
            src_path.as_path().as_os_str().to_string_lossy()
        );
        if src_path.is_file() {
            trace!("Found {} in 'src/'", file_name);
            return Ok(src_path);
        }
    }
    trace!(
        "Searching in current directory: {}",
        path.as_path().as_os_str().to_string_lossy()
    );
    if path.join(file_name).is_file() {
        trace!("Found {} in current dir", file_name);
        return Ok(path.join(file_name));
    }
    let dir = fs::read_dir(&path).unwrap();
    for entry in dir {
        let entry = entry.unwrap();
        let file_path = entry.path();
        debug!("{:?}", &file_path);
        if file_path.is_file() && file_path.file_name().is_some_and(|s| s == PROJECT_FILE) {
            is_init = true
        } else if file_path.is_file() && file_path.file_name().is_some_and(|s| s == file_name) {
            abort!("Did you mean {}? Consider moving it to a 'src/' directory or initializing a new project with 'sketchy new'", file_path.to_string_lossy());
        }
    }
    Err(is_init)
}

pub fn parse_args() -> Arguments {
    Arguments::parse()
}
fn get_current_dir() -> PathBuf {
    match env::current_dir() {
        Ok(path) => path,
        Err(e) => {
            log::error!("Error getting current file location: {}", e);
            std::process::exit(1);
        }
    }
}
