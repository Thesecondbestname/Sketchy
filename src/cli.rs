use clap::{Args, Parser, Subcommand};
use log::debug;
use std::path::{Path, PathBuf};
use std::{env, fs};

/// Compiles, interprets, runs and manages arrow programs!
#[derive(Parser, Debug, Clone)]
#[command(author, version, about, long_about = None, propagate_version = true)]
pub struct Arguments {
    ///What to do with your arrow program
    #[command(subcommand)]
    pub option: RunOptions,

    ///Debug the program in the current arrow environment using the interpreter.
    #[arg(short, long)]
    debug: bool,

    ///create a new arrow environment with a given name
    #[arg(short, long)]
    new: Option<String>,

    ///initialize the compiler to the path variable
    #[arg(long)]
    init: bool,

    ///This will not have any effect if run from the normal binary.
    #[arg(long)]
    upgrade: bool,

    ///Set the verbosity. Idk if I'll implement logging....
    #[arg(short, long)]
    pub is_verbose: bool,
}
impl Arguments {
    fn get_path_and_root(&self, verbose: bool) -> (String, PathBuf) {
        let path = match &self.option {
            RunOptions::Compiled(file) => (file.path.to_owned(), file.root.to_owned()),
            RunOptions::Run(file) => (file.path.to_owned(), file.root.to_owned()),
            RunOptions::Assemble(file) => (file.path.to_owned(), file.root.to_owned()),
            RunOptions::Test(file) => (file.path.to_owned(), file.root.to_owned()),
        };

        let dir = path.1.as_ref().map_or_else(get_current_dir, PathBuf::from);
        if verbose {
            debug!("looking for {:#?} in {:#?}", &path.0, dir);
        }
        return (path.0, Path::new(&dir).to_path_buf());
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Subcommand, Debug)]
pub enum RunOptions {
    ///Run the code compiled and optimized.
    Compiled(File),
    ///Run the code interpreted. This will be slower but skip the compilation process.
    Run(File),
    ///Just compiles the code. Not much to be said.
    Assemble(File),
    ///Test all tests in the current arrow environment
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
    pub fn find_file(&self) -> Option<std::path::PathBuf> {
        let fs = self.get_path_and_root(self.is_verbose);
        if self.is_verbose {
            debug!("Looking in {:#?}", &fs.1);
        }
        search_directory(fs.1, fs.0.as_str(), self.is_verbose)
    }
}
fn search_directory(path: PathBuf, file_name: &str, verbose: bool) -> Option<std::path::PathBuf> {
    if path.is_dir() {
        // check if the directory has a "src" subdirectory and search it first
        let src_path = path.join("src");
        if src_path.is_dir() {
            debug!("Searching in subdirectory 'src': {:?}", src_path);
            if let Some(path) = search_directory(src_path, file_name, verbose) {
                return Some(path);
            }
        }

        for entry in fs::read_dir(&path).unwrap() {
            let entry = entry.unwrap();
            let file_path = entry.path();
            if file_path.is_file() && file_path.file_name().unwrap() == file_name {
                return Some(file_path);
            }
            if file_path.is_dir() {
                if let Some(path) = search_directory(file_path, file_name, verbose) {
                    return Some(path);
                }
            }
        }
    }
    debug!("Not found in {:?}", &path);
    None
}

pub fn parse_args() -> Arguments {
    Arguments::parse()
}
fn get_current_dir() -> PathBuf {
    return match env::current_dir() {
        Ok(path) => path,
        Err(e) => {
            log::error!("Error getting file location: {}", e);
            std::process::exit(1);
        }
    };
}
