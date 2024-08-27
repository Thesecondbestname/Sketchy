use std::{env, fs, io::Write, path::PathBuf, process::Command};

use anyhow::anyhow;
use log::{error, info, trace};

use crate::cli::PROJECT_FILE;

pub fn check_version() -> Result<String, anyhow::Error> {
    let current_exe = env::current_exe()?;
    let _current_exe_name = current_exe.file_name().unwrap();

    #[cfg(target_os = "windows")]
    let version_cmd = "dumpbin /headers";
    #[cfg(target_os = "linux")]
    let version_cmd = "objdump -p";

    let output = Command::new(version_cmd).arg(current_exe).output()?;

    let output_str = String::from_utf8(output.stdout)?;
    let version_str = output_str.lines().find(|line| line.contains("version"));
    if let Some(version) = version_str {
        let path = env::var("PATH")?;
        let paths: Vec<PathBuf> = env::split_paths(&path).collect();

        for (_i, path) in paths.iter().enumerate() {
            if path.join(version).exists() {
                return Ok(version.to_string());
            }
        }
    } else {
        println!("No version information found");
    }
    Err(anyhow!("Failed to check the installed"))
}
pub fn copy_to_path() -> anyhow::Result<()> {
    let current_exe = env::current_exe()?;
    let Some(current_exe_name) = current_exe.file_name() else {
        return Err(anyhow::Error::msg("Failed to get current exe file"));
    };

    #[cfg(target_os = "windows")]
    let path_var = "%PATH%";
    #[cfg(target_os = "linux")]
    let path_var = "PATH";

    let path_var_value = env::var(path_var)?;
    let new_path = format!(
        "{}:{}",
        current_exe
            .parent()
            .ok_or_else(|| anyhow!("Failed to get parent directory"))?
            .to_str()
            .ok_or_else(|| anyhow!("Failed to convert file name to valid unicode"))?,
        path_var_value
    );
    env::set_var(path_var, new_path);
    println!(
        "Successfully added {} to the {} environment variable.",
        current_exe_name
            .to_str()
            .ok_or_else(|| anyhow!("Failed to convert file name to valid unicode"))?,
        path_var
    );
    std::fs::remove_file(current_exe)?;
    Ok(())
}
fn create_project_file(path: PathBuf, name: &String) -> std::result::Result<std::fs::File, String> {
    let mut proj = fs::File::create_new(&path)
        .map_err(|_| format!("failed to create {}", path.as_os_str().to_string_lossy()))?;
    let toml = format!("[project]\n\tname={}", name);
    proj.write(toml.as_bytes()).map_err(|_| {
        format!(
            "failed to populate {} with default values",
            path.as_os_str().to_string_lossy()
        )
    })?;
    Ok(proj)
}
pub fn create_env(name: &String, graphics: bool) -> Result<(), String> {
    let current_env = env::current_dir();
    current_env
        .map_or_else(
            |_| Err(String::from("Failed to get current env")),
            |a: PathBuf| {
                fs::create_dir(a.join(name))
                    .inspect(|()| trace!("Successfully created project root"))
                    .map_or_else(
                        |_| Err(format!("Failed to create project root {}", name)),
                        |()| Ok(a.join(name)),
                    )
            },
        )
        .and_then(|path| {
            fs::create_dir(path.join("bin"))
                .and(fs::create_dir(path.join("src")))
                .map_err(|_| "Failed to initialize project directories".to_owned())
                .and_then(|()| {
                    {
                        {
                            fs::File::create_new(path.join(PROJECT_FILE))
                                .inspect(|_| trace!("Successfully created sketchy project file"))
                        }
                        .and(
                            fs::File::create_new(path.join("src").join("main.sk"))
                                .inspect(|_| trace!("Successfully created src/main.sk")),
                        )
                    }
                    .map_or_else(
                        |_| Err("Failed to create project files".to_owned()),
                        |_| {
                            trace!("Successfully created src and bin subdirs");
                            Ok(())
                        },
                    )
                })
        })
}
