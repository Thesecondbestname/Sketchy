use std::{env, fs, path::PathBuf, process::Command};

use anyhow::anyhow;
use log::{error, info, trace};

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
pub fn create_env(name: &String, graphics: bool) -> Result<fs::File, anyhow::Error> {
    let current_env = env::current_dir();
    current_env
        .map_or_else(
            |b| Err(anyhow!(b)),
            |a: PathBuf| {
                fs::create_dir(a.join(name))
                    .inspect(|()| trace!("Successfully created project root"))
                    .map_or_else(
                        |a| {
                            error!("Failed to create project root {}", name);
                            Err(anyhow!(a))
                        },
                        |()| Ok(a.join(name)),
                    )
            },
        )
        .and_then(|path| {
            fs::create_dir(path.join("bin"))
                .and(fs::create_dir(path.join("src")))
                .and_then(|()| {
                    fs::File::create_new(path.join("src").join("main.sk"))
                        .inspect(|a| trace!("Successfully created src/main.sk"))
                })
                .inspect(|e| trace!("Successfully created src and bin subdirs"))
                .map_err(|e| {
                    error!("Failed to create Directory");
                    anyhow!(e)
                })
        })
}
