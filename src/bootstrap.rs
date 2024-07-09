use std::{env, path::PathBuf, process::Command};

pub fn check_version() -> Option<String> {
    let current_exe = env::current_exe().unwrap();
    let _current_exe_name = current_exe.file_name().unwrap();

    #[cfg(target_os = "windows")]
    let version_cmd = "dumpbin /headers";
    #[cfg(target_os = "linux")]
    let version_cmd = "objdump -p";

    let output = Command::new(version_cmd).arg(current_exe).output().unwrap();

    let output_str = String::from_utf8(output.stdout).unwrap();
    let version_str = output_str.lines().find(|line| line.contains("version"));
    if let Some(version) = version_str {
        let path = env::var("PATH").unwrap();
        let paths: Vec<PathBuf> = env::split_paths(&path).collect();

        for (_i, path) in paths.iter().enumerate() {
            if path.join(version).exists() {
                return Some(version.to_string());
            }
        }
    } else {
        println!("No version information found");
    }
    None
}
pub fn copy_to_path() {
    let current_exe = env::current_exe().unwrap();
    let current_exe_name = current_exe.file_name().unwrap();

    #[cfg(target_os = "windows")]
    let path_var = "%PATH%";
    #[cfg(target_os = "linux")]
    let path_var = "PATH";

    let path_var_value = env::var(path_var).unwrap();
    let new_path = format!(
        "{}:{}",
        current_exe.parent().unwrap().to_str().unwrap(),
        path_var_value
    );
    env::set_var(path_var, &new_path);
    println!(
        "Successfully added {} to the {} environment variable.",
        current_exe_name.to_str().unwrap(),
        path_var
    );
    std::fs::remove_file(current_exe).unwrap();
}
