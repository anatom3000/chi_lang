use std::{fs, io, path::PathBuf, process::Command, fmt::Debug};

use crate::TranspileError;

#[derive(Debug)]
pub enum CompilationError {
    UnsupportedPlatform(&'static str),
    IoError(io::Error),
    CompilerError(String),
    TranspileError(TranspileError),
}

pub fn compile(transpiled_file: PathBuf, log_file: PathBuf, binary_file: PathBuf) -> Result<(), CompilationError> {

    #[cfg(target_os = "windows")]
    return Err(CompilationError::UnsupportedPlatform("windows"));

    let command = format!("clang {} -o {} -Wall -Wextra", transpiled_file.display(), binary_file.display());

    let output = Command::new("sh")
        .arg("-c")
        .arg(command)
        .output()
        .map_err(|e| CompilationError::IoError(e))?;

    if !output.status.success() {
        return Err(CompilationError::CompilerError(
            String::from_utf8_lossy(output.stderr.as_slice()).to_string(),
        ));
    }

    fs::write(&log_file, output.stdout)
        .map_err(|e| CompilationError::IoError(e))?;

    fs::write(log_file, output.stderr)
    .map_err(|e| CompilationError::IoError(e))?;

    Ok(())
}
