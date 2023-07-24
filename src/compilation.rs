use std::{path::PathBuf, process::Command, io, fs};

pub enum CompilationError {
    UnsupportedPlatform(&'static str),
    IoError(io::Error),
    CompilerError(String)
}

pub fn compile(target_dir: PathBuf) -> Result<(), CompilationError> {
    if cfg!(target_os = "windows") {
        return Err(CompilationError::UnsupportedPlatform("Windows"))
    }

    let makefile_path = target_dir.join("Makefile");

    assert!(makefile_path.exists());

    let output = Command::new("sh")
        .arg("-c")
        .arg(format!("make -f {}", makefile_path.display()))
        .output()
        .map_err(|e| CompilationError::IoError(e))?;

    if !output.status.success() {
        return Err(CompilationError::CompilerError(String::from_utf8_lossy(output.stderr.as_slice()).to_string()));
    }

    fs::write(target_dir.join("build/chi_compile.log"), output.stdout).map_err(|e| CompilationError::IoError(e))?;

    Ok(())
}