use std::process::ExitCode;

use crap_lang::*;
use std::env;

fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();

    let src = match args.get(1) {
        Some(path) => path,
        None => {
            eprintln!("Please provide an input file as the first argument");
            eprintln!("Exitting...");
            return ExitCode::FAILURE;
        }
    };

    let target_dir = "generated";

    match compile_and_run(src, target_dir) {
        Ok(code) => {
            ExitCode::from(code)
        },
        Err(compilation::CompilationError::TranspileError(err)) => {
            eprintln!("An error occured during transpilation: {err:#?}");
            ExitCode::FAILURE
        },
        Err(compilation::CompilationError::CompilerError(msg)) => {
            eprintln!("Compiler failed to compile generared C code: \n{msg}");
            ExitCode::FAILURE
        },
        Err(compilation::CompilationError::UnsupportedPlatform(plat)) => {
            eprintln!("Cannot compile generated C code on {plat}, skipping compilation...");
            ExitCode::FAILURE
        },
        Err(compilation::CompilationError::IoError(err)) => {
            eprintln!("IO Error while compiling generated C code: {err}");
            ExitCode::FAILURE
        },
    }
}
