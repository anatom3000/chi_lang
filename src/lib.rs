use std::path::{Path, PathBuf};
use std::io;
use std::process::Command;

use analysis::AnalysisError;
use compilation::CompilationError;
use parser::ParserError;

use crate::module::Module;

mod analysis;
mod ast;
mod lexer;
mod parser;
mod module;
mod transpiler;
pub mod compilation;

#[derive(Debug)]
pub enum TranspileError {
    FileNameError(String),
    FileError(io::Error),
    WrongExtension(PathBuf),
    NonAlphanumericModuleName(String),
    ParserError(Vec<ParserError>),
    AnalysisError(AnalysisError)
}

fn transpile(main_file: &str, target_dir: &str) -> Result<String, TranspileError> {
    let main_path = Path::new(main_file).to_path_buf();
    
    let mut main_module = Module::main(main_path)?;

    main_module.analyse().map_err(|e| TranspileError::AnalysisError(e))?;

    let module_name = main_module.path[0].clone();

    main_module.transpile(target_dir.into())?;

    Ok(module_name)
}

fn compile(main_file: &str, target_dir: &str) -> Result<String, CompilationError> {
    let module_name = transpile(main_file, target_dir).map_err(|e|CompilationError::TranspileError(e))?;

    compilation::compile(PathBuf::from(target_dir))?;
    Ok(module_name)
}

pub fn compile_and_run(main_file: &str, target_dir: &str) -> Result<u8, CompilationError> {
    let module_name = compile(main_file, target_dir)?;

    Ok(
        Command::new(format!("{target_dir}/build/{module_name}"))
            .status().unwrap()
            .code().unwrap_or(-1) as u8
    )
}

#[cfg(test)]
mod tests {
    pub fn compile_and_test(main_file: &'static str, target_dir: &str) -> Result<u8, super::CompilationError> {
        use std::{process, fs};

        // clear the previously generated code
        let _ = fs::remove_dir_all(target_dir);

        let module_name = super::compile(main_file, target_dir)?;

        Ok(
            process::Command::new(format!("{target_dir}/build/{module_name}"))
                // don't print program output during tests!
                .stdout(process::Stdio::null())
                .stderr(process::Stdio::null())
                .status().unwrap()
                .code().unwrap_or(-1) as u8
        )
    }
    

    macro_rules! test_program {
        ($name:ident: $path:expr) => {
            #[test]
            fn $name() {
                let result = compile_and_test($path, &format!("generated_tests/{}", stringify!($name))).unwrap();
                assert_eq!(result, 0);
            }
        };
    }

    test_program!(test_assignment: "examples/assignment.chi");
    test_program!(test_control_flow: "examples/control_flow.chi");
    test_program!(test_externs: "examples/externs.chi");
    test_program!(test_literals: "examples/literals.chi");
    test_program!(test_newlines: "examples/newlines.chi");
    test_program!(test_recursion: "examples/recursion.chi");
    test_program!(test_references: "examples/references.chi");
    test_program!(test_structs: "examples/structs.chi");
    test_program!(test_modules: "examples/modules/modules.chi");
}