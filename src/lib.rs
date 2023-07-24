use std::path::{Path, PathBuf};
use std::io;

use analysis::AnalysisError;
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

pub fn transpile(main_file: &str, target_dir: &str) -> Result<String, TranspileError> {
    let main_path = Path::new(main_file).to_path_buf();
    
    let mut main_module = Module::main(main_path)?;

    main_module.analyse().map_err(|e| TranspileError::AnalysisError(e))?;

    let module_name = main_module.path[0].clone();

    main_module.transpile(target_dir.into())?;

    Ok(module_name)
}
