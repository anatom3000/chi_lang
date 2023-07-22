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

#[derive(Debug)]
pub enum TranspileError {
    FileNameError(String),
    FileError(io::Error),
    WrongExtension(PathBuf),
    NonAlphanumericModuleName(String),
    ParserError(Vec<ParserError>),
    AnalysisError(AnalysisError)
}

pub fn transpile(main_file: &str) -> Result<(), TranspileError> {
    let main_path = Path::new(main_file).to_path_buf();
    
    let mut main_module = Module::main(main_path)?;

    main_module.analyse().map_err(|e| TranspileError::AnalysisError(e))?;


    main_module.transpile("generated".into())
}
