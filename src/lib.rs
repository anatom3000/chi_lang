use std::fs::File;
use std::path::{Path, PathBuf};
use std::io::{self, Write};

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
    
    let main_module = Module::main(main_path)?;

    let stmts = main_module.statements()?;

    let mut scope = analysis::ModuleScope::new(stmts);

    if let Err(e) = scope.analyse() {
        dbg!(e);
        panic!();
    }

    let t = transpiler::Transpiler::transpile("main".to_string(), scope);

    let mut c = File::create("generated/main.c").map_err(|e| TranspileError::FileError(e))?;
    let mut h = File::create("generated/main.h").map_err(|e| TranspileError::FileError(e))?;

    write!(c, "{}", t.lines()).map_err(|e| TranspileError::FileError(e))?;
    write!(h, "{}", t.header()).map_err(|e| TranspileError::FileError(e))?; 

    Ok(())
}
