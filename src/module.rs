use std::{fmt, path::PathBuf, ffi::OsStr, fs};

use crate::{TranspileError, ast, parser};

#[derive(Debug, Clone)]
pub(crate) struct Module {
    //root: PathBuf,
    pub(crate) file: PathBuf,
    pub(crate) path: Vec<String>,
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Module {} at \"{}\"", self.path.join("."), self.file.display())
    }
}

impl Module {
    pub(crate) fn main(root: PathBuf) -> Result<Self, TranspileError> {
        let mut root = root.canonicalize().map_err(|e| TranspileError::FileError(e))?;

        if let Some(ext) = root.extension() {
            if ext != OsStr::new("chi") {
                return Err(TranspileError::WrongExtension(root));
            }
        }

        let file = root.clone();

        root.pop();

        Ok(Module { path: vec![], file })
    }

    fn child(&self, end: String) -> Result<Self, TranspileError> {
        if end.chars().any(|x| (!x.is_alphanumeric()) && x != '_') { return Err(TranspileError::NonAlphanumericModuleName(end)); }

        let Self { mut path, mut file } = self.clone();

        file.set_extension("");
        file.push(end.clone());
        file.set_extension("chi");

        path.push(end);

        Ok(Module { path, file })
    }

    fn parent(&self) -> Self {
        let Self { mut path, mut file } = self.clone();

        file.pop();
        file.set_extension("chi");

        path.pop();

        Module { path, file }
    }

    fn source(&self) -> Result<String, TranspileError> {
        fs::read_to_string(&self.file).map_err(|e| TranspileError::FileError(e))
    }

    pub fn statements(&self) -> Result<Vec<ast::Statement>, TranspileError> {
        parser::Parser::from_source(&self.source()?).parse().map_err(|e| TranspileError::ParserError(e))
    }

}