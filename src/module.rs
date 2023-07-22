use std::{fmt, path::PathBuf, ffi::OsStr, fs, collections::HashMap};

use crate::{TranspileError, ast, parser, analysis::{ModuleScope, AnalysisError}, transpiler::ModuleTranspiler};

#[derive(Debug, Clone)]
pub(crate) struct Module {
    pub(crate) file: PathBuf,
    pub(crate) path: Vec<String>,
    pub(crate) scope: Option<ModuleScope>,
    is_main: bool
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Module {} at \"{}\"", self.path.join("."), self.file.display())
    }
}

impl Module {
    pub(crate) fn main(root: PathBuf) -> Result<Self, TranspileError> {
        let mut root = root.canonicalize().map_err(|e| TranspileError::FileError(e))?;

        let package = root.file_stem().unwrap().to_str().unwrap().to_string();

        if let Some(ext) = root.extension() {
            if ext != OsStr::new("chi") {
                return Err(TranspileError::WrongExtension(root));
            }
        }

        let file = root.clone();

        root.pop();

        Ok(Module { path: vec![package], file, scope: None, is_main: true })
    }

    pub(crate) fn child(&self, end: String) -> Result<Self, AnalysisError> {
        let Self { mut path, file, scope: _, is_main } = self.clone();

        path.push(end.clone());

        if !is_main && file.parent().unwrap().file_name().unwrap() != file.file_stem().expect("module file have a stem") {
            return Err(AnalysisError::RootModuleFileOutside { path, file })
        }

        let mut single_file_module = file.clone();

        single_file_module.pop();
        single_file_module.push(end.clone());
        single_file_module.set_extension("chi");

        let mut multiple_files_module_root = file.clone();
        multiple_files_module_root.push(end.clone());
        multiple_files_module_root.push(end.clone());
        multiple_files_module_root.set_extension("chi");

        let single_exists = single_file_module.is_file();
        let multiple_exists = multiple_files_module_root.is_file();
        
        // module file resolution
        let file = if single_exists && multiple_exists {
                                return Err(AnalysisError::DuplicateModuleFile { path, files: (single_file_module, multiple_files_module_root) })
                            } else if single_exists {
                                single_file_module
                            } else if multiple_exists {
                                multiple_files_module_root
                            } else {
                                return Err(AnalysisError::UnknownModule { path })
                            };


        Ok(Module { path, file, scope: None, is_main: false })
    }

    fn source(&self) -> Result<String, AnalysisError> {
        Ok(fs::read_to_string(&self.file).unwrap())
    }

    fn statements(&self) -> Result<Vec<ast::Statement>, AnalysisError> {
        parser::Parser::from_source(&self.source()?).parse().map_err(|e| AnalysisError::ParserError(e))
    }

    pub(crate) fn declaration_pass(&mut self) -> Result<(), AnalysisError> {
        let mut scope = ModuleScope::new(self.statements()?);

        scope.declaration_pass(self)?;

        self.scope = Some(scope);

        Ok(())
    }

    pub(crate) fn execution_pass(&mut self) -> Result<(), AnalysisError> {
        let scope = self.scope.as_mut().expect("declaration pass called before execution_pass");

        scope.execution_pass()?;

        Ok(())
    }

    pub fn analyse(&mut self) -> Result<(), AnalysisError> {
        self.declaration_pass()?;
        self.execution_pass()?;

        Ok(())
    }

    pub fn transpile(self, target_dir: PathBuf) -> Result<(), TranspileError> {
        let mut transpiled_modules = HashMap::new();

        ModuleTranspiler::transpile(self.path, self.scope.expect("analysis called before transpilation"), &mut transpiled_modules);


        for m in transpiled_modules.into_values() {
            let source = m.source();
            let header = m.header();

            let source_path = target_dir.join(m.source_path);
            let header_path = target_dir.join(m.header_path);
            fs::create_dir_all(source_path.clone().parent().unwrap()).unwrap();

            fs::write(source_path, source).map_err(|e| TranspileError::FileError(e))?;
            fs::write(header_path, header).map_err(|e| TranspileError::FileError(e))?;
        }

        Ok(())
    }
}