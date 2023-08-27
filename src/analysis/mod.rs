use std::collections::HashMap;
use std::ffi::OsStr;
use std::path::PathBuf;
use std::{fs, mem, vec, iter, ptr};

use crate::analysis::expression::BinaryOperator;
use crate::analysis::resources::ResourceKind;
use crate::ast::{self, FunctionKind};
use crate::parser::ParserError;
use crate::transpiler::ModuleTranspiler;
use crate::{lexer, parser, TranspileError};

use self::expression::{Expression, Type, TypeDefinition, TypeKind, UnaryOperator};
use self::resources::{FunctionHead, Resource, Scope, LocatedScope, Visibility, Variable, GLOBAL_SCOPE};


#[macro_use]
pub(crate) mod builtins;
pub(crate) mod resources;
pub(crate) mod expression;


#[derive(Debug, Clone)]
pub enum AnalysisError {
    UnknownBinaryOperator(lexer::TokenData),
    UnknownUnaryOperator(lexer::TokenData),
    UnknownResource{
        path: Vec<String>,
        found: Vec<ResourceKind>,
    },
    UnsupportedBinaryOperation {
        op: BinaryOperator,
        left: Type,
        right: Type,
    },
    UnsupportedUnaryOperation {
        op: UnaryOperator,
        argument: Type,
    },
    WrongArgumentCount {
        function: Vec<String>,
        expected: usize,
        found: usize,
    },
    UnexpectedType {
        expected: Type,
        found: Type,
    },
    NonExternVariadic {
        name: String,
    },
    StatementInWrongContext {
        statement: ast::Statement,
        found_context: &'static str,
    },
    UnknownIntegerSize {
        found: usize,
        expected: &'static [usize],
    },
    UnknownFloatSize {
        found: usize,
        expected: &'static [usize],
    },
    MemberAccessOnNonStruct {
        instance_type: Type,
        member: String,
    },
    UnknownMember {
        instance_type: Type,
        member: String,
    },
    UnknownMethod {
        instance_type: Type,
        method: String
    },
    DuplicateStructMember {
        struct_name: String,
        duplicated_member: String,
    },
    MissingMemberInInit {
        struct_name: Vec<String>,
        missing: String,
    },
    AssignmentOnValue {
        value: Expression,
    },
    ParserError(Vec<ParserError>),
    DuplicateModuleFile {
        path: Vec<String>,
        files: (PathBuf, PathBuf),
    },
    RootModuleFileOutside {
        path: Vec<String>,
        file: PathBuf,
    },
    ResourceShadowing {
        name: String
    },
    WriteOnImmutableReference,
    MutRefOfImmutableValue {
        value: Expression
    },
    ExpectedMutableValue {
        value: Expression
    },
    EmptyReturn,
    ExternMethod {
        recv: Type,
        name: String
    },
    PrivateResource {
        path: Vec<String>,
    },
    NotAStruct {
        found: Type,
    },
    AmbiguousOverloading {
        defined: FunctionHead,
        conflicting: FunctionHead
    },
    AmbiguousCall {
        candidates: Vec<FunctionHead>,
    },
    InvalidArguments {
        expected: Vec<Vec<Type>>,
        found: Vec<Type>
    }
}

impl AnalysisError {
    fn with_member(self, member: String) -> Self {
        match self {
            AnalysisError::NotAStruct { found } => AnalysisError::MemberAccessOnNonStruct {
                instance_type: found,
                member: member,
            },
            e => e
        }
    }
}


#[derive(Clone, Debug)]
pub(crate) enum Statement {
    Expression(Expression),
    LetAssignment {
        name: String,
        value: Expression,
    },
    Assignment {
        lhs: Expression,
        rhs: Expression,
    },
    StructDeclaration {
        name: String,
    },
    If {
        conditions_and_bodies: Vec<(Expression, Vec<Statement>)>,
        else_body: Option<Vec<Statement>>,
    },
    While {
        condition: Expression,
        body: Vec<Statement>,
    },
    Return(Option<Expression>),
    Import(String),
}


#[derive(Clone, Debug)]
pub struct FunctionScope {
    pub(crate) head: FunctionHead,
    pub(crate) source: Vec<ast::Statement>,
    pub(crate) statements: Vec<Statement>,
    pub(crate) resources: HashMap<String, Vec<ResourceKind>>,
    pub(crate) module: *mut ModuleScope
}

impl FunctionScope {
    fn module(&self) -> &mut ModuleScope {
        unsafe {
            if self.module == ptr::null_mut() {
                panic!("attempt to access parent ModuleScope while FuntionScope did not have access it")
            } else {
                &mut *self.module
            }
        }
    }

    fn execution_pass(&mut self) -> Result<(), AnalysisError> {
        let source = mem::take(&mut self.source);

        // TODO: accumulate errors in a vector to catch more than one error at compile time
        for stmt in source {
            let stmt = self.analyse_statement(stmt.clone())?;

            self.statements.push(stmt);
        }
        Ok(())
    }

    fn add_resource(&mut self, at: String, mut resource: ResourceKind) -> Result<(), AnalysisError> {
        use std::collections::hash_map::Entry;

        // borrowing rules! (or does it?)
        let unsafe_self = unsafe {&mut *(self as *mut _)};

        match self.resources.entry(at) {
            Entry::Vacant(x) => {
                x.insert(vec![resource]);
            },
            Entry::Occupied(mut x) => {
                match &mut resource {
                    ResourceKind::Alias(_)
                    | ResourceKind::Module(_)
                    => {
                        if x.get().iter().all(|r| matches!(r, ResourceKind::Method(_))) {
                            // methods are not real resources, no disambiguation required
                            x.get_mut().push(resource);
                        } else {
                            return Err(AnalysisError::ResourceShadowing { name: x.key().clone() })
                        }
                    },
                    ResourceKind::Variable(var_to_insert) => {
                        // allow variable overloading in function scopes
                        
                        for res in x.get_mut() {
                            match res {
                                ResourceKind::Method(_) => (),
                                ResourceKind::Variable(old_var) => {
                                    mem::swap(old_var, var_to_insert)
                                },
                                _ => return Err(AnalysisError::ResourceShadowing { name: x.key().clone() })
                            }
                        }
                    }
                    ResourceKind::Function(head) => {
                        for res in x.get() {
                            match &res {
                                ResourceKind::Function(existing_head) => {
                                    if existing_head.arguments.len() == head.arguments.len() {

                                        let is_similar = 
                                            iter::zip(&existing_head.arguments, &head.arguments)
                                            .all(|((_, existing), (_, new))| 
                                                   existing.coerce_method(unsafe_self, new).is_some() 
                                                || new.coerce_method(unsafe_self, existing).is_some()
                                            );

                                        if is_similar {
                                            return Err(AnalysisError::AmbiguousOverloading {
                                                defined: head.clone(),
                                                conflicting: existing_head.clone()
                                            });
                                        }
                                    }
                                },
                                ResourceKind::Method(_) // methods are not real
                                | ResourceKind::Type(_) // to support constructors as plain functions
                                    => (),
                                ResourceKind::Alias     (_)
                                | ResourceKind::Module  (_) 
                                | ResourceKind::Variable(_) 
                                    => return Err(AnalysisError::ResourceShadowing { name: x.key().clone() })
                            }
                        }
                        x.get_mut().push(resource);
                    },
                    ResourceKind::Method(_) => x.get_mut().push(resource),
                    _ => todo!()
                }
            }
        };

        Ok(())
    }


    fn analyse_statement(&mut self, stmt: ast::Statement) -> Result<Statement, AnalysisError> {
        match stmt {
            ast::Statement::Expression(expr) => Ok(Statement::Expression(self.type_expression(expr, false)?)),
            ast::Statement::LetAssignment { name, value } => {
                let rhs = self.type_expression(value, true)?;

                self.add_resource(name.clone(), ResourceKind::Variable(Variable {
                    type_: rhs.type_.clone(),
                    mutable: true
                }))?;
                Ok(Statement::LetAssignment {
                    name: name,
                    value: rhs,
                })
            }
            ast::Statement::Assignment { lhs, rhs } => {
                let lhs = self.type_expression(lhs, true)?;
                let lhs_ty = lhs.type_lhs(self)?;

                let rhs = self.type_expression(rhs, false)?
                    .coerce_to_new(self, &lhs_ty, false)?;

                Ok(Statement::Assignment { lhs, rhs })
            }
            ast::Statement::If {
                conditions_and_bodies,
                else_body,
            } => {
                let mut typed_conditions_and_bodies = vec![];
                for (condition, body) in conditions_and_bodies {
                    // TODO: invalidate variables that may not exist (let var = ... in if block)
                    let typed_condition = self.type_expression(condition, false)?
                        .coerce_to_new(self, &type_!(bool), false)?;

                    let mut inner = vec![];
                    for stmt in body {
                        inner.push(self.analyse_statement(stmt)?);
                    }
                    typed_conditions_and_bodies.push((typed_condition, inner))
                }

                match else_body {
                    Some(body) => {
                        let mut inner = vec![];
                        for stmt in body {
                            inner.push(self.analyse_statement(stmt)?);
                        }

                        Ok(Statement::If {
                            conditions_and_bodies: typed_conditions_and_bodies,
                            else_body: Some(inner),
                        })
                    }
                    None => Ok(Statement::If {
                        conditions_and_bodies: typed_conditions_and_bodies,
                        else_body: None,
                    }),
                }
            }
            ast::Statement::While { condition, body } => {
                let condition = self.type_expression(condition, false)?
                    .coerce_to_new(self, &type_!(bool), false)?;

                let mut inner = vec![];
                for stmt in body {
                    inner.push(self.analyse_statement(stmt)?);
                }

                Ok(Statement::While {
                    condition,
                    body: inner,
                })
            }
            ast::Statement::Return(expr) => {
                let expr = match expr {
                    Some(expr) => {
                        Some(
                            self.type_expression(expr, false)?
                                .coerce_to_new(self, &self.head.return_type, false)?
                        )
                    }
                    None => {
                        if self.head.return_type != type_!(void) {
                            return Err(AnalysisError::EmptyReturn)
                        }
                        
                        None
                    }
                };

                Ok(Statement::Return(expr))
            }
            other @ (ast::Statement::FunctionDeclaration { .. }
            | ast::Statement::ExternFunctionDeclaration { .. }
            | ast::Statement::ExternBlock { .. }
            | ast::Statement::StructDeclaration { .. }
            | ast::Statement::Import { .. }) => Err(AnalysisError::StatementInWrongContext {
                statement: other,
                found_context: "function body",
            }),
        }
    }

    fn new(source: Vec<ast::Statement>, head: FunctionHead) -> Self {
        let resources = HashMap::from_iter(
            head.arguments
            .iter()
            .cloned()
            .map(|(name, type_)| (name.clone(), vec![ResourceKind::Variable(Variable { mutable: true, type_ })]))
        );

        FunctionScope {
            head,
            source,
            statements: vec![],
            resources,
            module: ptr::null_mut()
        }
    }

}

#[derive(Clone)]
pub struct ModuleScope {
    pub(crate) file: PathBuf,
    pub(crate) path: Vec<String>,
    is_main: bool,

    source: Vec<ast::Statement>,
    pub(crate) statements: Vec<Statement>,
    pub(crate) declared_functions: Vec<(String, FunctionScope)>,
    pub(crate) declared_methods: Vec<(String, FunctionScope)>,
    pub(crate) resources: HashMap<String, Vec<Resource>>,
    pub(crate) externs: Vec<String>,
}

impl std::fmt::Debug for ModuleScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}Module at \"{}\">", if self.is_main{"Main "}else{""}, self.path.join("."))
    }
}

impl ModuleScope {
    pub fn analyse(&mut self) -> Result<(), AnalysisError> {
        self.declaration_pass()?;
        self.execution_pass()?;

        Ok(())
    }

    pub(crate) fn declaration_pass(&mut self) -> Result<(), AnalysisError> {
        for stmt in self.source.clone() {
            match stmt {
                ast::Statement::FunctionDeclaration {
                    kind,
                    return_type,
                    arguments,
                    body,
                    is_variadic,
                    visibility
                } => {

                    match kind {
                        FunctionKind::Function(name) => {
                            if is_variadic {
                                return Err(AnalysisError::NonExternVariadic { name });
                            }
        
                            let mut typed_arguments = vec![];
                            for (name, ty) in arguments {
                                typed_arguments.push((name, self.analyse_type(ty)?))
                            }
        
                            let mut return_type = self.analyse_type(return_type)?;

                            let is_main = self.is_main && name == "main";
        
                            if is_main {
                                // implicitly return int if function is main
                                if return_type == type_!(void) {
                                    return_type = type_!(int);
                                }
                                
                                if return_type != type_!(int) {
                                    return Err(AnalysisError::UnexpectedType {
                                        expected: type_!(int),
                                        found: return_type,
                                    });
                                }
                                
                            }
                                    
                            let head: FunctionHead = FunctionHead {
                                return_type,
                                arguments: typed_arguments,
                                is_variadic: None,
                                no_mangle: is_main
                            };
        
                            let func = FunctionScope::new(body, head.clone());
        
                            self.add_resource(name.clone(), Resource { kind: ResourceKind::Function(head), visibility })?;
                            self.declared_functions.push((name.clone(), func));
                        }
                        FunctionKind::Method { receiver: (recv_name, recv_type), name: method_name } => {
                            let recv_type = self.analyse_type(recv_type)?;

                            if is_variadic {
                                return Err(AnalysisError::NonExternVariadic { name: format!("({recv_type}).{method_name}") });
                            }

                            // a method is a function that takes its receiver as the first argument
                            let mut typed_arguments = vec![(recv_name, recv_type.clone())];
                            for (name, ty) in arguments {
                                typed_arguments.push((name, self.analyse_type(ty)?))
                            }

                            let return_type = self.analyse_type(return_type)?;

                            let head = FunctionHead {
                                return_type,
                                arguments: typed_arguments,
                                is_variadic: None,
                                no_mangle: false
                            };

                            let func = FunctionScope::new(body, head.clone());

                            self.add_resource(method_name.clone(), Resource { kind: ResourceKind::Method(head), visibility })?;
                            self.declared_methods.push((method_name, func));
                        }
                    }
                }
                ast::Statement::ExternBlock { source, body } => {
                    for stmt in body {
                        match stmt {
                            ast::Statement::ExternFunctionDeclaration {
                                kind,
                                arguments,
                                return_type,
                                is_variadic,
                                visibility,
                            } => {

                                let mut typed_arguments = vec![];
                                for (name, ty) in arguments {
                                    typed_arguments.push((name, self.analyse_type(ty)?))
                                }

                                let name = match kind {
                                    FunctionKind::Function(name) => name,
                                    FunctionKind::Method { receiver: (_, recv), name } => return Err(AnalysisError::ExternMethod { recv: self.analyse_type(recv)?, name })
                                };

                                let return_type = self.analyse_type(return_type)?;

                                let func = FunctionHead {
                                    return_type,
                                    arguments: typed_arguments,
                                    is_variadic: Some(is_variadic),
                                    no_mangle: true
                                };

                                self.add_resource(name, Resource { kind: ResourceKind::Function(func), visibility })?;
                            }
                            _ => unreachable!(
                                "extern blocks should only contain body-less function declarations"
                            ),
                        }
                    }

                    self.externs.push(source)
                }
                ast::Statement::StructDeclaration { name, members, visibility } => {
                    let mut typed_members = HashMap::new();
                    for (m_name, m_type) in members {
                        if typed_members.contains_key(&m_name) {
                            return Err(AnalysisError::DuplicateStructMember {
                                struct_name: name,
                                duplicated_member: m_name,
                            });
                        }

                        typed_members.insert(m_name, self.analyse_type(m_type)?);
                    }

                    self.add_resource(
                        name.clone(),
                        Resource { 
                            kind: ResourceKind::Type(TypeDefinition {
                                kind: TypeKind::Struct {
                                    members: typed_members,
                                },
                                // TODO: operator overloading
                                // (or at least provide a default internal implementation for `==`)
                                binary_operations: HashMap::new(),
                                unary_operations: HashMap::new(),
                            }),
                            visibility
                        }
                    )?;
                    self.statements.push(Statement::StructDeclaration { name })
                }
                ast::Statement::Import { kind, visibility } => match kind {
                    ast::Import::Absolute(path) => self.add_resource(
                        path.last().expect("path is not empty").clone(), 
                        Resource {
                            kind: ResourceKind::Alias(path),
                            visibility 
                        }
                    )?,
                    ast::Import::Relative(name) => self.add_submodule(name, visibility)?,
                },
                other @ (ast::Statement::Expression(_)
                | ast::Statement::If { .. }
                | ast::Statement::While { .. }
                | ast::Statement::Return { .. }
                | ast::Statement::ExternFunctionDeclaration { .. }
                | ast::Statement::LetAssignment { .. }
                | ast::Statement::Assignment { .. }) => {
                    return Err(AnalysisError::StatementInWrongContext {
                        statement: other,
                        found_context: "module",
                    })
                }
            }
        }
        Ok(())
    }

    fn add_submodule(&mut self, end: String, visibility: Visibility) -> Result<(), AnalysisError> {
        let mut submodule = self.child(end.clone())?;

        submodule.declaration_pass()?;

        self.statements
            .push(Statement::Import(end.clone()));

        self.add_resource(end, Resource { kind: ResourceKind::Module(submodule), visibility })?;

        Ok(())
    }

    pub(crate) fn execution_pass(&mut self) -> Result<(), AnalysisError> {
        let mut declared_functions = mem::take(&mut self.declared_functions);

        for (_, func) in &mut declared_functions {
            // SAFETY: func has exclusive access to self
            func.module = self as *mut _;
            func.execution_pass()?;
            // set pointer to null to revoke access to self
            func.module = ptr::null_mut();
        }

        self.declared_functions = declared_functions;

        let mut declared_methods = mem::take(&mut self.declared_methods);

        for (_, func) in &mut declared_methods {
            // SAFETY: func has exclusive access to self
            func.module = self as *mut _;
            func.execution_pass()?;
            // set pointer to null to revoke access to self
            func.module = ptr::null_mut();
        }

        self.declared_methods = declared_methods;

        for res in self.resources.values_mut() {
            for r in res {
                match &mut r.kind {
                    ResourceKind::Module(m) => {
                        m.execution_pass()?;
                    },
                    ResourceKind::Alias(path) => {
                        GLOBAL_SCOPE.get_resource(path)?;
                    },
                    _ => {}
                }
            }
        }

        Ok(())
    }

    fn add_resource(&mut self, at: String, resource: Resource) -> Result<(), AnalysisError> {
        use std::collections::hash_map::Entry;

        // borrowing rules! (or does it?)
        let unsafe_self = unsafe {&mut *(self as *mut _)};

        match self.resources.entry(at) {
            Entry::Vacant(x) => {
                x.insert(vec![resource]);
            },
            Entry::Occupied(mut x) => {
                match &resource.kind {
                    ResourceKind::Alias(_)
                    | ResourceKind::Module(_)
                    | ResourceKind::Variable(_)
                    => {
                        if x.get().iter().all(|r| matches!(r.kind, ResourceKind::Method(_))) {
                            // methods are not real resources, no disambiguation required
                            x.get_mut().push(resource);
                        } else {
                            return Err(AnalysisError::ResourceShadowing { name: x.key().clone() })
                        }
                    },
                    ResourceKind::Function(head) => {
                        for res in x.get() {
                            match &res.kind {
                                ResourceKind::Function(existing_head) => {
                                    if existing_head.arguments.len() == head.arguments.len() {

                                        let is_similar = 
                                            iter::zip(&existing_head.arguments, &head.arguments)
                                            .all(|((_, existing), (_, new))| 
                                                   existing.coerce_method(unsafe_self, new).is_some() 
                                                || new.coerce_method(unsafe_self, existing).is_some()
                                            );

                                        if is_similar {
                                            return Err(AnalysisError::AmbiguousOverloading {
                                                defined: head.clone(),
                                                conflicting: existing_head.clone()
                                            });
                                        }
                                    }
                                },
                                ResourceKind::Method(_) // methods are not real
                                | ResourceKind::Type(_) // to support constructors as plain functions
                                    => (),
                                ResourceKind::Alias     (_)
                                | ResourceKind::Module  (_) 
                                | ResourceKind::Variable(_) 
                                    => return Err(AnalysisError::ResourceShadowing { name: x.key().clone() })
                            }
                        }
                        x.get_mut().push(resource);
                    },
                    ResourceKind::Method(_) => x.get_mut().push(resource),
                    _ => todo!()
                }
            }
        };

        Ok(())
    }

    pub fn main(root: PathBuf) -> Result<String, TranspileError> {
        let mut root = root
            .canonicalize()
            .map_err(|e| TranspileError::FileError(e))?;

        let package = root.file_stem().unwrap().to_str().unwrap().to_string();

        if let Some(ext) = root.extension() {
            if ext != OsStr::new("chi") {
                return Err(TranspileError::WrongExtension(root));
            }
        }

        let file = root.clone();

        root.pop();

        let source = parser::Parser::from_source(&fs::read_to_string(&file).unwrap())
            .parse()
            .map_err(|e| TranspileError::ParserError(e))?;

        let new = ModuleScope {
            path: vec![package.clone()],
            is_main: true,
            file,
            source,
            statements: vec![],
            resources: HashMap::new(),
            declared_functions: vec![],
            declared_methods: vec![],
            externs: vec![],
        };

        resources::PACKAGES.with(|p| p.insert(package.clone(), new));
        
        Ok(package)
    }

    pub(crate) fn child(&self, end: String) -> Result<Self, AnalysisError> {
        let ModuleScope {
            file,
            mut path,
            is_main,
            ..
        } = self.clone();

        path.push(end.clone());

        if !is_main
            && file.parent().unwrap().file_name().unwrap()
                != file.file_stem().expect("module file have a stem")
        {
            return Err(AnalysisError::RootModuleFileOutside {
                path: path,
                file: file,
            });
        }

        let mut single_file_module = file.clone();

        single_file_module.pop();
        single_file_module.push(end.clone());
        single_file_module.set_extension("chi");

        let mut multiple_files_module_root = file.clone();
        multiple_files_module_root.pop();
        multiple_files_module_root.push(end.clone());
        multiple_files_module_root.push(end.clone());
        multiple_files_module_root.set_extension("chi");

        let single_exists = single_file_module.is_file();
        let multiple_exists = multiple_files_module_root.is_file();

        // module file resolution
        let file = if single_exists && multiple_exists {
            return Err(AnalysisError::DuplicateModuleFile {
                path: path,
                files: (single_file_module, multiple_files_module_root),
            });
        } else if single_exists {
            single_file_module
        } else if multiple_exists {
            multiple_files_module_root
        } else {
            return Err(AnalysisError::UnknownResource { path: path.clone(), found: vec![] })
        };

        let source = parser::Parser::from_source(&fs::read_to_string(&file).unwrap())
            .parse()
            .map_err(|e| AnalysisError::ParserError(e))?;

        Ok(ModuleScope {
            file,
            path,
            is_main: false,
            source,
            statements: vec![],
            resources: HashMap::new(),
            declared_functions: vec![],
            declared_methods: vec![],
            externs: vec![],
        })
    }

    pub fn transpile(&mut self, target_file: PathBuf) -> Result<(), TranspileError> {
        let transpiled = ModuleTranspiler::transpile(self);

        fs::write(target_file, transpiled.to_string()).map_err(|e| TranspileError::FileError(e))?;

        Ok(())
    }
}
