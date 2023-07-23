use std::collections::HashMap;
use std::fmt::Display;
use std::path::PathBuf;
use std::{mem, vec};

use crate::ast::{self, Literal};
use crate::lexer;
use crate::module::Module;
use crate::parser::ParserError;

#[macro_use]
pub mod builtins;

#[derive(Debug, Clone)]
pub enum AnalysisError {
    UnknownBinaryOperator(lexer::TokenData),
    UnknownUnaryOperator(lexer::TokenData),
    UnknownFunction(Vec<String>),
    UnknownVariable(Vec<String>),
    UnknownType(Vec<String>),
    UnknownModule(Vec<String>),
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
    WrongArgumentType {
        function: Vec<String>,
        expected: Type,
        found: Type,
    },
    UnexpectedType {
        expected: Type,
        found: Type
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
        expected: &'static [usize]
    },
    UnknownFloatSize {
        found: usize,
        expected: &'static [usize]
    },
    MemberAccessOnNonStruct {
        instance_type: Type,
        member: String
    },
    UnknownMember {
        instance_type: Type,
        member: String
    },
    DuplicateStructMember {
        struct_name: String,
        duplicated_member: String
    },
    NonStructInit {
        found: Type
    },
    MissingMemberInInit {
        struct_name: Vec<String>,
        missing: String
    },
    AssignmentOnValue {
        value: Expression
    },
    ParserError(Vec<ParserError>),
    DuplicateModuleFile {
        path: Vec<String>,
        files: (PathBuf, PathBuf)
    },
    RootModuleFileOutside {
        path: Vec<String>,
        file: PathBuf
    }
}


#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Mul,
    Div,
    Equal,
    NotEqual,
    Greater,
    GreaterOrEqual,
    Lesser,
    LesserOrEqual,
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use BinaryOperator::*;
        write!(
            f,
            "{}",
            match *self {
                Plus => "+",
                Minus => "-",
                Mul => "*",
                Div => "/",
                Equal => "==",
                NotEqual => "!=",
                Greater => ">",
                GreaterOrEqual => ">=",
                Lesser => "<",
                LesserOrEqual => "<=",
            }
        )
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
    Not,
    Plus,
    Minus,
    Ref,
    Deref
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use UnaryOperator::*;
        write!(
            f,
            "{}",
            match *self {
                Not => "not",
                Plus => "+",
                Minus => "-",
                Ref => "&",
                Deref => "*"
            }
        )
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    Void,
    Path(Vec<String>),
    // Array {
    //     base: Box<Type>,
    //     size: String
    // },
    Reference(Box<Type>),
    // Function {
    //     return_type: Box<Type>,
    //     arguments: Vec<Type>
    // }
}


impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            Self::Void => write!(f, "void"),
            Self::Path(name) => write!(f, "{}", name.join(".")),
            // Self::Array { base, size } => write!(f, "{}[{}]", base, size),
            Self::Reference(inner) => write!(f, "&{}", inner),
            // Self::Function { return_type, arguments } => {
            //     write!(f, "def(")?;
            //     write!(f, "{}", arguments.iter().map(ToString::to_string).collect::<Vec<_>>().join(", "))?;
            //     match *return_type {
            //         Self::Void => write!(f, ")"),
            //         return_type => write!(f, ") -> {}", return_type),
            //     }
            // }
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionData {
    Binary {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    Unary {
        operator: UnaryOperator,
        argument: Box<Expression>,
    },
    ParenBlock(Box<Expression>),
    FunctionCall {
        function: Vec<String>,
        arguments: Vec<Expression>,
    },
    StructMember {
        instance: Box<Expression>,
        member: String
    },
    StructInit {
        path: Vec<String>,
        members: HashMap<String, Expression>
    },
    Variable(Vec<String>),
    Literal(Literal),
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub data: ExpressionData,
    pub type_: Type
}

impl Expression {
    fn type_lhs(&self, scope: &FunctionScope, module: &ModuleScope) -> Result<Type, AnalysisError> {
        match &self.data {
            ExpressionData::Variable(name) => Ok(scope.get_variable(module, name)?.clone()),
            ExpressionData::StructMember { instance, member } => {
                let instance_type = module.get_type(match &instance.type_ {
                    Type::Path(path) => path,
                    _ => return Err(AnalysisError::MemberAccessOnNonStruct { instance_type: instance.type_.clone(), member: member.clone() })
                })?;

                match instance_type.kind {
                    TypeKind::Struct { ref members } => Ok(members.get(member as &str)
                        .ok_or(AnalysisError::UnknownMember { instance_type: instance.type_.clone(), member: member.clone() })?.clone()),
                    _ => return Err(AnalysisError::MemberAccessOnNonStruct { instance_type: instance.type_.clone(), member: member.clone() })
                }
            },
            ExpressionData::Unary { operator: UnaryOperator::Deref, argument } => {
                argument.type_lhs(scope, module)?;
                Ok(self.type_.clone())
            },
            _ => Err(AnalysisError::AssignmentOnValue { value: self.clone() })
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct FunctionHead {
    pub(crate) return_type: Type,
    pub(crate) arguments: Vec<(String, Type)>,
    pub(crate) is_variadic: bool,
}


#[derive(Debug, Clone)]
pub enum TypeKind {
    Primitive,
    Struct {
        members: HashMap<String, Type>
    }
}

#[derive(Clone, Debug)]
pub struct TypeDefinition {
    pub(crate) kind: TypeKind,
    supported_binary_operations: HashMap<BinaryOperator, HashMap<Type, Type>>,
    supported_unary_operations: HashMap<UnaryOperator, Type>
}

impl TypeDefinition {
    fn apply_binary(&self, op: BinaryOperator, right: Type) -> Option<&Type> {
        self.supported_binary_operations.get(&op)?.get(&right)
    }

    fn apply_unary(&self, op: UnaryOperator) -> Option<&Type> {
        self.supported_unary_operations.get(&op)
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
    FunctionDeclaration {
        name: String,
    },
    StructDeclaration {
        name: String
    },
    If {
        conditions_and_bodies: Vec<(Expression, Vec<Statement>)>,
        else_body: Option<Vec<Statement>>,
    },
    While {
        condition: Expression,
        body: Vec<Statement>
    },
    Return(Option<Expression>),
    Import(Vec<String>)
}

#[derive(Clone, Debug)]
pub struct FunctionScope {
    pub(crate) head: FunctionHead,
    pub(crate) source: Vec<ast::Statement>,
    pub(crate) statements: Vec<Statement>,
    // TODO: custom F
    // types: HashMap<String, Type>,
    pub(crate) variables: HashMap<String, Type>,
}

impl FunctionScope {
    fn get_function_head<'a>(&'a self, module: &'a ModuleScope, name: &Vec<String>) -> Option<&FunctionHead> {
        module.get_function_head(name)
    }

    fn get_variable<'a>(&'a self, module: &'a ModuleScope, path: &Vec<String>) -> Result<&Type, AnalysisError> {
        if path.len() == 1 {
            if let Some(ty) = self.variables.get(&path[0]) {
                return Ok(ty)
            }
        }
        module.get_variable(path)
    }

    fn execution_pass(&mut self, module: &ModuleScope) -> Result<(), AnalysisError> {
        let source = mem::take(&mut self.source);

        // TODO: accumulate errors in a vector to catch more than one error at compile time
        for stmt in source {
            let stmt = self.analyse_statement(module, stmt.clone())?;

            self.statements.push(stmt);
        }
        Ok(())
    }

    fn analyse_statement(
        &mut self,
        module: &ModuleScope,
        stmt: ast::Statement,
    ) -> Result<Statement, AnalysisError> {
        use ast::Statement::*;
        match stmt {
            Expression(expr) => Ok(Statement::Expression(self.type_expression(module, expr)?)),
            LetAssignment {
                name,
                value,
            } => {
                let rhs = self.type_expression(module, value)?;
                self.variables.insert(name.clone(), rhs.type_.clone());
                Ok(Statement::LetAssignment {
                    name: name,
                    value: rhs,
                })
            },
            Assignment { lhs, rhs } => {
                let rhs = self.type_expression(module, rhs)?;
                let lhs = self.type_expression(module, lhs)?;
                let lhs_ty = lhs.type_lhs(self, module)?;

                // TODO: coherce types
                if lhs_ty != rhs.type_ {
                    return Err(AnalysisError::UnexpectedType { expected: lhs_ty.clone(), found: rhs.type_ })
                }

                Ok(Statement::Assignment { lhs, rhs })

            },
            If {
                conditions_and_bodies,
                else_body,
            } => {
                let mut typed_conditions_and_bodies = vec![];
                for (condition, body) in conditions_and_bodies {
                    // TODO: invalidate variables that may not exist (let var = ... in if block)
                    let typed_condition = self.type_expression(module, condition)?;

                    if typed_condition.type_ != type_!(bool) {
                        return Err(AnalysisError::UnexpectedType { expected: type_!(bool), found: typed_condition.type_ })
                    }

                    let mut inner = vec![];
                    for stmt in body {
                        inner.push(self.analyse_statement(module, stmt)?);
                    }
                    typed_conditions_and_bodies
                        .push((typed_condition, inner))
                }

                match else_body {
                    Some(body) => {
                        let mut inner = vec![];
                        for stmt in body {
                            inner.push(self.analyse_statement(module, stmt)?);
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
            },
            While { condition, body } => {
                let condition = self.type_expression(module, condition)?;
                if condition.type_ != type_!(bool) {
                    return Err(AnalysisError::UnexpectedType { expected: type_!(bool), found: condition.type_ })
                }

                let mut inner = vec![];
                for stmt in body {
                    inner.push(self.analyse_statement(module, stmt)?);
                }

                Ok(Statement::While { condition, body: inner })
            },
            Return(expr) => {
                let expr = match expr {
                    Some(expr) => {
                        let typed = self.type_expression(module, expr)?;
                        if self.head.return_type != typed.type_ {
                            return Err(AnalysisError::UnexpectedType { expected: self.head.return_type.clone(), found: typed.type_ })
                        }
                        Some(typed)
                    },
                    None => {
                        if self.head.return_type != type_!(void) {
                            return Err(AnalysisError::UnexpectedType { expected: self.head.return_type.clone(), found: type_!(void) })
                        }
                        None
                    }
                };

                Ok(Statement::Return(expr))
            },
            other @ (
                  FunctionDeclaration {..}
                | ExternFunctionDeclaration {..}
                | ExternBlock {..}
                | StructDeclaration {..}
                | Import(_)
            ) => Err(AnalysisError::StatementInWrongContext {
                statement: other,
                found_context: "function body",
            }),
        }
    }

    fn new(source: Vec<ast::Statement>, head: FunctionHead) -> Self {
        let mut variables = HashMap::new();

        variables.extend(head.arguments.iter().cloned().map(|(name, type_)| {
            (
                name.clone(),
                type_
            )
        }));

        FunctionScope {
            head,
            source,
            statements: vec![],
            variables,
        }
    }

    fn type_expression(
        &self,
        module: &ModuleScope,
        expr: ast::Expression,
    ) -> Result<Expression, AnalysisError> {
        match expr {
            ast::Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left = self.type_expression(module, *left)?;
                let right = self.type_expression(module, *right)?;

                use crate::lexer::TokenData;
                let operator = match operator {
                    TokenData::Plus => BinaryOperator::Plus,
                    TokenData::Minus => BinaryOperator::Minus,
                    TokenData::Star => BinaryOperator::Mul,
                    TokenData::Slash => BinaryOperator::Div,
                    TokenData::Equal => BinaryOperator::Equal,
                    TokenData::NotEqual => BinaryOperator::NotEqual,
                    TokenData::Greater => BinaryOperator::Greater,
                    TokenData::GreatorOrEqual => BinaryOperator::GreaterOrEqual,
                    TokenData::Lesser => BinaryOperator::Lesser,
                    TokenData::LesserOrEqual => BinaryOperator::LesserOrEqual,
                    other => return Err(AnalysisError::UnknownBinaryOperator(other)),
                };
                match (&left.type_, &right.type_) {
                    (Type::Path(ltype), Type::Path(rtype)) => {
                        let ltypedef = module.get_type(ltype)?;
                        
                        Ok(match ltypedef.apply_binary(operator, Type::Path(rtype.clone())) {
                            Some(type_) => Expression { data: ExpressionData::Binary { left: Box::new(left), operator, right: Box::new(right) }, type_: type_.clone() },
                            None => {
                                let rtypedef = module.get_type(rtype)?;
        
                                match rtypedef.apply_binary(operator, left.type_.clone()) {
                                    Some(type_) => Expression { data: ExpressionData::Binary { left: Box::new(left), operator, right: Box::new(right) }, type_: type_.clone() },
                                    None => return Err(AnalysisError::UnsupportedBinaryOperation { op: operator, left: left.type_, right: right.type_ })
                                }
                            }
                        })
                    },
                    (left, right) => Err(AnalysisError::UnsupportedBinaryOperation { op: operator, left: left.clone(), right: right.clone() })
                }

            }
            ast::Expression::Unary { operator, argument } => {
                let argument = self.type_expression(module, *argument)?;

                use crate::lexer::TokenData;
                let operator = match operator {
                    TokenData::Plus => UnaryOperator::Plus,
                    TokenData::Minus => UnaryOperator::Minus,
                    TokenData::Not => UnaryOperator::Not,
                    TokenData::Ref => UnaryOperator::Ref,
                    TokenData::Star => UnaryOperator::Deref,
                    other => return Err(AnalysisError::UnknownUnaryOperator(other))
                };
                
                match (argument.type_.clone(), operator) {
                    (arg_type, UnaryOperator::Ref) => Ok(Expression {
                        data: ExpressionData::Unary { operator: UnaryOperator::Ref, argument: Box::new(argument) },
                        type_: Type::Reference(Box::new(arg_type))
                    }),
                    (Type::Path(path), _) => {
                        let arg_type = module.get_type(&path)?;
                        match arg_type.apply_unary(operator) {
                            Some(ty) => Ok(Expression {
                                data: ExpressionData::Unary { operator, argument: Box::new(argument) },
                                type_: ty.clone()
                            }),
                            None => Err(AnalysisError::UnsupportedUnaryOperation { op: operator, argument: Type::Path(path) })
                        }
                    },
                    (Type::Reference(inner), UnaryOperator::Deref) => Ok(Expression {
                        data: ExpressionData::Unary { operator: UnaryOperator::Deref, argument: Box::new(argument) }, 
                        type_: *inner
                    }),
                    (argument, op) => Err(AnalysisError::UnsupportedUnaryOperation { op, argument }),
                }
            }
            ast::Expression::ParenBlock(inner) => {
                let inner = self.type_expression(module, *inner)?;
                let type_ = inner.type_.clone();
                Ok(Expression {
                    data: ExpressionData::ParenBlock(Box::new(inner)),
                    type_,
                })
            }
            ast::Expression::FunctionCall {
                function: function_name,
                arguments,
            } => {
                let function = match self.get_function_head(module, &function_name) {
                    Some(func) => func.clone(),
                    None => return Err(AnalysisError::UnknownFunction(function_name)),
                };

                let mut typed_args = vec![];
                if function.is_variadic {
                    if function.arguments.len() > arguments.len() {
                        return Err(AnalysisError::WrongArgumentCount {
                            function: function_name,
                            expected: function.arguments.len(),
                            found: arguments.len(),
                        });
                    }

                    for (index, found) in arguments.into_iter().enumerate() {
                        let found = self.type_expression(module, found)?;

                        match function.arguments.get(index) {
                            Some(expected) => {
                                if expected.1 == found.type_ {
                                    typed_args.push(found)
                                } else {
                                    return Err(AnalysisError::WrongArgumentType {
                                        function: function_name,
                                        expected: expected.1.clone(),
                                        found: found.type_,
                                    });
                                }
                            }
                            None => typed_args.push(found),
                        }
                    }
                } else {
                    if function.arguments.len() != arguments.len() {
                        return Err(AnalysisError::WrongArgumentCount {
                            function: function_name,
                            expected: function.arguments.len(),
                            found: arguments.len(),
                        });
                    }

                    for (expected, found) in std::iter::zip(function.arguments.iter(), arguments) {
                        let found = self.type_expression(module, found)?;
                        if expected.1 == found.type_ {
                            typed_args.push(found)
                        } else {
                            return Err(AnalysisError::WrongArgumentType {
                                function: function_name,
                                expected: expected.1.clone(),
                                found: found.type_,
                            });
                        }
                    }
                }

                Ok(Expression {
                    data: ExpressionData::FunctionCall {
                        function: function_name,
                        arguments: typed_args,
                    },
                    type_: function.return_type,
                })
            }
            ast::Expression::Variable(name) => Ok(Expression {
                type_: self.get_variable(module, &name)?.clone(),
                data: ExpressionData::Variable(name),
            }),
            ast::Expression::StructMember { instance, member } => {
                let instance = self.type_expression(module, *instance)?;

                match instance.type_.clone() {
                    Type::Path(path) => match module.get_type(&path)?.kind.clone() {
                        TypeKind::Struct { members } => {
                            // get the type of the member if it exists, otherwise return an error
                            let member_type = members.get(&member)
                                .ok_or(AnalysisError::UnknownMember { instance_type: instance.type_.clone(), member: member.clone() })?.clone();

                            Ok(Expression { data: ExpressionData::StructMember { instance: Box::new(instance), member }, type_: member_type })
                        },
                        _ => Err(AnalysisError::MemberAccessOnNonStruct { instance_type: instance.type_, member })
                    },
                    _ => todo!("member access for reference types")
                }
            },
            ast::Expression::StructInit { path, mut members } => {
                match module.get_type(&path)?.kind.clone() {
                    TypeKind::Struct { members: decl_members } => {
                        let mut typed_members = HashMap::new();

                        for (m_name, m_type) in decl_members {
                            let value = members.get(&m_name)
                                .ok_or(AnalysisError::MissingMemberInInit { struct_name: path.clone(), missing: m_name.clone() })?.clone();
                            let value = self.type_expression(module, value)?;

                            if value.type_ != m_type {
                                return Err(AnalysisError::UnexpectedType { expected: m_type, found: value.type_ })
                            }

                            members.remove(&m_name);

                            typed_members.insert(m_name, value);
                        }

                        if !members.is_empty() {
                            return Err(AnalysisError::UnknownMember { 
                                instance_type: Type::Path(path), 
                                member: members.into_keys().next().expect("members is not empty") 
                            })
                        }

                        Ok(Expression { 
                            data: ExpressionData::StructInit { path: path.clone(), members: typed_members }, 
                            type_: Type::Path(path)
                        })
                    },
                    _ => Err(AnalysisError::NonStructInit { found: Type::Path(path) })
                }
            }
            ast::Expression::Literal(lit) => module.type_literal(lit)
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExternScope {
    pub(crate) source: String,
    pub(crate) functions: HashMap<String, FunctionHead>,
}

#[derive(Debug, Clone)]
pub(crate) enum Resource {
    Function(FunctionScope),
    Type(TypeDefinition),
    Variable(Type),
    #[allow(dead_code)]
    Module(Module)
}

#[derive(Clone, Debug)]
pub(crate) struct ModuleScope {
    source: Vec<ast::Statement>,
    pub(crate) statements: Vec<Statement>,
    pub(crate) resources: HashMap<String, Resource>,
    pub(crate) externs: Vec<ExternScope>,
}

impl ModuleScope {
    pub(crate) fn declaration_pass(&mut self, module: &Module) -> Result<(), AnalysisError> {
        let mut declared_functions = HashMap::new();

        for stmt in self.source.clone() {
            match stmt {
                ast::Statement::FunctionDeclaration {
                    name,
                    return_type,
                    arguments,
                    body,
                    is_variadic,
                } => {
                    if is_variadic {
                        return Err(AnalysisError::NonExternVariadic { name });
                    }

                    let arguments: Vec<(String, Type)> = arguments
                        .into_iter()
                        .map(|(name, ty)| (name, self.analyse_new_type(ty)))
                        .collect();

                    let mut return_type = self.analyse_new_type(return_type);

                    if name == "main" {
                        // implicitly return int if function is main
                        if return_type == type_!(void) {
                            return_type = type_!(int);
                        } else if return_type != type_!(int) {
                            return Err(AnalysisError::UnexpectedType { expected: type_!(int), found: return_type });
                        }
                    }

                    let head = FunctionHead {
                        return_type,
                        arguments,
                        is_variadic: false,
                    };

                    let func = FunctionScope::new(body, head);

                    self.resources.insert(name.clone(), Resource::Function(func.clone()));

                    declared_functions.insert(name.clone(), func);

                    self.statements
                        .push(Statement::FunctionDeclaration { name })
                }
                ast::Statement::ExternBlock { source, body } => {
                    let mut functions = HashMap::new();
                    for stmt in body {
                        match stmt {
                            ast::Statement::ExternFunctionDeclaration {
                                name,
                                arguments,
                                return_type,
                                is_variadic,
                            } => {
                                let arguments: Vec<(String, Type)> = arguments
                                    .into_iter()
                                    .map(|(name, ty)| (name, self.analyse_new_type(ty)))
                                    .collect();

                                let return_type = self.analyse_new_type(return_type);

                                let func = FunctionHead {
                                    return_type,
                                    arguments,
                                    is_variadic,
                                };

                                functions.insert(name, func);
                            }
                            _ => unreachable!(
                                "extern blocks should only contain body-less function declarations"
                            ),
                        }
                    }

                    self.externs.push(ExternScope { source, functions })
                }
                ast::Statement::LetAssignment {
                    name,
                    value,
                } => {
                    let rhs = self.type_expression(value)?;

                    self.resources.insert(name.clone(), Resource::Variable(rhs.type_.clone()));
                    self.statements.push(Statement::LetAssignment {
                        name,
                        value: rhs,
                    });
                },
                ast::Statement::StructDeclaration { name, members } => {
                    let mut typed_members = HashMap::new();
                    for (m_name, m_type) in members {
                        if typed_members.contains_key(&m_name) {
                            return Err(AnalysisError::DuplicateStructMember { struct_name: name, duplicated_member: m_name })
                        }
                        typed_members.insert(m_name, self.analyse_type(m_type)?);
                    }

                    self.resources.insert(name.clone(), Resource::Type(TypeDefinition { 
                        kind: TypeKind::Struct { 
                            members: typed_members
                        }, 
                        // TODO: operator overloading
                        // (or at least provide a default internal implementation for `==`)
                        supported_binary_operations: HashMap::new(),
                        supported_unary_operations: HashMap::new()
                    }));
                    self.statements.push(Statement::StructDeclaration { name })
                },
                ast::Statement::Import(kind) => match kind {
                    ast::Import::Absolute(_) => todo!("absolute paths"),
                    ast::Import::Relative(name) => self.import_local_module(module, name)?
                },
                other @ ( 
                      ast::Statement::Expression(_) 
                    | ast::Statement::If {..}
                    | ast::Statement::While {..}
                    | ast::Statement::Return {..}
                    | ast::Statement::ExternFunctionDeclaration {..}
                    | ast::Statement::Assignment {..}
                ) => {
                    return Err(AnalysisError::StatementInWrongContext {
                        statement: other,
                        found_context: "module",
                    })
                },
            }
        }
        Ok(())
    }

    fn import_local_module(&mut self, module: &Module, end: String) -> Result<(), AnalysisError> {
        let mut submodule = module.child(end.clone())?;

        submodule.declaration_pass()?;

        self.statements.push(Statement::Import(submodule.path.clone()));

        self.resources.insert(end, Resource::Module(submodule));

        Ok(())
    }

    pub(crate) fn execution_pass(&mut self) -> Result<(), AnalysisError> {
        let module = self.clone();

        for (_, res) in self.resources.iter_mut() {
            match res {
                Resource::Function(func) => func.execution_pass(&module)?,
                Resource::Module(m) => m.execution_pass()?,
                _ => ()
            }            
        }

        Ok(())
    }

    pub(crate) fn get_resource(&self, path: &Vec<String>) -> Option<&Resource> {
        if path.len() == 1 {
            if let Some(res) = builtins::TYPES.get(path) {
                return Some(res)
            }
        }
        let mut module = self;
        
        for (index, name) in path.iter().enumerate() {
            if index+1 == path.len() {
                return module.resources.get(name);
            }
            match module.resources.get(name)? {
                Resource::Module(Module { scope: Some(scope), .. }) => module = scope,
                _ => return None
            }
        }

        None
    }

    pub(crate) fn get_function(&self, path: &Vec<String>) -> Option<&FunctionScope> {
        match self.get_resource(path) {
            Some(Resource::Function(func)) => Some(func),
            _ => None
        }
    }

    pub(crate) fn get_function_head(&self, name: &Vec<String>) -> Option<&FunctionHead> {
        match self.get_function(name) {
            Some(f) => Some(&f.head),
            None => {
                if name.len() != 1 {
                    return None;
                }

                for ext in self.externs.iter() {
                    if let Some(func) = ext.functions.get(&name[0]) {
                        return Some(func);
                    }
                }
                None
            }
        }
    }

    pub(crate) fn get_variable(&self, path: &Vec<String>) -> Result<&Type, AnalysisError> {
        match self.get_resource(path) {
            Some(Resource::Variable(v)) => Ok(v),
            _ => Err(AnalysisError::UnknownVariable(path.clone()))
        }
    }

    pub fn get_type(&self, path: &Vec<String>) -> Result<&TypeDefinition, AnalysisError> {
        match self.get_resource(path) {
            Some(Resource::Type(ty)) => Ok(ty),
            _ => Err(AnalysisError::UnknownType(path.clone())),
        }
    }

    pub fn new(source: Vec<ast::Statement>) -> Self {
        ModuleScope {
            source,
            statements: vec![],
            resources: HashMap::new(),
            externs: vec![],
        }
    }

    fn type_expression(&self, expression: ast::Expression) -> Result<Expression, AnalysisError> {
        FunctionScope::new(vec![], FunctionHead { return_type: Type::Void, arguments: vec![], is_variadic: false }).type_expression(self, expression)
    }

    fn analyse_new_type(&self, ty: ast::Type) -> Type {
        match ty {
            ast::Type::Void => Type::Void,
            ast::Type::Path(name) => Type::Path(name),
            ast::Type::Reference(inner) => Type::Reference(Box::new(self.analyse_new_type(*inner))),
            // Type::Array { base, size } => Type::Array { base: Box::new(self.analyse_new_type(*base)), size },
            // Type::Function { return_type, arguments } => {
            //     let return_type = match *return_type {
            //         Some(ty) => self.analyse_new_type(ty),
            //         None => Type::Void
            //     };
            //
            //     let arguments = arguments.into_iter().map(|ty| self.analyse_new_type(ty)).collect();
            //
            //     Type::Function { return_type: Box::new(return_type), arguments }
            // }
        }
    }

    fn analyse_type(&self, ty: ast::Type) -> Result<Type, AnalysisError> {
        Ok(match ty {
            ast::Type::Void => Type::Void,
            ast::Type::Path(name) => Type::Path(self.get_type(&name).map(|_| name)?),
            ast::Type::Reference(inner) => Type::Reference(Box::new(self.analyse_type(*inner)?)),
        })
    }

    fn type_literal(&self, value: Literal) -> Result<Expression, AnalysisError> {
        let type_ = match &value {
            Literal::String(_) => type_!(&char),
            Literal::Integer { value: _, signed, size } => match size {
                None => if *signed { type_!(int) } else { type_!(uint) },
                Some(8) => if *signed { type_!(i8) } else { type_!(u8) },
                Some(16) => if *signed { type_!(i16) } else { type_!(u16) },
                Some(32) => if *signed { type_!(i32) } else { type_!(u32) },
                Some(64) => if *signed { type_!(i64) } else { type_!(u64) },
                Some(other) => return Err(AnalysisError::UnknownIntegerSize { found: *other, expected: &[8, 16, 32, 64] })
            },
            Literal::Float { value: _, size } => match size {
                None => type_!(float),
                Some(32) => type_!(float32),
                Some(64) => type_!(float64),
                Some(128) => type_!(float128),
                Some(other) => return Err(AnalysisError::UnknownFloatSize { found: *other, expected: &[32, 64, 128] })
            }
            Literal::Null => type_!(void),
            Literal::False | Literal::True => type_!(bool)
        };

        Ok(Expression { data: ExpressionData::Literal(value), type_ })
    }

}
