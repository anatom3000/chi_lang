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
    UnknownResource(Vec<String>),
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
            ExpressionData::Variable(name) => Ok(match scope.get_variable(module, name)? {
                VariableOrAttribute::Attribute(expr) => expr.type_,
                VariableOrAttribute::Variable(ty) => ty
            }),
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
    pub(crate) is_variadic: Option<bool>,
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

pub(crate) enum VariableOrAttribute {
    Variable(Type),
    Attribute(Expression)
}

#[derive(Clone, Debug)]
pub struct FunctionScope {
    pub(crate) head: FunctionHead,
    pub(crate) source: Vec<ast::Statement>,
    pub(crate) statements: Vec<Statement>,
    pub(crate) variables: HashMap<String, Type>,
}

impl FunctionScope {
    fn get_function_head<'a>(&'a self, module: &'a ModuleScope, name: &Vec<String>) -> Result<FunctionHead, AnalysisError> {
        module.get_function_head(name)
    }

    fn get_variable<'a>(&'a self, module: &'a ModuleScope, path: &Vec<String>) -> Result<VariableOrAttribute, AnalysisError> {
        match self.variables.get(&path[0]) {
            Some(ty) if path.len() == 1 => return Ok(VariableOrAttribute::Variable(ty.clone())),
            Some(ty) => Ok(VariableOrAttribute::Attribute(
                module.get_struct_member(
                    Expression { 
                            data: ExpressionData::Variable(vec![path[0].clone()]), 
                            type_: ty.clone()
                        }, 
                        &path[1..]
                )?
            )),
            None => module.get_variable(path)
        }
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
                let function = self.get_function_head(module, &function_name)?;

                let mut typed_args = vec![];
                if let Some(true) = function.is_variadic {
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
            ast::Expression::Variable(name) => Ok(match self.get_variable(module, &name)? {
                VariableOrAttribute::Variable(var_type) => Expression { data: ExpressionData::Variable(name), type_: var_type },
                VariableOrAttribute::Attribute(expr) => expr
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


#[derive(Debug, Clone)]
pub(crate) enum Resource {
    Function(FunctionHead),
    Type(TypeDefinition),
    Variable(Type),
    #[allow(dead_code)]
    Module(Module)
}

#[derive(Clone, Debug)]
pub(crate) struct ModuleScope {
    source: Vec<ast::Statement>,
    pub(crate) statements: Vec<Statement>,
    declared_functions: HashMap<String, FunctionScope>,
    pub(crate) resources: HashMap<String, Resource>,
    pub(crate) externs: Vec<String>,
}

impl ModuleScope {
    pub(crate) fn declaration_pass(&mut self, module: &Module) -> Result<(), AnalysisError> {

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
                        is_variadic: None,
                    };

                    let func = FunctionScope::new(body, head.clone());

                    self.resources.insert(name.clone(), Resource::Function(head));
                    self.declared_functions.insert(name.clone(), func);

                    self.statements
                        .push(Statement::FunctionDeclaration { name })
                }
                ast::Statement::ExternBlock { source, body } => {
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
                                    is_variadic: Some(is_variadic),
                                };
                                

                                self.resources.insert(name, Resource::Function(func));
                            }
                            _ => unreachable!(
                                "extern blocks should only contain body-less function declarations"
                            ),
                        }
                    }

                    self.externs.push(source)
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

        for func in self.declared_functions.values_mut() {
            func.execution_pass(&module)?;
        }

        for res in self.resources.values_mut() {
            if let Resource::Module(m) = res {
                m.execution_pass()?;
            }
        }

        Ok(())
    }

    pub(crate) fn get_resource(&self, path: &Vec<String>) -> Result<Resource, AnalysisError> {
        if path.len() == 1 {
            if let Some(res) = builtins::TYPES.get(path) {
                return Ok(res.clone())
            }
        }
        let mut module = self;
        
        for (index, name) in path.iter().enumerate() {
            if index+1 == path.len() {
                return module.resources.get(name).cloned()
                    .ok_or_else(|| AnalysisError::UnknownResource(path.clone()));
            }
            match module.resources.get(name).ok_or_else(|| AnalysisError::UnknownResource(path.clone()))? {
                Resource::Module(Module { scope: Some(scope), .. }) => module = scope,
                _ => return Err(AnalysisError::UnknownResource(path.clone()))
            }
        }

        Err(AnalysisError::UnknownResource(path.clone()))
    }

    pub(crate) fn get_variable(&self, path: &Vec<String>) -> Result<VariableOrAttribute, AnalysisError> {
        if let Some(Resource::Variable(var_type)) = builtins::TYPES.get(&path[..0]) {
            return Ok(VariableOrAttribute::Attribute(
                self.get_struct_member(
                    Expression { 
                            data: ExpressionData::Variable(Vec::from(&path[..0])), 
                            type_: var_type.clone()
                        }, 
                        &path[1..]
                )?
            ));
        }

        let mut module = self;

        for (index, name) in path.iter().enumerate() {
            match module.resources.get(name).ok_or_else(|| AnalysisError::UnknownResource(path.clone()))? {
                Resource::Module(Module { scope: Some(scope), .. }) => module = scope,
                Resource::Variable(var_type) if index+1 == path.len() => return Ok(VariableOrAttribute::Variable(var_type.clone())),
                Resource::Variable(var_type) => return Ok(VariableOrAttribute::Attribute(
                    module.get_struct_member(
                        Expression { 
                                data: ExpressionData::Variable(Vec::from(&path[..=index])), 
                                type_: var_type.clone()
                            }, 
                            &path[1..]
                    )?
                )),
                _ => return Err(AnalysisError::UnknownResource(path.clone()))
            }
        }

        Err(AnalysisError::UnknownVariable(path.clone()))
    }

    pub(crate) fn get_struct_member(&self, instance: Expression, path: &[String]) -> Result<Expression, AnalysisError> {
        let typedef = match instance.type_ {
            Type::Path(ref path) => self.get_type(path)?,
            _ => return Err(AnalysisError::MemberAccessOnNonStruct { instance_type: instance.type_.clone(), member: path[0].clone() })
        };
        match typedef.kind {
            TypeKind::Struct { members } => {
                match members.get(&path[0]) {
                    Some(member) => {
                        let instance = Expression {
                            data: ExpressionData::StructMember { instance: Box::new(instance), member: path[0].clone() }, 
                            type_: member.clone()
                        };
                        if path.len() == 1 {
                            Ok(instance)
                        } else {
                            self.get_struct_member(instance, &path[1..])
                        }
                    },
                    None => Err(AnalysisError::UnknownMember { instance_type: instance.type_.clone(), member: path[0].clone() })
                }
            },
            _ => Err(AnalysisError::MemberAccessOnNonStruct { instance_type: instance.type_.clone(), member: path[0].clone() })
        } 
    }

    pub(crate) fn get_function(&self, name: &String) -> Result<&FunctionScope, AnalysisError> {
        self.declared_functions.get(name).ok_or(AnalysisError::UnknownFunction(vec![name.clone()]))
    }

    pub(crate) fn get_function_head(&self, path: &Vec<String>) -> Result<FunctionHead, AnalysisError> {
        match self.get_resource(path)? {
            Resource::Function(func) => Ok(func),
            _ => Err(AnalysisError::UnknownFunction(path.clone()))
        }
    }

    pub fn get_type(&self, path: &Vec<String>) -> Result<TypeDefinition, AnalysisError> {
        match self.get_resource(path)? {
            Resource::Type(ty) => Ok(ty),
            _ => Err(AnalysisError::UnknownType(path.clone())),
        }
    }

    pub fn new(source: Vec<ast::Statement>) -> Self {
        ModuleScope {
            source,
            statements: vec![],
            resources: HashMap::new(),
            declared_functions: HashMap::new(),
            externs: vec![],
        }
    }

    fn type_expression(&self, expression: ast::Expression) -> Result<Expression, AnalysisError> {
        FunctionScope::new(vec![], FunctionHead { return_type: Type::Void, arguments: vec![], is_variadic: None }).type_expression(self, expression)
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
