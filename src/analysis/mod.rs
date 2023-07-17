use std::collections::HashMap;
use std::fmt::Display;
use std::mem;

use crate::ast::{self, Literal};
use crate::lexer;

#[macro_use]
pub mod builtins;

#[derive(Debug, Clone)]
pub enum AnalysisError {
    UnknownBinaryOperator(lexer::TokenData),
    UnknownUnaryOperator(lexer::TokenData),
    UnknownFunction(String),
    UnknownVariable(String),
    UnknownType(Type),
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
        function: String,
        expected: usize,
        found: usize,
    },
    WrongArgumentType {
        function: String,
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
            }
        )
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    Void,
    Identifier(String),
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
            Self::Identifier(name) => write!(f, "{}", name),
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
pub(crate) enum ExpressionData {
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
        // TODO: namespaces
        function: String,
        arguments: Vec<Expression>,
    },
    Variable(String),
    Literal(Literal),
}

#[derive(Debug, Clone)]
pub(crate) struct Expression {
    pub data: ExpressionData,
    pub type_: Type
}

impl Expression {

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
        members: Vec<(String, Type)>
    }
}

#[derive(Clone, Debug)]
pub struct TypeDefinition {
    pub(crate) kind: TypeKind,
    supported_binary_operations: HashMap<BinaryOperator, HashMap<Type, Type>>,
    supported_unary_operations: HashMap<UnaryOperator, Type>,

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
        name: String,
        value: Expression,
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
    Return(Option<Expression>)
}

#[derive(Clone, Debug)]
pub struct FunctionScope {
    pub(crate) head: FunctionHead,
    pub(crate) source: Vec<ast::Statement>,
    pub(crate) statements: Vec<Statement>,
    // TODO: custom F
    // types: HashMap<String, Type>,
    pub(crate) variables: HashMap<String, Expression>,
}

impl FunctionScope {
    fn get_function<'a>(&'a self, module: &'a ModuleScope, name: &str) -> Option<&FunctionHead> {
        module.get_function(name)
    }

    fn get_variable<'a>(&'a self, module: &'a ModuleScope, name: &str) -> Option<&Expression> {
        self.variables
            .get(name)
            .or_else(|| module.get_variable(name))
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
                self.variables.insert(name.clone(), rhs.clone());
                Ok(Statement::LetAssignment {
                    name: name,
                    value: rhs,
                })
            },
            Assignment { name, value } => {
                let value = self.type_expression(module, value)?;
                let previous_value = match self.get_variable(module, &name) {
                    Some(var) => var,
                    None => return Err(AnalysisError::UnknownVariable(name))
                };

                // TODO: coherce types
                if value.type_ != previous_value.type_ {
                    return Err(AnalysisError::UnexpectedType { expected: previous_value.type_.clone(), found: value.type_ })
                }

                self.variables.insert(name.clone(), value.clone());

                Ok(Statement::Assignment { name, value })

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
                | ExternFunctionDeclaration{..}
                | ExternBlock{..}
                | StructDeclaration { .. }
            ) => Err(AnalysisError::StatementInWrongContext {
                statement: other,
                found_context: "function body",
            }),
        }
    }

    fn new(source: Vec<ast::Statement>, module: &ModuleScope, head: FunctionHead) -> Self {
        let mut variables = module.global_variables.clone();

        variables.extend(head.arguments.iter().cloned().map(|(name, type_)| {
            (
                name.clone(),
                Expression {
                    data: ExpressionData::Variable(name),
                    type_
                },
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
                    (Type::Identifier(ltype), Type::Identifier(rtype)) => {
                        let ltypedef = module.get_type(ltype)?;
                        
                        Ok(match ltypedef.apply_binary(operator, Type::Identifier(rtype.clone())) {
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
                    other => return Err(AnalysisError::UnknownUnaryOperator(other))
                };
                
                match argument.type_.clone() {
                    Type::Identifier(name) => {
                        let arg_type = module.get_type(&name)?;
                        match arg_type.apply_unary(operator) {
                            Some(ty) => Ok(Expression {
                                data: ExpressionData::Unary { operator, argument: Box::new(argument) },
                                type_: ty.clone()
                            }),
                            None => Err(AnalysisError::UnsupportedUnaryOperation { op: operator, argument: Type::Identifier(name) })
                        }
                    },
                    other => Err(AnalysisError::UnsupportedUnaryOperation { op: operator, argument: other }),
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
                let function = match self.get_function(module, &function_name) {
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
            ast::Expression::Variable(name) => match self.get_variable(module, &name) {
                Some(var) => Ok(Expression {
                    data: ExpressionData::Variable(name),
                    type_: var.type_.clone(),
                }),
                None => Err(AnalysisError::UnknownVariable(name)),
            },
            ast::Expression::Literal(lit) => module.type_literal(lit)
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExternScope {
    pub(crate) source: String,
    pub(crate) functions: HashMap<String, FunctionHead>,
}

#[derive(Clone, Debug)]
pub(crate) struct ModuleScope {
    source: Vec<ast::Statement>,
    pub(crate) statements: Vec<Statement>,
    pub(crate) declared_functions: HashMap<String, FunctionScope>,
    pub(crate) types: HashMap<String, TypeDefinition>,
    pub(crate) externs: Vec<ExternScope>,
    pub(crate) global_variables: HashMap<String, Expression>,
}

impl ModuleScope {
    fn declaration_pass(&mut self) -> Result<(), AnalysisError> {
        let mut declared_functions = HashMap::new();

        for stmt in self.source.iter() {
            match stmt.clone() {
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

                    let func = FunctionScope::new(body, self, head);

                    self.declared_functions.insert(name.clone(), func.clone());

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

                    self.global_variables
                        .insert(name.clone(), rhs.clone());
                    self.statements.push(Statement::LetAssignment {
                        name,
                        value: rhs,
                    });
                },
                ast::Statement::StructDeclaration { name, members } => {
                    let mut typed_members = vec![];
                    for (name, ty) in members {
                        typed_members.push((name, self.analyse_type(ty)?))
                    }

                    self.types.insert(name.clone(), TypeDefinition { 
                        kind: TypeKind::Struct { 
                            members: typed_members
                        }, 
                        // TODO: operator overloading
                        // (or at least provide a default internal implementation for `==`)
                        supported_binary_operations: HashMap::new(),
                        supported_unary_operations: HashMap::new()
                    });
                    self.statements.push(Statement::StructDeclaration { name })
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

    fn execution_pass(&mut self) -> Result<(), AnalysisError> {
        let module = self.clone();

        for (_, func) in self.declared_functions.iter_mut() {
            func.execution_pass(&module)?;
        }

        Ok(())
    }

    fn get_function(&self, name: &str) -> Option<&FunctionHead> {
        match self.declared_functions.get(name) {
            Some(func) => Some(&func.head),
            None => {
                for ext in self.externs.iter() {
                    if let Some(func) = ext.functions.get(name) {
                        return Some(func);
                    }
                }
                None
            }
        }
    }

    fn get_variable(&self, name: &str) -> Option<&Expression> {
        self.global_variables.get(name)
    }

    pub fn get_type(&self, name: &String) -> Result<&TypeDefinition, AnalysisError> {
        self.types.get(name).ok_or_else(|| AnalysisError::UnknownType(Type::Identifier(name.clone())))
    }

    pub fn from_source(source: Vec<ast::Statement>) -> Self {
        ModuleScope {
            source,
            statements: vec![],
            types: builtins::TYPES.clone(),
            declared_functions: HashMap::new(),
            externs: vec![],
            global_variables: HashMap::new(),
        }
    }

    pub fn analyse(&mut self) -> Result<(), AnalysisError> {
        self.declaration_pass()?;
        self.execution_pass()?;
        Ok(())
    }

    fn type_expression(&self, expression: ast::Expression) -> Result<Expression, AnalysisError> {
        match expression {
            ast::Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left = self.type_expression(*left)?;
                let right = self.type_expression(*right)?;

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

                let ltype = left.type_.clone();
                let rtype = right.type_.clone();
                if ltype == rtype {
                    Ok(Expression {
                        data: ExpressionData::Binary {
                            left: Box::new(left),
                            operator,
                            right: Box::new(right),
                        },
                        type_: ltype,
                    })
                } else {
                    Err(AnalysisError::UnsupportedBinaryOperation {
                        op: operator,
                        left: ltype,
                        right: rtype,
                    })
                }
            }
            ast::Expression::Unary { operator, argument } => {
                let argument = self.type_expression(*argument)?;
                let atype = argument.type_.clone();

                use crate::lexer::TokenData;
                match operator {
                    TokenData::Plus => Ok(Expression {
                        type_: atype,
                        data: ExpressionData::Unary {
                            operator: UnaryOperator::Plus,
                            argument: Box::new(argument),
                        },
                    }),
                    TokenData::Minus => Ok(Expression {
                        type_: atype,
                        data: ExpressionData::Unary {
                            operator: UnaryOperator::Minus,
                            argument: Box::new(argument),
                        },
                    }),
                    TokenData::Not => {
                        if atype == Type::Identifier("bool".to_string()) {
                            Ok(Expression {
                                type_: atype,
                                data: ExpressionData::Unary {
                                    operator: UnaryOperator::Not,
                                    argument: Box::new(argument),
                                },
                            })
                        } else {
                            Err(AnalysisError::UnsupportedUnaryOperation {
                                op: UnaryOperator::Not,
                                argument: atype,
                            })
                        }
                    }
                    other => Err(AnalysisError::UnknownUnaryOperator(other)),
                }
            }
            ast::Expression::ParenBlock(inner) => {
                let inner = self.type_expression(*inner)?;
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
                let function = match self.get_function(&function_name) {
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
                        let found = self.type_expression(found)?;

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
                        let found = self.type_expression(found)?;
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
            ast::Expression::Variable(name) => match self.get_variable(&name) {
                Some(var) => Ok(Expression {
                    data: ExpressionData::Variable(name),
                    type_: var.type_.clone(),
                }),
                None => Err(AnalysisError::UnknownVariable(name)),
            },
            ast::Expression::Literal(lit) => self.type_literal(lit)
        }
    }

    fn analyse_new_type(&self, ty: ast::Type) -> Type {
        match ty {
            ast::Type::Void => Type::Void,
            ast::Type::Identifier(name) => Type::Identifier(name),
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
            ast::Type::Identifier(name) => Type::Identifier(self.get_type(&name).map(|_| name)?),
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
