use std::collections::HashMap;

use crate::{lexer::TokenData, analysis::Visibility};

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    Integer {
        value: String,
        signed: bool,
        size: Option<usize>,
    },
    Float {
        value: String,
        size: Option<usize>,
    },
    Null,
    True,
    False,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Binary {
        left: Box<Expression>,
        operator: TokenData,
        right: Box<Expression>,
    },
    Unary {
        operator: TokenData,
        argument: Box<Expression>,
    },
    ParenBlock(Box<Expression>),
    FunctionCall {
        function: Vec<String>,
        arguments: Vec<Expression>,
    },
    MethodCall {
        instance: Box<Expression>,
        method: String,
        arguments: Vec<Expression>,
    },
    StructMember {
        instance: Box<Expression>,
        member: String,
    },
    StructInit {
        path: Vec<String>,
        members: HashMap<String, Expression>,
    },
    Variable(Vec<String>),
    Literal(Literal),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Path(Vec<String>),
    // Array {
    //     base: Box<Type>,
    //     size: String
    // },
    Reference {
        inner: Box<Type>,
        mutable: bool
    },
    // Function {
    //     return_type: Box<Option<Type>>,
    //     arguments: Vec<Type>
    // }
}

#[derive(Debug, Clone)]
pub enum FunctionKind {
    Function(String),
    Method {
        receiver: (String, Type),
        name: String
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
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
        visibility: Visibility,
        kind: FunctionKind,
        arguments: Vec<(String, Type)>,
        return_type: Type,
        body: Vec<Statement>,
        is_variadic: bool,
    },
    ExternBlock {
        source: String,
        body: Vec<Statement>,
    },
    StructDeclaration {
        visibility: Visibility,
        name: String,
        members: HashMap<String, Type>,
    },
    ExternFunctionDeclaration {
        kind: FunctionKind,
        arguments: Vec<(String, Type)>,
        return_type: Type,
        is_variadic: bool,
        visibility: Visibility,
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
    Import {
        kind: Import,
        visibility: Visibility,
    },
}

#[derive(Debug, Clone)]
pub enum Import {
    Relative(String),
    Absolute(Vec<String>),
}
