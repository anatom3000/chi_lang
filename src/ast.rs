use std::collections::HashMap;

use crate::lexer::TokenData;

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    Integer {
        value: String,
        signed: bool,
        size: Option<usize>
    },
    Float {
        value: String,
        size: Option<usize>
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
        // TODO: namespaces
        function: String,
        arguments: Vec<Expression>,
    },
    StructMember {
        instance: Box<Expression>,
        member: String
    },
    StructInit {
        name: String,
        members: HashMap<String, Expression>
    },
    Variable(String),
    Literal(Literal),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Identifier(String),
    // Array {
    //     base: Box<Type>,
    //     size: String
    // },
    Reference(Box<Type>),
    // Function {
    //     return_type: Box<Option<Type>>,
    //     arguments: Vec<Type>
    // }
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
        name: String,
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
        name: String,
        members: HashMap<String, Type>
    },
    ExternFunctionDeclaration {
        name: String,
        arguments: Vec<(String, Type)>,
        return_type: Type,
        is_variadic: bool,
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
    Import(Import)
}

#[derive(Debug, Clone)]
pub enum Import {
    Relative(String),
    Absolute(Vec<String>)
}