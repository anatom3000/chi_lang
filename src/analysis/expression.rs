use std::{fmt::Display, collections::HashMap};

use crate::ast::Literal;

use super::{AnalysisError, resources::{Scope, VariableOrAttribute, Variable}};

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
    MutRef,
    Deref,
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
                MutRef => "!",
                Deref => "*",
            }
        )
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
        id: usize,
    },
    MethodCall {
        method: String,
        arguments: Vec<Expression>,
        id: usize
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

#[derive(Debug, Clone)]
pub struct Expression {
    pub data: ExpressionData,
    pub type_: Type,
    pub mutable: bool,
}

impl Expression {
    pub(crate) fn type_lhs(&self, scope: &impl Scope) -> Result<Type, AnalysisError> {
        match &self.data {
            ExpressionData::Variable(name) => match scope.get_variable(name)? {
                VariableOrAttribute::Attribute(expr) => Ok(expr.type_),
                VariableOrAttribute::Variable(Variable { type_, mutable }) => match mutable {
                    true => Ok(type_.clone()),
                    false => Err(AnalysisError::WriteOnImmutableReference)
                },
            },
            ExpressionData::StructMember { instance, member } => {
                let instance_type = scope.get_named_type(&instance.type_)
                    .map_err(|e| e.with_member(member.clone()))?;

                match instance_type.kind {
                    TypeKind::Struct { ref members } => Ok(members
                        .get(member as &str)
                        .ok_or(AnalysisError::UnknownMember {
                            instance_type: instance.type_.clone(),
                            member: member.clone(),
                        })?
                        .clone()),
                    _ => {
                        return Err(AnalysisError::MemberAccessOnNonStruct {
                            instance_type: instance.type_.clone(),
                            member: member.clone(),
                        })
                    }
                }
            }
            ExpressionData::Unary {
                operator: UnaryOperator::Deref,
                argument,
            } => {
                argument.type_lhs(scope)?;
                Ok(self.type_.clone())
            }
            _ => Err(AnalysisError::AssignmentOnValue {
                value: self.clone(),
            }),
        }
    }

    pub(crate) fn coerce_to_new<'a>(self, scope: &impl Scope, expected: &Type, allow_implicit_mutable_coercion: bool) -> Result<Expression, AnalysisError> {
        let method = self.type_.coerce_method(scope, expected)
            .ok_or_else(|| AnalysisError::UnexpectedType { expected: expected.clone(), found: self.type_.clone() })?;
        
        method.apply(scope, self, allow_implicit_mutable_coercion)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Void,
    Path(Vec<String>),
    // Array {
    //     base: Box<Type>,
    //     size: String
    // },
    Reference{
        inner: Box<Type>,
        mutable: bool
    },
    // Function {
    //     return_type: Box<Type>,
    //     arguments: Vec<Type>
    // }
}

impl Type {    
    pub(crate) fn coerce_method(&self, scope: &impl Scope, expected: &Type) -> Option<CoercionMethod> {
        match (expected, self) {
            (expected, found) if expected == found => (expected == found).then_some(CoercionMethod::None),
            (Type::Reference { inner: expected_inner, mutable: false }, Type::Reference { inner: found_inner, mutable: true }) 
                => found_inner.coerce_method(scope, expected_inner),
            (Type::Reference { inner, mutable }, found) => {
                Some(CoercionMethod::Nested {
                    inner: Box::new(found.coerce_method(scope, inner)?), 
                    then:  Box::new(if *mutable {CoercionMethod::MutRef} else {CoercionMethod::Ref})
                })
            },
            _ => None,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            Self::Void => write!(f, "void"),
            Self::Path(name) => write!(f, "{}", name.join(".")),
            // Self::Array { base, size } => write!(f, "{}[{}]", base, size),
            Self::Reference{inner, mutable: false} => write!(f, "&{}", inner),
            Self::Reference{inner, mutable: true}  => write!(f, "!{}", inner),
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

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Debug, Clone)]
pub(crate) enum CoercionMethod {
    None,
    Ref,
    MutRef,
    Nested {
        inner: Box<CoercionMethod>,
        then: Box<CoercionMethod>
    }
}

impl CoercionMethod {
    pub fn apply(&self, scope: &impl Scope, value: Expression, allow_implicit_mutable_coercion: bool) -> Result<Expression, AnalysisError> {
        match self {
            Self::None => Ok(value),
            Self::Ref => Ok(Expression {
                type_: Type::Reference { inner: Box::new(value.type_.clone()), mutable: false },
                data: ExpressionData::Unary { operator: UnaryOperator::Ref, argument: Box::new(value) },
                mutable: false,
            }),
            Self::MutRef => {
                if !allow_implicit_mutable_coercion || !value.mutable {
                    return Err(AnalysisError::ExpectedMutableValue { value })
                }

                Ok(Expression {
                    type_: Type::Reference { inner: Box::new(value.type_.clone()), mutable: true },
                    data: ExpressionData::Unary { operator: UnaryOperator::Ref, argument: Box::new(value) },
                    mutable: true,
                })
            },
            Self::Nested { inner, then } => {
                let value = inner.apply(scope, value, allow_implicit_mutable_coercion)?;
                let value = then.apply(scope, value, allow_implicit_mutable_coercion)?;
                Ok(value)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Primitive,
    Struct { members: HashMap<String, Type> },
}

#[derive(Clone, Debug)]
pub struct TypeDefinition {
    pub(crate) kind: TypeKind,
    pub(crate) binary_operations: HashMap<BinaryOperator, HashMap<Type, Type>>,
    pub(crate) unary_operations: HashMap<UnaryOperator, Type>,
}

impl TypeDefinition {
    pub fn apply_binary(&self, op: BinaryOperator, right: Type) -> Option<&Type> {
        self.binary_operations.get(&op)?.get(&right)
    }

    pub fn apply_unary(&self, op: UnaryOperator) -> Option<&Type> {
        self.unary_operations.get(&op)
    }
}