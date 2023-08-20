use std::cell::UnsafeCell;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::fmt::Display;
use std::hash::Hash;
use std::path::PathBuf;
use std::{fs, mem, vec};

use crate::ast::{self, Literal, FunctionKind};
use crate::parser::ParserError;
use crate::transpiler::ModuleTranspiler;
use crate::{lexer, parser, TranspileError};


#[macro_use]
pub mod builtins;


// I know this is really bad but I don't care
pub(crate) struct GlobalPackage(pub(crate) UnsafeCell<HashMap<String, ResourceKind>>);

impl GlobalPackage {
    fn new() -> Self {
        GlobalPackage(UnsafeCell::new(HashMap::new()))
    }
    
    pub fn get_res(&self, k: &String) -> Option<&ResourceKind> {
        unsafe { &*self.0.get() }.get(k)
    }

    pub fn get_mut(&self, k: &String) -> Option<&mut ModuleScope> {
        match self.get_res_mut(k)? {
            ResourceKind::Module(m) => Some(m),
            _ => unreachable!("resources stored in global package should a module")
        }
    }
    
    pub fn get_res_mut(&self, k: &String) -> Option<&mut ResourceKind> {
        unsafe { &mut *self.0.get() }.get_mut(k)
    }

    pub fn insert(&self, k: String, v: ModuleScope) {
        unsafe { &mut *self.0.get() }.insert(k, ResourceKind::Module(v));
    }
}

// peak Rust moment
unsafe impl Sync for GlobalPackage {}

// look, I tried everything but this is the simplest solution
// I am NOT using RwLock<HashMap<String, Arc<RwLock<ModuleScope>>>>
thread_local! {
    pub(crate) static PACKAGES: GlobalPackage = GlobalPackage::new();
}


pub(crate) fn get_global_resource<'a, 'b>(path: &'a [String], from: Option<&[String]>) -> Result<Vec<&'b ResourceKind>, AnalysisError> {
    PACKAGES.with(|p| 
        p.get_res(&path[0])
        .ok_or_else(|| AnalysisError::UnknownResource{path: Vec::from(path), found: None})
        .and_then(|m| {
            if path.len() == 1 {
                Ok(vec![m])
            } else {
                match m {
                    ResourceKind::Module(m) => Ok(m.get_local_resource(&path[1..], from)?
                        .into_iter()
                        .collect()),
                    _ => unreachable!("resources stored in global package are modules")
                }
            }
            
        })
        .map(|x| x.into_iter().map(|y| unsafe { &*(y as *const _) }).collect()) // lifetimes!
    )
}

pub(crate) fn extract_function_head<'a>(
            resources: Vec<&'a ResourceKind>,
        ) -> Result<Vec<&'a FunctionHead>, AnalysisError> {
    Ok(resources.into_iter().filter_map(|x| match x {
        ResourceKind::Function(func) => Some(func),
        _ => None
    }).collect())
}

pub(crate) fn extract_type<'a>(
            resources: Vec<&'a ResourceKind>,
            path: &Vec<String>
        ) -> Result<&'a TypeDefinition, AnalysisError> {
    Ok(resources.iter().filter_map(|x| match x {
        ResourceKind::Type(ty) => Some(ty),
        _ => None
    }).next()
    .ok_or(AnalysisError::UnknownResource { path: path.clone(), found: resources.into_iter().next().cloned() })?)
}

#[derive(Debug, Clone)]
pub enum AnalysisError {
    UnknownBinaryOperator(lexer::TokenData),
    UnknownUnaryOperator(lexer::TokenData),
    UnknownResource{
        path: Vec<String>,
        found: Option<ResourceKind>,
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Void,
    Path(Vec<String>),
    // Array {m
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
    fn mutability_hint(&self) -> bool {
        !matches!(self, Type::Reference { inner: _, mutable: false })
    }
    
    fn can_coerce_to(&self, module: &ModuleScope, expected: &Type) -> Result<bool, AnalysisError> {
        match (expected, self) {
            (Self::Path(expected), Self::Path(found)) => Ok(expected == found),
            (expected, found) if expected == found => Ok(true),
            (Type::Reference { inner: expected_inner, mutable: false }, Type::Reference { inner: found_inner, mutable: true }) 
                => found_inner.can_coerce_to(module, expected_inner),
            _ => Ok(false),
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
    MethodCall {
        receiver: Type,
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

#[derive(Debug, Clone)]
pub struct Expression {
    pub data: ExpressionData,
    pub type_: Type,
    pub mutable: bool,
}

impl Expression {
    fn type_lhs(&self, scope: &FunctionScope, module: &ModuleScope) -> Result<Type, AnalysisError> {
        match &self.data {
            ExpressionData::Variable(name) => match scope.get_variable(module, name)? {
                VariableOrAttribute::Attribute(expr) => Ok(expr.type_),
                VariableOrAttribute::Variable(Variable { type_, mutable }) => match mutable {
                    true => Ok(type_),
                    false => Err(AnalysisError::WriteOnImmutableReference)
                },
            },
            ExpressionData::StructMember { instance, member } => {
                let instance_type = module.get_named_type(&instance.type_)
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
                argument.type_lhs(scope, module)?;
                Ok(self.type_.clone())
            }
            _ => Err(AnalysisError::AssignmentOnValue {
                value: self.clone(),
            }),
        }
    }

    fn coerce_to(self, module: &ModuleScope, expected: &Type, mutable: bool) -> Result<Expression, AnalysisError> {
        if mutable && !self.mutable {
            return Err(AnalysisError::ExpectedMutableValue { value: self })
        }
        
        if self.type_.can_coerce_to(module, expected)? {
            return Ok(self)
        }

        match (self.type_.clone(), expected) {
            (found, Type::Reference { inner, mutable: false }) => {
                let new = self.coerce_to(module, inner, mutable)?;
                Ok(Expression {
                    data: ExpressionData::Unary { operator: UnaryOperator::Ref, argument: Box::new(new) },
                    mutable: false,
                    type_: Type::Reference { inner: Box::new(found), mutable: false }
                })
            }

            (found, expected) => Err(AnalysisError::UnexpectedType { expected: expected.clone(), found }),
        }
    }
    
    fn coerce_to_mutable(self, module: &ModuleScope, expected: &Type, mutable: bool) -> Result<Expression, AnalysisError> {
        if mutable && !self.mutable {
            return Err(AnalysisError::ExpectedMutableValue { value: self })
        }
        
        if self.type_.can_coerce_to(module, expected)? {
            return Ok(self)
        }

        match (self.type_.clone(), expected) {
            (found, Type::Reference { inner, mutable: ref_is_mut }) => {
                let new = self.coerce_to_mutable(module, inner, mutable)?;
                Ok(Expression {
                    data: ExpressionData::Unary { operator: UnaryOperator::Ref, argument: Box::new(new) },
                    mutable: false,
                    type_: Type::Reference { inner: Box::new(found), mutable: *ref_is_mut }
                })
            }

            (found, expected) => Err(AnalysisError::UnexpectedType { expected: expected.clone(), found }),
        }
    }

}

#[derive(Clone, Debug)]
pub struct FunctionHead {
    pub(crate) return_type: Type,
    pub(crate) arguments: Vec<(String, Type)>,
    pub(crate) is_variadic: Option<bool>,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Primitive,
    Struct { members: HashMap<String, Type> },
}

#[derive(Clone, Debug)]
pub struct TypeDefinition {
    pub(crate) kind: TypeKind,
    binary_operations: HashMap<BinaryOperator, HashMap<Type, Type>>,
    unary_operations: HashMap<UnaryOperator, Type>,
}

impl TypeDefinition {
    fn apply_binary(&self, op: BinaryOperator, right: Type) -> Option<&Type> {
        self.binary_operations.get(&op)?.get(&right)
    }

    fn apply_unary(&self, op: UnaryOperator) -> Option<&Type> {
        self.unary_operations.get(&op)
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

#[derive(Debug, Clone)]
pub struct Variable {
    type_: Type,
    mutable: bool,
}

pub(crate) enum VariableOrAttribute {
    Variable(Variable),
    Attribute(Expression),
}

#[derive(Clone, Debug)]
pub struct FunctionScope {
    pub(crate) head: FunctionHead,
    pub(crate) source: Vec<ast::Statement>,
    pub(crate) statements: Vec<Statement>,
    pub(crate) variables: HashMap<String, Variable>,
}

impl FunctionScope {
    fn get_function_head<'a>(
        &'a self,
        module: &'a ModuleScope,
        name: &Vec<String>,
    ) -> Result<Vec<&FunctionHead>, AnalysisError> {
        module.get_function_head(name)
    }

    fn get_method_head<'a>(
        &'a self,
        module: &'a ModuleScope,
        mut instance: Expression,
        method_name: String,
    ) -> Result<(Expression, &FunctionHead), AnalysisError> {
        let candidates = module.declared_methods.get(&method_name).ok_or(AnalysisError::UnknownMethod { instance_type: instance.type_.clone(), method: method_name.clone() })?;

        if let Some(func) = candidates.get(&instance.type_) {
            return Ok((instance, &func.head))
        }

        for (type_, func) in candidates {
            let mut base = type_.clone();
            let mut derefs = vec![];
            let mut found = false;
            while let Type::Reference { inner, mutable } = base {
                derefs.push(mutable);
                base = *inner;
                if base == instance.type_ { found = true; break }
            }

            if found {
                for mutable in derefs.into_iter().rev() {
                    instance = Expression {
                        type_: Type::Reference {
                            inner: Box::new(instance.type_.clone()),
                            mutable
                        },
                        data: ExpressionData::Unary { operator: if mutable {UnaryOperator::MutRef} else {UnaryOperator::Ref}, argument: Box::new(instance) },
                        mutable,
                    }
                }

                return Ok((instance, &func.head))
            }
        }

        Err(AnalysisError::UnknownMethod { instance_type: instance.type_, method: method_name })
    }

    fn get_variable<'a>(
        &'a self,
        module: &'a ModuleScope,
        path: &Vec<String>,
    ) -> Result<VariableOrAttribute, AnalysisError> {
        match self.variables.get(&path[0]) {
            Some(var) if path.len() == 1 => return Ok(VariableOrAttribute::Variable(var.clone())),
            Some(Variable { type_, mutable }) => Ok(VariableOrAttribute::Attribute(module.get_struct_member(
                Expression {
                    data: ExpressionData::Variable(vec![path[0].clone()]),
                    type_: type_.clone(),
                    mutable: *mutable
                },
                &path[1..],
            )?)),
            None => Err(AnalysisError::UnknownResource { path: path.clone(), found: None }),
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
        match stmt {
            ast::Statement::Expression(expr) => Ok(Statement::Expression(self.type_expression(module, expr, false)?)),
            ast::Statement::LetAssignment { name, value } => {
                let rhs = self.type_expression(module, value, true)?;
                self.variables.insert(name.clone(), Variable {
                    type_: rhs.type_.clone(),
                    mutable: rhs.type_.mutability_hint()
                });
                Ok(Statement::LetAssignment {
                    name: name,
                    value: rhs,
                })
            }
            ast::Statement::Assignment { lhs, rhs } => {
                let lhs = self.type_expression(module, lhs, true)?;
                let lhs_ty = lhs.type_lhs(self, module)?;

                let rhs = self.type_expression(module, rhs, false)?
                    .coerce_to(module, &lhs_ty, false)?;

                Ok(Statement::Assignment { lhs, rhs })
            }
            ast::Statement::If {
                conditions_and_bodies,
                else_body,
            } => {
                let mut typed_conditions_and_bodies = vec![];
                for (condition, body) in conditions_and_bodies {
                    // TODO: invalidate variables that may not exist (let var = ... in if block)
                    let typed_condition = self.type_expression(module, condition, false)?
                        .coerce_to(module, &type_!(bool), false)?;

                    let mut inner = vec![];
                    for stmt in body {
                        inner.push(self.analyse_statement(module, stmt)?);
                    }
                    typed_conditions_and_bodies.push((typed_condition, inner))
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
            }
            ast::Statement::While { condition, body } => {
                let condition = self.type_expression(module, condition, false)?
                    .coerce_to(module, &type_!(bool), false)?;

                let mut inner = vec![];
                for stmt in body {
                    inner.push(self.analyse_statement(module, stmt)?);
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
                            self.type_expression(module, expr, false)?
                                .coerce_to(module, &self.head.return_type, false)?
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
        let variables = HashMap::from_iter(
            head.arguments
            .iter()
            .cloned()
            .map(|(name, type_)| (name.clone(), Variable { mutable: type_.mutability_hint(), type_ }))
        );

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
        maybe_mutable: bool
    ) -> Result<Expression, AnalysisError> {
        match expr {
            ast::Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left = self.type_expression(module, *left, maybe_mutable)?;
                let right = self.type_expression(module, *right, maybe_mutable)?;

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
                        let rtypedef = module.get_type(rtype)?;

                        Ok(
                            match ltypedef.apply_binary(operator, Type::Path(rtype.clone()))
                            {
                                Some(type_) => Expression {
                                    data: ExpressionData::Binary {
                                        left: Box::new(left),
                                        operator,
                                        right: Box::new(right),
                                    },
                                    type_: type_.clone(),
                                    mutable: maybe_mutable && type_.mutability_hint()
                                },
                                None => match rtypedef.apply_binary(operator, left.type_.clone()) {
                                    Some(type_) => Expression {
                                        data: ExpressionData::Binary {
                                            left: Box::new(left),
                                            operator,
                                            right: Box::new(right),
                                        },
                                        type_: type_.clone(),
                                        mutable: maybe_mutable && type_.mutability_hint()
                                    },
                                    None => {
                                        return Err(AnalysisError::UnsupportedBinaryOperation {
                                            op: operator,
                                            left: left.type_,
                                            right: right.type_,
                                        })
                                    }
                                },
                            },
                        )
                    }
                    (left, right) => Err(AnalysisError::UnsupportedBinaryOperation {
                        op: operator,
                        left: left.clone(),
                        right: right.clone(),
                    }),
                }
            }
            ast::Expression::Unary { operator, argument } => {
                let argument = self.type_expression(module, *argument, true)?;

                use crate::lexer::TokenData;
                let operator = match operator {
                    TokenData::Plus => UnaryOperator::Plus,
                    TokenData::Minus => UnaryOperator::Minus,
                    TokenData::Not => UnaryOperator::Not,
                    TokenData::Ref => UnaryOperator::Ref,
                    TokenData::MutRef => UnaryOperator::MutRef,
                    TokenData::Star => UnaryOperator::Deref,
                    other => return Err(AnalysisError::UnknownUnaryOperator(other)),
                };

                match (argument.type_.clone(), operator) {
                    (arg_type, UnaryOperator::Ref) => Ok(Expression {
                        data: ExpressionData::Unary {
                            operator: UnaryOperator::Ref,
                            argument: Box::new(argument),
                        },
                        type_: Type::Reference {
                            inner: Box::new(arg_type),
                            mutable: false,
                        },
                        mutable: false
                    }),
                    (arg_type, UnaryOperator::MutRef) => {
                        if !argument.mutable {
                            return Err(AnalysisError::MutRefOfImmutableValue { value: argument })
                        }

                        Ok(Expression {
                            data: ExpressionData::Unary {
                                operator: UnaryOperator::Ref,
                                argument: Box::new(argument),
                            },
                            type_: Type::Reference {
                                inner: Box::new(arg_type),
                                mutable: maybe_mutable,
                            },
                            mutable: maybe_mutable
                        })
                    },
                    (Type::Path(path), _) => {
                        let arg_type = module.get_type(&path)?;
                        match arg_type.apply_unary(operator) {
                            Some(ty) => Ok(Expression {
                                data: ExpressionData::Unary {
                                    operator,
                                    argument: Box::new(argument),
                                },
                                type_: ty.clone(),
                                mutable: maybe_mutable
                            }),
                            None => Err(AnalysisError::UnsupportedUnaryOperation {
                                op: operator,
                                argument: Type::Path(path),
                            }),
                        }
                    }
                    (Type::Reference{inner, mutable}, UnaryOperator::Deref) => Ok(Expression {
                        data: ExpressionData::Unary {
                            operator: UnaryOperator::Deref,
                            argument: Box::new(argument),
                        },
                        type_: *inner,
                        mutable: maybe_mutable && mutable,
                    }),
                    (argument, op) => {
                        Err(AnalysisError::UnsupportedUnaryOperation { op, argument })
                    }
                }
            }
            ast::Expression::ParenBlock(inner) => {
                let inner = self.type_expression(module, *inner, maybe_mutable)?;
                let type_ = inner.type_.clone();
                Ok(Expression {
                    mutable: inner.mutable,
                    data: ExpressionData::ParenBlock(Box::new(inner)),
                    type_,
                })
            }
            ast::Expression::FunctionCall {
                function: function_name,
                arguments,
            } => {
                // TODO: function overloading
                let function = self.get_function_head(module, &function_name)?[0];

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
                        let found = self.type_expression(module, found, true)?;

                        match function.arguments.get(index) {
                            Some(expected) => {
                                // TODO: ownership
                                let should_be_mutable = expected.1.mutability_hint();

                                let found = found.coerce_to(module, &expected.1, should_be_mutable)?;

                                typed_args.push(found)
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
                        // TODO: ownership
                        let should_be_mutable = expected.1.mutability_hint();

                        typed_args.push(
                            self.type_expression(module, found, should_be_mutable)?
                                .coerce_to(module, &expected.1, should_be_mutable)?
                        )
                    }
                }

                let path = module.make_path_absolute(function_name)?;

                Ok(Expression {
                    data: ExpressionData::FunctionCall {
                        function: path,
                        arguments: typed_args,
                    },
                    type_: function.return_type.clone(),
                    mutable: maybe_mutable && function.return_type.mutability_hint()
                })
            }
            ast::Expression::MethodCall { instance, method, arguments } => {
                let instance = self.type_expression(module,*instance, true)?;
                let (instance, function) = self.get_method_head(module, instance, method.clone())?;

                let mut typed_args = vec![];

                if function.arguments.len() != arguments.len()+1 {
                    return Err(AnalysisError::WrongArgumentCount {
                        function: vec![format!("{}.{}", function.arguments[0].1, method)],
                        expected: function.arguments.len(),
                        found: arguments.len(),
                    });
                }

                let mut func_args_iter = function.arguments.iter();

                let (_, receiver_type) = func_args_iter.next().expect("methods always have at least one parameter");

                typed_args.push(instance.coerce_to_mutable(module, receiver_type, receiver_type.mutability_hint())?);

                for (expected, found) in std::iter::zip(func_args_iter, arguments) {
                    // TODO: ownership
                    let should_be_mutable = expected.1.mutability_hint();

                    typed_args.push(
                        self.type_expression(module, found, should_be_mutable)?
                            .coerce_to(module, &expected.1, should_be_mutable)?
                    )
                }

                Ok(Expression {
                    data: ExpressionData::MethodCall {
                        receiver: receiver_type.clone(),
                        method,
                        arguments: typed_args
                    },
                    type_: function.return_type.clone(),
                    mutable: maybe_mutable && function.return_type.mutability_hint()
                })
            },
            ast::Expression::Variable(name) => Ok(match self.get_variable(module, &name)? {
                VariableOrAttribute::Variable(var) => Expression {
                    data: ExpressionData::Variable(name),
                    type_: var.type_,
                    mutable: maybe_mutable && var.mutable
                },
                VariableOrAttribute::Attribute(expr) => expr,
            }),
            ast::Expression::StructMember { instance, member } => {
                let instance = self.type_expression(module, *instance, maybe_mutable)?;

                match module.get_named_type(&instance.type_)
                        .map_err(|e| e.with_member(member.clone()))?.kind.clone() {
                    TypeKind::Struct { members } => {
                        // get the type of the member if it exists, otherwise return an error
                        let member_type = members
                            .get(&member)
                            .ok_or(AnalysisError::UnknownMember {
                                instance_type: instance.type_.clone(),
                                member: member.clone(),
                            })?
                            .clone();

                        Ok(Expression {
                            mutable: instance.mutable,
                            data: ExpressionData::StructMember {
                                instance: Box::new(instance),
                                member,
                            },
                            type_: member_type,
                        })
                    }
                    _ => todo!("member access for reference types"),
                }
            }
            ast::Expression::StructInit { path, mut members } => {
                let ty = module.get_type(&path)?;
                let path = module.make_path_absolute(path)?;
                match ty.kind.clone() {
                    TypeKind::Struct {
                        members: decl_members,
                    } => {
                        let mut typed_members = HashMap::new();

                        for (m_name, m_type) in decl_members {
                            let value = members
                                .get(&m_name)
                                .ok_or(AnalysisError::MissingMemberInInit {
                                    struct_name: path.clone(),
                                    missing: m_name.clone(),
                                })?
                                .clone();

                            // TODO: ownership
                            members.remove(&m_name);

                            typed_members.insert(
                                m_name, 
                                self.type_expression(module, value, maybe_mutable)?
                                    .coerce_to(module, &m_type, maybe_mutable)?
                            );
                        }

                        if !members.is_empty() {
                            return Err(AnalysisError::UnknownMember {
                                instance_type: Type::Path(path),
                                member: members.into_keys().next().expect("members is not empty"),
                            });
                        }

                        Ok(Expression {
                            data: ExpressionData::StructInit {
                                path: path.clone(),
                                members: typed_members,
                            },
                            type_: Type::Path(path.clone()),
                            mutable: true
                        })
                    }
                    _ => Err(AnalysisError::NotAStruct {
                        found: Type::Path(path),
                    }),
                }
            }
            ast::Expression::Literal(lit) => module.type_literal(lit),
        }
    }

    
}

#[derive(Debug, Clone)]
pub enum ResourceKind {
    Function(FunctionHead),
    Method(FunctionHead),
    Type(TypeDefinition),
    #[allow(dead_code)]
    Variable(Variable),
    Module(ModuleScope),
    Alias(Vec<String>)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Visibility {
    Public,
    Module,
}

impl Default for Visibility {
    fn default() -> Self {
        // TODO: make resources private by default
        Visibility::Public
    }
}

#[derive(Clone, Debug)]
pub(crate) struct Resource {
    pub kind: ResourceKind,
    #[allow(dead_code)]
    pub visibility: Visibility
}

#[derive(Clone, Debug)]
pub struct ModuleScope {
    pub(crate) file: PathBuf,
    pub(crate) path: Vec<String>,
    is_main: bool,

    source: Vec<ast::Statement>,
    pub(crate) statements: Vec<Statement>,
    pub(crate) declared_functions: HashMap<String, FunctionScope>,
    pub(crate) declared_methods: HashMap<String, HashMap<Type, FunctionScope>>,
    pub(crate) resources: HashMap<String, Vec<Resource>>,
    pub(crate) externs: Vec<String>,
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
        
                            if name == "main" {
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
                            };
        
                            let func = FunctionScope::new(body, head.clone());
        
                            self.add_resource(name.clone(), Resource { kind: ResourceKind::Function(head), visibility })?;
                            self.declared_functions.insert(name.clone(), func);
        
                            self.statements
                                .push(Statement::FunctionDeclaration { name })
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
                                is_variadic: None
                            };

                            let func = FunctionScope::new(body, head.clone());

                            self.add_resource(method_name.clone(), Resource { kind: ResourceKind::Method(head), visibility })?;


                            self.declared_methods.entry(method_name).or_insert_with(HashMap::new)
                                .insert(recv_type, func);
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

        for func in declared_functions.values_mut() {
            func.execution_pass(self)?;
        }

        self.declared_functions = declared_functions;

        let mut declared_methods = mem::take(&mut self.declared_methods);


        for impls in declared_methods.values_mut() {
            for func in impls.values_mut() {
                func.execution_pass(self)?;
            }
        }

        self.declared_methods = declared_methods;

        for res in self.resources.values_mut() {
            for r in res {
                match &mut r.kind {
                    ResourceKind::Module(m) => {
                        m.execution_pass()?;
                    },
                    ResourceKind::Alias(path) => {
                        get_global_resource(&path, Some(&self.path))?;
                    },
                    _ => {}

                }
            }
        }

        Ok(())
    }

    fn get_local_resource(&self, path: &[String], from: Option<&[String]>) -> Result<Vec<&ResourceKind>, AnalysisError> {
        let mut module = self;

        let mut path_iter = path.iter();
        let end = path_iter.next_back().expect("path is not empty");

        for name in path_iter {
            match module.resources.get(name).map(Vec::as_slice) {
                Some([Resource {kind: ResourceKind::Alias(res_path), visibility: _}]) => {
                    if let [ResourceKind::Module(m)] = get_global_resource(res_path, from)?.as_slice() {
                        module = m;
                    } else {
                        return Err(AnalysisError::UnknownResource{ path: Vec::from(path), found: None })
                    }
                },
                Some([Resource {kind: ResourceKind::Module(m), visibility: _}]) => {
                    module = m;
                },
                _ => return Err(AnalysisError::UnknownResource{ path: Vec::from(path), found: None })
            }
        }
        
        // TODO: visibility
        match module.resources.get(end).map(Vec::as_slice) {
            Some([Resource { kind: ResourceKind::Alias(res_path), visibility: _ }]) => get_global_resource(res_path, from),
            Some([Resource { kind, visibility: _ }]) => Ok(vec![kind]),
            _ => return Err(AnalysisError::UnknownResource{ path: Vec::from(path), found: None })
        }
        
    }

    pub(crate) fn get_resource(&self, path: &[String]) -> Result<Vec<&ResourceKind>, AnalysisError> {

        if path.len() == 1 {
            if let Some(res) = builtins::BUILTINS.get(&path[0]) {
                return Ok(res.iter().collect()); // convert &Vec<ResourceKind> to Vec<&ResourceKind>
            }
        }

        self.get_local_resource(path, Some(&self.path))
    }

    pub(crate) fn get_struct_member(
        &self,
        instance: Expression,
        path: &[String],
    ) -> Result<Expression, AnalysisError> {
        let typedef = self.get_named_type(&instance.type_)
            .map_err(|e| e.with_member(path[0].clone()))?;

        match &typedef.kind {
            TypeKind::Struct { members } => match members.get(&path[0]) {
                Some(member) => {
                    let instance = Expression {
                        mutable: instance.mutable,
                        data: ExpressionData::StructMember {
                            instance: Box::new(instance),
                            member: path[0].clone(),
                        },
                        type_: member.clone(),
                    };
                    if path.len() == 1 {
                        Ok(instance)
                    } else {
                        self.get_struct_member(instance, &path[1..])
                    }
                }
                None => Err(AnalysisError::UnknownMember {
                    instance_type: instance.type_.clone(),
                    member: path[0].clone(),
                }),
            },
            _ => Err(AnalysisError::MemberAccessOnNonStruct {
                instance_type: instance.type_.clone(),
                member: path[0].clone(),
            }),
        }
    }

    pub(crate) fn get_function(&self, name: &String) -> Result<&FunctionScope, AnalysisError> {
        self.declared_functions
            .get(name)
            .ok_or(AnalysisError::UnknownResource { path: vec![name.clone()], found: None })
    }

    pub(crate) fn get_function_head(
        &self,
        path: &Vec<String>,
    ) -> Result<Vec<&FunctionHead>, AnalysisError> {
        Ok(self.get_resource(path)?.into_iter().filter_map(|x| match x {
            ResourceKind::Function(func) => Some(func),
            _ => None
        }).collect())
    }

    pub fn get_type(&self, path: &Vec<String>) -> Result<&TypeDefinition, AnalysisError> {
        let resources = self.get_resource(path)?;

        Ok(resources.iter().filter_map(|x| match x {
            ResourceKind::Type(func) => Some(func),
            _ => None
        }).next()
        .ok_or(AnalysisError::UnknownResource { path: path.clone(), found: resources.into_iter().next().cloned() })?)
    }

    pub fn get_named_type(&self, type_: &Type) -> Result<&TypeDefinition, AnalysisError> {
        match type_ {
            Type::Path(path) => extract_type(get_global_resource(path, None)?, path),
            _ => Err(AnalysisError::NotAStruct {
                found: type_.clone()
            })
        }
    }

    fn add_resource(&mut self, at: String, resource: Resource) -> Result<(), AnalysisError>{
        use std::collections::hash_map::Entry;
        match self.resources.entry(at) {
            Entry::Vacant(x) => x.insert(vec![resource]),
            Entry::Occupied(x) => return Err(AnalysisError::ResourceShadowing { name: x.key().clone() })
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
            declared_functions: HashMap::new(),
            declared_methods: HashMap::new(),
            externs: vec![],
        };

        PACKAGES.with(|p| p.insert(package.clone(), new));
        
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
            return Err(AnalysisError::UnknownResource { path: path.clone(), found: None })
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
            declared_functions: HashMap::new(),
            declared_methods: HashMap::new(),
            externs: vec![],
        })
    }

    fn analyse_type(&self, ty: ast::Type) -> Result<Type, AnalysisError> {
        Ok(match ty {
            ast::Type::Void => Type::Void,
            ast::Type::Path(name) => {
                self.get_type(&name)?;
                Type::Path(self.make_path_absolute(name)?)
            }
            ast::Type::Reference { inner, mutable } => Type::Reference {
                inner: Box::new(self.analyse_type(*inner)?),
                mutable: mutable
            },
        })
    }

    pub(crate) fn make_path_absolute(&self, mut path: Vec<String>) -> Result<Vec<String>, AnalysisError> {
        if path.len() == 1 && builtins::BUILTINS.get(&path[0]).is_some() {
            return Ok(path);
        }

        if self.is_main && path.len() == 1 && path[0] == "main" {
            return Ok(path);
        }

        let mut nonlocal = false;
        let mut absolute = Vec::with_capacity(path.len());

        let mut module = self;

        let mut path_iter = path.iter();
        let end = path_iter.next_back().expect("path is not empty").clone();


        for name in path_iter {
            match module.resources.get(name).map(Vec::as_slice) {
                Some([Resource {kind: ResourceKind::Alias(path), ..}]) => {
                    if let [ResourceKind::Module(m)] = get_global_resource(&path, None)?.as_slice() {
                        absolute.clear();
                        absolute.append(&mut path.clone());
                        module = m
                    }
                },
                Some([Resource {kind: ResourceKind::Module(m), ..}]) => {
                    if !nonlocal {
                        absolute.append(&mut self.path.clone());
                    }
                    absolute.push(name.clone());
                    module = m
                },
                _ => return Err(AnalysisError::UnknownResource { path: path.clone(), found: None }),
            }
            nonlocal = true;
        }
        
        if nonlocal {
            match module.resources.get(&end).map(Vec::as_slice) {
                Some([Resource { kind: ResourceKind::Alias(res_path), .. }]) => {
                    Ok(res_path.clone())
                },
                Some(_) => {
                    absolute.push(end);
                    Ok(absolute)
                }
                _ => return Err(AnalysisError::UnknownResource { path: path.clone(), found: None }),
            }
        } else {
            match module.resources.get(&end).map(Vec::as_slice) {
                Some([Resource { kind: ResourceKind::Alias(res_path), .. }]) => {
                    Ok(res_path.clone())
                },
                Some(_) => {
                    let mut absolute = self.path.clone();
                    absolute.append(&mut path);
                    Ok(absolute)
                }
                _ => return Err(AnalysisError::UnknownResource { path: path.clone(), found: None }),
            }
            
        }       
    }

    fn type_literal(&self, value: Literal) -> Result<Expression, AnalysisError> {
        let type_ = match &value {
            Literal::String(_) => type_!(str),
            Literal::Integer {
                value: _,
                signed,
                size,
            } => match size {
                None => {
                    if *signed {
                        type_!(int)
                    } else {
                        type_!(uint)
                    }
                }
                Some(8) => {
                    if *signed {
                        type_!(int8)
                    } else {
                        type_!(uint8)
                    }
                }
                Some(16) => {
                    if *signed {
                        type_!(int16)
                    } else {
                        type_!(uint16)
                    }
                }
                Some(32) => {
                    if *signed {
                        type_!(int32)
                    } else {
                        type_!(uint32)
                    }
                }
                Some(64) => {
                    if *signed {
                        type_!(int64)
                    } else {
                        type_!(uint64)
                    }
                }
                Some(other) => {
                    return Err(AnalysisError::UnknownIntegerSize {
                        found: *other,
                        expected: &[8, 16, 32, 64],
                    })
                }
            },
            Literal::Float { value: _, size } => match size {
                None => type_!(float),
                Some(32) => type_!(float32),
                Some(64) => type_!(float64),
                Some(128) => type_!(float128),
                Some(other) => {
                    return Err(AnalysisError::UnknownFloatSize {
                        found: *other,
                        expected: &[32, 64, 128],
                    })
                }
            },
            Literal::Null => type_!(&void),
            Literal::False | Literal::True => type_!(bool),
        };

        Ok(Expression {
            data: ExpressionData::Literal(value),
            type_,
            mutable: true
        })
    }

    pub fn transpile(&mut self, target_file: PathBuf) -> Result<(), TranspileError> {
        let transpiled = ModuleTranspiler::transpile(self);

        fs::write(target_file, transpiled.to_string()).map_err(|e| TranspileError::FileError(e))?;

        Ok(())
    }
}
