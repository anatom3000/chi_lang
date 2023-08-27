use crate::analysis::ResourceKind;

use super::{BinaryOperator, Type, TypeDefinition, TypeKind, UnaryOperator};
use lazy_static::lazy_static;
use std::collections::HashMap;

macro_rules! num_type {
    ($self:ident ; $($smaller:ident)*) => {{
        let results = HashMap::from([
            (type_!($self), type_!($self)),
            $( (type_!($smaller), type_!($self)), )*
        ]);
        let cmp_results = HashMap::from([
            (type_!($self), type_!(bool)),
            $( (type_!($smaller), type_!(bool)), )*
        ]);

        let path = stringify!($self).to_string();


        (path, vec![ResourceKind::Type(TypeDefinition {
            kind: TypeKind::Primitive,
            binary_operations: HashMap::from([
                (BinaryOperator::Plus, results.clone()),
                (BinaryOperator::Minus, results.clone()),
                (BinaryOperator::Mul, results.clone()),
                (BinaryOperator::Div, results.clone()),
                (BinaryOperator::Equal, cmp_results.clone()),
                (BinaryOperator::NotEqual, cmp_results.clone()),
                (BinaryOperator::Greater, cmp_results.clone()),
                (BinaryOperator::GreaterOrEqual, cmp_results.clone()),
                (BinaryOperator::Lesser, cmp_results.clone()),
                (BinaryOperator::LesserOrEqual, cmp_results.clone()),
            ]),
            unary_operations: HashMap::from([
                (UnaryOperator::Minus, type_!($self)),
                (UnaryOperator::Plus, type_!($self))
            ])
        })])

    }};
    (unsigned $self:ident) => {{
        let results = HashMap::from([
            (type_!($self), type_!($self)),
        ]);
        let cmp_results = HashMap::from([
            (type_!($self), type_!(bool)),
        ]);

        let path = stringify!($self).to_string();


        (path, vec![ResourceKind::Type(TypeDefinition {
            kind: TypeKind::Primitive,
            binary_operations: HashMap::from([
                (BinaryOperator::Plus, results.clone()),
                (BinaryOperator::Minus, results.clone()),
                (BinaryOperator::Mul, results.clone()),
                (BinaryOperator::Div, results.clone()),
                (BinaryOperator::Equal, cmp_results.clone()),
                (BinaryOperator::NotEqual, cmp_results.clone()),
                (BinaryOperator::Greater, cmp_results.clone()),
                (BinaryOperator::GreaterOrEqual, cmp_results.clone()),
                (BinaryOperator::Lesser, cmp_results.clone()),
                (BinaryOperator::LesserOrEqual, cmp_results.clone()),
            ]),
            unary_operations: HashMap::from([
                (UnaryOperator::Plus, type_!($self))
            ])
        })])
    }};
}

// helper macro
macro_rules! type_ {
    (void) => {
        Type::Void
    };
    ($first:ident) => {
        Type::Path(vec![stringify!($first).to_string()])
    };
    (& $tok:tt) => {
        Type::Reference {
            inner: Box::new(type_!($tok)),
            mutable: false
        }
    };
    (! $tok:tt) => {
        Type::Reference {
            inner: Box::new(type_!($tok)),
            mutable: true
        }
    };
}


lazy_static! {
    pub(super) static ref BUILTINS: HashMap<String, Vec<ResourceKind>> = HashMap::from([
        num_type!(int; uint),
        num_type!(unsigned uint),
        num_type!(int8;  uint8),
        num_type!(int16; uint16),
        num_type!(int32; uint32),
        num_type!(int64; uint64),
        num_type!(unsigned uint8),
        num_type!(unsigned uint16),
        num_type!(unsigned uint32),
        num_type!(unsigned uint64),
        (
            "bool".to_string(),
            vec![ResourceKind::Type(TypeDefinition {
                kind: TypeKind::Primitive,
                binary_operations: HashMap::from([(
                    BinaryOperator::Equal,
                    HashMap::from([(type_!(bool), type_!(bool))])
                )]),
                unary_operations: HashMap::from([(UnaryOperator::Not, type_!(bool))])
            })]
        ),
        (
            "char".to_string(),
            vec![ResourceKind::Type(TypeDefinition {
                kind: TypeKind::Primitive,
                binary_operations: HashMap::new(),
                unary_operations: HashMap::new()
            })]
        ),
        (
            "str".to_string(),
            vec![ResourceKind::Type(TypeDefinition {
                kind: TypeKind::Primitive,
                binary_operations: HashMap::new(),
                unary_operations: HashMap::new()
            })]
        ),
        num_type!(float;),
        num_type!(float32;),
        num_type!(float64;),
        num_type!(usize;),
        num_type!(isize; usize)
    ]);
    pub(super) static ref INTS: [Type; 12] = [
        type_!(int),
        type_!(int8),
        type_!(int16),
        type_!(int32),
        type_!(int64),
        type_!(uint),
        type_!(uint8),
        type_!(uint16),
        type_!(uint32),
        type_!(uint64),
        type_!(usize),
        type_!(isize),
    ];
    static ref FLOATS: [Type; 3] = [
        type_!(float),
        type_!(float32),
        type_!(float64),
    ];
}
