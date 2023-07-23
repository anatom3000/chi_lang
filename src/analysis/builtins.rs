use std::collections::HashMap;
use lazy_static::lazy_static;
use super::{Type, TypeDefinition, TypeKind, BinaryOperator, UnaryOperator, Resource};

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
        (vec![stringify!($self).to_string()], Resource::Type(TypeDefinition {
            kind: TypeKind::Primitive,
            supported_binary_operations: HashMap::from([
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
            supported_unary_operations: HashMap::from([
                (UnaryOperator::Minus, type_!($self)),
                (UnaryOperator::Plus, type_!($self))
            ])
        }))
        
    }};
    (unsigned $self:ident) => {{
        let results = HashMap::from([
            (type_!($self), type_!($self)), 
        ]);
        let cmp_results = HashMap::from([
            (type_!($self), type_!(bool)),
        ]);
        (vec![stringify!($self).to_string()], Resource::Type(TypeDefinition {
            kind: TypeKind::Primitive,
            supported_binary_operations: HashMap::from([
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
            supported_unary_operations: HashMap::from([
                (UnaryOperator::Plus, type_!($self))
            ])
        }))
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
        Type::Reference(Box::new(type_!($tok)))
    }
}    


lazy_static! {
    pub(super) static ref TYPES: HashMap<Vec<String>, Resource> = HashMap::from([
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

        
        (vec!["bool".to_string()], Resource::Type(TypeDefinition {
            kind: TypeKind::Primitive,
            supported_binary_operations: HashMap::from([(BinaryOperator::Equal, HashMap::from([(type_!(bool), type_!(bool))]))]),
            supported_unary_operations: HashMap::from([(UnaryOperator::Not, type_!(bool))])
        })),

        num_type!(float;),
        num_type!(float32;),
        num_type!(float64;),
        num_type!(float128;),

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

    static ref FLOATS: [Type; 4] = [
        type_!(float),
        type_!(float32),
        type_!(float64),
        type_!(float128),
    ];
}
