use std::ops::Range;

use crate::ast::{infix::InfixOp, ty::Type};

#[derive(Debug, Clone)]
pub enum TypeCheckError {
    Boolean(Type, Range<usize>),
    Integer(Type, Range<usize>),
    IdentifierTypeNotMatching(String, Type, Type, Range<usize>),
    IdentifierNotFound(String, Range<usize>),
    IncompatibleTypesForOperand(InfixOp, Type, Type, Range<usize>),
    InfixTypesNotMatching(Type, Type, Range<usize>),
    DeclarationTypesNotMatching(Type, Type, String, Range<usize>),
}
