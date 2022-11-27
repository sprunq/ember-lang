use crate::ast::{infix::Infix, ty::Type};

#[derive(Debug, Clone)]
pub enum TypeCheckError {
    PlaceHolder,
    Assign,
    Boolean(Option<Type>),
    Integer(Option<Type>),
    IdentifierNoType,
    IdentifierTypeNotMatching(Option<Type>, Type),
    InfixOperandsNotMatching,
    IfContainsError,
    WhiteContainsError,
    IdentifierNotFound(String),
    IncompatibleTypesForOperand(Infix, Type, Type),
    TypesNotMatching(Type, Type),
}
