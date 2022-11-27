use crate::ast::{infix::InfixOp, ty::Type};

#[derive(Debug, Clone)]
pub enum TypeCheckError {
    PlaceHolder,
    Assign,
    Boolean(Type),
    Integer(Type),
    IdentifierNoType,
    IdentifierTypeNotMatching(Type, Type),
    InfixOperandsNotMatching,
    IfContainsError,
    WhiteContainsError,
    IdentifierNotFound(String),
    IncompatibleTypesForOperand(InfixOp, Type, Type),
    TypesNotMatching(Type, Type),
}
