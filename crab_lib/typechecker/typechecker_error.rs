use crate::ast::ty::Type;

#[derive(Debug, Clone)]
pub enum TypeCheckError {
    PlaceHolder,
    Assign,
    Boolean(Option<Type>),
    Integer(Option<Type>),
    IdentifierNoType,
    IdentifierTypeNotMatching,
    InfixOperandsNotMatching,
    IfContainsError,
    WhiteContainsError,
}
