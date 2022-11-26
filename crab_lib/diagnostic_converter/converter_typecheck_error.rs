use crate::{parser::parse_error::ParseErr, typechecker::typechecker_error::TypeCheckError};
use codespan_reporting::diagnostic::{Diagnostic, Label};

pub fn build_typecheck_error_diagnostic(
    error: TypeCheckError,
    file_id: usize,
) -> Diagnostic<usize> {
    match error {
        TypeCheckError::PlaceHolder => todo!(),
        TypeCheckError::Assign => todo!(),
        TypeCheckError::Boolean(_) => todo!(),
        TypeCheckError::Integer(_) => todo!(),
        TypeCheckError::IdentifierNoType => todo!(),
        TypeCheckError::IdentifierTypeNotMatching(_, _) => todo!(),
        TypeCheckError::InfixOperandsNotMatching => todo!(),
        TypeCheckError::IfContainsError => todo!(),
        TypeCheckError::WhiteContainsError => todo!(),
        TypeCheckError::IdentifierNotFound(ident) => Diagnostic::error()
            .with_message("Identifier not found")
            .with_code("T019"),
    }
}
