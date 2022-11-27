use crate::typechecker::typechecker_error::TypeCheckError;
use codespan_reporting::diagnostic::{Diagnostic, Label};

pub fn build_typecheck_error_diagnostic(
    error: TypeCheckError,
    file_id: usize,
) -> Diagnostic<usize> {
    match error {
        TypeCheckError::Boolean(val, pos) => Diagnostic::error()
            .with_message("Expected Boolean")
            .with_code("T001")
            .with_labels(vec![Label::primary(file_id, pos)
                .with_message(format!("expected a boolean type but got {val}"))]),

        TypeCheckError::Integer(val, pos) => Diagnostic::error()
            .with_message("Expected Integer")
            .with_code("T001")
            .with_labels(vec![Label::primary(file_id, pos)
                .with_message(format!("expected an integer type but got {val}"))]),

        TypeCheckError::IdentifierTypeNotMatching(ident, ident_type, expr_type, pos) => {
            Diagnostic::error()
                .with_message("Identifier types not matching")
                .with_code("T003")
                .with_labels(vec![Label::primary(file_id, pos).with_message(format!(
                    "{ident} of type {ident_type} is not compatible with type {expr_type}"
                ))])
        }

        TypeCheckError::IdentifierNotFound(ident, pos) => Diagnostic::error()
            .with_message("Identifier not found")
            .with_code("T004")
            .with_labels(vec![Label::primary(file_id, pos).with_message(format!(
                "Could not find identifier {ident} in this scope"
            ))]),

        TypeCheckError::IncompatibleTypesForOperand(op, l, r, pos) => Diagnostic::error()
            .with_message("Incompatible types for operand")
            .with_code("T005")
            .with_labels(vec![Label::primary(file_id, pos).with_message(format!(
                "Operand {op} does not supprt types {l} and {r}"
            ))]),

        TypeCheckError::InfixTypesNotMatching(l, r, pos) => Diagnostic::error()
            .with_message("Infix types not matching")
            .with_code("T006")
            .with_labels(vec![Label::primary(file_id, pos)
                .with_message(format!("{l} and {r} are not compatible"))]),

        TypeCheckError::DeclarationTypesNotMatching(l, r, ident, pos) => Diagnostic::error()
            .with_message("Declaration types not matching")
            .with_code("T007")
            .with_labels(vec![Label::primary(file_id, pos).with_message(format!(
                "Cannot assign {r} to variable {ident} of type {l}"
            ))]),
    }
}
