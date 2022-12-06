use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::typechecker::typechecker_error::TypeCheckErr;

pub fn build_typecheck_error_diagnostic(error: TypeCheckErr, file_id: usize) -> Diagnostic<usize> {
    match error {
        TypeCheckErr::FunctionDuplicate {
            trying_to_init_ident,
            existing_fun_ident,
        } => Diagnostic::error()
            .with_message("Function Duplicate")
            .with_code("T001")
            .with_labels(vec![
                Label::primary(file_id, trying_to_init_ident.pos).with_message(format!(
                    "cannot declare function {}",
                    trying_to_init_ident.inner
                )),
                Label::secondary(file_id, existing_fun_ident.pos)
                    .with_message("already exists here"),
            ]),
        TypeCheckErr::NoMainFunctionFound => Diagnostic::error()
            .with_message("No main function defined")
            .with_code("T002"),
        TypeCheckErr::IncompatibleOperandTypes { op, left, right } => Diagnostic::error()
            .with_message("Incompatibe Operands")
            .with_code("T003")
            .with_labels(vec![Label::primary(file_id, op.pos).with_message(format!(
                "cannot apply {} to {left} and {right}",
                op.inner
            ))]),
        TypeCheckErr::AssignmentMismatch {
            ident,
            expected,
            actual,
        } => Diagnostic::error()
            .with_message("Wrong type for Indentifier")
            .with_code("T004")
            .with_labels(vec![Label::primary(file_id, ident.pos).with_message(
                format!(
                    "cannot assign type {actual} to {} of type {expected}",
                    ident.inner
                ),
            )]),

        TypeCheckErr::StatementShouldNotReturnAnything { expr, ty } => Diagnostic::error()
            .with_message("Statements do not have return values")
            .with_code("T005")
            .with_labels(vec![Label::primary(file_id, expr.pos)
                .with_message(format!("cannot return type {ty} from a statement"))]),
        TypeCheckErr::DeclarationTypesNotMatching {
            ident,
            declared_ty,
            value_ty,
        } => Diagnostic::error()
            .with_message("Declaration types not matching")
            .with_code("T006")
            .with_labels(vec![Label::primary(file_id, ident.pos.clone())
                .with_message(format!(
                    "cannot assign type {} to {ident} since it has been specified with type {}",
                    if value_ty.is_some() {
                        value_ty.unwrap().to_string()
                    } else {
                        "None".to_string()
                    },
                    if declared_ty.is_some() {
                        declared_ty.unwrap().to_string()
                    } else {
                        "None".to_string()
                    },
                ))]),
        TypeCheckErr::VariableDuplicate {
            ident,
            previous_type,
            value_ty,
        } => Diagnostic::error()
            .with_message("Variable Duplicate")
            .with_code("T007")
            .with_labels(vec![Label::primary(file_id, ident.pos.clone())
                .with_message(format!(
                    "cannot declare {} with {} since it has already exists for {}",
                    ident, value_ty, previous_type
                ))]),
        TypeCheckErr::TypeMismatch {
            expected,
            actual,
            positon,
        } => Diagnostic::error()
            .with_message("Type mismatch")
            .with_code("T008")
            .with_labels(vec![Label::primary(file_id, positon).with_message(
                match (expected, actual) {
                    (None, None) => format!("how did you get here"),
                    (None, Some(b)) => format!("expected None type but got {}", b),
                    (Some(a), None) => format!("expected type {} but got no None", a),
                    (Some(a), Some(b)) => format!("expected type {} but got {}", a, b),
                },
            )]),
    }
}
