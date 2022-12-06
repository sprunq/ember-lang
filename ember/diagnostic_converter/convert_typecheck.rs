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
    }
}
