use crate::typechecker::typechecker_error::TypeCheckError;
use codespan_reporting::diagnostic::Diagnostic;

pub fn build_typecheck_error_diagnostic(
    error: TypeCheckError,
    _file_id: usize,
) -> Diagnostic<usize> {
    match error {
        _ => todo!(),
    }
}
