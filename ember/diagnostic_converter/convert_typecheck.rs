use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::typechecker::typechecker_error::TypeCheckErr;

pub fn build_typecheck_error_diagnostic(error: TypeCheckErr, file_id: usize) -> Diagnostic<usize> {
    todo!()
}
