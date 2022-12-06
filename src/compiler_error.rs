use codespan_reporting::diagnostic::Diagnostic;
use ember::{
    diagnostic_converter::{
        convert_parse::build_parse_error_diagnostic,
        convert_typecheck::build_typecheck_error_diagnostic,
    },
    parser::parse_error::ParseErr,
    typechecker::typechecker_error::TypeCheckErr,
};

pub enum CompilerError {
    Parser(ParseErr),
    TypeCheck(TypeCheckErr),
}

impl CompilerError {
    pub fn convert_to_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
        match self {
            CompilerError::Parser(errs) => build_parse_error_diagnostic(errs.to_owned(), file_id),
            CompilerError::TypeCheck(errs) => {
                build_typecheck_error_diagnostic(errs.to_owned(), file_id)
            }
        }
    }
}
