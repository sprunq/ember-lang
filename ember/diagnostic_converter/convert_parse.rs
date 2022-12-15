use crate::parser::parse_error::ParseErr;
use codespan_reporting::diagnostic::{Diagnostic, Label};

pub fn build_parse_error_diagnostic(error: ParseErr, file_id: usize) -> Diagnostic<usize> {
    match error {
        ParseErr::ExpectedPrefixToken { actual } => Diagnostic::error()
            .with_message("Expected a valid Prefix")
            .with_code("P001")
            .with_labels(vec![Label::primary(file_id, actual.span).with_message(
                format!("expected prefix of - but got {}", actual.token),
            )]),

        ParseErr::ExpectedInfixToken { actual } => Diagnostic::error()
            .with_message("Expected a valid Infix")
            .with_code("P002")
            .with_labels(vec![Label::primary(file_id, actual.span).with_message(
                format!(
                    "expected infix of [+, -, *, /, <, >, ==, !=] but got {}",
                    actual.token
                ),
            )]),

        ParseErr::UnsupportedInfixToken(info) => Diagnostic::error()
            .with_message("Encountered an unsupported Infix token")
            .with_code("P018")
            .with_labels(vec![Label::primary(file_id, info.span).with_message(
                format!(
                    "infix operand not of [+, -, *, /, <, >, ==, !=] but {}",
                    info.token
                ),
            )]),

        ParseErr::TokenNotFound(info) => Diagnostic::error()
            .with_message("Invalid Token")
            .with_code("P019")
            .with_labels(vec![Label::primary(file_id, info.span)
                .with_message(format!("invalid token {}", info.token))]),

        ParseErr::ParseIntError(info, int_error) => Diagnostic::error()
            .with_message("Failed to parse Int")
            .with_code("P020")
            .with_labels(vec![
                Label::primary(file_id, info.span).with_message(format!("{}", int_error))
            ]),

        ParseErr::NotATopLevelStatement(info) => Diagnostic::error()
            .with_message("Expected a top level statement")
            .with_code("P024")
            .with_labels(vec![Label::primary(file_id, info.span)
                .with_message(format!("expected a function but got {}", info.token))]),

        ParseErr::TokenMismatch { expected, actual } => Diagnostic::error()
            .with_message("Wrong token")
            .with_code("P025")
            .with_labels(vec![Label::primary(file_id, actual.span).with_message(
                format!("expected a {} but got {}", expected, actual.token),
            )]),
    }
}
