use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::parser::parse_error::ParseErr;

pub fn build_parse_error_diagnostic(error: ParseErr, file_id: usize) -> Diagnostic<usize> {
    match error {
        ParseErr::ExpectedPrefixToken(info) => Diagnostic::error()
            .with_message(format!("Expected a Prefix"))
            .with_code("P001")
            .with_labels(vec![Label::primary(file_id, info.start_idx..info.end_idx)]),
        ParseErr::ExpectedInfixToken(info) => Diagnostic::error()
            .with_message(format!("Expected an Infix"))
            .with_code("P002")
            .with_labels(vec![Label::primary(file_id, info.start_idx..info.end_idx)]),
        ParseErr::ExpectedIdentifierToken(info) => Diagnostic::error()
            .with_message(format!("Expected an Identifier"))
            .with_code("P003")
            .with_labels(vec![Label::primary(file_id, info.start_idx..info.end_idx)]),
        ParseErr::ExpectedIntegerToken(info) => Diagnostic::error()
            .with_message(format!("Expected an Integer"))
            .with_code("P005")
            .with_labels(vec![Label::primary(file_id, info.start_idx..info.end_idx)]),
        ParseErr::ExpectedLparen(info) => Diagnostic::error()
            .with_message(format!("Expected an opening Parenthesis"))
            .with_code("P008")
            .with_labels(vec![Label::primary(file_id, info.start_idx..info.end_idx)]),
        ParseErr::ExpectedRparen(info) => Diagnostic::error()
            .with_message(format!("Expected a closing Parenthesis"))
            .with_code("P009")
            .with_labels(vec![Label::primary(file_id, info.start_idx..info.end_idx)]),
        ParseErr::ExpectedLbrace(info) => Diagnostic::error()
            .with_message(format!("Expected an opening Brace"))
            .with_code("P010")
            .with_labels(vec![Label::primary(file_id, info.start_idx..info.end_idx)]),
        ParseErr::ExpectedRbrace(info) => Diagnostic::error()
            .with_message(format!("Expected a closing Brace"))
            .with_code("P011")
            .with_labels(vec![Label::primary(file_id, info.start_idx..info.end_idx)]),
        ParseErr::ExpectedRbracket(info) => Diagnostic::error()
            .with_message(format!("Expected a closing Bracket"))
            .with_code("P012")
            .with_labels(vec![Label::primary(file_id, info.start_idx..info.end_idx)]),
        ParseErr::ExpectedAssign(info) => Diagnostic::error()
            .with_message(format!("Expected an Assignment"))
            .with_code("P013")
            .with_labels(vec![Label::primary(file_id, info.start_idx..info.end_idx)]),
        ParseErr::ExpectedSemicolon(info) => Diagnostic::error()
            .with_message(format!("Expected a Semicolon"))
            .with_code("P014")
            .with_labels(vec![Label::primary(file_id, info.start_idx..info.end_idx)]),
        ParseErr::ExpectedLiteral(info) => Diagnostic::error()
            .with_message(format!("Expected a Literal"))
            .with_code("P017")
            .with_labels(vec![Label::primary(file_id, info.start_idx..info.end_idx)]),
        ParseErr::UnsupportedInfixToken(info) => Diagnostic::error()
            .with_message(format!("Encountered an unsupported Infix token"))
            .with_code("P018")
            .with_labels(vec![Label::primary(file_id, info.start_idx..info.end_idx)]),
        ParseErr::TokenNotFound(info) => Diagnostic::error()
            .with_message(format!("Invalid Token"))
            .with_code("P019")
            .with_labels(vec![Label::primary(file_id, info.start_idx..info.end_idx)]),
    }
}
