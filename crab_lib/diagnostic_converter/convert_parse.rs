use crate::parser::parse_error::ParseErr;
use codespan_reporting::diagnostic::{Diagnostic, Label};

pub fn build_parse_error_diagnostic(error: ParseErr, file_id: usize) -> Diagnostic<usize> {
    match error {
        ParseErr::ExpectedPrefixToken(info) => Diagnostic::error()
            .with_message("Expected a valid Prefix")
            .with_code("P001")
            .with_labels(vec![Label::primary(file_id, info.span).with_message(
                format!("expected prefix of [-] but got {}", info.token),
            )]),

        ParseErr::ExpectedInfixToken(info) => Diagnostic::error()
            .with_message("Expected a valid Infix")
            .with_code("P002")
            .with_labels(vec![Label::primary(file_id, info.span).with_message(
                format!(
                    "expected infix of [+, -, *, /, <, >, ==, !=] but got {}",
                    info.token
                ),
            )]),

        ParseErr::ExpectedIdentifierToken(info) => Diagnostic::error()
            .with_message("Expected an Identifier")
            .with_code("P003")
            .with_labels(vec![Label::primary(file_id, info.span).with_message(
                format!("expected an identifier but got {}", info.token),
            )]),

        ParseErr::ExpectedIntegerToken(info) => Diagnostic::error()
            .with_message("Expected an Integer")
            .with_code("P005")
            .with_labels(vec![Label::primary(file_id, info.span)
                .with_message(format!("expected an integer but got {}", info.token))]),

        ParseErr::ExpectedLparen(info) => Diagnostic::error()
            .with_message("Expected an opening Parenthesis")
            .with_code("P008")
            .with_labels(vec![Label::primary(file_id, info.span)
                .with_message(format!("expected ( but got {}", info.token))]),

        ParseErr::ExpectedRparen(info) => Diagnostic::error()
            .with_message("Expected a closing Parenthesis")
            .with_code("P009")
            .with_labels(vec![Label::primary(file_id, info.span)
                .with_message(format!("expected ) but got {}", info.token))]),

        ParseErr::ExpectedLbrace(info) => Diagnostic::error()
            .with_message("Expected an opening Brace")
            .with_code("P010")
            .with_labels(vec![Label::primary(file_id, info.span)
                .with_message(format!("expected [ but got {}", info.token))]),

        ParseErr::ExpectedRbrace(info) => Diagnostic::error()
            .with_message("Expected a closing Brace")
            .with_code("P011")
            .with_labels(vec![Label::primary(file_id, info.span)
                .with_message(format!("expected ] but got {}", info.token))]),

        ParseErr::ExpectedRbracket(info) => Diagnostic::error()
            .with_message("Expected a closing Bracket")
            .with_code("P012")
            .with_labels(vec![Label::primary(file_id, info.span)
                .with_message(format!("expected }} but got {}", info.token))]),

        ParseErr::ExpectedAssign(info) => Diagnostic::error()
            .with_message("Expected an Assignment")
            .with_code("P013")
            .with_labels(vec![Label::primary(file_id, info.span)
                .with_message(format!("expected = but got {}", info.token))]),

        ParseErr::ExpectedSemicolon(info) => Diagnostic::error()
            .with_message("Expected a Semicolon")
            .with_code("P014")
            .with_labels(vec![Label::primary(file_id, info.span)
                .with_message(format!("expected ; but got {}", info.token))]),

        ParseErr::ExpectedLiteral(info) => Diagnostic::error()
            .with_message("Expected a Literal")
            .with_code("P017")
            .with_labels(vec![Label::primary(file_id, info.span)
                .with_message(format!("expected a literal but got {}", info.token))]),

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

        ParseErr::ExpectedBoolToken(info) => Diagnostic::error()
            .with_message("Expected a Boolean")
            .with_code("P021")
            .with_labels(vec![Label::primary(file_id, info.span)
                .with_message(format!("expected true/false but got {}", info.token))]),
    }
}
