use crate::lexer::token::TokenInfo;
use std::fmt;

#[derive(Debug, Clone)]
pub enum ParseErr {
    ExpectedPrefixToken(TokenInfo),
    ExpectedInfixToken(TokenInfo),
    ExpectedIdentifierToken(TokenInfo),
    ExpectedBoolToken(TokenInfo),
    ExpectedIntegerToken(TokenInfo),
    ExpectedFloatToken(TokenInfo),
    ExpectedStringToken(TokenInfo),
    ExpectedLparen(TokenInfo),
    ExpectedRparen(TokenInfo),
    ExpectedLbrace(TokenInfo),
    ExpectedRbrace(TokenInfo),
    ExpectedRbracket(TokenInfo),
    ExpectedAssign(TokenInfo),
    ExpectedSemicolon(TokenInfo),
    ExpectedComma(TokenInfo),
    ExpectedColon(TokenInfo),
    ExpectedLiteral(TokenInfo),
    UnsupportedInfixToken(TokenInfo),
    TokenNotFound(TokenInfo),
}

impl fmt::Display for ParseErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            ParseErr::ExpectedPrefixToken(info) => {
                write!(f, "{}", format_token("Prefix", info))
            }
            ParseErr::ExpectedInfixToken(info) => {
                write!(f, "{}", format_token("Infix", info))
            }
            ParseErr::ExpectedIdentifierToken(info) => {
                write!(f, "{}", format_token("Indetifier", info))
            }
            ParseErr::ExpectedBoolToken(info) => {
                write!(f, "{}", format_token("Boolean", info))
            }
            ParseErr::ExpectedIntegerToken(info) => {
                write!(f, "{}", format_token("Integer", info))
            }
            ParseErr::ExpectedFloatToken(info) => {
                write!(f, "{}", format_token("Float", info))
            }
            ParseErr::ExpectedStringToken(info) => {
                write!(f, "{}", format_token("String", info))
            }
            ParseErr::ExpectedLparen(info) => write!(f, "{}", format_token("LParen", info)),
            ParseErr::ExpectedRparen(info) => write!(f, "{}", format_token("RParen", info)),
            ParseErr::ExpectedLbrace(info) => write!(f, "{}", format_token("LBrace", info)),
            ParseErr::ExpectedRbrace(info) => write!(f, "{}", format_token("RBrace", info)),
            ParseErr::ExpectedRbracket(info) => {
                write!(f, "{}", format_token("RBracket", info))
            }
            ParseErr::ExpectedAssign(info) => write!(f, "{}", format_token("Assign", info)),
            ParseErr::ExpectedSemicolon(info) => {
                write!(f, "{}", format_token("Semicolon", info))
            }
            ParseErr::ExpectedComma(info) => write!(f, "{}", format_token("Comma", info)),
            ParseErr::ExpectedColon(info) => write!(f, "{}", format_token("Colon", info)),
            ParseErr::UnsupportedInfixToken(info) => {
                write!(f, "{}", format_token("=,+=,*=,/=", info))
            }
            ParseErr::TokenNotFound(info) => {
                write!(
                    f,
                    "Token {} not found. [{}-{}]",
                    info.token, info.start_idx, info.end_idx
                )
            }
            ParseErr::ExpectedLiteral(info) => write!(f, "{}", format_token("Literal", info)),
        }
    }
}

fn format_token(expected_token: &str, error_token: &TokenInfo) -> String {
    format!(
        "Expected '{}' but got '{:?}' at position {} to {}",
        expected_token, error_token.token, error_token.start_idx, error_token.end_idx
    )
}
