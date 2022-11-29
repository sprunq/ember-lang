use std::num::ParseIntError;

use crate::ast::token::TokenInfo;

#[derive(Debug, Clone)]
pub enum ParseErr {
    ExpectedPrefixToken(TokenInfo),
    ExpectedInfixToken(TokenInfo),
    ExpectedIdentifierToken(TokenInfo),
    ExpectedIntegerToken(TokenInfo),
    ExpectedLparen(TokenInfo),
    ExpectedRparen(TokenInfo),
    ExpectedLbrace(TokenInfo),
    ExpectedRbrace(TokenInfo),
    ExpectedRbracket(TokenInfo),
    ExpectedAssign(TokenInfo),
    ExpectedSemicolon(TokenInfo),
    ExpectedLiteral(TokenInfo),
    ParseIntError(TokenInfo, ParseIntError),
    UnsupportedInfixToken(TokenInfo),
    TokenNotFound(TokenInfo),
    ExpectedBoolToken(TokenInfo),
}
