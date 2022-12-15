use crate::syntax::token::{Token, TokenInfo};
use std::num::ParseIntError;

#[derive(Debug, Clone)]
pub enum ParseErr {
    TokenMismatch { expected: Token, actual: TokenInfo },
    ParseIntError(TokenInfo, ParseIntError),
    UnsupportedInfixToken(TokenInfo),
    TokenNotFound(TokenInfo),
    NotATopLevelStatement(TokenInfo),
    ExpectedPrefixToken { actual: TokenInfo },
    ExpectedInfixToken { actual: TokenInfo },
}
