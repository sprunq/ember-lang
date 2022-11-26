use std::fmt;

use crate::lexer::token::Token;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    I64,
    Bool,
}

impl Type {
    pub fn from_token(token: &Token) -> Option<Self> {
        match token {
            Token::I64 => Some(Type::I64),
            Token::Bool => Some(Type::Bool),
            _ => None,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Type::I64 => write!(f, "i64"),
            Type::Bool => write!(f, "bool"),
        }
    }
}
