use super::{infix::InfixOp, token::Token};
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    I64,
    Bool,
    Void,
}

impl Type {
    pub fn from_token(token: &Token) -> Option<Self> {
        match token {
            Token::I64 => Some(Type::I64),
            Token::Bool => Some(Type::Bool),
            Token::Void => Some(Type::Void),
            _ => None,
        }
    }

    pub fn type_interaction(&self, operand: InfixOp, other_type: Type) -> Option<Type> {
        match (self, other_type) {
            (Type::I64, Type::I64) => match operand {
                InfixOp::Eq | InfixOp::NotEq | InfixOp::Lt | InfixOp::Gt => Some(Type::Bool),
                _ => Some(Type::I64),
            },
            (Type::Bool, Type::Bool) => match operand {
                InfixOp::Eq | InfixOp::NotEq | InfixOp::Assign => Some(Type::Bool),
                _ => None,
            },
            _ => None,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Type::I64 => write!(f, "i64"),
            Type::Bool => write!(f, "bool"),
            Type::Void => write!(f, "void"),
        }
    }
}
