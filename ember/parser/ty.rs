use super::syntax::InfixOp;
use crate::lexer::token::Token;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
}

impl Type {
    pub fn from_token(token: &Token) -> Option<Self> {
        match token {
            Token::Int => Some(Type::Int),
            Token::Bool => Some(Type::Bool),
            _ => None,
        }
    }

    pub fn type_interaction(&self, operand: InfixOp, other_type: Type) -> Option<Type> {
        match (self, other_type) {
            (Type::Int, Type::Int) => match operand {
                InfixOp::Eq | InfixOp::NotEq | InfixOp::Lt | InfixOp::Gt => Some(Type::Bool),
                _ => Some(Type::Int),
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
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
        }
    }
}
