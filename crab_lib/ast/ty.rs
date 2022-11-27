use std::fmt;

use crate::{lexer::token::Token, typechecker::typechecker_error::TypeCheckError};

use super::infix::Infix;

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

    pub fn type_interaction(
        &self,
        operand: &Infix,
        other_type: &Type,
    ) -> Result<Type, TypeCheckError> {
        match (self, other_type) {
            (Type::I64, Type::I64) => match operand {
                Infix::Eq | Infix::NotEq | Infix::Lt | Infix::Gt => Ok(Type::Bool),
                _ => Ok(Type::I64),
            },
            (Type::Bool, Type::Bool) => match operand {
                Infix::Eq | Infix::NotEq => Ok(Type::Bool),
                _ => Err(TypeCheckError::IncompatibleTypesForOperand(
                    operand.to_owned(),
                    self.to_owned(),
                    other_type.to_owned(),
                )),
            },
            _ => Err(TypeCheckError::IncompatibleTypesForOperand(
                operand.to_owned(),
                self.to_owned(),
                other_type.to_owned(),
            )),
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
