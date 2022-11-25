use super::{expression::TypedExpr, ty::Type};
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub sequence: Vec<Stmt>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.sequence {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Declaration {
        ty: Type,
        ident: String,
        value: TypedExpr,
    },
    Expression {
        expr: TypedExpr,
    },
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmt::Declaration { ty, ident, value } => {
                write!(f, "{} {} = {}", ty, ident, value)
            }
            Stmt::Expression { expr } => write!(f, "{};", expr),
        }
    }
}
