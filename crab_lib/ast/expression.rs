use super::{infix::Infix, prefix::Prefix, ty::Type};
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Infix {
        op: Infix,
        left: Box<TypedExpr>,
        right: Box<TypedExpr>,
    },
    Prefix {
        op: Prefix,
        expr: Box<TypedExpr>,
    },
    Identifier(String),
    IntegerLiteral(i64),
    Assign {
        ident: String,
        // =, +=, *=, ...
        operand: Infix,
        expr: Box<TypedExpr>,
    },
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Infix { op, left, right } => write!(f, "({left} {op} {right})"),
            Expr::Identifier(ident) => write!(f, "{ident}"),
            Expr::IntegerLiteral(int) => write!(f, "{int}"),
            Expr::Prefix { op, expr } => write!(f, "({op}{expr})"),
            Expr::Assign {
                ident,
                operand: op,
                expr,
            } => write!(f, "{ident} {op} {expr}"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypedExpr {
    pub ty: Option<Type>,
    pub expr: Expr,
}

impl TypedExpr {
    pub fn new(expr: Expr) -> Self {
        TypedExpr { ty: None, expr }
    }
}

impl fmt::Display for TypedExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.expr)
    }
}
