use super::{infix::Infix, prefix::Prefix, typed_expression::TypedExpr};
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
    BooleanLiteral(bool),
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
            Expr::BooleanLiteral(bool) => write!(f, "{}", bool),
            Expr::Prefix { op, expr } => write!(f, "({op}{expr})"),
            Expr::Assign {
                ident,
                operand: op,
                expr,
            } => write!(f, "{ident} {op} {expr}"),
        }
    }
}
