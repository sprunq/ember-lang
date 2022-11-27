use super::{infix::InfixOp, prefix::PrefixOp, typed_expression::TypedExpr};
use std::{fmt, ops::Range};

#[derive(Debug, PartialEq, Clone)]
pub struct Node {
    pub pos: Range<usize>,
    pub inner: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Infix {
        op: InfixOp,
        left: Box<TypedExpr>,
        right: Box<TypedExpr>,
    },
    Prefix {
        op: PrefixOp,
        expr: Box<TypedExpr>,
    },
    Identifier(String),
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    Assign {
        ident: Box<TypedExpr>,
        // =, +=, *=, ...
        operand: InfixOp,
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
