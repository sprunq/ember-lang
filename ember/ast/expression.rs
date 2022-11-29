use super::{ast_node::AstNode, infix::InfixOp, prefix::PrefixOp, typed_expression::TypedExpr};
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Infix {
        op: AstNode<InfixOp>,
        left: Box<AstNode<TypedExpr>>,
        right: Box<AstNode<TypedExpr>>,
    },
    Prefix {
        op: AstNode<PrefixOp>,
        expr: Box<AstNode<TypedExpr>>,
    },
    Identifier(AstNode<String>),
    IntegerLiteral(AstNode<i64>),
    BooleanLiteral(AstNode<bool>),
    Assign {
        ident: Box<AstNode<TypedExpr>>,
        operand: AstNode<InfixOp>,
        expr: Box<AstNode<TypedExpr>>,
    },
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Infix { op, left, right } => write!(f, "({left} {op} {right})"),
            Expr::Identifier(ident) => write!(f, "{ident}"),
            Expr::IntegerLiteral(int) => write!(f, "{int}"),
            Expr::BooleanLiteral(bool) => write!(f, "{bool}"),
            Expr::Prefix { op, expr } => write!(f, "({op}{expr})"),
            Expr::Assign {
                ident,
                operand: op,
                expr,
            } => write!(f, "{ident} {op} {expr}"),
        }
    }
}
