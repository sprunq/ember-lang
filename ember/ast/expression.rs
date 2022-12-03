use super::{
    ast_node::AstNode, infix::InfixOp, prefix::PrefixOp, ty::Type, typed_expression::TypedExpr,
};
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
        ident: AstNode<String>,
        operand: AstNode<InfixOp>,
        expr: Box<AstNode<TypedExpr>>,
    },
    FunctionParameter {
        name: AstNode<String>,
        ty: AstNode<Type>,
    },
    FunctionInvocation {
        name: AstNode<String>,
        args: Vec<AstNode<TypedExpr>>,
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
            Expr::FunctionParameter { name, ty } => write!(f, "{ty} {name}"),
            Expr::FunctionInvocation { name, args } => {
                let arguments = args
                    .iter()
                    .map(|f| format!("{}", f.inner))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{name}({arguments})")
            }
        }
    }
}
