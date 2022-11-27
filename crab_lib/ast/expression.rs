use super::{infix::InfixOp, prefix::PrefixOp, typed_expression::TypedExpr};
use std::{fmt, ops::Range};

#[derive(Debug, PartialEq, Clone)]
pub struct Node {
    pub pos: Range<usize>,
    pub inner: TypedExpr,
}
impl Node {
    pub fn new(expr: TypedExpr, pos: Range<usize>) -> Self {
        Self { pos, inner: expr }
    }

    pub fn new_boxed(expr: TypedExpr, pos: Range<usize>) -> Box<Self> {
        Box::new(Node::new(expr, pos))
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Infix {
        op: InfixOp,
        left: Box<Node>,
        right: Box<Node>,
    },
    Prefix {
        op: PrefixOp,
        expr: Box<Node>,
    },
    Identifier(String, Range<usize>),
    IntegerLiteral(i64, Range<usize>),
    BooleanLiteral(bool, Range<usize>),
    Assign {
        ident: Box<Node>,
        // =, +=, *=, ...
        operand: InfixOp,
        expr: Box<Node>,
    },
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Infix { op, left, right } => write!(f, "({left} {op} {right})"),
            Expr::Identifier(ident, _) => write!(f, "{ident}"),
            Expr::IntegerLiteral(int, _) => write!(f, "{int}"),
            Expr::BooleanLiteral(bool, _) => write!(f, "{}", bool),
            Expr::Prefix { op, expr } => write!(f, "({op}{expr})"),
            Expr::Assign {
                ident,
                operand: op,
                expr,
            } => write!(f, "{ident} {op} {expr}"),
        }
    }
}
