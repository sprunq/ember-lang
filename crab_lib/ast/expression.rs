use super::{infix::Infix, prefix::Prefix, sequence::Sequence, ty::Type};
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Infix(Infix, Box<TypedExpr>, Box<TypedExpr>),
    Prefix(Prefix, Box<TypedExpr>),
    Identifier(String),
    IntegerLiteral(i64),
    If(Box<TypedExpr>, Sequence, Option<Sequence>),
    Empty,
    WhileLoop(Box<TypedExpr>, Sequence),
    Assign(String, Infix, Box<TypedExpr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Infix(op, l, r) => write!(f, "({} {} {})", l, op, r),
            Expr::Identifier(ident) => write!(f, "{}", ident),
            Expr::IntegerLiteral(int) => write!(f, "{}", int),
            Expr::Empty => write!(f, ""),
            Expr::Prefix(prefix, expr) => write!(f, "({}{})", prefix, expr),
            Expr::If(condition, consequence, alternative) => {
                write!(f, "if {} {}", condition, consequence)?;
                if let Some(alt) = alternative {
                    write!(f, " else {}", alt)?;
                }
                Ok(())
            }
            Expr::WhileLoop(condition, consequence) => {
                write!(f, "while ({}) {{{}}}", condition, consequence)
            }
            Expr::Assign(ident, op, expr) => write!(f, "{} {} {}", ident, op, expr),
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
