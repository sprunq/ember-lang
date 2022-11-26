use std::fmt;

use super::{expression::Expr, ty::Type};

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
