use super::{sequence::Sequence, ty::Type, typed_expression::TypedExpr};
use std::fmt;

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
    While {
        condition: Box<TypedExpr>,
        body: Sequence,
    },
    If {
        condition: Box<TypedExpr>,
        body: Sequence,
        alternative: Option<Sequence>,
    },
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmt::Declaration { ty, ident, value } => {
                write!(f, "{ty} {ident} = {value};")
            }
            Stmt::Expression { expr } => write!(f, "{expr};"),
            Stmt::While { condition, body } => {
                write!(f, "while ({condition}){{{body}}};")
            }
            Stmt::If {
                condition,
                body,
                alternative,
            } => {
                write!(f, "if {condition} {{ {body} }}")?;
                if let Some(alt) = alternative {
                    write!(f, " else {{ {alt} }}")?;
                }
                write!(f, ";")?;
                Ok(())
            }
        }
    }
}
