use super::{expression::Node, ty::Type};
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Declaration {
        ty: Type,
        ident: Node,
        value: Node,
    },
    Expression {
        expr: Node,
    },
    While {
        condition: Box<Node>,
        body: Box<Stmt>,
    },
    If {
        condition: Box<Node>,
        body: Box<Stmt>,
        alternative: Option<Box<Stmt>>,
    },
    Sequence {
        statements: Box<Vec<Stmt>>,
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
            Stmt::Sequence { statements } => {
                for stmt in statements.iter() {
                    write!(f, "{}", stmt)?;
                }
                Ok(())
            }
        }
    }
}
