use super::{ast_node::AstNode, ty::Type, typed_expression::TypedExpr};
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Declaration {
        ty: Type,
        ident: AstNode<TypedExpr>,
        value: AstNode<TypedExpr>,
    },
    Expression {
        expr: AstNode<TypedExpr>,
    },
    While {
        condition: Box<AstNode<TypedExpr>>,
        body: Box<Stmt>,
    },
    If {
        condition: Box<AstNode<TypedExpr>>,
        body: Box<Stmt>,
        alternative: Option<Box<Stmt>>,
    },
    Sequence {
        statements: Box<Vec<Stmt>>,
    },
    FunctionDefinition {
        name: AstNode<TypedExpr>,
        parameters: Vec<AstNode<TypedExpr>>,
        return_type: Type,
        body: Box<Stmt>,
    },
    Return {
        value: Option<AstNode<TypedExpr>>,
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
                write!(f, "while({condition}){{{body}}};")
            }
            Stmt::If {
                condition,
                body,
                alternative,
            } => {
                write!(f, "if{condition} {{ {body} }}")?;
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
            Stmt::FunctionDefinition {
                name,
                parameters,
                return_type,
                body,
            } => {
                let decl = parameters
                    .iter()
                    .map(|f| format!("{}", f.inner))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "fn {name}({decl}) -> {return_type} {{\n{body}}};")
            }
            Stmt::Return { value } => {
                if let Some(val) = value {
                    write!(f, "return {val};")
                } else {
                    write!(f, "return;")
                }
            }
        }
    }
}
