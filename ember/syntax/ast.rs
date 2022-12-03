use super::{
    operands::{InfixOp, PrefixOp},
    ty::Type,
};
use std::{fmt, ops::Range};

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Declaration {
        ty: Option<Type>,
        ident: Spanned<String>,
        value: Spanned<Expr>,
    },
    Expression {
        expr: Spanned<Expr>,
    },
    While {
        condition: Box<Spanned<Expr>>,
        body: Box<Stmt>,
    },
    If {
        condition: Box<Spanned<Expr>>,
        body: Box<Stmt>,
        alternative: Option<Box<Stmt>>,
    },
    Sequence {
        statements: Box<Vec<Stmt>>,
    },
    FunctionDefinition {
        name: Spanned<String>,
        parameters: Vec<(Spanned<String>, Spanned<Type>)>,
        return_type: Type,
        body: Box<Stmt>,
    },
    Return {
        value: Option<Spanned<Expr>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary {
        op: Spanned<InfixOp>,
        left: Box<Spanned<Expr>>,
        right: Box<Spanned<Expr>>,
    },
    Unary {
        op: Spanned<PrefixOp>,
        expr: Box<Spanned<Expr>>,
    },
    Identifier(Spanned<String>),
    IntegerLiteral(Spanned<i64>),
    BooleanLiteral(Spanned<bool>),
    Assign {
        ident: Spanned<String>,
        operand: Spanned<InfixOp>,
        expr: Box<Spanned<Expr>>,
    },
    FunctionParameter {
        name: Spanned<String>,
        ty: Spanned<Type>,
    },
    FunctionInvocation {
        name: Spanned<String>,
        args: Vec<Spanned<Expr>>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Spanned<T: std::fmt::Display> {
    pub pos: Range<usize>,
    pub inner: T,
}

impl<T: std::fmt::Display> Spanned<T> {
    pub fn new(expr: T, pos: Range<usize>) -> Self {
        Self { pos, inner: expr }
    }

    pub fn new_boxed(expr: T, pos: Range<usize>) -> Box<Self> {
        Box::new(Spanned::new(expr, pos))
    }
}

impl<T: std::fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmt::Declaration { ty, ident, value } => {
                if let Some(t) = ty {
                    write!(f, "let {ident} : {t} = {value};")
                } else {
                    write!(f, "let {ident} = {value};")
                }
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
                    .map(|f| format!("{} : {}", f.0, f.1))
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

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Binary { op, left, right } => write!(f, "({left} {op} {right})"),
            Expr::Identifier(ident) => write!(f, "{ident}"),
            Expr::IntegerLiteral(int) => write!(f, "{int}"),
            Expr::BooleanLiteral(bool) => write!(f, "{bool}"),
            Expr::Unary { op, expr } => write!(f, "({op}{expr})"),
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
