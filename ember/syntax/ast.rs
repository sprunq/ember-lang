use std::{fmt, ops::Range};

use super::{
    operands::{InfixOp, PrefixOp},
    ty::Type,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Declaration {
        ty: Option<Type>,
        ident: Spanned<String>,
        value: Spanned<TypedExpr>,
    },
    Expression {
        expr: Spanned<TypedExpr>,
    },
    While {
        condition: Box<Spanned<TypedExpr>>,
        body: Box<Stmt>,
    },
    If {
        condition: Box<Spanned<TypedExpr>>,
        body: Box<Stmt>,
        alternative: Option<Box<Stmt>>,
    },
    Sequence {
        statements: Box<Vec<Stmt>>,
    },
    FunctionDefinition {
        name: Spanned<String>,
        parameters: Vec<Spanned<TypedExpr>>,
        return_type: Type,
        body: Box<Stmt>,
    },
    Return {
        value: Option<Spanned<TypedExpr>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Infix {
        op: Spanned<InfixOp>,
        left: Box<Spanned<TypedExpr>>,
        right: Box<Spanned<TypedExpr>>,
    },
    Prefix {
        op: Spanned<PrefixOp>,
        expr: Box<Spanned<TypedExpr>>,
    },
    Identifier(Spanned<String>),
    IntegerLiteral(Spanned<i64>),
    BooleanLiteral(Spanned<bool>),
    Assign {
        ident: Spanned<String>,
        operand: Spanned<InfixOp>,
        expr: Box<Spanned<TypedExpr>>,
    },
    FunctionParameter {
        name: Spanned<String>,
        ty: Spanned<Type>,
    },
    FunctionInvocation {
        name: Spanned<String>,
        args: Vec<Spanned<TypedExpr>>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct AstRoot {
    pub sequence: Stmt,
}

#[derive(Debug, Clone, PartialEq)]
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

impl<T: std::fmt::Display> fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}

impl fmt::Display for AstRoot {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.sequence)?;
        Ok(())
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
