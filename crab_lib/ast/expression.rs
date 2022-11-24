use std::fmt;

use super::{infix::Infix, prefix::Prefix, statement::SequenceStatement};

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Infix(Infix, Box<Expression>, Box<Expression>),
    Prefix(Prefix, Box<Expression>),
    Identifier(String),
    IntegerLiteral(i64),
    If(
        Box<Expression>,
        SequenceStatement,
        Option<SequenceStatement>,
    ),
    Empty,
    WhileLoop(Box<Expression>, SequenceStatement),
    Assign(String, Infix, Box<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Infix(op, l, r) => write!(f, "({} {} {})", l, op, r),
            Expression::Identifier(ident) => write!(f, "{}", ident),
            Expression::IntegerLiteral(int) => write!(f, "{}", int),
            Expression::Empty => write!(f, ""),
            Expression::Prefix(prefix, expr) => write!(f, "({}{})", prefix, expr),
            Expression::If(condition, consequence, alternative) => {
                write!(f, "if {} {}", condition, consequence)?;
                if let Some(alt) = alternative {
                    write!(f, " else {}", alt)?;
                }
                Ok(())
            }
            Expression::WhileLoop(condition, consequence) => {
                write!(f, "while ({}) {{{}}}", condition, consequence)
            }
            Expression::Assign(ident, op, expr) => write!(f, "{} {} {}", ident, op, expr),
        }
    }
}
