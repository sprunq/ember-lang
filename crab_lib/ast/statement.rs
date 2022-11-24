use std::fmt;

use super::expression::Expression;

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(String, Expression),
    Expression(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let(ident, expr) => write!(f, "let {} = {};", ident, expr),
            Statement::Expression(expr) => write!(f, "{};", expr),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub sequence: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.sequence {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct SequenceStatement {
    pub statements: Vec<Statement>,
}

impl fmt::Display for SequenceStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{{ {} }}", stmt)?;
        }
        Ok(())
    }
}
