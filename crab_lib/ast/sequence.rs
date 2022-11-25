use std::fmt;

use super::statement::Stmt;

#[derive(Debug, PartialEq, Clone)]
pub struct Sequence {
    pub statements: Vec<Stmt>,
}

impl fmt::Display for Sequence {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}
