use super::statement::Stmt;
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub sequence: Vec<Stmt>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.sequence {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}
