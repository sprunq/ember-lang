use super::statement::Stmt;
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub sequence: Stmt,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.sequence)?;
        Ok(())
    }
}
