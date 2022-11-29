use super::statement::Stmt;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct AstRoot {
    pub sequence: Stmt,
}

impl fmt::Display for AstRoot {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.sequence)?;
        Ok(())
    }
}
