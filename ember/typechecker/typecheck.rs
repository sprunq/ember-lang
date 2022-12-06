use crate::syntax::ast::Stmt;

use super::typechecker_error::TypeCheckErr;

pub struct TypeChecker {}

impl TypeChecker {
    pub fn new() -> Self {
        Self {}
    }

    pub fn typecheck(&self, program: &Stmt) -> Result<(), TypeCheckErr> {
        Ok(())
    }
}
