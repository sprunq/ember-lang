#![allow(unused_variables)]

use crate::ast::{
    expression::Expr, program::Program, sequence::Sequence, statement::Stmt, ty::Type,
    typed_expression::TypedExpr,
};
use std::collections::HashMap;

use super::typechecker_error::TypeCheckError;

pub struct TypeChecker {}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {}
    }

    pub fn typecheck(&mut self, program: Program) -> Option<TypeCheckError> {
        let mut env = HashMap::new();
        self.check_statements(&mut env, program.sequence)
    }

    fn check_statements(
        &mut self,
        env: &mut HashMap<String, Type>,
        sequence: Vec<Stmt>,
    ) -> Option<TypeCheckError> {
        for stmt in sequence {
            let x = self.check_statement(env, stmt);
            if x.is_some() {
                return x;
            }
        }
        None
    }

    fn check_statement(
        &mut self,
        env: &mut HashMap<String, Type>,
        stmt: Stmt,
    ) -> Option<TypeCheckError> {
        match stmt {
            Stmt::Declaration { ty, ident, value } => {
                env.insert(ident, ty.clone());
                match self.check_expression(env, value, Some(ty.clone())) {
                    Err(e) => return Some(e),
                    Ok(t) => {
                        if ty == t {
                            return None;
                        } else {
                            return Some(TypeCheckError::TypesNotMatching(ty, t));
                        }
                    }
                }
            }

            Stmt::Expression { expr } => self.check_expression(env, expr, None).err(),
            Stmt::While { condition, body } => {
                let cond = self.check_expression(env, *condition, Some(Type::Bool));
                let b = self.check_statements(env, body.statements);

                if cond.is_err() {
                    return cond.err();
                } else if b.is_some() {
                    return b;
                } else {
                    None
                }
            }
            Stmt::If {
                condition,
                body,
                alternative,
            } => {
                let cond = self.check_expression(env, *condition, Some(Type::Bool));
                let bod = self.check_statements(env, body.statements);
                let alt =
                    self.check_statements(env, alternative.unwrap_or(Sequence::new()).statements);

                if cond.is_err() {
                    return cond.err();
                } else if bod.is_some() {
                    return bod;
                } else if alt.is_some() {
                    return alt;
                } else {
                    None
                }
            }
        }
    }

    fn check_expression(
        &self,
        env: &HashMap<String, Type>,
        expression: TypedExpr,
        expected_type: Option<Type>,
    ) -> Result<Type, TypeCheckError> {
        match expression.expr {
            Expr::Infix { op, left, right } => {
                let l = self.check_expression(env, *left, None)?;
                let r = self.check_expression(env, *right, None)?;
                let actual_t = l.type_interaction(&op, &r)?;
                if let Some(expected_t) = expected_type {
                    if actual_t != expected_t {
                        return Err(TypeCheckError::TypesNotMatching(actual_t, expected_t));
                    }
                }
                Ok(actual_t)
            }
            Expr::Prefix { op, expr } => self.check_expression(env, *expr, expected_type),
            Expr::Identifier(ident) => {
                let type_opt = env.get(&ident);
                match type_opt {
                    Some(ty) => {
                        if Some(ty) == expected_type.as_ref() || expected_type == None {
                            Ok(ty.clone())
                        } else {
                            Err(TypeCheckError::IdentifierTypeNotMatching(
                                expected_type,
                                ty.clone(),
                            ))
                        }
                    }
                    None => Err(TypeCheckError::IdentifierNotFound(ident)),
                }
            }
            Expr::IntegerLiteral(_) => {
                if expected_type == Some(Type::I64) || expected_type == None {
                    Ok(Type::I64)
                } else {
                    Err(TypeCheckError::Integer(expected_type))
                }
            }
            Expr::BooleanLiteral(_) => {
                if expected_type == Some(Type::Bool) || expected_type == None {
                    Ok(Type::Bool)
                } else {
                    Err(TypeCheckError::Boolean(expected_type))
                }
            }
            Expr::Assign {
                ident,
                operand,
                expr,
            } => match env.get(&ident) {
                Some(ty) => self.check_expression(env, *expr, Some(ty.clone())),
                None => Err(TypeCheckError::IdentifierNotFound(ident)),
            },
        }
    }
}
