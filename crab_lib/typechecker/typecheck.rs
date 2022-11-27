#![allow(unused_variables)]

use crate::ast::{
    expression::Expr, program::Program, statement::Stmt, ty::Type, typed_expression::TypedExpr,
};
use std::collections::HashMap;

use super::typechecker_error::TypeCheckError;

pub struct TypeChecker {}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {}
    }

    pub fn typecheck(&mut self, program: Program) -> Result<Type, TypeCheckError> {
        let mut env = HashMap::new();
        self.check_statement(&mut env, program.sequence)
    }

    fn check_statement(
        &mut self,
        env: &mut HashMap<String, Type>,
        stmt: Stmt,
    ) -> Result<Type, TypeCheckError> {
        match stmt {
            Stmt::Declaration { ty, ident, value } => {
                env.insert(ident, ty.clone());
                let val_type = self.check_expression(env, value, ty.clone())?;
                if ty == val_type {
                    return Ok(Type::Void);
                } else {
                    return Err(TypeCheckError::TypesNotMatching(ty, val_type));
                }
            }

            Stmt::Expression { expr } => self.check_expression(env, expr, Type::Void),
            Stmt::While { condition, body } => {
                let cond = self.check_expression(env, *condition, Type::Bool)?;
                let bod = self.check_statement(env, *body)?;
                Ok(Type::Void)
            }
            Stmt::If {
                condition,
                body,
                alternative,
            } => {
                let cond = self.check_expression(env, *condition, Type::Bool)?;
                let bod = self.check_statement(env, *body)?;
                let alt = self.check_statement(
                    env,
                    *alternative.unwrap_or(Box::new(Stmt::Sequence {
                        statements: Box::new(Vec::new()),
                    })),
                )?;
                Ok(Type::Void)
            }
            Stmt::Sequence { statements } => {
                let mut extended_env = env.clone();
                for stmt in statements.into_iter() {
                    self.check_statement(&mut extended_env, stmt)?;
                }
                Ok(Type::Void)
            }
        }
    }

    fn check_expression(
        &self,
        env: &HashMap<String, Type>,
        expression: TypedExpr,
        expected_type: Type,
    ) -> Result<Type, TypeCheckError> {
        match expression.expr {
            Expr::Infix { op, left, right } => {
                let l = self.check_expression(env, *left, Type::Void)?;
                let r = self.check_expression(env, *right, Type::Void)?;
                let actual_t = l.type_interaction(&op, &r)?;
                if actual_t != expected_type {
                    return Err(TypeCheckError::TypesNotMatching(actual_t, expected_type));
                }
                Ok(actual_t)
            }
            Expr::Prefix { op, expr } => self.check_expression(env, *expr, expected_type),
            Expr::Identifier(ident) => {
                let type_opt = env.get(&ident);
                match type_opt {
                    Some(ty) => {
                        if *ty == expected_type || expected_type == Type::Void {
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
                if expected_type == Type::I64 || expected_type == Type::Void {
                    Ok(Type::I64)
                } else {
                    Err(TypeCheckError::Integer(expected_type))
                }
            }
            Expr::BooleanLiteral(_) => {
                if expected_type == Type::Bool || expected_type == Type::Void {
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
                Some(ty) => self.check_expression(env, *expr, ty.clone()),
                None => Err(TypeCheckError::IdentifierNotFound(ident)),
            },
        }
    }
}
