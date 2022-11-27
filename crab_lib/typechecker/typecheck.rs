#![allow(unused_variables)]

use crate::ast::{
    expression::Expr, program::Program, statement::Stmt, ty::Type, typed_expression::TypedExpr,
};
use std::collections::HashMap;

use super::typechecker_error::TypeCheckError;

pub struct TypeChecker {}

impl TypeChecker {
    pub fn typecheck(program: Program) -> Result<Type, TypeCheckError> {
        let mut env = HashMap::new();
        Self::check_statement(&mut env, program.sequence)
    }

    fn check_statement(
        env: &mut HashMap<String, Type>,
        stmt: Stmt,
    ) -> Result<Type, TypeCheckError> {
        match stmt {
            Stmt::Declaration { ty, ident, value } => {
                env.insert(ident, ty.clone());
                let val_type = Self::check_expression(env, value, ty.clone())?;
                if ty == val_type {
                    Ok(Type::Void)
                } else {
                    Err(TypeCheckError::TypesNotMatching(ty, val_type))
                }
            }

            Stmt::Expression { expr } => Self::check_expression(env, expr, Type::Void),
            Stmt::While { condition, body } => {
                let cond = Self::check_expression(env, *condition, Type::Bool)?;
                let bod = Self::check_statement(env, *body)?;
                Ok(Type::Void)
            }
            Stmt::If {
                condition,
                body,
                alternative,
            } => {
                let cond = Self::check_expression(env, *condition, Type::Bool)?;
                let bod = Self::check_statement(env, *body)?;
                let alt = Self::check_statement(
                    env,
                    *alternative.unwrap_or_else(|| {
                        Box::new(Stmt::Sequence {
                            statements: Box::new(Vec::new()),
                        })
                    }),
                )?;
                Ok(Type::Void)
            }
            Stmt::Sequence { statements } => {
                let mut extended_env = env.clone();
                for stmt in statements.into_iter() {
                    Self::check_statement(&mut extended_env, stmt)?;
                }
                Ok(Type::Void)
            }
        }
    }

    fn check_expression(
        env: &HashMap<String, Type>,
        expression: TypedExpr,
        expected_type: Type,
    ) -> Result<Type, TypeCheckError> {
        match expression.expr {
            Expr::Infix { op, left, right } => {
                let l = Self::check_expression(env, *left, Type::Void)?;
                let r = Self::check_expression(env, *right, Type::Void)?;
                let actual_t = l.type_interaction(&op, &r)?;
                if actual_t != expected_type {
                    return Err(TypeCheckError::TypesNotMatching(actual_t, expected_type));
                }
                Ok(actual_t)
            }
            Expr::Prefix { op, expr } => Self::check_expression(env, *expr, expected_type),
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
                Some(ty) => Self::check_expression(env, *expr, ty.clone()),
                None => Err(TypeCheckError::IdentifierNotFound(ident)),
            },
        }
    }
}
