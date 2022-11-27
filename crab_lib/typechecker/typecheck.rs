#![allow(unused_variables)]

use crate::ast::{
    ast_root::AstRoot,
    expression::{Expr, Node},
    statement::Stmt,
    ty::Type,
};
use std::collections::HashMap;

use super::typechecker_error::TypeCheckError;

pub struct TypeChecker {}

impl TypeChecker {
    pub fn typecheck(program: &mut AstRoot) -> Result<Type, TypeCheckError> {
        let mut env = HashMap::new();
        Self::check_statement(&mut env, &mut program.sequence)
    }

    fn check_statement(
        env: &mut HashMap<String, Type>,
        stmt: &mut Stmt,
    ) -> Result<Type, TypeCheckError> {
        match stmt {
            Stmt::Declaration { ty, ident, value } => {
                env.insert(ident.to_string(), ty.clone());
                let val_type = Self::check_expression(env, value, ty)?;
                if *ty == val_type {
                    value.inner.ty = Some(val_type);
                    Ok(Type::Void)
                } else {
                    Err(TypeCheckError::TypesNotMatching(ty.clone(), val_type))
                }
            }

            Stmt::Expression { expr } => Self::check_expression(env, expr, &Type::Void),
            Stmt::While { condition, body } => {
                let cond = Self::check_expression(env, &mut *condition, &Type::Bool)?;
                let bod = Self::check_statement(env, &mut *body)?;
                Ok(Type::Void)
            }
            Stmt::If {
                condition,
                body,
                alternative,
            } => {
                let cond = Self::check_expression(env, &mut *condition, &Type::Bool)?;
                let bod = Self::check_statement(env, &mut *body)?;
                let alt = Self::check_statement(
                    env,
                    &mut alternative.to_owned().unwrap_or({
                        Box::new(Stmt::Sequence {
                            statements: Box::new(Vec::new()),
                        })
                    }),
                )?;
                Ok(Type::Void)
            }
            Stmt::Sequence { statements } => {
                let mut extended_env = env.clone();
                for mut stmt in statements.to_owned().into_iter() {
                    Self::check_statement(&mut extended_env, &mut stmt)?;
                }
                Ok(Type::Void)
            }
        }
    }

    fn check_expression(
        env: &mut HashMap<String, Type>,
        expression: &mut Node,
        expected_type: &Type,
    ) -> Result<Type, TypeCheckError> {
        match expression.inner.expr.clone() {
            Expr::Infix {
                op,
                mut left,
                mut right,
            } => {
                let l = Self::check_expression(env, &mut left, &Type::Void)?;
                let r = Self::check_expression(env, &mut right, &Type::Void)?;
                let actual_t = l.type_interaction(&op, &r)?;
                if actual_t != expected_type.clone() {
                    return Err(TypeCheckError::TypesNotMatching(
                        actual_t,
                        expected_type.clone(),
                    ));
                }
                expression.inner.ty = Some(actual_t.clone());
                Ok(actual_t)
            }
            Expr::Prefix { op, mut expr } => Self::check_expression(env, &mut expr, expected_type),
            Expr::Identifier(ident, pos) => {
                let type_opt = env.get(ident.as_str());
                match type_opt {
                    Some(ty) => {
                        if *ty == *expected_type || *expected_type == Type::Void {
                            expression.inner.ty = Some(ty.clone());
                            Ok(ty.clone())
                        } else {
                            Err(TypeCheckError::IdentifierTypeNotMatching(
                                expected_type.clone(),
                                ty.clone(),
                            ))
                        }
                    }
                    None => Err(TypeCheckError::IdentifierNotFound(ident.to_string())),
                }
            }
            Expr::IntegerLiteral(_, pos) => {
                if *expected_type == Type::I64 || *expected_type == Type::Void {
                    expression.inner.ty = Some(Type::I64);
                    Ok(Type::I64)
                } else {
                    Err(TypeCheckError::Integer(expected_type.clone()))
                }
            }
            Expr::BooleanLiteral(_, pos) => {
                if *expected_type == Type::Bool || *expected_type == Type::Void {
                    expression.inner.ty = Some(Type::Bool);
                    Ok(Type::Bool)
                } else {
                    Err(TypeCheckError::Boolean(expected_type.clone()))
                }
            }
            Expr::Assign {
                ident,
                operand,
                mut expr,
            } => match env.get(&ident.to_string()) {
                Some(ty) => {
                    expression.inner.ty = Some(ty.clone());
                    Self::check_expression(env, &mut expr, &ty.clone())
                }
                None => Err(TypeCheckError::IdentifierNotFound(ident.to_string())),
            },
        }
    }
}
