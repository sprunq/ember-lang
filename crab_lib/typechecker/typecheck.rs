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
                    Err(TypeCheckError::DeclarationTypesNotMatching(
                        ty.clone(),
                        val_type,
                        ident.to_string(),
                        ident.pos.start..value.pos.end,
                    ))
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
                let l = Self::check_expression(env, &mut left, expected_type)?;
                let r = Self::check_expression(env, &mut right, expected_type)?;
                let type_interaction_res = l.type_interaction(&op, &r);
                if let None = type_interaction_res {
                    return Err(TypeCheckError::IncompatibleTypesForOperand(
                        op,
                        l,
                        r,
                        left.pos.start..right.pos.end,
                    ));
                }
                let actual_t = type_interaction_res.unwrap();
                if actual_t != expected_type.clone() {
                    return Err(TypeCheckError::InfixTypesNotMatching(
                        actual_t,
                        expected_type.clone(),
                        left.pos.start..right.pos.end,
                    ));
                }
                expression.inner.ty = Some(actual_t.clone());
                Ok(actual_t)
            }
            Expr::Prefix { op, mut expr } => Self::check_expression(env, &mut expr, expected_type),
            Expr::Identifier(ident, pos) => {
                let type_opt = env.get(ident.as_str());
                match type_opt {
                    Some(ty) => Ok(ty.clone()),
                    None => Err(TypeCheckError::IdentifierNotFound(ident.to_string(), pos)),
                }
            }
            Expr::IntegerLiteral(_, pos) => Ok(Type::I64),
            Expr::BooleanLiteral(_, pos) => Ok(Type::Bool),
            Expr::Assign {
                mut ident,
                operand,
                mut expr,
            } => {
                let ident_type = Self::check_expression(env, &mut ident, expected_type)?;
                let expr_type = Self::check_expression(env, &mut expr, &ident_type)?;
                let type_interaction_res = ident_type.type_interaction(&operand, &expr_type);
                if let None = type_interaction_res {
                    return Err(TypeCheckError::IncompatibleTypesForOperand(
                        operand,
                        ident_type,
                        expr_type,
                        ident.pos.start..expr.pos.end,
                    ));
                }
                let actual_t = type_interaction_res.unwrap();
                expression.inner.ty = Some(actual_t.clone());
                Ok(actual_t)
            }
        }
    }
}
