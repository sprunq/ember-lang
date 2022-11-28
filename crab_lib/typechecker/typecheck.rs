#![allow(unused_variables)]

use crate::ast::{
    ast_node::AstNode, ast_root::AstRoot, expression::Expr, statement::Stmt, ty::Type,
    typed_expression::TypedExpr,
};
use std::{collections::HashMap, ops::Range};

use super::typechecker_error::TypeCheckError;

pub struct TypeChecker {}

impl TypeChecker {
    pub fn typecheck(program: &AstRoot) -> Result<Type, TypeCheckError> {
        let mut env = HashMap::new();
        Self::check_statement(&mut env, &program.sequence)
    }

    fn check_statement(
        env: &mut HashMap<String, (Type, Range<usize>)>,
        stmt: &Stmt,
    ) -> Result<Type, TypeCheckError> {
        match stmt {
            Stmt::Declaration { ty, ident, value } => {
                let type_opt = env.get(&ident.inner.expr.to_string());
                if let Some(t) = type_opt {
                    return Err(TypeCheckError::VariableAlreadyExists {
                        ident: ident.inner.expr.to_string(),
                        initialized_with_type: t.0.to_owned(),
                        tried_to_init_with: ty.to_owned(),
                        position_ident: ident.pos.to_owned(),
                        position_init_ident: t.1.to_owned(),
                    });
                }
                env.insert(ident.to_string(), (ty.clone(), ident.pos.clone()));
                let val_type = Self::check_expression(env, value, ty)?;
                if *ty == val_type {
                    Ok(Type::Void)
                } else {
                    Err(TypeCheckError::DeclarationTypesNotMatching {
                        l: ty.to_owned(),
                        r: val_type,
                        ident: ident.to_string(),
                        pos: ident.pos.start..value.pos.end,
                    })
                }
            }
            Stmt::Expression { expr } => Self::check_expression(env, expr, &Type::Void),
            Stmt::While { condition, body } => {
                let cond = Self::check_expression(env, condition, &Type::Bool)?;
                let bod = Self::check_statement(env, body)?;
                Ok(Type::Void)
            }
            Stmt::If {
                condition,
                body,
                alternative,
            } => {
                let cond = Self::check_expression(env, &*condition, &Type::Bool)?;
                let bod = Self::check_statement(env, body)?;
                let alt = Self::check_statement(
                    env,
                    &alternative.to_owned().unwrap_or_else(|| {
                        Box::new(Stmt::Sequence {
                            statements: Box::new(Vec::new()),
                        })
                    }),
                )?;
                Ok(Type::Void)
            }
            Stmt::Sequence { statements } => {
                let mut extended_env = env.clone();
                for stmt in statements.iter() {
                    Self::check_statement(&mut extended_env, stmt)?;
                }
                Ok(Type::Void)
            }
        }
    }

    fn check_expression(
        env: &mut HashMap<String, (Type, Range<usize>)>,
        expression: &AstNode<TypedExpr>,
        expected_type: &Type,
    ) -> Result<Type, TypeCheckError> {
        match &expression.inner.expr {
            Expr::Infix { op, left, right } => {
                let l = Self::check_expression(env, &left, expected_type)?;
                let r = Self::check_expression(env, &right, expected_type)?;
                let type_interaction_res = l.type_interaction(&op.inner, &r);
                if type_interaction_res.is_none() {
                    return Err(TypeCheckError::IncompatibleTypesForOperand {
                        op: op.inner.to_owned(),
                        l,
                        r,
                        pos: left.pos.start..right.pos.end,
                    });
                }
                let actual_t = type_interaction_res.unwrap();
                if actual_t != *expected_type {
                    return Err(TypeCheckError::InfixTypesNotMatching {
                        l: actual_t,
                        r: expected_type.to_owned(),
                        pos: left.pos.start..right.pos.end,
                    });
                }
                Ok(actual_t)
            }
            Expr::Prefix { op, expr } => Self::check_expression(env, expr, expected_type),
            Expr::Identifier(ident) => {
                let type_opt = env.get(ident.inner.as_str());
                match type_opt {
                    Some(ty) => Ok(ty.0.to_owned()),
                    None => Err(TypeCheckError::IdentifierNotFound {
                        ident: ident.to_string(),
                        pos: ident.pos.to_owned(),
                    }),
                }
            }
            Expr::IntegerLiteral(_) => Ok(Type::I64),
            Expr::BooleanLiteral(_) => Ok(Type::Bool),
            Expr::Assign {
                ident,
                operand,
                expr,
            } => {
                let ident_type = Self::check_expression(env, &ident, expected_type)?;
                let expr_type = Self::check_expression(env, &expr, &ident_type)?;
                let type_interaction_res = ident_type.type_interaction(&operand.inner, &expr_type);
                if type_interaction_res.is_none() {
                    return Err(TypeCheckError::IncompatibleTypesForOperand {
                        op: operand.inner.to_owned(),
                        l: ident_type,
                        r: expr_type,
                        pos: ident.pos.start..expr.pos.end,
                    });
                }
                let actual_t = type_interaction_res.unwrap();
                Ok(actual_t)
            }
        }
    }
}
