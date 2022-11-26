#![allow(unused_variables)]

use crate::ast::{
    expression::{Expr, TypedExpr},
    infix::Infix,
    sequence::Sequence,
    statement::Stmt,
    ty::Type,
};
use std::collections::HashMap;

use super::typechecker_error::TypeCheckError;

pub struct TypeChecker {
    environment: HashMap<String, Type>,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            environment: HashMap::new(),
        }
    }
    pub fn check(&mut self, sequence: Vec<Stmt>) -> Option<TypeCheckError> {
        for stmt in sequence {
            let x = self.check_statement(stmt);
            if x.is_some() {
                return x;
            }
        }
        None
    }

    pub fn check_statement(&mut self, stmt: Stmt) -> Option<TypeCheckError> {
        match stmt {
            Stmt::Declaration { ty, ident, value } => {
                self.environment.insert(ident, ty.clone());
                self.check_expression(value, Some(ty)).err()
            }

            Stmt::Expression { expr } => self.check_expression(expr, None).err(),
            Stmt::While { condition, body } => {
                let cond = self.check_expression(*condition, Some(Type::Bool));
                let b = self.check(body.statements);

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
                let cond = self.check_expression(*condition, Some(Type::Bool));
                let bod = self.check(body.statements);
                let alt = self.check(alternative.unwrap_or(Sequence::new()).statements);

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

    pub fn check_expression(
        &self,
        expression: TypedExpr,
        expected_type: Option<Type>,
    ) -> Result<Type, TypeCheckError> {
        match expression.expr {
            Expr::Infix { op, left, right } => {
                let l = self.check_expression(*left, None)?;
                let r = self.check_expression(*right, None)?;

                match (&l, &r) {
                    (Type::I64, Type::I64) | (Type::Bool, Type::Bool) => match op {
                        Infix::Eq | Infix::NotEq | Infix::Lt | Infix::Gt => Ok(Type::Bool),
                        _ => Ok(l),
                    },
                    _ => Err(TypeCheckError::InfixOperandsNotMatching),
                }
            }
            Expr::Prefix { op, expr } => self.check_expression(*expr, expected_type),
            Expr::Identifier(ident) => {
                let type_opt = self.environment.get(&ident);
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
            } => match self.environment.get(&ident) {
                Some(ty) => self.check_expression(*expr, Some(ty.clone())),
                None => Err(TypeCheckError::IdentifierNotFound(ident)),
            },
        }
    }
}
