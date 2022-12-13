use super::typechecker_error::TypeCheckErr;
use crate::syntax::{
    ast::{Expr, Spanned, Stmt},
    operands::InfixOp,
    ty::Type,
};
use std::collections::HashMap;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct FunctionSignature<'a> {
    name: &'a Spanned<String>,
    parameters: &'a Vec<(Spanned<String>, Spanned<Type>)>,
    return_type: &'a Option<Spanned<Type>>,
}

#[derive(Debug, Clone)]
struct SymbolTable {
    symbols: HashMap<String, Type>,
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }

    fn insert(&mut self, name: &str, ty: Type) -> Option<Type> {
        self.symbols.insert(name.to_owned(), ty)
    }

    fn get(&self, name: &str) -> Option<&Type> {
        self.symbols.get(name)
    }
}

pub struct TypeChecker<'a> {
    function_signatures: HashMap<String, FunctionSignature<'a>>,
    symbol_table: SymbolTable,
    errors: Vec<TypeCheckErr>,
    current_fun_signature: Option<FunctionSignature<'a>>,
}

impl<'a> TypeChecker<'a> {
    pub fn check(functions: &'a Vec<Stmt>) -> Vec<TypeCheckErr> {
        let (function_signatures, errors) = Self::get_function_signatures(functions);
        let mut sel = Self {
            function_signatures,
            errors,
            symbol_table: SymbolTable::new(),
            current_fun_signature: None,
        };

        for fun in functions {
            sel.check_statement(fun);
        }

        sel.errors
    }

    fn emit_error(&mut self, error: TypeCheckErr) {
        self.errors.push(error);
    }

    fn check_statement(&mut self, statement: &Stmt) {
        match statement {
            Stmt::Declaration { ty, ident, value } => {
                let value_ty = self.check_expression(&value.inner);

                let type_to_declare_with = match (ty, value_ty) {
                    (None, Some(inferred)) => {
                        // infer from value type
                        Some(inferred)
                    }
                    (Some(te), Some(tv)) => {
                        // check both same
                        if *te != tv {
                            self.emit_error(TypeCheckErr::DeclarationTypesNotMatching {
                                ident: ident.clone(),
                                declared_ty: ty.clone(),
                                value_ty: value_ty,
                            });
                        }
                        Some(tv)
                    }
                    _ => {
                        // error
                        self.emit_error(TypeCheckErr::DeclarationTypesNotMatching {
                            ident: ident.clone(),
                            declared_ty: ty.clone(),
                            value_ty,
                        });
                        None
                    }
                };
                match type_to_declare_with {
                    Some(t) => {
                        if let Some(prev) = self.symbol_table.insert(&ident.inner, t) {
                            self.emit_error(TypeCheckErr::VariableDuplicate {
                                ident: ident.clone(),
                                previous_type: prev,
                                value_ty: t,
                            });
                        }
                    }
                    None => {}
                }
            }
            Stmt::Expression { expr } => {
                self.check_expression(&expr.inner);
            }
            Stmt::While { condition, body } => {
                let condition_ty = self.check_expression(&condition.inner);
                if condition_ty != Some(Type::Bool) {
                    self.emit_error(TypeCheckErr::TypeMismatch {
                        expected: Some(Type::Bool),
                        actual: condition_ty,
                        positon: condition.pos.clone(),
                    })
                }
                self.check_statement(body);
            }
            Stmt::If {
                condition,
                body,
                alternative,
            } => {
                let condition_ty = self.check_expression(&condition.inner);
                if condition_ty != Some(Type::Bool) {
                    self.emit_error(TypeCheckErr::TypeMismatch {
                        expected: Some(Type::Bool),
                        actual: condition_ty,
                        positon: condition.pos.clone(),
                    })
                }
                self.check_statement(body);
                if let Some(alternative) = alternative {
                    self.check_statement(alternative);
                }
            }
            Stmt::Sequence { statements } => {
                for stmt in statements.iter() {
                    self.check_statement(stmt);
                }
            }
            Stmt::FunctionDefinition {
                name,
                parameters,
                return_type: _,
                body,
            } => {
                if let Some(fun_sig) = self.function_signatures.get(&name.inner).cloned() {
                    self.current_fun_signature = Some(fun_sig);
                    let old_env = self.symbol_table.to_owned();
                    self.symbol_table = SymbolTable::new();
                    for param in parameters {
                        self.symbol_table.insert(&param.0.inner, param.1.inner);
                    }
                    self.check_statement(body);
                    self.symbol_table = old_env;
                } else {
                    self.current_fun_signature = None;
                    self.emit_error(TypeCheckErr::IdentifierNotFound {
                        identifier: name.inner.clone(),
                        positon: name.pos.clone(),
                    })
                }
            }
            Stmt::Return { value } => {
                let unwrapped_sig_ret_type = match &self.current_fun_signature {
                    Some(fun_sig) => match fun_sig.return_type {
                        Some(sig_ret) => Some(sig_ret),
                        None => None,
                    },
                    None => None,
                };
                match (unwrapped_sig_ret_type, value) {
                    (Some(t_sig_ret), Some(ret_expr)) => {
                        let val_type = self.check_expression(&ret_expr.inner);
                        if let Some(val_type) = val_type {
                            if t_sig_ret.inner == val_type {
                                // success
                            } else {
                                self.emit_error(TypeCheckErr::TypeMismatch {
                                    expected: Some(t_sig_ret.inner),
                                    actual: Some(val_type),
                                    positon: ret_expr.pos.clone(),
                                })
                            }
                        }
                    }
                    (None, None) => {
                        // success
                    }
                    (None, Some(val)) => {
                        let val_type = self.check_expression(&val.inner);
                        let actual_ret_ty_maybe_none = match val_type {
                            Some(t) => Some(t),
                            None => None,
                        };
                        self.emit_error(TypeCheckErr::TypeMismatch {
                            expected: None,
                            actual: actual_ret_ty_maybe_none,
                            positon: val.pos.clone(),
                        })
                    }
                    (Some(expected), None) => self.emit_error(TypeCheckErr::TypeMismatch {
                        expected: Some(expected.inner),
                        actual: None,
                        positon: expected.pos.clone(),
                    }),
                }
            }
        }
    }

    fn check_expression(&mut self, expression: &Expr) -> Option<Type> {
        match expression {
            Expr::Binary { op, left, right } => {
                // Check that the operands have compatible types
                let left_ty = self.check_expression(&left.inner);
                let right_ty = self.check_expression(&right.inner);
                let op_ty = self.check_binary_operator_type(op.clone(), left_ty, right_ty);
                op_ty
            }
            Expr::Unary { op: _, expr } => self.check_expression(&expr.inner),
            Expr::Identifier(ident) => {
                let ty = self.symbol_table.get(&ident.inner).copied();
                if ty.is_none() {
                    self.emit_error(TypeCheckErr::IdentifierNotFound {
                        identifier: ident.inner.clone(),
                        positon: ident.pos.clone(),
                    })
                }
                ty
            }
            Expr::IntegerLiteral(_) => Some(Type::I64),
            Expr::BooleanLiteral(_) => Some(Type::Bool),
            Expr::Assign {
                ident,
                operand: _,
                expr,
            } => {
                // Check that the right-hand side of the assignment has the same type as the left-hand side
                let expr_ty = self.check_expression(&expr.inner);
                let expected_ty = self.symbol_table.get(&ident.inner).cloned();
                match (expr_ty, expected_ty) {
                    (Some(operand_ty), Some(expected_ty)) if operand_ty == expected_ty => {
                        Some(operand_ty)
                    }
                    (operand_ty, expected_ty) => {
                        // If the operand has the wrong type, raise a type error
                        self.emit_error(TypeCheckErr::TypeMismatch {
                            expected: expected_ty,
                            actual: operand_ty,
                            positon: ident.pos.start..expr.pos.end,
                        });
                        None
                    }
                }
            }
            Expr::FunctionParameter { name: _, ty: _ } => unreachable!(),
            Expr::FunctionInvocation { name, args: _ } => {
                let sig = self.function_signatures.get(&name.inner);
                match sig {
                    Some(fn_sig) => {
                        self.current_fun_signature = Some(fn_sig.clone());

                        // check args

                        match fn_sig.return_type {
                            Some(t) => Some(t.inner),
                            None => None,
                        }
                    }
                    None => {
                        self.emit_error(TypeCheckErr::IdentifierNotFound {
                            identifier: name.inner.clone(),
                            positon: name.pos.clone(),
                        });
                        None
                    }
                }
            }
        }
    }

    fn check_binary_operator_type(
        &mut self,
        op: Spanned<InfixOp>,
        left_ty: Option<Type>,
        right_ty: Option<Type>,
    ) -> Option<Type> {
        match (left_ty, right_ty) {
            (Some(left), Some(right)) => {
                // If both operands have a well-defined type, check if they are compatible
                match left.type_interaction(op.inner, right) {
                    Some(ty) => Some(ty),
                    None => {
                        // If the operands have the wrong type, raise a type error
                        self.emit_error(TypeCheckErr::IncompatibleOperandTypes { op, left, right });
                        None
                    }
                }
            }
            _ => {
                // If one or both operands do not have a well-defined type, return `None`
                None
            }
        }
    }

    fn get_function_signatures(
        functions: &'a Vec<Stmt>,
    ) -> (HashMap<String, FunctionSignature<'a>>, Vec<TypeCheckErr>) {
        let mut fun_sigs = HashMap::<String, FunctionSignature>::new();
        let mut errs = Vec::new();
        for fun in functions {
            if let Stmt::FunctionDefinition {
                name,
                parameters,
                return_type,
                body: _,
            } = fun
            {
                let fs = FunctionSignature {
                    name,
                    parameters,
                    return_type,
                };
                if let Some(other_sig) = fun_sigs.insert(name.inner.clone(), fs) {
                    errs.push(TypeCheckErr::FunctionDuplicate {
                        trying_to_init_ident: name.clone(),
                        existing_fun_ident: other_sig.name.clone(),
                    });
                }
            }
        }

        if !fun_sigs.contains_key("main") {
            errs.push(TypeCheckErr::NoMainFunctionFound)
        }

        (fun_sigs, errs)
    }
}
