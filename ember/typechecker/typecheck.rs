use super::typechecker_error::TypeCheckErr;
use crate::syntax::{
    ast::{Expr, Spanned, Stmt},
    operands::InfixOp,
    ty::Type,
};
use std::collections::HashMap;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    name: Spanned<String>,
    parameters: Vec<(Spanned<String>, Spanned<Type>)>,
    return_type: Option<Spanned<Type>>,
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

pub struct TypeChecker {
    function_signatures: HashMap<String, FunctionSignature>,
    symbol_table: SymbolTable,
    errors: Vec<TypeCheckErr>,
    current_fun_signature: Option<FunctionSignature>,
    is_errored: bool,
}

impl TypeChecker {
    pub fn check(functions: &mut Vec<Stmt>) -> Vec<TypeCheckErr> {
        let (function_signatures, errors) = Self::get_function_signatures(functions);
        let mut sel = Self {
            function_signatures,
            errors,
            symbol_table: SymbolTable::new(),
            current_fun_signature: None,
            is_errored: false,
        };

        for fun in functions {
            sel.check_statement(fun);
        }

        sel.errors
    }

    fn emit_error(&mut self, error: TypeCheckErr) {
        if !self.is_errored {
            self.is_errored = true;
            self.errors.push(error);
        }
    }

    fn check_statement(&mut self, statement: &mut Stmt) {
        match statement {
            Stmt::Declaration { ty, ident, value } => {
                self.check_declaration(value, ty, ident);
            }
            Stmt::Expression { expr } => {
                self.check_expression(&expr.inner);
            }
            Stmt::While { condition, body } => {
                self.check_while(condition, body);
            }
            Stmt::If {
                condition,
                body,
                alternative,
            } => {
                self.check_if(condition, body, alternative);
            }
            Stmt::Sequence { statements } => {
                for stmt in statements.iter_mut() {
                    self.is_errored = false;
                    self.check_statement(stmt);
                }
            }
            Stmt::FunctionDefinition {
                name,
                parameters,
                return_type: _,
                body,
            } => {
                self.check_function_definition(name, parameters, body);
            }
            Stmt::Return { value } => {
                self.check_return(value);
            }
            Stmt::Break => {}
        }
    }

    fn check_expression(&mut self, expression: &Expr) -> Option<Type> {
        match expression {
            Expr::Binary { op, left, right } => self.check_binary(left, right, op),
            Expr::Unary { op: _, expr } => self.check_expression(&expr.inner),
            Expr::Identifier(ident) => self.check_identifier(ident),
            Expr::IntegerLiteral(_) => Some(Type::Int),
            Expr::BooleanLiteral(_) => Some(Type::Bool),
            Expr::Assign {
                ident,
                operand: _,
                expr,
            } => self.check_assign(expr, ident),
            Expr::FunctionParameter { name: _, ty: _ } => unreachable!(),
            Expr::FunctionInvocation { name, args } => self.check_function_invocation(name, args),
        }
    }

    fn check_binary(
        &mut self,
        left: &Box<Spanned<Expr>>,
        right: &Box<Spanned<Expr>>,
        op: &Spanned<InfixOp>,
    ) -> Option<Type> {
        // Check that the operands have compatible types
        let left_ty = self.check_expression(&left.inner);
        let right_ty = self.check_expression(&right.inner);
        self.check_binary_operator_type(op.clone(), left_ty, right_ty)
    }

    fn check_assign(&mut self, expr: &Box<Spanned<Expr>>, ident: &Spanned<String>) -> Option<Type> {
        // Check that the right-hand side of the assignment has the same type as the left-hand side
        let expr_ty = self.check_expression(&expr.inner);
        let expected_ty = self.symbol_table.get(&ident.inner).cloned();
        match (expr_ty, expected_ty) {
            (Some(operand_ty), Some(expected_ty)) if operand_ty == expected_ty => Some(operand_ty),
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

    fn check_function_invocation(
        &mut self,
        name: &Spanned<String>,
        args: &Vec<Spanned<Expr>>,
    ) -> Option<Type> {
        let sig = self.function_signatures.get(&name.inner).cloned();
        match sig {
            Some(fn_sig) => {
                // check args
                let decl_types = fn_sig
                    .parameters
                    .iter()
                    .map(|p| Some(p.1.inner))
                    .collect::<Vec<_>>();
                let invo_types = args
                    .iter()
                    .map(|p| self.check_expression(&p.inner))
                    .collect::<Vec<_>>();

                if decl_types != invo_types {
                    self.emit_error(TypeCheckErr::FunctionInvocationNotMatchingDeclaration {
                        identifier: name.inner.clone(),
                        delcaration_types: decl_types,
                        invocation_types: invo_types,
                        decl_pos: fn_sig.name.pos.clone(),
                        invo_pos: name.pos.clone(),
                    })
                }

                fn_sig.return_type.as_ref().map(|t| t.inner)
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

    fn check_identifier(&mut self, ident: &Spanned<String>) -> Option<Type> {
        let ty = self.symbol_table.get(&ident.inner).copied();
        if ty.is_none() {
            self.emit_error(TypeCheckErr::IdentifierNotFound {
                identifier: ident.inner.clone(),
                positon: ident.pos.clone(),
            })
        }
        ty
    }

    fn check_declaration(
        &mut self,
        value: &mut Spanned<Expr>,
        ty: &mut Option<Type>,
        ident: &mut Spanned<String>,
    ) {
        let value_ty = self.check_expression(&value.inner);
        let type_to_declare_with = match (&ty, value_ty) {
            (None, Some(inferred)) => {
                // infer from value type
                Some(inferred)
            }
            (Some(te), Some(tv)) => {
                // check both same
                if *te != tv {
                    self.emit_error(TypeCheckErr::DeclarationTypesNotMatching {
                        ident: ident.clone(),
                        declared_ty: *ty,
                        value_ty,
                    });
                }
                Some(tv)
            }
            _ => {
                // error
                self.emit_error(TypeCheckErr::DeclarationTypesNotMatching {
                    ident: ident.clone(),
                    declared_ty: *ty,
                    value_ty,
                });
                None
            }
        };
        if let Some(t) = type_to_declare_with {
            *ty = Some(t);
            if let Some(prev) = self.symbol_table.insert(&ident.inner, t) {
                self.emit_error(TypeCheckErr::VariableDuplicate {
                    ident: ident.clone(),
                    previous_type: prev,
                    value_ty: t,
                });
            }
        }
    }

    fn check_while(&mut self, condition: &Box<Spanned<Expr>>, body: &mut Box<Stmt>) {
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

    fn check_if(
        &mut self,
        condition: &mut Box<Spanned<Expr>>,
        body: &mut Box<Stmt>,
        alternative: &mut Option<Box<Stmt>>,
    ) {
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

    fn check_return(&mut self, value: &mut Option<Spanned<Expr>>) {
        let unwrapped_sig_ret_type = match &self.current_fun_signature {
            Some(fun_sig) => match fun_sig.return_type.clone() {
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
                self.emit_error(TypeCheckErr::TypeMismatch {
                    expected: None,
                    actual: val_type,
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

    fn check_function_definition(
        &mut self,
        name: &mut Spanned<String>,
        parameters: &mut Vec<(Spanned<String>, Spanned<Type>)>,
        mut body: &mut Box<Stmt>,
    ) {
        if let Some(fun_sig) = self.function_signatures.get(&name.inner).cloned() {
            self.current_fun_signature = Some(fun_sig);
            let old_env = self.symbol_table.to_owned();
            self.symbol_table = SymbolTable::new();
            for param in parameters {
                self.symbol_table.insert(&param.0.inner, param.1.inner);
            }
            self.check_statement(&mut body);
            self.symbol_table = old_env;
        } else {
            self.current_fun_signature = None;
            self.emit_error(TypeCheckErr::IdentifierNotFound {
                identifier: name.inner.clone(),
                positon: name.pos.clone(),
            })
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
        functions: &Vec<Stmt>,
    ) -> (HashMap<String, FunctionSignature>, Vec<TypeCheckErr>) {
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
                    name: name.clone(),
                    parameters: parameters.clone(),
                    return_type: return_type.clone(),
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
