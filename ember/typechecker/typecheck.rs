use super::typechecker_error::TypeCheckError;
use crate::ast::{
    ast_node::AstNode, ast_root::AstRoot, expression::Expr, infix::InfixOp, statement::Stmt,
    ty::Type, typed_expression::TypedExpr,
};
use std::{collections::HashMap, ops::Range};

/*
Pretty shit typechecker. Needs a rework.
*/
pub struct TypeChecker<'source> {
    pub input: &'source str,
    current_stack: Vec<String>,
}

impl<'source> TypeChecker<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            input: source,
            current_stack: vec!["".to_string()],
        }
    }

    pub fn typecheck(&mut self, program: &AstRoot) -> Result<Type, TypeCheckError> {
        let mut env = HashMap::new();
        self.check_statement(&mut env, &program.sequence, None)
    }

    fn insert_scoped_var(
        &mut self,
        env: &mut HashMap<String, (Type, Range<usize>)>,
        name: &String,
        ty: Type,
        pos: Range<usize>,
    ) {
        let binding = "_".to_string();
        let current_scope = self.current_stack.last().unwrap_or(&binding);
        env.insert(format!("__{current_scope}::{name}"), (ty, pos));
    }

    fn get_scoped_var(
        &mut self,
        env: &mut HashMap<String, (Type, Range<usize>)>,
        name: &String,
    ) -> Option<(Type, Range<usize>)> {
        for current_scope in self.current_stack.iter().rev() {
            println!("{}", current_scope);
            if let Some(x) = env.get(&format!("__{current_scope}::{name}")) {
                return Some((x.0, x.1.clone()));
            }
        }
        None
    }

    fn check_statement(
        &mut self,
        env: &mut HashMap<String, (Type, Range<usize>)>,
        stmt: &Stmt,
        expected_type: Option<Type>,
    ) -> Result<Type, TypeCheckError> {
        match stmt {
            Stmt::Declaration { ty, ident, value } => self.check_declaration(env, ident, ty, value),
            Stmt::Expression { expr } => self.check_expression(env, expr, Type::Void),
            Stmt::While { condition, body } => self.check_while(env, condition, body),
            Stmt::If {
                condition,
                body,
                alternative,
            } => self.check_if(env, condition, body, alternative),
            Stmt::Sequence { statements } => self.check_sequence(env, statements, expected_type),
            Stmt::FunctionDefinition {
                name,
                parameters,
                return_type,
                body,
            } => self.check_function(env, return_type, name, parameters, body),
            Stmt::Return { value } => self.check_return(value, env, expected_type),
        }
    }

    fn check_expression(
        &mut self,
        env: &mut HashMap<String, (Type, Range<usize>)>,
        expression: &AstNode<TypedExpr>,
        expected_type: Type,
    ) -> Result<Type, TypeCheckError> {
        match &expression.inner.expr {
            Expr::Infix { op, left, right } => {
                self.check_infix(env, left, right, op, expected_type)
            }
            Expr::Prefix { op: _, expr } => self.check_expression(env, expr, expected_type),
            Expr::Identifier(ident) => self.check_identifier(env, ident),
            Expr::IntegerLiteral(lit) => self.check_int_literal(expected_type, lit),
            Expr::BooleanLiteral(lit) => self.check_bool_literal(expected_type, lit),
            Expr::Assign {
                ident,
                operand,
                expr,
            } => self.check_assign(env, ident, expected_type, expr, operand),
            Expr::FunctionParameter { name: _, ty: _ } => {
                unreachable!(
                    "Case gets handled in FunctionParameterInvocation. Should not get called."
                )
            }
            Expr::FunctionInvocation { name, args } => {
                self.check_function_invocation(args, env, name)
            }
        }
    }

    fn check_function_invocation(
        &mut self,
        args: &[AstNode<TypedExpr>],
        env: &mut HashMap<String, (Type, Range<usize>)>,
        name: &AstNode<TypedExpr>,
    ) -> Result<Type, TypeCheckError> {
        let fun_name_str = name.inner.expr.to_string();
        self.current_stack.push(fun_name_str.clone());
        let mut all_params: HashMap<String, (Type, Range<usize>)> = HashMap::new();
        for n in env.iter() {
            if n.0.starts_with(&format!("__{name}::")) && !n.0.ends_with(&format!("::{name}")) {
                all_params.insert(n.0.to_string(), n.1.clone());
            }
        }
        let a_len = args.len();
        let aa_len = all_params.len();
        if a_len != aa_len {
            let other_pos = if let Some(other_p) = all_params.values().into_iter().next() {
                other_p.1.clone()
            } else {
                0..0
            };
            return Err(TypeCheckError::ArgumentCountNotMatching {
                name: fun_name_str,
                pos: name.pos.clone(),
                called_with_arg_count: a_len,
                expected_with_arg_cont: aa_len,
                other_pos,
            });
        }

        let name_str = name.inner.expr.to_string();
        let ret = if let Some(func) = self.get_scoped_var(env, &name_str) {
            Ok(func.0)
        } else {
            Err(TypeCheckError::IdentifierNotFound {
                ident: name_str.to_string(),
                pos: name.pos.clone(),
            })
        };
        self.current_stack.pop();
        ret
    }

    fn check_assign(
        &mut self,
        env: &mut HashMap<String, (Type, Range<usize>)>,
        ident: &AstNode<TypedExpr>,
        expected_type: Type,
        expr: &AstNode<TypedExpr>,
        operand: &AstNode<InfixOp>,
    ) -> Result<Type, TypeCheckError> {
        let ident_type = self.check_expression(env, ident, expected_type)?;
        let expr_type = self.check_expression(env, expr, ident_type)?;
        let type_interaction_res = ident_type.type_interaction(operand.inner, expr_type);
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

    fn check_identifier(
        &mut self,
        env: &mut HashMap<String, (Type, Range<usize>)>,
        ident: &AstNode<String>,
    ) -> Result<Type, TypeCheckError> {
        let name_str = ident.inner.to_string();
        let type_opt = self.get_scoped_var(env, &name_str);
        match type_opt {
            Some(ty) => Ok(ty.0.to_owned()),
            None => Err(TypeCheckError::IdentifierNotFound {
                ident: ident.to_string(),
                pos: ident.pos.to_owned(),
            }),
        }
    }

    fn check_infix(
        &mut self,
        env: &mut HashMap<String, (Type, Range<usize>)>,
        left: &AstNode<TypedExpr>,
        right: &AstNode<TypedExpr>,
        op: &AstNode<InfixOp>,
        expected_type: Type,
    ) -> Result<Type, TypeCheckError> {
        // No expected type. Compare the resulting types instead
        let l = self.check_expression(env, left, Type::Void)?;
        let r = self.check_expression(env, right, Type::Void)?;
        let type_interaction_res = l.type_interaction(op.inner, r);
        if type_interaction_res.is_none() {
            return Err(TypeCheckError::IncompatibleTypesForOperand {
                op: op.inner.to_owned(),
                l,
                r,
                pos: left.pos.start..right.pos.end,
            });
        }
        let actual_t = type_interaction_res.unwrap();
        if actual_t != expected_type && expected_type != Type::Void {
            return Err(TypeCheckError::InfixTypesNotMatching {
                l: actual_t,
                r: expected_type.to_owned(),
                pos: left.pos.start..right.pos.end,
            });
        }
        Ok(actual_t)
    }

    fn check_return(
        &mut self,
        value: &Option<AstNode<TypedExpr>>,
        env: &mut HashMap<String, (Type, Range<usize>)>,
        expected_type: Option<Type>,
    ) -> Result<Type, TypeCheckError> {
        let mut span = 0..0;
        let actual_type = if let Some(val) = value {
            span = val.pos.clone();
            self.check_expression(env, val, expected_type.unwrap_or(Type::Void))?
        } else {
            Type::Void
        };
        let expected_type = expected_type.unwrap_or(Type::Void);
        if expected_type == actual_type {
            Ok(expected_type)
        } else {
            Err(TypeCheckError::NotMatchingExpetectedType {
                expected: expected_type,
                actual: actual_type,
                pos: span,
            })
        }
    }

    fn check_function(
        &mut self,
        env: &mut HashMap<String, (Type, Range<usize>)>,
        return_type: &Type,
        name: &AstNode<TypedExpr>,
        parameters: &[AstNode<TypedExpr>],
        body: &Stmt,
    ) -> Result<Type, TypeCheckError> {
        if let Some(func) = env.get(&format!("__{name}")) {
            return Err(TypeCheckError::FunctionDuplicate {
                name: name.inner.to_string(),
                pos: name.pos.clone(),
                other_pos: func.1.clone(),
            });
        };
        let name_str = name.inner.expr.to_string();
        self.current_stack.push(name_str.clone());
        self.insert_scoped_var(env, &name_str, *return_type, name.pos.clone());
        for param in parameters.iter() {
            let param = match &param.inner.expr {
                Expr::FunctionParameter { name: p_name, ty } => {
                    (p_name.inner.expr.to_string(), ty.clone().inner)
                }
                _ => unreachable!(),
            };
            self.insert_scoped_var(env, &param.0.to_string(), param.1, name.pos.clone());
        }
        self.check_statement(env, body, Some(*return_type))?;
        self.current_stack.pop();
        Ok(Type::Void)
    }

    fn check_sequence(
        &mut self,
        env: &mut HashMap<String, (Type, Range<usize>)>,
        statements: &[Stmt],
        expected_type: Option<Type>,
    ) -> Result<Type, TypeCheckError> {
        let mut extended_env = env.clone();
        for stmt in statements.iter() {
            self.check_statement(&mut extended_env, stmt, expected_type)?;
        }
        Ok(Type::Void)
    }

    fn check_if(
        &mut self,
        env: &mut HashMap<String, (Type, Range<usize>)>,
        condition: &AstNode<TypedExpr>,
        body: &Stmt,
        alternative: &Option<Box<Stmt>>,
    ) -> Result<Type, TypeCheckError> {
        self.check_expression(env, condition, Type::Bool)?;
        self.check_statement(env, body, None)?;
        self.check_statement(
            env,
            &alternative.to_owned().unwrap_or_else(|| {
                Box::new(Stmt::Sequence {
                    statements: Box::new(Vec::new()),
                })
            }),
            None,
        )?;
        Ok(Type::Void)
    }

    fn check_while(
        &mut self,
        env: &mut HashMap<String, (Type, Range<usize>)>,
        condition: &AstNode<TypedExpr>,
        body: &Stmt,
    ) -> Result<Type, TypeCheckError> {
        self.check_expression(env, condition, Type::Bool)?;
        self.check_statement(env, body, None)?;
        Ok(Type::Void)
    }

    fn check_declaration(
        &mut self,
        env: &mut HashMap<String, (Type, Range<usize>)>,
        ident: &AstNode<TypedExpr>,
        ty: &Type,
        value: &AstNode<TypedExpr>,
    ) -> Result<Type, TypeCheckError> {
        let type_opt = env.get(&self.input[ident.pos.clone()]);
        if let Some(t) = type_opt {
            return Err(TypeCheckError::VariableAlreadyExists {
                ident: ident.inner.expr.to_string(),
                initialized_with_type: t.0.to_owned(),
                tried_to_init_with: ty.to_owned(),
                position_ident: ident.pos.to_owned(),
                position_init_ident: t.1.to_owned(),
            });
        }

        self.insert_scoped_var(
            env,
            &self.input[ident.pos.clone()].to_string(),
            *ty,
            ident.pos.clone(),
        );

        let val_type = self.check_expression(env, value, *ty)?;
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
    fn check_int_literal(
        &self,
        expected_type: Type,
        lit: &AstNode<i64>,
    ) -> Result<Type, TypeCheckError> {
        if expected_type != Type::I64 && expected_type != Type::Void {
            return Err(TypeCheckError::NotMatchingExpetectedType {
                expected: expected_type,
                actual: Type::I64,
                pos: lit.pos.clone(),
            });
        }
        Ok(Type::I64)
    }

    fn check_bool_literal(
        &self,
        expected_type: Type,
        lit: &AstNode<bool>,
    ) -> Result<Type, TypeCheckError> {
        if expected_type != Type::Bool && expected_type != Type::Void {
            return Err(TypeCheckError::NotMatchingExpetectedType {
                expected: expected_type,
                actual: Type::Bool,
                pos: lit.pos.clone(),
            });
        }
        Ok(Type::Bool)
    }
}
