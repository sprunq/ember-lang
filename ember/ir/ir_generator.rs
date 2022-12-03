#![allow(unused_variables)]
#![allow(dead_code)]
use super::{
    instruction::{IRInstruction, Label, Register, Value},
    operands::{BinaryOp, CompareOp},
};
use crate::syntax::{
    ast::{AstRoot, Expr, Spanned, Stmt, TypedExpr},
    operands::{InfixOp, PrefixOp},
    ty::Type,
};

pub struct IRGenerator {
    register_count: usize,
    label_count: usize,
    instructions: Vec<IRInstruction>,
}
impl Default for IRGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl IRGenerator {
    pub fn new() -> Self {
        Self {
            register_count: 0,
            label_count: 0,
            instructions: Vec::new(),
        }
    }

    pub fn gen_code(&mut self, ast: &AstRoot) -> &Vec<IRInstruction> {
        self.gen_statements(&ast.sequence);
        &self.instructions
    }

    fn new_register(&mut self) -> Register {
        self.register_count += 1;
        Register(self.register_count)
    }

    fn new_label(&mut self) -> Label {
        self.label_count += 1;
        Label(self.label_count)
    }

    fn gen_statements(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Declaration { ty, ident, value } => {
                self.gen_stmt_declaration(value, ident);
            }
            Stmt::Expression { expr } => {
                self.gen_stmt_expr(expr);
            }
            Stmt::While { condition, body } => {
                self.gen_stmt_while(condition, body);
            }
            Stmt::If {
                condition,
                body,
                alternative,
            } => {
                self.gen_stmt_if(condition, body, alternative);
            }
            Stmt::Sequence { statements } => {
                self.gen_stmt_sequence(statements);
            }
            Stmt::FunctionDefinition {
                name,
                parameters,
                return_type,
                body,
            } => {
                let body_label = self.new_label();
                let mut name_str = name.inner.clone();
                name_str.insert_str(0, "__");
                let params = parameters
                    .iter()
                    .map(|f| (f.inner.expr.to_string(), f.inner.ty.unwrap_or(Type::Void)))
                    .collect();

                let old_code = self.instructions.to_owned();
                self.instructions = vec![];

                self.gen_statements(body);

                let node = IRInstruction::FunctionDefinition {
                    name: name_str,
                    parameters: params,
                    body: self.instructions.to_owned(),
                };
                self.instructions = old_code;
                self.instructions.push(node);
            }
            Stmt::Return { value } => {
                let node = if let Some(val) = value {
                    let reg = self.gen_expressions(&val.inner);
                    IRInstruction::Return { register: reg }
                } else {
                    IRInstruction::Return { register: None }
                };
                self.instructions.push(node);
            }
        }
    }

    fn gen_expressions(&mut self, expr: &TypedExpr) -> Option<Register> {
        match &expr.expr {
            Expr::Infix { op, left, right } => self.gen_expr_infix(left, right, op),
            Expr::Prefix { op, expr } => self.gen_expr_prefix(expr, op),
            Expr::Identifier(ident) => self.gen_expr_ident(&ident.inner),
            Expr::IntegerLiteral(literal) => self.gen_expr_int_literal(literal),
            Expr::BooleanLiteral(literal) => self.gen_expr_bool_literal(literal),
            Expr::Assign {
                ident,
                operand,
                expr,
            } => self.gen_expr_assign(ident, operand, expr),
            Expr::FunctionParameter { name, ty } => todo!(),
            Expr::FunctionInvocation { name, args } => {
                let mut arg_regs = vec![];
                for arg in args {
                    let arg_reg = self
                        .gen_expressions(&arg.inner)
                        .unwrap_or(Register(usize::MAX));
                    arg_regs.push(arg_reg);
                }
                let target = self.new_register();
                let mut name_str = name.inner.clone();
                name_str.insert_str(0, "__");
                let node = IRInstruction::FunctionInvocation {
                    name: name_str,
                    registers: arg_regs,
                    target,
                };
                self.instructions.push(node);
                Some(target)
            }
        }
    }

    fn gen_stmt_while(&mut self, condition: &Spanned<TypedExpr>, body: &Stmt) {
        let top_label = self.new_label();
        let start_label = self.new_label();
        let merge_label = self.new_label();

        self.instructions
            .push(IRInstruction::Branch { label: top_label });

        self.instructions
            .push(IRInstruction::Label { name: top_label });

        let cond_reg = self
            .gen_expressions(&condition.inner)
            .unwrap_or(Register(usize::MAX));

        let cond_node = IRInstruction::BranchCond {
            condition: cond_reg,
            on_true: start_label,
            on_false: merge_label,
        };
        self.instructions.push(cond_node);

        self.instructions
            .push(IRInstruction::Label { name: start_label });
        self.gen_statements(body);
        self.instructions
            .push(IRInstruction::Branch { label: top_label });

        self.instructions
            .push(IRInstruction::Label { name: merge_label });
    }

    fn gen_stmt_if(
        &mut self,
        condition: &Spanned<TypedExpr>,
        body: &Stmt,
        alternative: &Option<Box<Stmt>>,
    ) {
        let cond_register = self
            .gen_expressions(&condition.inner)
            .unwrap_or(Register(usize::MAX));
        let t_label = self.new_label();
        let f_label = self.new_label();
        let merge_label = self.new_label();
        let cond_node = IRInstruction::BranchCond {
            condition: cond_register,
            on_true: t_label,
            on_false: f_label,
        };
        self.instructions.push(cond_node);
        // true block
        let t_label_node = IRInstruction::Label { name: t_label };
        self.instructions.push(t_label_node);
        self.gen_statements(body);
        let jmp_to_merge_node = IRInstruction::Branch { label: merge_label };
        self.instructions.push(jmp_to_merge_node);
        // false block
        if let Some(alt) = alternative {
            let f_label_node = IRInstruction::Label { name: f_label };
            self.instructions.push(f_label_node);
            self.gen_statements(alt);
            let jmp_to_merge_node = IRInstruction::Branch { label: merge_label };
            self.instructions.push(jmp_to_merge_node);
        }
        let merge_node = IRInstruction::Label { name: merge_label };
        self.instructions.push(merge_node);
    }

    fn gen_stmt_expr(&mut self, expr: &Spanned<TypedExpr>) {
        self.gen_expressions(&expr.inner);
    }

    fn gen_expr_prefix(
        &mut self,
        expr: &Spanned<TypedExpr>,
        op: &Spanned<PrefixOp>,
    ) -> Option<Register> {
        let expr_reg = self
            .gen_expressions(&expr.inner)
            .unwrap_or(Register(usize::MAX));
        let target = self.new_register();
        let prefix_node = match op.inner {
            PrefixOp::Minus => {
                // do value * -1;
                let int_literal_target = self.new_register();
                let int_literal = IRInstruction::MovI {
                    value: Value(-1),
                    target: int_literal_target,
                };
                self.instructions.push(int_literal);

                IRInstruction::ArithmeticBinaryI {
                    operand: BinaryOp::Mul,
                    left: int_literal_target,
                    right: expr_reg,
                    target,
                }
            }
        };
        self.instructions.push(prefix_node);
        Some(target)
    }

    fn gen_stmt_sequence(&mut self, statements: &[Stmt]) {
        for stmt in statements.iter() {
            self.gen_statements(stmt);
        }
    }

    fn gen_stmt_declaration(&mut self, value: &Spanned<TypedExpr>, ident: &Spanned<String>) {
        let node_decl = IRInstruction::Allocation {
            name: ident.inner.clone(),
        };
        self.instructions.push(node_decl);
        let a = self
            .gen_expressions(&value.inner)
            .unwrap_or(Register(usize::MAX));
        self.instructions.push(IRInstruction::StoreI {
            target: a,
            name: ident.inner.clone(),
        });
    }

    fn gen_expr_bool_literal(&mut self, literal: &Spanned<bool>) -> Option<Register> {
        let target = self.new_register();
        let node = IRInstruction::MovI {
            value: Value(i64::from(literal.inner)),
            target,
        };
        self.instructions.push(node);
        Some(target)
    }

    fn gen_expr_int_literal(&mut self, literal: &Spanned<i64>) -> Option<Register> {
        let target = self.new_register();
        let node = IRInstruction::MovI {
            value: Value(literal.inner),
            target,
        };
        self.instructions.push(node);
        Some(target)
    }

    fn gen_expr_ident(&mut self, ident: &str) -> Option<Register> {
        let s = ident.to_owned();
        let target = self.new_register();
        let node = IRInstruction::LoadI { name: s, target };
        self.instructions.push(node);
        Some(target)
    }

    fn gen_expr_infix(
        &mut self,
        left: &Spanned<TypedExpr>,
        right: &Spanned<TypedExpr>,
        op: &Spanned<InfixOp>,
    ) -> Option<Register> {
        let left_reg = self
            .gen_expressions(&left.inner)
            .unwrap_or(Register(usize::MAX));
        let right_reg = self
            .gen_expressions(&right.inner)
            .unwrap_or(Register(usize::MAX));
        let cmp_op = match op.inner {
            InfixOp::Eq => Some(CompareOp::Eq),
            InfixOp::NotEq => Some(CompareOp::NotEq),
            InfixOp::Lt => Some(CompareOp::Lt),
            InfixOp::Gt => Some(CompareOp::Gt),
            _ => None,
        };
        let bin_op = match op.inner {
            InfixOp::Plus => Some(BinaryOp::Add),
            InfixOp::Minus => Some(BinaryOp::Sub),
            InfixOp::Asterisk => Some(BinaryOp::Mul),
            InfixOp::Slash => Some(BinaryOp::Div),
            _ => None,
        };
        let target = self.new_register();
        let node = {
            if let Some(cmp) = cmp_op {
                Some(IRInstruction::CompareI {
                    left: left_reg,
                    operand: cmp,
                    right: right_reg,
                    target,
                })
            } else {
                bin_op.map(|bin| IRInstruction::ArithmeticBinaryI {
                    operand: bin,
                    left: left_reg,
                    right: right_reg,
                    target,
                })
            }
        };
        let n = node.unwrap();
        self.instructions.push(n);
        Some(target)
    }

    fn gen_expr_assign(
        &mut self,
        ident: &Spanned<String>,
        operand: &Spanned<InfixOp>,
        expr: &Spanned<TypedExpr>,
    ) -> Option<Register> {
        let expr_reg = self
            .gen_expressions(&expr.inner)
            .unwrap_or(Register(usize::MAX));

        match operand.inner {
            InfixOp::Assign => {
                let node = IRInstruction::LoadI {
                    name: ident.inner.to_string(),
                    target: expr_reg,
                };
                self.instructions.push(node);
                Some(expr_reg)
            }
            InfixOp::PlusEquals
            | InfixOp::MinusEquals
            | InfixOp::AsteriskEquals
            | InfixOp::SlashEuqals => {
                let op = match operand.inner {
                    InfixOp::PlusEquals => Some(BinaryOp::Add),
                    InfixOp::MinusEquals => Some(BinaryOp::Sub),
                    InfixOp::AsteriskEquals => Some(BinaryOp::Mul),
                    InfixOp::SlashEuqals => Some(BinaryOp::Div),
                    _ => None,
                };
                let ident_reg = self
                    .gen_expr_ident(&ident.inner)
                    .unwrap_or(Register(usize::MAX));

                let target = self.new_register();
                let node_arith = IRInstruction::ArithmeticBinaryI {
                    operand: op.unwrap(),
                    left: ident_reg,
                    right: expr_reg,
                    target,
                };

                self.instructions.push(node_arith);

                let node = IRInstruction::LoadI {
                    name: ident.inner.to_string(),
                    target,
                };
                self.instructions.push(node);
                Some(target)
            }
            _ => None,
        }
    }
}
