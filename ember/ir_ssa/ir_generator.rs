#![allow(unused_variables)]
#![allow(dead_code)]
use super::{
    instruction::{SSAInstruction, SSALabel, SSARegister, SSAValue},
    operands::{BinaryOp, CompareOp},
};
use crate::syntax::{
    ast::{Expr, Spanned, Stmt},
    operands::{InfixOp, PrefixOp},
};

pub struct IRGeneratorSSA {
    register_count: usize,
    label_count: usize,
    instructions: Vec<SSAInstruction>,
    loop_merge_label_stack: Vec<SSALabel>,
}
impl Default for IRGeneratorSSA {
    fn default() -> Self {
        Self::new()
    }
}

impl IRGeneratorSSA {
    pub fn new() -> Self {
        Self {
            register_count: 0,
            label_count: 0,
            instructions: Vec::new(),
            loop_merge_label_stack: Vec::new(),
        }
    }

    pub fn gen_code(&mut self, ast: &Vec<Stmt>) -> &Vec<SSAInstruction> {
        for stmt in ast {
            self.gen_statements(stmt);
        }
        &self.instructions
    }

    fn new_register(&mut self) -> SSARegister {
        self.register_count += 1;
        SSARegister(self.register_count)
    }

    fn new_label(&mut self) -> SSALabel {
        self.label_count += 1;
        SSALabel(self.label_count)
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
                let name_str = name.inner.clone();
                let params = parameters
                    .iter()
                    .map(|f| (f.0.inner.clone(), f.1.inner))
                    .collect();

                let old_code = self.instructions.to_owned();
                self.instructions = vec![];

                self.gen_statements(body);

                let node = SSAInstruction::FunctionDefinition {
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
                    SSAInstruction::Return { register: reg }
                } else {
                    SSAInstruction::Return { register: None }
                };
                self.instructions.push(node);
            }
            Stmt::Break => {
                if let Some(label) = self.loop_merge_label_stack.last() {
                    self.instructions
                        .push(SSAInstruction::Branch { label: *label });
                }
            }
        }
    }

    fn gen_expressions(&mut self, expr: &Expr) -> Option<SSARegister> {
        match &expr {
            Expr::Binary { op, left, right } => self.gen_expr_infix(left, right, op),
            Expr::Unary { op, expr } => self.gen_expr_prefix(expr, op),
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
                        .unwrap_or(SSARegister(usize::MAX));
                    arg_regs.push(arg_reg);
                }
                let target = self.new_register();
                let name_str = name.inner.clone();
                let node = SSAInstruction::FunctionInvocation {
                    name: name_str,
                    registers: arg_regs,
                    target,
                };
                self.instructions.push(node);
                Some(target)
            }
        }
    }

    fn gen_stmt_while(&mut self, condition: &Spanned<Expr>, body: &Stmt) {
        let top_label = self.new_label();
        let start_label = self.new_label();
        let merge_label = self.new_label();
        self.loop_merge_label_stack.push(merge_label);

        self.instructions
            .push(SSAInstruction::Branch { label: top_label });

        self.instructions
            .push(SSAInstruction::Label { name: top_label });

        let cond_reg = self
            .gen_expressions(&condition.inner)
            .unwrap_or(SSARegister(usize::MAX));

        let cond_node = SSAInstruction::BranchCond {
            condition: cond_reg,
            on_true: start_label,
            on_false: merge_label,
        };
        self.instructions.push(cond_node);

        self.instructions
            .push(SSAInstruction::Label { name: start_label });
        self.gen_statements(body);
        self.instructions
            .push(SSAInstruction::Branch { label: top_label });

        self.instructions
            .push(SSAInstruction::Label { name: merge_label });

        self.loop_merge_label_stack.pop();
    }

    fn gen_stmt_if(
        &mut self,
        condition: &Spanned<Expr>,
        body: &Stmt,
        alternative: &Option<Box<Stmt>>,
    ) {
        let cond_register = self
            .gen_expressions(&condition.inner)
            .unwrap_or(SSARegister(usize::MAX));
        let t_label = self.new_label();
        let f_label = self.new_label();
        let merge_label = self.new_label();
        let cond_node = SSAInstruction::BranchCond {
            condition: cond_register,
            on_true: t_label,
            on_false: f_label,
        };
        self.instructions.push(cond_node);
        // true block
        let t_label_node = SSAInstruction::Label { name: t_label };
        self.instructions.push(t_label_node);
        self.gen_statements(body);
        let jmp_to_merge_node = SSAInstruction::Branch { label: merge_label };
        self.instructions.push(jmp_to_merge_node);
        // false block
        let f_label_node = SSAInstruction::Label { name: f_label };
        self.instructions.push(f_label_node);
        if let Some(alt) = alternative {
            self.gen_statements(alt);
        }
        let jmp_to_merge_node = SSAInstruction::Branch { label: merge_label };
        self.instructions.push(jmp_to_merge_node);
        let merge_node = SSAInstruction::Label { name: merge_label };
        self.instructions.push(merge_node);
    }

    fn gen_stmt_expr(&mut self, expr: &Spanned<Expr>) {
        self.gen_expressions(&expr.inner);
    }

    fn gen_expr_prefix(
        &mut self,
        expr: &Spanned<Expr>,
        op: &Spanned<PrefixOp>,
    ) -> Option<SSARegister> {
        let expr_reg = self
            .gen_expressions(&expr.inner)
            .unwrap_or(SSARegister(usize::MAX));
        let target = self.new_register();
        let prefix_node = match op.inner {
            PrefixOp::Minus => {
                // do value * -1;
                let int_literal_target = self.new_register();
                let int_literal = SSAInstruction::MovI {
                    value: SSAValue(-1),
                    target: int_literal_target,
                };
                self.instructions.push(int_literal);

                SSAInstruction::ArithmeticBinaryI {
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

    fn gen_stmt_declaration(&mut self, value: &Spanned<Expr>, ident: &Spanned<String>) {
        let node_decl = SSAInstruction::Allocation {
            name: ident.inner.clone(),
        };
        self.instructions.push(node_decl);
        let a = self
            .gen_expressions(&value.inner)
            .unwrap_or(SSARegister(usize::MAX));
        self.instructions.push(SSAInstruction::StoreI {
            target: a,
            name: ident.inner.clone(),
        });
    }

    fn gen_expr_bool_literal(&mut self, literal: &Spanned<bool>) -> Option<SSARegister> {
        let target = self.new_register();
        let node = SSAInstruction::MovI {
            value: SSAValue(i64::from(literal.inner)),
            target,
        };
        self.instructions.push(node);
        Some(target)
    }

    fn gen_expr_int_literal(&mut self, literal: &Spanned<i64>) -> Option<SSARegister> {
        let target = self.new_register();
        let node = SSAInstruction::MovI {
            value: SSAValue(literal.inner),
            target,
        };
        self.instructions.push(node);
        Some(target)
    }

    fn gen_expr_ident(&mut self, ident: &str) -> Option<SSARegister> {
        let s = ident.to_owned();
        let target = self.new_register();
        let node = SSAInstruction::LoadI { name: s, target };
        self.instructions.push(node);
        Some(target)
    }

    fn gen_expr_infix(
        &mut self,
        left: &Spanned<Expr>,
        right: &Spanned<Expr>,
        op: &Spanned<InfixOp>,
    ) -> Option<SSARegister> {
        let left_reg = self
            .gen_expressions(&left.inner)
            .unwrap_or(SSARegister(usize::MAX));
        let right_reg = self
            .gen_expressions(&right.inner)
            .unwrap_or(SSARegister(usize::MAX));
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
                Some(SSAInstruction::CompareI {
                    left: left_reg,
                    operand: cmp,
                    right: right_reg,
                    target,
                })
            } else {
                bin_op.map(|bin| SSAInstruction::ArithmeticBinaryI {
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
        expr: &Spanned<Expr>,
    ) -> Option<SSARegister> {
        let expr_reg = self
            .gen_expressions(&expr.inner)
            .unwrap_or(SSARegister(usize::MAX));

        match operand.inner {
            InfixOp::Assign => {
                let node = SSAInstruction::LoadI {
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
                    .unwrap_or(SSARegister(usize::MAX));

                let target = self.new_register();
                let node_arith = SSAInstruction::ArithmeticBinaryI {
                    operand: op.unwrap(),
                    left: ident_reg,
                    right: expr_reg,
                    target,
                };

                self.instructions.push(node_arith);

                let node = SSAInstruction::StoreI {
                    target,
                    name: ident.inner.to_string(),
                };
                self.instructions.push(node);
                Some(target)
            }
            _ => None,
        }
    }
}
