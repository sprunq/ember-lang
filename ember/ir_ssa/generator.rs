#![allow(unused_variables)]
#![allow(dead_code)]
use super::{
    syntax::{SSABinaryOp, SSACompareOp},
    syntax::{SSAInstruction, SSALabel, SSARegister, SSAValue},
};
use crate::{
    parser::{
        syntax::{Expr, Spanned, Stmt},
        syntax::{InfixOp, PrefixOp},
    },
    typechecker::symbol_table::SymbolTable,
};

pub struct SSAGenerator {
    register_count: usize,
    label_count: usize,
    instructions: Vec<SSAInstruction>,
    loop_merge_label_stack: Vec<SSALabel>,
    symbol_table: SymbolTable,
}
impl Default for SSAGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl SSAGenerator {
    pub fn new() -> Self {
        let mut sel = Self {
            register_count: 0,
            label_count: 0,
            instructions: Vec::new(),
            loop_merge_label_stack: Vec::new(),
            symbol_table: SymbolTable::new(),
        };
        sel.symbol_table.push_scope();
        sel
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
                self.gen_stmt_function_definition(name, parameters, body);
            }
            Stmt::Return { value } => {
                self.gen_stmt_return(value);
            }
            Stmt::Break => {
                self.gen_stmt_break();
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
                self.gen_expr_function_invocation(args, name)
            }
        }
    }

    fn gen_expr_function_invocation(
        &mut self,
        args: &Vec<Spanned<Expr>>,
        name: &Spanned<String>,
    ) -> Option<SSARegister> {
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

    fn gen_stmt_break(&mut self) {
        if let Some(label) = self.loop_merge_label_stack.last() {
            self.instructions
                .push(SSAInstruction::Branch { label: *label });
        }
    }

    fn gen_stmt_return(&mut self, value: &Option<Spanned<Expr>>) {
        let node = if let Some(val) = value {
            let reg = self.gen_expressions(&val.inner);
            SSAInstruction::Return { register: reg }
        } else {
            SSAInstruction::Return { register: None }
        };
        self.instructions.push(node);
    }

    fn gen_stmt_function_definition(
        &mut self,
        name: &Spanned<String>,
        parameters: &[(Spanned<String>, Spanned<crate::parser::ty::Type>)],
        body: &Stmt,
    ) {
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
                let int_literal = SSAInstruction::Mov {
                    value: SSAValue(-1),
                    target: int_literal_target,
                };
                self.instructions.push(int_literal);

                SSAInstruction::ArithmeticBinary {
                    operand: SSABinaryOp::Mul,
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
        self.instructions.push(SSAInstruction::Store {
            source: a,
            target: ident.inner.clone(),
        });
    }

    fn gen_expr_bool_literal(&mut self, literal: &Spanned<bool>) -> Option<SSARegister> {
        let target = self.new_register();
        let node = SSAInstruction::Mov {
            value: SSAValue(i64::from(literal.inner)),
            target,
        };
        self.instructions.push(node);
        Some(target)
    }

    fn gen_expr_int_literal(&mut self, literal: &Spanned<i64>) -> Option<SSARegister> {
        let target = self.new_register();
        let node = SSAInstruction::Mov {
            value: SSAValue(literal.inner),
            target,
        };
        self.instructions.push(node);
        Some(target)
    }

    fn gen_expr_ident(&mut self, ident: &str) -> Option<SSARegister> {
        let s = ident.to_owned();
        let target = self.new_register();
        let node = SSAInstruction::Load { source: s, target };
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
            InfixOp::Eq => Some(SSACompareOp::Eq),
            InfixOp::NotEq => Some(SSACompareOp::NotEq),
            InfixOp::Lt => Some(SSACompareOp::Lt),
            InfixOp::Gt => Some(SSACompareOp::Gt),
            _ => None,
        };
        let bin_op = match op.inner {
            InfixOp::Plus => Some(SSABinaryOp::Add),
            InfixOp::Minus => Some(SSABinaryOp::Sub),
            InfixOp::Asterisk => Some(SSABinaryOp::Mul),
            InfixOp::Slash => Some(SSABinaryOp::Div),
            _ => None,
        };
        let target = self.new_register();
        let node = {
            if let Some(cmp) = cmp_op {
                Some(SSAInstruction::Compare {
                    left: left_reg,
                    operand: cmp,
                    right: right_reg,
                    target,
                })
            } else {
                bin_op.map(|bin| SSAInstruction::ArithmeticBinary {
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
                let node = SSAInstruction::Store {
                    source: expr_reg,
                    target: ident.inner.to_string(),
                };
                self.instructions.push(node);
                Some(expr_reg)
            }
            InfixOp::PlusEquals
            | InfixOp::MinusEquals
            | InfixOp::AsteriskEquals
            | InfixOp::SlashEuqals => {
                let op = match operand.inner {
                    InfixOp::PlusEquals => Some(SSABinaryOp::Add),
                    InfixOp::MinusEquals => Some(SSABinaryOp::Sub),
                    InfixOp::AsteriskEquals => Some(SSABinaryOp::Mul),
                    InfixOp::SlashEuqals => Some(SSABinaryOp::Div),
                    _ => None,
                };
                let ident_reg = self
                    .gen_expr_ident(&ident.inner)
                    .unwrap_or(SSARegister(usize::MAX));

                let tmp_reg = self.new_register();
                let node_arith = SSAInstruction::ArithmeticBinary {
                    operand: op.unwrap(),
                    left: ident_reg,
                    right: expr_reg,
                    target: tmp_reg,
                };

                self.instructions.push(node_arith);

                let node = SSAInstruction::Store {
                    target: ident.inner.to_string(),
                    source: tmp_reg,
                };
                self.instructions.push(node);
                Some(tmp_reg)
            }
            _ => None,
        }
    }
}
