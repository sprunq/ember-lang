use crate::ast::{
    ast_root::AstRoot, expression::Expr, infix::InfixOp, statement::Stmt,
    typed_expression::TypedExpr,
};

use super::{
    instruction::{IRInstruction, Label, Register, Value},
    operands::{BinaryOp, CompareOp},
};

pub struct IRGenerator {
    register_count: usize,
    label_count: usize,
    instructions: Vec<IRInstruction>,
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
                let a = self
                    .gen_expressions(&value.inner)
                    .unwrap_or(Register(usize::MAX));
                self.instructions.push(IRInstruction::StoreI {
                    target: a,
                    name: ident.inner.expr.to_string(),
                });
            }
            Stmt::Expression { expr } => todo!(),
            Stmt::While { condition, body } => todo!(),
            Stmt::If {
                condition,
                body,
                alternative,
            } => todo!(),
            Stmt::Sequence { statements } => {
                for stmt in statements.iter() {
                    self.gen_statements(stmt);
                }
            }
        }
    }

    fn gen_expressions(&mut self, expr: &TypedExpr) -> Option<Register> {
        match &expr.expr {
            Expr::Infix { op, left, right } => {
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
                    InfixOp::Assign => Some(BinaryOp::Assign),
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
                    } else if let Some(bin) = bin_op {
                        Some(IRInstruction::ArithmeticBinaryI {
                            operand: bin,
                            left: left_reg,
                            right: right_reg,
                            target,
                        })
                    } else {
                        None
                    }
                };
                let n = node.unwrap();
                self.instructions.push(n);
                Some(target)
            }
            Expr::Prefix { op, expr } => todo!(),
            Expr::Identifier(_) => todo!(),
            Expr::IntegerLiteral(literal) => {
                let target = self.new_register();
                let node = IRInstruction::MovI {
                    value: Value(literal.inner),
                    target,
                };
                self.instructions.push(node);
                Some(target)
            }
            Expr::BooleanLiteral(literal) => {
                let target = self.new_register();
                let node = IRInstruction::MovI {
                    value: Value(if literal.inner { 1 } else { 0 }),
                    target,
                };
                self.instructions.push(node);
                Some(target)
            }
            Expr::Assign {
                ident,
                operand,
                expr,
            } => todo!(),
        }
    }
}
