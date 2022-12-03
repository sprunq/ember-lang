use std::fmt;

use crate::syntax::ty::Type;

use super::operands::{BinaryOp, CompareOp};

#[derive(Debug, Copy, Clone)]
pub struct Value(pub i64);

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Label(pub usize);

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "L{}", self.0)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Register(pub usize);

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "R{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub enum IRInstruction {
    NOP,
    Allocation {
        name: String,
    },
    MovI {
        value: Value,
        target: Register,
    },
    LoadI {
        name: String,
        target: Register,
    },
    StoreI {
        target: Register,
        name: String,
    },
    CompareI {
        operand: CompareOp,
        left: Register,
        right: Register,
        target: Register,
    },
    ArithmeticBinaryI {
        operand: BinaryOp,
        left: Register,
        right: Register,
        target: Register,
    },
    Label {
        name: Label,
    },
    Branch {
        label: Label,
    },
    BranchCond {
        condition: Register,
        on_true: Label,
        on_false: Label,
    },
    FunctionDefinition {
        name: String,
        parameters: Vec<(String, Type)>,
        body: Vec<IRInstruction>,
    },
    FunctionInvocation {
        name: String,
        registers: Vec<Register>,
        target: Register,
    },
    Return {
        register: Option<Register>,
    },
}

impl fmt::Display for IRInstruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IRInstruction::NOP => write!(f, "\tNOP"),
            IRInstruction::MovI { value, target } => {
                write!(f, "\tMOVEI {value}, {target}")
            }
            IRInstruction::LoadI { target, name } => write!(f, "\tLOAD  {name}, {target}"),
            IRInstruction::StoreI { target, name } => write!(f, "\tSTORE {target}, {name}"),
            IRInstruction::ArithmeticBinaryI {
                left,
                operand,
                right,
                target,
            } => {
                write!(f, "\t{operand}I  {left}, {right}, {target}")
            }
            IRInstruction::Label { name } => write!(f, "\n{name}:"),
            IRInstruction::Branch { label: target } => write!(f, "\tJUMP  {target}"),
            IRInstruction::BranchCond {
                condition: cond,
                on_true,
                on_false,
            } => write!(f, "\tCJUMP {cond}, {on_true}, {on_false}"),
            IRInstruction::CompareI {
                left,
                operand,
                right,
                target,
            } => {
                write!(f, "\tCOMPI {operand}, {left}, {right}, {target}")
            }
            IRInstruction::Allocation { name } => {
                write!(f, "\tALLOC {name}")
            }
            IRInstruction::FunctionDefinition {
                name,
                parameters,
                body,
            } => {
                let param_str = parameters
                    .iter()
                    .map(|(n, _)| n.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                let body_str = body
                    .iter()
                    .map(|s| format!("{s}"))
                    .collect::<Vec<String>>()
                    .join("\n");
                write!(f, "{name}({param_str})\n{body_str}\n",)
            }
            IRInstruction::Return { register } => {
                if let Some(reg) = register {
                    write!(f, "\tRET   {}", reg)
                } else {
                    write!(f, "\tRET")
                }
            }
            IRInstruction::FunctionInvocation {
                name,
                registers,
                target,
            } => {
                let param_str = registers
                    .iter()
                    .map(|r| format!("{}", r))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "\tCALL  {name}, ({param_str}), {target}")
            }
        }
    }
}
