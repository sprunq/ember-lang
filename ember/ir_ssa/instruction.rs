use std::fmt;

use crate::syntax::ty::Type;

use super::operands::{BinaryOp, CompareOp};

#[derive(Debug, Copy, Clone)]
pub struct SSAValue(pub i64);

impl fmt::Display for SSAValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct SSALabel(pub usize);

impl fmt::Display for SSALabel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "L{}", self.0)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct SSARegister(pub usize);

impl fmt::Display for SSARegister {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "R{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub enum SSAInstruction {
    NOP,
    Allocation {
        name: String,
    },
    MovI {
        value: SSAValue,
        target: SSARegister,
    },
    LoadI {
        name: String,
        target: SSARegister,
    },
    StoreI {
        target: SSARegister,
        name: String,
    },
    CompareI {
        operand: CompareOp,
        left: SSARegister,
        right: SSARegister,
        target: SSARegister,
    },
    ArithmeticBinaryI {
        operand: BinaryOp,
        left: SSARegister,
        right: SSARegister,
        target: SSARegister,
    },
    Label {
        name: SSALabel,
    },
    Branch {
        label: SSALabel,
    },
    BranchCond {
        condition: SSARegister,
        on_true: SSALabel,
        on_false: SSALabel,
    },
    FunctionDefinition {
        name: String,
        parameters: Vec<(String, Type)>,
        body: Vec<SSAInstruction>,
    },
    FunctionInvocation {
        name: String,
        registers: Vec<SSARegister>,
        target: SSARegister,
    },
    Return {
        register: Option<SSARegister>,
    },
}

impl fmt::Display for SSAInstruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SSAInstruction::NOP => write!(f, "\tNOP"),
            SSAInstruction::MovI { value, target } => {
                write!(f, "\tMOVEI {value}, {target}")
            }
            SSAInstruction::LoadI { target, name } => write!(f, "\tLOAD  {name}, {target}"),
            SSAInstruction::StoreI { target, name } => write!(f, "\tSTORE {target}, {name}"),
            SSAInstruction::ArithmeticBinaryI {
                left,
                operand,
                right,
                target,
            } => {
                write!(f, "\t{operand}I  {left}, {right}, {target}")
            }
            SSAInstruction::Label { name } => write!(f, "\n{name}:"),
            SSAInstruction::Branch { label: target } => write!(f, "\tJMP   {target}"),
            SSAInstruction::BranchCond {
                condition: cond,
                on_true,
                on_false,
            } => write!(f, "\tJMPIF {cond}, {on_true}, {on_false}"),
            SSAInstruction::CompareI {
                left,
                operand,
                right,
                target,
            } => {
                write!(f, "\tCOMPI {operand}, {left}, {right}, {target}")
            }
            SSAInstruction::Allocation { name } => {
                write!(f, "\tALLOC {name}")
            }
            SSAInstruction::FunctionDefinition {
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
            SSAInstruction::Return { register } => {
                if let Some(reg) = register {
                    write!(f, "\tRET   {}", reg)
                } else {
                    write!(f, "\tRET")
                }
            }
            SSAInstruction::FunctionInvocation {
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
