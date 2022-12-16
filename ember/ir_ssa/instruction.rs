use super::operands::{SSABinaryOp, SSACompareOp};
use crate::parser::ty::Type;
use std::fmt;

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
    // Loads a constant into a register
    Mov {
        value: SSAValue,
        target: SSARegister,
    },
    // Load variable into register
    Load {
        source: String,
        target: SSARegister,
    },
    // Store register value in variable
    Store {
        target: String,
        source: SSARegister,
    },
    // Allocate a new variable
    Allocation {
        name: String,
    },
    Compare {
        operand: SSACompareOp,
        left: SSARegister,
        right: SSARegister,
        target: SSARegister,
    },
    ArithmeticBinary {
        operand: SSABinaryOp,
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
            SSAInstruction::NOP => write!(f, "\tnop"),
            SSAInstruction::Mov { value, target } => {
                write!(f, "\t{target} ← move {value}")
            }
            SSAInstruction::Load {
                target,
                source: name,
            } => write!(f, "\t{target} ← load {name}"),
            SSAInstruction::Store {
                target,
                source: name,
            } => write!(f, "\t{target} ← store {name}"),
            SSAInstruction::ArithmeticBinary {
                left,
                operand,
                right,
                target,
            } => {
                write!(f, "\t{target} ← {left} {operand} {right}")
            }
            SSAInstruction::Label { name } => write!(f, "\n{name}:"),
            SSAInstruction::Branch { label: target } => write!(f, "\tjump {target}"),
            SSAInstruction::BranchCond {
                condition: cond,
                on_true,
                on_false,
            } => write!(f, "\tjump_if {cond}, {on_true}, {on_false}"),
            SSAInstruction::Compare {
                left,
                operand,
                right,
                target,
            } => {
                write!(f, "\t{target} ← cmp {left} {operand} {right}, ")
            }
            SSAInstruction::Allocation { name } => {
                write!(f, "\talloc {name}")
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
                write!(f, "function {name}({param_str}):\n{body_str}\n",)
            }
            SSAInstruction::Return { register } => {
                if let Some(reg) = register {
                    write!(f, "\tret {}", reg)
                } else {
                    write!(f, "\tret")
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
                write!(f, "\t{target} ← call {name}({param_str})")
            }
        }
    }
}
