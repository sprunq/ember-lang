use std::fmt;

use super::syntax::{
    SSABinaryOp, SSACompareOp, SSAInstruction, SSALabel, SSAType, SSAUnaryOp, SSAValue,
};

impl fmt::Display for SSAInstruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SSAInstruction::NOP => write!(f, "\tnop"),
            SSAInstruction::MovI { value, target } => {
                write!(f, "\t{target} ← movei {value}")
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
            SSAInstruction::Allocation { name, ty } => {
                write!(f, "\tstackalloc [{ty}] {name}")
            }
            SSAInstruction::FunctionDefinition {
                name,
                parameters,
                body,
                return_type,
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
                write!(
                    f,
                    "function [{return_type}] {name}({param_str}):\n{body_str}\n",
                )
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
            SSAInstruction::Phi { target, values } => {
                let phi_vals = values
                    .iter()
                    .map(|r| format!("{}", r))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "\t{target} ← Φ({phi_vals})")
            }
        }
    }
}

impl fmt::Display for SSABinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SSABinaryOp::Add => write!(f, "+"),
            SSABinaryOp::Sub => write!(f, "-"),
            SSABinaryOp::Mul => write!(f, "*"),
            SSABinaryOp::Div => write!(f, "/"),
        }
    }
}

impl fmt::Display for SSACompareOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SSACompareOp::Eq => write!(f, "=="),
            SSACompareOp::NotEq => write!(f, "!="),
            SSACompareOp::Lt => write!(f, "<"),
            SSACompareOp::Gt => write!(f, ">"),
        }
    }
}

impl fmt::Display for SSAUnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SSAUnaryOp::Neg => write!(f, "!"),
            SSAUnaryOp::Not => write!(f, "-"),
        }
    }
}

impl fmt::Display for SSAValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

impl fmt::Display for SSALabel {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "block_{}", self.0)
    }
}

impl fmt::Display for SSAType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SSAType::I64 => write!(f, "i64"),
            SSAType::Void => write!(f, "void"),
        }
    }
}
