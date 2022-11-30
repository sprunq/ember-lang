use std::fmt;

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
    MovI {
        value: Value,
        target: Register,
    },
    LoadI {
        name: Value,
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
        condition: Value,
        on_true: Label,
        on_false: Label,
    },
}

impl fmt::Display for IRInstruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IRInstruction::NOP => write!(f, "NOP"),
            IRInstruction::MovI { value, target } => {
                write!(f, "MOVI {value}, {target}")
            }
            IRInstruction::LoadI { target, name } => write!(f, "LOAD {name}, {target}"),
            IRInstruction::StoreI { target, name } => write!(f, "STORE {target}, {name}"),
            IRInstruction::ArithmeticBinaryI {
                left,
                operand,
                right,
                target,
            } => {
                write!(f, "{operand}I {left}, {right}, {target}")
            }
            IRInstruction::Label { name } => write!(f, "{name}:"),
            IRInstruction::Branch { label: target } => write!(f, "BRANCH {target}"),
            IRInstruction::BranchCond {
                condition: cond,
                on_true,
                on_false,
            } => write!(f, "CBRANCH {cond}, {on_true}, {on_false}"),
            IRInstruction::CompareI {
                left,
                operand,
                right,
                target,
            } => {
                write!(f, "CMPI {operand}, {left}, {right}, {target}")
            }
        }
    }
}
