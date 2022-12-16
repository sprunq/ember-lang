use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum SSAUnaryOp {
    Neg,
    Not,
}
impl fmt::Display for SSAUnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SSAUnaryOp::Neg => write!(f, "!"),
            SSAUnaryOp::Not => write!(f, "-"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum SSABinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}
impl fmt::Display for SSABinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SSABinaryOp::Add => write!(f, "ADD"),
            SSABinaryOp::Sub => write!(f, "SUB"),
            SSABinaryOp::Mul => write!(f, "MUL"),
            SSABinaryOp::Div => write!(f, "DIV"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum SSACompareOp {
    Eq,
    NotEq,
    Lt,
    Gt,
}
impl fmt::Display for SSACompareOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SSACompareOp::Eq => write!(f, "="),
            SSACompareOp::NotEq => write!(f, "!="),
            SSACompareOp::Lt => write!(f, "<"),
            SSACompareOp::Gt => write!(f, ">"),
        }
    }
}
