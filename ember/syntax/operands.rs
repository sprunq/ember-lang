use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InfixOp {
    Eq,
    NotEq,
    Lt,
    Gt,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Assign,
    PlusEquals,
    MinusEquals,
    SlashEuqals,
    AsteriskEquals,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrefixOp {
    Minus,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Assign,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

impl fmt::Display for InfixOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InfixOp::Eq => write!(f, "=="),
            InfixOp::NotEq => write!(f, "!="),
            InfixOp::Lt => write!(f, "<"),
            InfixOp::Gt => write!(f, ">"),
            InfixOp::Plus => write!(f, "+"),
            InfixOp::Minus => write!(f, "-"),
            InfixOp::Asterisk => write!(f, "*"),
            InfixOp::Slash => write!(f, "/"),
            InfixOp::Assign => write!(f, "="),
            InfixOp::PlusEquals => write!(f, "+="),
            InfixOp::MinusEquals => write!(f, "-="),
            InfixOp::SlashEuqals => write!(f, "/="),
            InfixOp::AsteriskEquals => write!(f, "*="),
        }
    }
}

impl fmt::Display for PrefixOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PrefixOp::Minus => write!(f, "-"),
        }
    }
}