use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone)]
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
