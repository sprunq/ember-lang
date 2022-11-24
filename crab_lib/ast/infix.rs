use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Infix {
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

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Infix::Eq => write!(f, "=="),
            Infix::NotEq => write!(f, "!="),
            Infix::Lt => write!(f, "<"),
            Infix::Gt => write!(f, ">"),
            Infix::Plus => write!(f, "+"),
            Infix::Minus => write!(f, "-"),
            Infix::Asterisk => write!(f, "*"),
            Infix::Slash => write!(f, "/"),
            Infix::Assign => write!(f, "="),
            Infix::PlusEquals => write!(f, "+="),
            Infix::MinusEquals => write!(f, "-="),
            Infix::SlashEuqals => write!(f, "/="),
            Infix::AsteriskEquals => write!(f, "*="),
        }
    }
}
