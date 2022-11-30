use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrefixOp {
    Minus,
}

impl fmt::Display for PrefixOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PrefixOp::Minus => write!(f, "-"),
        }
    }
}
