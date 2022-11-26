use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal {
    Integer(i64),
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Literal::Integer(value) => write!(f, "{}", value),
        }
    }
}
