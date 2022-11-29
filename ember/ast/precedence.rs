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
