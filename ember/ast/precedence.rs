#[derive(Debug, PartialEq, Eq, PartialOrd)]
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
