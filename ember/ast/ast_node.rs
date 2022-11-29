use std::{fmt, ops::Range};

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct AstNode<T: std::fmt::Display> {
    pub pos: Range<usize>,
    pub inner: T,
}
impl<T: std::fmt::Display> AstNode<T> {
    pub fn new(expr: T, pos: Range<usize>) -> Self {
        Self { pos, inner: expr }
    }

    pub fn new_boxed(expr: T, pos: Range<usize>) -> Box<Self> {
        Box::new(AstNode::new(expr, pos))
    }
}

impl<T: std::fmt::Display> fmt::Display for AstNode<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.inner)
    }
}
