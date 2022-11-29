use std::ops::Range;

use crate::ast::{infix::InfixOp, ty::Type};

#[derive(Debug, Clone)]
pub enum TypeCheckError {
    Boolean {
        value: Type,
        pos: Range<usize>,
    },
    Integer {
        val: Type,
        pos: Range<usize>,
    },
    IdentifierTypeNotMatching {
        ident: String,
        ident_type: Type,
        expr_type: Type,
        pos: Range<usize>,
    },
    IdentifierNotFound {
        ident: String,
        pos: Range<usize>,
    },
    IncompatibleTypesForOperand {
        op: InfixOp,
        l: Type,
        r: Type,
        pos: Range<usize>,
    },
    InfixTypesNotMatching {
        l: Type,
        r: Type,
        pos: Range<usize>,
    },
    DeclarationTypesNotMatching {
        l: Type,
        r: Type,
        ident: String,
        pos: Range<usize>,
    },
    VariableAlreadyExists {
        ident: String,
        initialized_with_type: Type,
        tried_to_init_with: Type,
        position_ident: Range<usize>,
        position_init_ident: Range<usize>,
    },
    NotMatchingExpetectedType {
        expected: Type,
        actual: Type,
        pos: Range<usize>,
    },
}
