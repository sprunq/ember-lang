use std::ops::Range;

use crate::syntax::{
    ast::{Expr, Spanned},
    operands::InfixOp,
    ty::Type,
};

#[derive(Debug, Clone)]
pub enum TypeCheckErr {
    FunctionDuplicate {
        trying_to_init_ident: Spanned<String>,
        existing_fun_ident: Spanned<String>,
    },
    NoMainFunctionFound,
    IncompatibleOperandTypes {
        op: Spanned<InfixOp>,
        left: Type,
        right: Type,
    },
    AssignmentMismatch {
        ident: Spanned<String>,
        expected: Type,
        actual: Type,
    },
    StatementShouldNotReturnAnything {
        expr: Spanned<Expr>,
        ty: Type,
    },
    DeclarationTypesNotMatching {
        ident: Spanned<String>,
        declared_ty: Option<Type>,
        value_ty: Option<Type>,
    },
    VariableDuplicate {
        ident: Spanned<String>,
        previous_type: Type,
        value_ty: Type,
    },
    TypeMismatch {
        expected: Option<Spanned<Type>>,
        actual: Option<Spanned<Type>>,
        positon: Range<usize>,
    },
}
