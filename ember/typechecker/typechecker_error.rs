use std::ops::Range;

use crate::parser::{
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
        already_declared_ident: Spanned<String>,
    },
    TypeMismatch {
        expected: Option<Type>,
        actual: Option<Type>,
        positon: Range<usize>,
    },
    IdentifierNotFound {
        identifier: String,
        positon: Range<usize>,
    },
    FunctionInvocationNotMatchingDeclaration {
        identifier: String,
        delcaration_types: Vec<Option<Type>>,
        invocation_types: Vec<Option<Type>>,
        decl_pos: Range<usize>,
        invo_pos: Range<usize>,
    },
}
