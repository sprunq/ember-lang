use crate::syntax::ast::Spanned;

#[derive(Debug, Clone)]
pub enum TypeCheckErr {
    FunctionDuplicate {
        trying_to_init_ident: Spanned<String>,
        existing_fun_ident: Spanned<String>,
    },
    NoMainFunctionFound,
}
