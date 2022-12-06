use super::typechecker_error::TypeCheckErr;
use crate::syntax::{
    ast::{Spanned, Stmt},
    ty::Type,
};
use std::collections::HashMap;

#[allow(dead_code)]
pub struct FunctionSignature<'a> {
    name: &'a Spanned<String>,
    parameters: &'a Vec<(Spanned<String>, Spanned<Type>)>,
    return_type: &'a Option<Spanned<Type>>,
}

pub struct TypeChecker<'a> {
    pub function_signatures: HashMap<String, FunctionSignature<'a>>,
}

impl<'a> TypeChecker<'a> {
    pub fn check(functions: &'a Vec<Stmt>) -> Vec<TypeCheckErr> {
        let mut errs = Vec::new();
        let fs = Self::get_function_signatures(functions).unwrap_or_default();
        let sel = Self {
            function_signatures: fs,
        };

        if let Err(new_errs) = sel.typecheck() {
            errs.extend(new_errs);
        }

        errs
    }

    fn typecheck(&self) -> Result<(), Vec<TypeCheckErr>> {
        todo!()
    }

    fn get_function_signatures(
        functions: &'a Vec<Stmt>,
    ) -> Result<HashMap<String, FunctionSignature<'a>>, Vec<TypeCheckErr>> {
        let mut fun_sigs = HashMap::<String, FunctionSignature>::new();
        let mut errs = Vec::new();
        for fun in functions {
            if let Stmt::FunctionDefinition {
                name,
                parameters,
                return_type,
                body: _,
            } = fun
            {
                let fs = FunctionSignature {
                    name,
                    parameters,
                    return_type,
                };
                if let Some(other_sig) = fun_sigs.insert(name.inner.clone(), fs) {
                    errs.push(TypeCheckErr::FunctionDuplicate {
                        trying_to_init_ident: name.clone(),
                        existing_fun_ident: other_sig.name.clone(),
                    });
                }
            }
        }

        if !fun_sigs.contains_key("main") {
            errs.push(TypeCheckErr::NoMainFunctionFound)
        }

        if !errs.is_empty() {
            Err(errs)
        } else {
            Ok(fun_sigs)
        }
    }
}
