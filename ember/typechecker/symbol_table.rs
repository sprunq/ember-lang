use std::{collections::HashMap, ops::Range};

use crate::parser::ty::Type;

pub struct SymbolTable {
    variable_scopes: Vec<HashMap<String, VariableInformation>>,
}
impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            variable_scopes: Vec::new(),
        }
    }

    pub fn push_scope(&mut self) {
        self.variable_scopes.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.variable_scopes.pop();
    }

    pub fn insert_var_in_scope(&mut self, var: VariableInformation) -> Option<VariableInformation> {
        if self.variable_scopes.is_empty() {
            self.variable_scopes.push(HashMap::new());
        }
        self.variable_scopes
            .last_mut()
            .unwrap()
            .insert(var.ident.clone(), var)
    }

    pub fn get_variable_from_scope(&self, ident: &String) -> Option<&VariableInformation> {
        for scope in self.variable_scopes.iter().rev() {
            let var = scope.get(ident);
            if var.is_some() {
                return var;
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
pub struct VariableInformation {
    pub ident: String,
    pub position: Range<usize>,
    pub ty: Type,
}

impl VariableInformation {
    pub fn new(ident: String, ty: Type, position: Range<usize>) -> Self {
        Self {
            ident,
            position,
            ty,
        }
    }
}
