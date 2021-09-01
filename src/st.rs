use once_cell::sync::Lazy;

use std::collections::HashMap;
use std::iter::FromIterator;
use std::sync::Mutex;

use super::{LogMesg, VarType};

type SymbolTable = HashMap<String, SymbolInfo>;

pub static ST: Lazy<Mutex<SymbolTableStack>> = Lazy::new(|| Mutex::new(SymbolTableStack::new()));

#[derive(Debug)]
pub struct SymbolTableStack {
    stack: Vec<SymbolTable>,
    curr_fn: Option<SymbolInfo>,
}

/// Values of the symbol table
#[derive(Debug, Clone)]
pub enum SymbolInfo {
    Var(VarType),
    Function {
        ret_ty: Option<VarType>, // return type
        params: Vec<VarType>,    // name and type of parameters
    },
}

impl SymbolTableStack {
    pub fn new() -> SymbolTableStack {
        SymbolTableStack {
            stack: vec![SymbolTable::default()],
            curr_fn: None,
        }
    }

    pub fn push_table(&mut self) {
        self.stack.push(SymbolTable::default())
    }

    pub fn pop_table(&mut self) {
        self.stack.pop();
    }

    pub fn record_var(&mut self, name: &str, ty: &VarType) -> Result<(), LogMesg<&str>> {
        let table = self
            .stack
            .iter_mut()
            .last()
            .expect("Symbol table stack is empty");
        if let Some(SymbolInfo::Function { .. }) = table.get(name) {
            Err(LogMesg::err()
                .name("Invalid name")
                .cause("There is a function with the same name in the current scope")
                .help("Consider renaming the variable"))
        } else {
            table.insert(name.to_string(), SymbolInfo::Var(ty.clone()));
            Ok(())
        }
    }

    /// Records a function in the `SymbolTable` at the top of the `SymbolTableStack`.
    ///
    /// # Error
    /// If a function with the same name exists in the top `SymbolTable`, this function
    /// returns a `LogMesg` with a "Invalid name" error.
    pub fn record_func(
        &mut self,
        name: &str,
        ret_ty: Option<VarType>,
        params: Vec<VarType>,
    ) -> Result<(), LogMesg<String>> {
        // get the table at the top of the stack
        let table = self
            .stack
            .iter_mut()
            .last()
            .expect("Symbol table stack is empty");
        // check if a function with the same name exists in the same scope
        if let Some(SymbolInfo::Function { .. }) = table.get(name) {
            Err(LogMesg::err()
                .name("Invalid name".into())
                .cause("There is a function with the same name in the current scope".into())
                .help("Consider renaming the function".into()))
        } else {
            // if the function name is unique in the scope
            table.insert(name.to_string(), SymbolInfo::Function { ret_ty, params });
            self.curr_fn = table.get(name).cloned(); // set current function
            Ok(())
        }
    }

    fn search(&self, symbol: &str) -> Option<&SymbolInfo> {
        if let Some(table) = self.stack.iter().rev().find(|t| t.contains_key(symbol)) {
            return table.get(symbol);
        }
        None
    }

    pub fn search_var(&self, symbol: &str) -> Result<&VarType, LogMesg<String>> {
        if let Some(info) = self.search(symbol) {
            match info {
                SymbolInfo::Var(ty) => Ok(ty),
                _ => Err(LogMesg::err()
                    .name("Variable not defined".into())
                    .cause(format!("{} is a function not a variable", symbol))),
            }
        } else {
            Err(LogMesg::err()
                .name("Variable not defined".into())
                .cause(format!(
                    "Variable {} was not declared in this scope",
                    symbol
                )))
        }
    }

    pub fn search_fun(
        &self,
        symbol: &str,
    ) -> Result<(Option<VarType>, Vec<VarType>), LogMesg<String>> {
        if let Some(info) = self.search(&symbol) {
            match info {
                SymbolInfo::Function { ret_ty, params } => Ok((ret_ty.clone(), params.clone())),
                _ => Err(LogMesg::err()
                    .name("Function not defined".into())
                    .cause(format!("{} is a variable not a function", symbol))),
            }
        } else {
            Err(LogMesg::err()
                .name("Function not defined".into())
                .cause(format!(
                    "Function {} was not declared in this scope",
                    symbol
                )))
        }
    }

    /// If the current function exits, return it's return type and arguments map.
    pub fn curr_func(&self) -> Option<(&Option<VarType>, &Vec<VarType>)> {
        if let Some(SymbolInfo::Function { ret_ty, params }) = &self.curr_fn {
            Some((ret_ty, params))
        } else {
            None
        }
    }
}
