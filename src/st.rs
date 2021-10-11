use once_cell::sync::Lazy;
use console::style;

use std::collections::HashMap;
use std::sync::Mutex;

use super::{LogMesg, VarType, Visibility};

type SymbolTable = HashMap<String, SymbolInfo>;

pub static ST: Lazy<Mutex<SymbolTableStack>> = Lazy::new(|| Mutex::new(SymbolTableStack::new()));

#[derive(Debug)]
pub struct SymbolTableStack {
    stack: Vec<SymbolTable>,
    curr_fn: Option<String>, // name of the current function (if some)
}

/// Values of the symbol table
#[derive(Debug, Clone)]
pub enum SymbolInfo {
    /// Contains the `VarType` of the variable
    Var(VarType),
    /// Contains the return type and parameters (if some) of the function
    Function {
        ret_ty: Option<VarType>, // return type
        params: Vec<VarType>,    // name and type of parameters
        visibility: Visibility,
    },
    /// Contains a `HashMap` with the name and `VarType` of it's members
    Struct {
        members: Vec<(String, VarType)>,
        visibility: Visibility,
    },
    InvalidType,
}

impl SymbolTableStack {

    pub fn new() -> SymbolTableStack {
        SymbolTableStack {
            stack: vec![],
            curr_fn: None,
        }
    }

    pub fn push_table(&mut self) {
        self.stack.push(SymbolTable::default())
    }

    pub fn pop_table(&mut self) {
        self.stack.pop();
    }

    pub fn record_var(&mut self, name: &str, ty: &VarType) -> Result<(), LogMesg<String>> {
        let table = self
            .stack
            .iter_mut()
            .last()
            .expect("Symbol table stack is empty");

        if let Some(SymbolInfo::Function { .. }) = table.get(name) {
            Err(LogMesg::err()
                .name("Invalid name".into())
                .cause(format!("There is a function with the same name as {} in the current scope", name))
                .help("Consider renaming the function".into()))

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
        visibility: Visibility,
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
                .cause(format!("There is a function with the same name as {} in the current scope", name))
                .help("Consider renaming the function".into()))
        } else {
            // if the function name is unique in the scope
            table.insert(name.to_string(), SymbolInfo::Function { ret_ty, params, visibility });
            Ok(())
        }
    }

    pub fn record_struct(&mut self, 
                         name: &str, 
                         members: Vec<(String, VarType)>,
                         visibility: Visibility) -> Result<(), LogMesg<String>> {
        // get the table at the top of the stack
        let table = self
            .stack
            .iter_mut()
            .last()
            .expect("Symbol table stack is empty");
         
        // check if a struct definition with the same name exists in the same scope
        if let Some(SymbolInfo::Struct{..}) = table.get(name) {
            Err(LogMesg::err()
                .name("Invalid name".into())
                .cause(format!("There is a struct definition with the same name as {} in the current scope", name))
                .help("Consider renaming the struct".into()))
        } else {
            // if the struct definition is unique in the scope
            table.insert(name.to_string(), SymbolInfo::Struct{ members, visibility });
            Ok(())
        }
    }

    pub fn record_invalid(&mut self, symbol: &str) {
        if let Some(table) = self.stack.iter_mut().rev().find(|t| t.contains_key(symbol)) {
            table.get_mut(symbol);

        } else {
            // get the table at the top of the stack
            let table = self
                .stack
                .iter_mut()
                .last()
                .expect("Symbol table stack is empty");
            table.insert(symbol.to_string(), 
                         SymbolInfo::InvalidType); 
        }
    }

    pub fn is_invalid(&self, symbol: &str) -> Option<bool> {
        self.search(symbol).map(|info| if let SymbolInfo::InvalidType = info {
            true
        } else { false })
    }

    fn search(&self, symbol: &str) -> Option<&SymbolInfo> {
        if let Some(table) = self.stack.iter().rev().find(|t| t.contains_key(symbol)) {
            return table.get(symbol);
        }
        None
    }

    pub fn search_var(&self, symbol: &str) -> Result<Option<&VarType>, LogMesg<String>> {
        if let Some(info) = self.search(symbol) {
            match info {
                SymbolInfo::Var(ty) => Ok(Some(ty)),
                SymbolInfo::Function{..} => Err(LogMesg::err()
                    .name(format!("Variable {} not defined", symbol))
                    .cause(format!("{} is a function not a variable", symbol))),
                SymbolInfo::Struct{..} => Err(LogMesg::err()
                    .name(format!("Variable {} not defined", symbol))
                    .cause(format!("{} is a struct not a variable", symbol))),
                SymbolInfo::InvalidType => Ok(None)
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

    /// Search a function in the current module given it's name. If the function is searched
    /// to search is outside the current module, the `public` argument has to be set to `true`, 
    /// else to `false`.
    pub fn search_fun(
        &self,
        symbol: &str,
    ) -> Result<Option<(Option<VarType>, Vec<VarType>)>, LogMesg<String>> {
        if let Some(info) = self.search(symbol) {
            match info {
                SymbolInfo::Function { ret_ty, params, visibility } => Ok(Some((ret_ty.clone(), params.clone()))),
                SymbolInfo::Var(_) => Err(LogMesg::err()
                    .name(format!("Function {} not defined", symbol))
                    .cause(format!("{} is a variable not a function", symbol))),
                SymbolInfo::Struct{..} => Err(LogMesg::err()
                    .name(format!("Function {} not defined", symbol))
                    .cause(format!("{} is a struct not a function", symbol))),
                SymbolInfo::InvalidType => Ok(None)
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

    pub fn search_struct(
        &self,
        symbol: &str,
    ) -> Result<Option<Vec<(String, VarType)>>, LogMesg<String>> {
        if let Some(info) = self.search(symbol) {
            match info {
                SymbolInfo::Struct { members, visibility } => Ok(Some(members.clone())),
                SymbolInfo::Function {..} => Err(LogMesg::err()
                    .name(format!("Struct {} not defined", symbol))
                    .cause(format!("{} is a function not a struct", symbol))),
                SymbolInfo::Var(_) => Err(LogMesg::err()
                    .name(format!("Struct {} not defined", symbol))
                    .cause(format!("{} is a variable not a struct", symbol))),
                SymbolInfo::InvalidType => Ok(None),
            }
        } else {
            Err(LogMesg::err()
                .name(format!("Struct {} not defined", symbol))
                .cause(format!(
                    "Struct {} was not declared in this scope",
                    symbol
                )))
        }
    }

    pub fn symbol_type(&self, symbol: &str) -> Result<Option<VarType>, LogMesg<String>> {
        match self.search(symbol) {
            Some(info) => Ok(match info {
                SymbolInfo::Var(ty) => Some(ty.clone()),
                SymbolInfo::Struct{..} => Some(VarType::Struct(symbol.into())),
                SymbolInfo::InvalidType => None, 
                // TODO: Missing function type as variant of `VarType`
                SymbolInfo::Function {..} => todo!(),
            }),
            None => Err(LogMesg::err().name("Undefined type".into())
                .cause(format!("{} is not a valid type or it is not declared", symbol)))
        }
    }

    /// Get the name of the current function, if some.
    pub fn curr_func(&self) -> Option<&str> {
        self.curr_fn.as_deref()
    }

    /// If the current function exits, return it's return type and argument types list.
    pub fn curr_func_info(&self) -> Option<(Option<VarType>, Vec<VarType>)> {
        match &self.curr_fn {
            Some(name) => match self.search_fun(&name) {
                Ok(Some(info)) => Some(info),
                Ok(None) => unreachable!(),
                Err(_) => None,
            },
            None => None,
        }
    }

    /// Set the name of the current function being processed.
    pub fn curr_func_set(&mut self, name: &str) {
        self.curr_fn = Some(name.into());
    }

    /// Sets the current function to `None`. This function should be called after finishing
    /// processing a function.
    pub fn curr_func_restore(&mut self) {
        self.curr_fn = None;
    }

    pub fn struct_member(&self, struct_name: &str, member_name: &str) -> Result<Option<(usize, VarType)>, LogMesg<String>> {
        let members = match self.search_struct(struct_name)? {
            Some(m) => m,
            None => return Ok(None),
        };

        match members.iter()
            .enumerate()
            .find(|(_, (name, _))| *name == member_name)
            .map(|(i, (_, ty))| (i, ty.clone())) {
                Some(val) => Ok(Some(val)),
                None => Err(LogMesg::err()
                   .name("Wrong member".into())
                   .cause(format!("Member {} does not exist in {}", 
                                  style(member_name).italic(), 
                                  style(struct_name).bold()))),
        }
    }
}

impl Default for SymbolTableStack {
    fn default() -> Self {
        Self::new()
    }
}
