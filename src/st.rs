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
    curr_fn: Option<SymbolInfo>,
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
                .cause("There is a function with the same name in the current scope".into())
                .help("Consider renaming the function".into()))
        } else {
            // if the function name is unique in the scope
            table.insert(name.to_string(), SymbolInfo::Function { ret_ty, params, visibility });
            self.curr_fn = table.get(name).cloned(); // set current function
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
                .cause("There is a struct definition with the same name in the current scope".into())
                .help("Consider renaming the struct".into()))
        } else {
            // if the struct definition is unique in the scope
            table.insert(name.to_string(), SymbolInfo::Struct{ members, visibility });
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
                SymbolInfo::Function{..} => Err(LogMesg::err()
                    .name(format!("Variable {} not defined", symbol))
                    .cause(format!("{} is a function not a variable", symbol))),
                SymbolInfo::Struct{..} => Err(LogMesg::err()
                    .name(format!("Variable {} not defined", symbol))
                    .cause(format!("{} is a struct not a variable", symbol))),
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
    ) -> Result<(Option<VarType>, Vec<VarType>), LogMesg<String>> {
        if let Some(info) = self.search(symbol) {
            match info {
                SymbolInfo::Function { ret_ty, params, visibility } => Ok((ret_ty.clone(), params.clone())),
                SymbolInfo::Var(_) => Err(LogMesg::err()
                    .name(format!("Function {} not defined", symbol))
                    .cause(format!("{} is a variable not a function", symbol))),
                SymbolInfo::Struct{..} => Err(LogMesg::err()
                    .name(format!("Function {} not defined", symbol))
                    .cause(format!("{} is a struct not a function", symbol))),
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
    ) -> Result<Vec<(String, VarType)>, LogMesg<String>> {
        if let Some(info) = self.search(symbol) {
            match info {
                SymbolInfo::Struct { members, visibility } => Ok(members.clone()),
                SymbolInfo::Function {..} => Err(LogMesg::err()
                    .name(format!("Struct {} not defined", symbol))
                    .cause(format!("{} is a function not a struct", symbol))),
                SymbolInfo::Var(_) => Err(LogMesg::err()
                    .name(format!("Struct {} not defined", symbol))
                    .cause(format!("{} is a variable not a struct", symbol))),
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

    pub fn symbol_type(&self, symbol: &str) -> Result<VarType, LogMesg<String>> {
        match self.search(symbol) {
            Some(info) => Ok(match info {
                SymbolInfo::Var(ty) => ty.clone(),
                SymbolInfo::Struct{..} => VarType::Struct(symbol.into()),
                // TODO: Missing function type as variant of `VarType`
                SymbolInfo::Function {..} => todo!(),
            }),
            None => Err(LogMesg::err().name("Undefined type".into())
                .cause(format!("{} is not a valid type or it is not declared", symbol)))
        }
    }

    /// If the current function exits, return it's return type and arguments map.
    pub fn curr_func(&self) -> Option<(&Option<VarType>, &Vec<VarType>)> {
        if let Some(SymbolInfo::Function { ret_ty, params, .. }) = &self.curr_fn {
            Some((ret_ty, params))
        } else {
            None
        }
    }

    pub fn struct_member(&self, struct_name: &str, member_name: &str) -> Result<(usize, VarType), LogMesg<String>> {
        let members = self.search_struct(struct_name)?;
        match members.iter()
            .enumerate()
            .find(|(_, (name, _))| *name == member_name)
            .map(|(i, (_, ty))| (i, ty.clone())) {
                Some(val) => Ok(val),
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
