use inkwell::values::{FunctionValue, GlobalValue, PointerValue};

use std::collections::HashMap;

use super::VarType;

type Table<'ctx> = HashMap<String, STEntry<'ctx>>;

#[derive(Debug)]
pub struct CodegenST<'ctx>(Vec<Table<'ctx>>);

#[derive(Debug)]
pub enum STEntry<'ctx> {
    Variable {
        ty: VarType,
        ptr: PointerValue<'ctx>,
    },
    Global {
        value: GlobalValue<'ctx>,
        ty: VarType,
    },
    FunctionRef {
        value: FunctionValue<'ctx>,
        ty: VarType,
    },
}

impl<'ctx> CodegenST<'ctx> {
    pub fn push_table(&mut self) {
        self.0.push(HashMap::new())
    }

    pub fn pop_table(&mut self) {
        let _ = self.0.pop();
    }

    /*
    fn top_table(&self) -> &Table<'ctx> {
        self.0.last().expect("Codegen symbol table is empty, cannot get top table")
    }
    */

    fn top_table_mut(&mut self) -> &mut Table<'ctx> {
        self.0
            .last_mut()
            .expect("Codegen symbol table is empty, cannot get top table")
    }

    pub fn search_variable(&self, symbol: &str) -> (&VarType, PointerValue<'ctx>, bool) {
        let table = self
            .0
            .iter()
            .rev()
            .find(|t| t.contains_key(symbol))
            .expect("Symbol does not exist in the symbol table");

        match table.get(symbol) {
            Some(STEntry::Variable { ty, ptr }) => (ty, *ptr, false),
            Some(STEntry::Global { value, ty }) => (ty, value.as_pointer_value(), false),
            Some(STEntry::FunctionRef { value, ty }) => {
                (ty, value.as_global_value().as_pointer_value(), true)
            }
            // if the symbol isn't a variable, it might be a function
            None => unreachable!(),
        }
    }

    pub fn search_global(&self, symbol: &str) -> Option<GlobalValue<'ctx>> {
        let table = match self.0.iter().rev().find(|t| t.contains_key(symbol)) {
            Some(v) => v,
            None => {
                println!("Cannot find symbol with name: {}", symbol);
                dbg!(self);
                println!();
                unreachable!();
            }
        };

        match table.get(symbol) {
            Some(STEntry::Global { value, .. }) => Some(*value),
            Some(STEntry::Variable { .. }) | Some(STEntry::FunctionRef { .. }) => None,
            None => unreachable!("Symbol {} does not exist", symbol),
        }
    }

    pub fn register_variable(&mut self, symbol: &str, ty: VarType, ptr: PointerValue<'ctx>) {
        self.top_table_mut()
            .insert(symbol.into(), STEntry::Variable { ty, ptr });
    }

    pub fn register_global(&mut self, symbol: &str, ty: VarType, value: GlobalValue<'ctx>) {
        self.top_table_mut()
            .insert(symbol.into(), STEntry::Global { ty, value });
    }

    pub fn register_function(&mut self, symbol: &str, ty: VarType, value: FunctionValue<'ctx>) {
        self.top_table_mut()
            .insert(symbol.into(), STEntry::FunctionRef { ty, value });
    }
}

impl<'ctx> Default for CodegenST<'ctx> {
    fn default() -> Self {
        CodegenST(vec![HashMap::new()])
    }
}
