use inkwell::values::{PointerValue, GlobalValue};

use std::collections::HashMap;

use super::VarType;

type Table<'ctx> = HashMap<String, STEntry<'ctx>>;

pub struct CodegenST<'ctx>(Vec<Table<'ctx>>);

pub enum STEntry<'ctx> {
    Variable {
        ty: VarType,
        ptr: PointerValue<'ctx>,
    },
    Global {
        value: GlobalValue<'ctx>,
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

    pub fn search_variable(&self, symbol: &str) -> (&VarType, PointerValue<'ctx>) {
        let table = self
            .0
            .iter()
            .rev()
            .find(|t| t.contains_key(symbol))
            .expect("Symbol does not exist in the symbol table");

        match table.get(symbol) {
            Some(STEntry::Variable { ty, ptr }) => (ty, *ptr),
            Some(STEntry::Global { value, ty }) => (ty, value.as_pointer_value()),
            None => unreachable!(),
        }
    }

    pub fn search_global(&self, symbol: &str) -> Option<GlobalValue<'ctx>> {
        let table = self
            .0
            .iter()
            .rev()
            .find(|t| t.contains_key(symbol))
            .expect("Symbol for global value does not exist in the symbol table");

        match table.get(symbol) {
            Some(STEntry::Global { value, .. }) => Some(*value),
            Some(STEntry::Variable {..}) => None,
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
}

impl<'ctx> Default for CodegenST<'ctx> {
    fn default() -> Self {
        CodegenST(vec![HashMap::new()])
    }
}
