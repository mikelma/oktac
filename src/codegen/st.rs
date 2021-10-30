use inkwell::values::PointerValue;

use std::collections::HashMap;

use super::VarType;

type Table<'ctx> = HashMap<String, STEntry<'ctx>>;

pub struct CodegenST<'ctx>(Vec<Table<'ctx>>);

pub enum STEntry<'ctx> {
    Variable {
        ty: VarType,
        ptr: PointerValue<'ctx>,
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

    pub fn search_variable(&self, symbol: &str) -> (&VarType, &PointerValue<'ctx>) {
        let table = self
            .0
            .iter()
            .rev()
            .find(|t| t.contains_key(symbol))
            .expect("Symbol does not exist in the symbol table");

        match table.get(symbol) {
            Some(STEntry::Variable { ty, ptr }) => (ty, ptr),
            None => unreachable!(),
        }
    }

    pub fn register_variable(&mut self, symbol: &str, ty: VarType, ptr: PointerValue<'ctx>) {
        self.top_table_mut()
            .insert(symbol.into(), STEntry::Variable { ty, ptr });
    }
}

impl<'ctx> Default for CodegenST<'ctx> {
    fn default() -> Self {
        CodegenST(vec![])
    }
}
