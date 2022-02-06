use console::style;
// use once_cell::sync::Lazy;

use std::collections::HashMap;
use std::sync::Arc;

use super::{LogMesg, VarType, Visibility, AstNode};

type SymbolTable = HashMap<String, (SymbolInfo, SymbolType)>;

#[derive(Debug)]
pub struct SymbolTableStack {
    stack: Vec<SymbolTable>,
    curr_fn: Option<String>, // name of the current function (if some)
}

type EnumVariants = Vec<(String, Vec<(String, VarType)>)>;

/// Values of the symbol table
#[derive(Debug, Clone)]
pub enum SymbolInfo {
    /// Contains the `VarType` of the variable
    Var(VarType),

    // /// Contains the `VarType` of the constant variable
    // ConstVar(VarType),
    /// Contains the return type and parameters (if some) of the function
    Function {
        ret_ty: Option<VarType>, // return type
        params: Vec<VarType>,    // name and type of parameters
        visibility: Visibility,
        variadic: bool, // NOTE: Only extern functions are allowed to be variadic
    },
    /// Contains a `HashMap` with the name and `VarType` of it's members
    Struct {
        members: Vec<(String, VarType)>,
        visibility: Visibility,
    },
    /// Contantains the list of variants (including possible field name and types of the variants)
    Enum {
        variants: EnumVariants,
        visibility: Visibility,
    },
    Alias {
        ty: VarType,
        visibility: Visibility,
    },
    ConstVar {
        ty: VarType,
        visibility: Visibility,
        value: AstNode,
    },
    Macro {
        visibility: Visibility,
        code: Arc<String>,
    },
    /// A struct with no body yet
    OpaqueStruct(Visibility),
    /// An enum with no body yet
    OpaqueEnum(Visibility),
    /// An opaque type alias
    OpaqueAlias(Visibility),
    /// A constant variable with no value yet
    OpaqueConstVar {
        visibility: Visibility, 
        ty: VarType, 
    },
}

/// Type of the symbol. Type can be: `Internal` for symbols
/// defined in the current unit, or `External` symbols exported from
/// another unit
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolType {
    Internal,
    External,
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

    pub fn record_var(&mut self, name: &str, ty: VarType) -> Result<(), LogMesg> {
        self.record(name, SymbolInfo::Var(ty), SymbolType::Internal)
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
        variadic: bool,
    ) -> Result<(), LogMesg> {
        self.record(
            name,
            SymbolInfo::Function {
                ret_ty,
                params,
                visibility,
                variadic,
            },
            SymbolType::Internal,
        )
    }

    pub fn record_struct(
        &mut self,
        name: &str,
        members: Vec<(String, VarType)>,
        visibility: Visibility,
    ) -> Result<(), LogMesg> {
        self.record(
            name,
            SymbolInfo::Struct {
                members,
                visibility,
            },
            SymbolType::Internal,
        )
    }

    pub fn record_alias(
        &mut self,
        name: &str,
        ty: VarType,
        visibility: Visibility,
    ) -> Result<(), LogMesg> {
        self.record(
            name,
            SymbolInfo::Alias { ty, visibility },
            SymbolType::Internal,
        )
    }

    pub fn record_const_var(
        &mut self,
        name: &str,
        ty: VarType,
        visibility: Visibility,
        value: AstNode,
    ) -> Result<(), LogMesg> {
        self.record(
            name,
            SymbolInfo::ConstVar { ty, visibility, value },
            SymbolType::Internal,
        )
    }

    pub fn record_enum(
        &mut self,
        name: &str,
        variants: EnumVariants,
        visibility: Visibility,
    ) -> Result<(), LogMesg> {
        self.record(
            name,
            SymbolInfo::Enum {
                variants,
                visibility,
            },
            SymbolType::Internal,
        )
    }

    pub fn record_macro(
        &mut self,
        name: &str,
        code: String,
        visibility: Visibility,
    ) -> Result<(), LogMesg> {
        self.record(
            name,
            SymbolInfo::Macro {
                code: Arc::new(code),
                visibility,
            },
            SymbolType::Internal,
        )
    }

    fn record(
        &mut self,
        name: &str,
        info: SymbolInfo,
        symbol_type: SymbolType,
    ) -> Result<(), LogMesg> {
        match (self.search(name), &info) {
            (None, _)
            | (Some((SymbolInfo::Var(_), _)), SymbolInfo::Var(_))
            | (Some((SymbolInfo::OpaqueStruct(_), _)), SymbolInfo::Struct { .. })
            | (Some((SymbolInfo::OpaqueAlias(_), _)), SymbolInfo::Alias { .. })
            | (Some((SymbolInfo::OpaqueConstVar { .. }, _)), SymbolInfo::ConstVar { .. })
            | (Some((SymbolInfo::OpaqueEnum(_), _)), SymbolInfo::Enum { .. }) => {
                // get the table at the top of the stack
                let table = self
                    .stack
                    .iter_mut()
                    .last()
                    .expect("Symbol table stack is empty");
                let _ = table.insert(name.to_string(), (info, symbol_type));
                Ok(())
            }
            _ => Err(LogMesg::err()
                .name("Invalid name".into())
                .cause(format!(
                    "There is a symbol with the same name \
                                    as {} in the current scope",
                    name
                ))
                .help("Consider renaming the symbol".into())),
        }
    }

    pub fn record_opaque_struct(
        &mut self,
        name: &str,
        visibility: Visibility,
    ) -> Result<(), LogMesg> {
        self.record(
            name,
            SymbolInfo::OpaqueStruct(visibility),
            SymbolType::Internal,
        )
    }

    pub fn record_opaque_enum(
        &mut self,
        name: &str,
        visibility: Visibility,
    ) -> Result<(), LogMesg> {
        self.record(
            name,
            SymbolInfo::OpaqueEnum(visibility),
            SymbolType::Internal,
        )
    }

    pub fn record_opaque_alias(
        &mut self,
        name: &str,
        visibility: Visibility,
    ) -> Result<(), LogMesg> {
        self.record(
            name,
            SymbolInfo::OpaqueAlias(visibility),
            SymbolType::Internal,
        )
    }

    pub fn record_opaque_const_var(
        &mut self,
        name: &str,
        visibility: Visibility,
        ty: VarType,
    ) -> Result<(), LogMesg> {
        self.record(
            name,
            SymbolInfo::OpaqueConstVar { visibility, ty },
            SymbolType::Internal,
        )
    }

    fn search(&self, symbol: &str) -> Option<&(SymbolInfo, SymbolType)> {
        if let Some(table) = self.stack.iter().rev().find(|t| t.contains_key(symbol)) {
            return table.get(symbol);
        }
        None
    }

    pub fn is_type(&self, symbol: &str) -> bool {
        match self.search(symbol) {
            Some((info, _)) => matches!(
                info,
                SymbolInfo::Struct { .. }
                    | SymbolInfo::Enum { .. }
                    | SymbolInfo::Alias { .. }
                    | SymbolInfo::OpaqueEnum(_)
                    | SymbolInfo::OpaqueStruct(_)
                    | SymbolInfo::OpaqueAlias(_)
            ),
            None => false,
        }
    }

    /// If the given symbol refers to a declared variable, this function returns a tuple with its
    /// type and a boolean idicating if it's a constant variable or not. If the symbol does not
    /// exist in the symbol table, returns an error.
    pub fn search_var(&self, symbol: &str) -> Result<(&VarType, bool), LogMesg> {
        if let Some(info) = self.search(symbol) {
            match &info.0 {
                SymbolInfo::Var(ty) => Ok((ty, false)),
                // as constant variables are parsed early in the parsing process, opaque constant
                // variables can be requested here
                SymbolInfo::ConstVar { ty, .. } 
                | SymbolInfo::OpaqueConstVar { ty, .. } => Ok((ty, true)),
                SymbolInfo::Function { .. } => Err(LogMesg::err()
                    .name("Variable not defined")
                    .cause(format!("{} is a function not a variable", symbol))),
                SymbolInfo::Struct { .. } => Err(LogMesg::err()
                    .name("Variable not defined")
                    .cause(format!("{} is a struct type not a variable", symbol))),
                SymbolInfo::Enum { .. } => Err(LogMesg::err()
                    .name("Variable not defined")
                    .cause(format!("{} is an enum type not a variable", symbol))),
                SymbolInfo::Alias { .. } => Err(LogMesg::err()
                    .name("Variable not defined")
                    .cause(format!("{} is a type alias not a variable", symbol))),
                SymbolInfo::Macro { .. } => Err(LogMesg::err()
                    .name("Variable not defined")
                    .cause(format!("{} is a macro not a variable", symbol))),
                SymbolInfo::OpaqueStruct(_)
                | SymbolInfo::OpaqueEnum(_)
                | SymbolInfo::OpaqueAlias(_) => {
                    unreachable!("Error finiding variable: {}\n{:#?}", symbol, self)
                }
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

    /// Search a function in the current module given it's name.
    pub fn search_fun(
        &self,
        symbol: &str,
    ) -> Result<(Option<VarType>, Vec<VarType>, bool), LogMesg> {
        if let Some(info) = self.search(symbol) {
            match &info.0 {
                SymbolInfo::Function {
                    ret_ty,
                    params,
                    variadic,
                    ..
                } => Ok((ret_ty.clone(), params.clone(), *variadic)),
                SymbolInfo::Var(_) | SymbolInfo::ConstVar { .. } => Err(LogMesg::err()
                    .name("Function not defined")
                    .cause(format!("{} is a variable not a function", symbol))),
                SymbolInfo::Struct { .. } => Err(LogMesg::err()
                    .name("Function not defined")
                    .cause(format!("{} is a struct type not a function", symbol))),
                SymbolInfo::Enum { .. } => Err(LogMesg::err()
                    .name("Function not defined")
                    .cause(format!("{} is an enum type not a function", symbol))),
                SymbolInfo::Alias { .. } => Err(LogMesg::err()
                    .name("Function not defined")
                    .cause(format!("{} is a type alias not a function", symbol))),
                SymbolInfo::Macro { .. } => Err(LogMesg::err()
                    .name("Function not defined")
                    .cause(format!("{} is a macro not a function", symbol))),
                SymbolInfo::OpaqueStruct(_)
                | SymbolInfo::OpaqueEnum(_)
                | SymbolInfo::OpaqueConstVar { .. }
                | SymbolInfo::OpaqueAlias(_) => unreachable!(),
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

    pub fn is_macro(&self, symbol: &str) -> bool {
        matches!(self.search(symbol), Some((SymbolInfo::Macro { .. }, _)))
    }

    /// Search a macro in the current module given it's name.
    pub fn search_macro(&self, symbol: &str) -> Result<Arc<String>, LogMesg> {
        if let Some(info) = self.search(symbol) {
            match &info.0 {
                SymbolInfo::Macro { code, .. } => Ok(Arc::clone(code)),
                SymbolInfo::Var(_) | SymbolInfo::ConstVar { .. } => Err(LogMesg::err()
                    .name("Macro not defined")
                    .cause(format!("{} is a variable not a macro", symbol))),
                SymbolInfo::Struct { .. } => Err(LogMesg::err()
                    .name("Macro not defined")
                    .cause(format!("{} is a struct type not a macro", symbol))),
                SymbolInfo::Enum { .. } => Err(LogMesg::err()
                    .name("Macro not defined")
                    .cause(format!("{} is an enum type not a macro", symbol))),
                SymbolInfo::Alias { .. } => Err(LogMesg::err()
                    .name("Macro not defined")
                    .cause(format!("{} is a type alias not a macro", symbol))),
                SymbolInfo::Function { .. } => Err(LogMesg::err()
                    .name("Macro not defined")
                    .cause(format!("{} is a function not a macro", symbol))),
                SymbolInfo::OpaqueStruct(_)
                | SymbolInfo::OpaqueEnum(_)
                | SymbolInfo::OpaqueConstVar { .. }
                | SymbolInfo::OpaqueAlias(_) => unreachable!(),
            }
        } else {
            Err(LogMesg::err()
                .name("Macro not defined".into())
                .cause(format!("Macro {} was not declared in this scope", symbol)))
        }
    }

    pub fn search_struct(&self, symbol: &str) -> Result<Option<Vec<(String, VarType)>>, LogMesg> {
        if let Some(info) = self.search(symbol) {
            match &info.0 {
                SymbolInfo::Struct { members, .. } => Ok(Some(members.clone())),
                SymbolInfo::Function { .. } => Err(LogMesg::err()
                    .name("Struct not defined")
                    .cause(format!("{} is a function not a struct", symbol))),
                SymbolInfo::Var(_) | SymbolInfo::ConstVar { .. } => Err(LogMesg::err()
                    .name("Struct not defined")
                    .cause(format!("{} is a variable not a struct", symbol))),
                SymbolInfo::Enum { .. } => Err(LogMesg::err()
                    .name("Struct {} not defined")
                    .cause(format!("{} is an enum type not a struct", symbol))),
                SymbolInfo::Alias { .. } => Err(LogMesg::err()
                    .name("Struct {} not defined")
                    .cause(format!("{} is a type alias not a struct", symbol))),
                SymbolInfo::Macro { .. } => Err(LogMesg::err()
                    .name("Struct {} not defined")
                    .cause(format!("{} is a macro not a struct", symbol))),
                SymbolInfo::OpaqueStruct(_)
                | SymbolInfo::OpaqueEnum(_)
                | SymbolInfo::OpaqueConstVar { .. }
                | SymbolInfo::OpaqueAlias(_) => unreachable!(),
            }
        } else {
            Err(LogMesg::err()
                .name("Struct not defined")
                .cause(format!("Struct {} was not declared in this scope", symbol)))
        }
    }

    pub fn search_enum(
        &self,
        symbol: &str,
    ) -> Result<Option<Vec<(String, Vec<(String, VarType)>)>>, LogMesg> {
        if let Some(info) = self.search(symbol) {
            match &info.0 {
                SymbolInfo::Enum { variants, .. } => Ok(Some(variants.clone())),
                SymbolInfo::OpaqueStruct(_) | SymbolInfo::OpaqueEnum(_) => unreachable!(),
                _ => Err(LogMesg::err()
                    .name("Undefined type".into())
                    .cause(format!("Type {} is not an enum", symbol))),
            }
        } else {
            Err(LogMesg::err().name("Undefined type".into()).cause(format!(
                "Enum type {} was not declared in this scope",
                symbol
            )))
        }
    }

    pub fn search_enum_variant(
        &self,
        enum_name: &str,
        variant: &str,
    ) -> Result<Option<(usize, Vec<(String, VarType)>)>, LogMesg> {
        if let Some(info) = self.search(enum_name) {
            match &info.0 {
                SymbolInfo::Enum { variants, .. } => {
                    match variants
                        .iter()
                        .enumerate()
                        .find(|(_, (v, _))| v == variant)
                        .map(|(i, v)| (i, v.1.clone()))
                    {
                        Some(v) => Ok(Some(v)),
                        None => Err(LogMesg::err().name("Invalid field".into()).cause(format!(
                            "Variant {} does not exist in enum type {}",
                            variant, enum_name
                        ))),
                    }
                }
                SymbolInfo::OpaqueStruct(_) | SymbolInfo::OpaqueEnum(_) => unreachable!(),
                _ => Err(LogMesg::err()
                    .name("Undefined type".into())
                    .cause(format!("Type {} is not an enum", enum_name))),
            }
        } else {
            Err(LogMesg::err().name("Undefined type".into()).cause(format!(
                "Enum type {} was not declared in this scope",
                enum_name
            )))
        }
    }

    pub fn symbol_type(&self, symbol: &str) -> Result<VarType, LogMesg> {
        match self.search(symbol) {
            Some(info) => Ok(match &info.0 {
                SymbolInfo::Var(ty) | SymbolInfo::ConstVar { ty, .. } => ty.clone(),
                SymbolInfo::Struct { .. } | SymbolInfo::OpaqueStruct(_) => {
                    VarType::Struct(symbol.into())
                }
                SymbolInfo::Enum { .. } | SymbolInfo::OpaqueEnum(_) => VarType::Enum(symbol.into()),
                SymbolInfo::Alias { ty, .. } => VarType::Alias {
                    name: symbol.into(),
                    ty: Box::new(ty.clone()),
                },
                SymbolInfo::OpaqueConstVar { .. } => unreachable!(),
                SymbolInfo::OpaqueAlias(_) => unreachable!(),
                // TODO: Missing function type as variant of `VarType`
                SymbolInfo::Function { .. } => todo!(),
                SymbolInfo::Macro { .. } => todo!(),
            }),
            None => Err(LogMesg::err().name("Undefined type".into()).cause(format!(
                "{} is not a valid type or it is not declared",
                symbol
            ))),
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
                Ok((ret_ty, params, _)) => Some((ret_ty, params)),
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

    pub fn struct_member(
        &self,
        struct_name: &str,
        member_name: &str,
    ) -> Result<Option<(usize, VarType)>, LogMesg> {
        let members = match self.search_struct(struct_name)? {
            Some(m) => m,
            None => return Ok(None),
        };

        match members
            .iter()
            .enumerate()
            .find(|(_, (name, _))| *name == member_name)
            .map(|(i, (_, ty))| (i, ty.clone()))
        {
            Some(val) => Ok(Some(val)),
            None => Err(LogMesg::err().name("Wrong member".into()).cause(format!(
                "Member {} does not exist in {}",
                style(member_name).italic(),
                style(struct_name).bold()
            ))),
        }
    }

    /// Returns a vector with all the internal (see `SymbolType`) public symbols (and their respective `SymbolInfo`).
    pub fn export_symbols(&self) -> Vec<(String, SymbolInfo)> {
        let mut public_symbols = vec![];
        for table in &self.stack {
            for (name, (info, symbol_type)) in table {
                if *symbol_type == SymbolType::Internal {
                    match info {
                        SymbolInfo::OpaqueStruct(visibility)
                        | SymbolInfo::OpaqueEnum(visibility)
                        | SymbolInfo::OpaqueAlias(visibility)
                        | SymbolInfo::OpaqueConstVar { visibility, .. }
                        | SymbolInfo::Struct { visibility, .. }
                        | SymbolInfo::Function { visibility, .. }
                        | SymbolInfo::Enum { visibility, .. }
                        | SymbolInfo::Alias { visibility, .. }
                        | SymbolInfo::ConstVar { visibility, .. }
                            if *visibility == Visibility::Pub =>
                        {
                            public_symbols.push((name.into(), info.clone()))
                        }
                        _ => (),
                    }
                }
            }
        }

        public_symbols
    }

    pub fn import_symbols(&mut self, symbols: Vec<(String, SymbolInfo)>) {
        for (name, info) in symbols {
            self.record(&name, info, SymbolType::External).expect(
                format!(
                    "There is a symbol with the \
                             same name as an imported symbol {}",
                    name
                )
                .as_str(),
            );
        }
    }
}

impl Default for SymbolTableStack {
    fn default() -> Self {
        Self::new()
    }
}
