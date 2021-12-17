use std::fmt;

use super::{current_unit_st, LogMesg};

// TODO: This is used to get the size of pointer types, and should change with different CPU
// architectures.
const PTR_SIZE: usize = 8; // specific to 64 bit CPUs

#[derive(Debug, PartialEq, Clone, Hash)]
pub enum VarType {
    UInt8,
    Int8,
    UInt16,
    Int16,
    UInt32,
    Int32,
    Int64,
    UInt64,
    Float32,
    Float64,
    Boolean,
    Array {
        inner: Box<VarType>,
        len: usize,
    },
    /// Contains inner type of the slice.
    Slice(Box<VarType>),
    /// Contains the name of the struct type.
    Struct(String),
    /// Contains the name of the enum type.
    Enum(String),
    /// Contains the type it refers to.
    Ref(Box<VarType>),
    /// A type for pointers to `void`. Used to interface with C code.
    /// This is the equivalent of `void*` in C.
    CVoidRef,
    /// Utf-8 encoded string.
    Str,
    /// An alias is a user defined type that refers to another type.
    Alias {
        name: String,
        ty: Box<VarType>,
    },
    Unknown,
}

impl VarType {
    pub fn is_literal(&self) -> bool {
        matches!(
            &self,
            VarType::UInt8
                | VarType::Int8
                | VarType::UInt16
                | VarType::Int16
                | VarType::UInt32
                | VarType::Int32
                | VarType::Int64
                | VarType::UInt64
                | VarType::Float32
                | VarType::Float64
                | VarType::Boolean
        )
    }

    /// Returns the size of the type in bytes. If the type is `Unknown`,
    ///
    /// **NOTE**: If the size in bits of the type is lower than 8, this function will return 1 as
    /// the size in bytes of the type.
    pub fn size(&self) -> usize {
        match self {
            VarType::UInt8 | VarType::Int8 => 1,
            VarType::UInt16 | VarType::Int16 => 2,
            VarType::UInt32 | VarType::Int32 | VarType::Float32 => 4,
            VarType::Int64 | VarType::UInt64 | VarType::Float64 => 8,
            VarType::Boolean => 1,
            VarType::Array { inner, len } => inner.size() * len,
            VarType::Slice(_) => {
                // slices are structs with the following layout: { base_ptr, slice_length }
                // therefore, the size of a slice is the size of a pointer plus the size of the
                // integer used to log the length of the slice, in okta, this integer is u32.
                PTR_SIZE + 4
            }
            VarType::Str => PTR_SIZE + 4,
            VarType::Ref(_) => PTR_SIZE,
            VarType::Struct(name) => {
                let res = current_unit_st!().search_struct(name);
                match res {
                    Ok(Some(members)) => members.iter().map(|(_, ty)| ty.size()).sum(),
                    Ok(None) => 0,
                    Err(e) => {
                        e.send().unwrap();
                        0
                    }
                }
            },
            VarType::Enum(name) => {
                let res = current_unit_st!().search_enum(name);
                match res {
                    Ok(Some(fields)) => fields
                        .iter()
                        .map(|(_, fields)| fields.iter().map(|(_, ty)| ty.size()).sum())
                        .max()
                        .unwrap_or(0), // if there are no fields in any of the variants
                    Ok(None) => 0,
                    Err(e) => {
                        e.send().unwrap();
                        0
                    }
                }
            }
            // variants.iter()
            // .map(|(_, fields)| fields.iter().map(|(_, ty)| ty.size()).sum()).max().unwrap_or(0),
            VarType::CVoidRef => PTR_SIZE,
            VarType::Alias { ty, .. } => ty.size(),
            VarType::Unknown => 0,
        }
    }

    /// Returns a compact string representation of the type. This is intended to be used for thigs
    /// such as name mangling.
    pub fn compact_str_fmt(&self) -> String {
        match self {
            VarType::Array { inner, len } => format!("arr{}_{}", len, inner),
            VarType::Slice(inner) => format!("slice_{}", inner),
            VarType::Ref(inner) => format!("ref_{}", inner),
            _ => format!("{}", self),
        }
    }

    /// If the `VarType` is an alias type, the function resolves all the aliases until it reached a
    /// real type. If the `VarType` is not an alias, the function returns a copy the `VarType`.
    pub fn resolve_alias(&self) -> VarType {
        match self {
            VarType::Alias { ty, .. } => ty.resolve_alias(),
            _ => self.clone(),
        }
    }
}

impl fmt::Display for VarType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VarType::Int8 => write!(f, "i8"),
            VarType::UInt8 => write!(f, "u8"),
            VarType::Int16 => write!(f, "i16"),
            VarType::UInt16 => write!(f, "u16"),
            VarType::Int32 => write!(f, "i32"),
            VarType::UInt32 => write!(f, "u32"),
            VarType::Int64 => write!(f, "i64"),
            VarType::UInt64 => write!(f, "u64"),
            VarType::Float32 => write!(f, "f32"),
            VarType::Float64 => write!(f, "f64"),
            VarType::Boolean => write!(f, "bool"),
            VarType::Str => write!(f, "str"),
            VarType::Array { inner, len } => write!(f, "[{};{}]", inner, len),
            VarType::Slice(inner) => write!(f, "[{}]", inner),
            VarType::Ref(inner) => write!(f, "&{}", inner),
            VarType::Struct(name) | VarType::Enum(name) => write!(f, "{}", name),
            VarType::CVoidRef => write!(f, "c_voidptr"),
            VarType::Alias { name, .. } => write!(f, "{}", name),
            VarType::Unknown => write!(f, "unknown"),
        }
    }
}

/// Given a symbol name, this function recursively finds all the symbol's dependencies, storing them
/// in the `dependecies` vector given as argument.
pub fn type_dependencies(symbol: &str, dependecies: &mut Vec<String>) -> Result<(), LogMesg> {
    let ty = current_unit_st!().symbol_type(symbol)?;

    let all_types = match ty {
        VarType::Struct(name) => {
            let (_, members): (Vec<_>, Vec<_>) = current_unit_st!()
                .search_struct(&name)
                .unwrap()
                .unwrap()
                .iter()
                .cloned() // TODO: To optimize
                .unzip();
            members
        }
        VarType::Enum(name) => {
            let (_, fields): (Vec<_>, Vec<_>) = current_unit_st!()
                .search_enum(&name)
                .unwrap()
                .unwrap()
                .iter()
                .cloned() // TODO: To optimize
                .unzip();

            let (_, f): (Vec<_>, Vec<_>) = fields
                .concat()
                .iter()
                .cloned() // TODO: To optimize
                .unzip();
            f
        }
        _ => panic!("Cannot get dependecies of type {:?}", ty),
    };

    for ty in all_types {
        match ty {
            VarType::Struct(name) | VarType::Enum(name) => {
                if dependecies.contains(&name) {
                    // endless recursive loop detected, just return here, as all dependencies are
                    // already contained in the `dependecies` list
                    return Ok(());
                } else {
                    dependecies.push(name.clone());
                    type_dependencies(&name, dependecies)?;
                }
            }
            _ => continue,
        }
    }

    Ok(())
}
