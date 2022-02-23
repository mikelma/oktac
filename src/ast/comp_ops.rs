use console::style;
use once_cell::sync::Lazy;
use pest::iterators::Pair;

use std::{collections::HashMap, fmt};

use super::parser::*;
use crate::LogMesg;

pub struct CompOpts {
    opts: HashMap<String, Value>,
    ty: SymbolType,
}

// -------- Default compilation options for structs -------- //
static DEFAULT_STRUCT: Lazy<HashMap<String, Value>> = Lazy::new(|| {
    let mut map = HashMap::new();

    map.insert("packed".into(), Value::Boolean(true));
    map.insert("derive".into(), Value::List(vec![], ValueType::String));

    map
});

static DEFAULT_ENUMS: Lazy<HashMap<String, Value>> = Lazy::new(|| {
    let map = HashMap::new();

    // NOTE: Compilation options for enums will go here in the future

    map
});

static DEFAULT_MACROS: Lazy<HashMap<String, Value>> = Lazy::new(|| {
    let mut map = HashMap::new();

    map.insert("path".into(), Value::Optional(ValueType::String));

    map
});

static DEFAULT_FUNCTIONS: Lazy<HashMap<String, Value>> = Lazy::new(|| {
    let mut map = HashMap::new();

    map.insert("inline".into(), Value::Boolean(false));

    map
});

// --------------------------------------------------------- //

#[derive(Clone)]
pub enum Value {
    Int(isize),
    Float(f64),
    String(String),
    Boolean(bool),
    List(Vec<Value>, ValueType),
    Optional(ValueType), // Optional value. Contains expected type
}

#[derive(Clone, PartialEq, Eq)]
pub enum ValueType {
    Int,
    Float,
    String,
    Boolean,
    List(Box<ValueType>),
}

#[derive(Clone, Copy)]
pub enum SymbolType {
    Macro,
    Strct,
    Enum,
    Function,
}

/// Parse all compilation options of a symbol.
pub fn parse_comp_ops(pair: Pair<Rule>, symbol_type: SymbolType) -> CompOpts {
    let pair_str = pair.as_str();
    let pair_loc = pair.as_span().start_pos().line_col().0;

    let mut opts = HashMap::new();

    for opts_pair in pair.into_inner() {
        let mut inner = opts_pair.into_inner();

        // get option's name
        let option = inner.next().unwrap().as_str();

        // check if the option has been already set
        if opts.contains_key(option) {
            LogMesg::err()
                .name("Repeated compilation option")
                .cause(format!(
                    "Compilation option {} has been set multiple times",
                    style(option).italic()
                ))
                .help("Consider removing redundant options".into())
                .lines(pair_str)
                .location(pair_loc)
                .send()
                .unwrap();
            break;
        }

        // check if the compilation option exists
        if let Err(err) = check_option(symbol_type, option) {
            err.lines(pair_str).location(pair_loc).send().unwrap();
            break;
        }

        // this unwrap cannot fail, as the option is guaranteed to exist in `check_option`
        let expected_type = symbol_type.get_default().get(option).unwrap().get_type();

        let value_pair = inner.next().unwrap();

        match parse_comp_opts_value(value_pair, option, &expected_type) {
            Ok(value) => {
                let _ = opts.insert(option.to_string(), value);
            }
            Err(err) => err.lines(pair_str).location(pair_loc).send().unwrap(),
        }
    }

    CompOpts {
        opts,
        ty: symbol_type,
    }
}

/// Parse the value of a compilation option. If the type of the parsed value does not match with
/// the expected type, the function returns an error.
fn parse_comp_opts_value(
    pair: Pair<Rule>,
    option: &str,
    expected_type: &ValueType,
) -> Result<Value, LogMesg> {
    let type_error = || {
        Err(LogMesg::err()
            .name("Invalid compilation type")
            .cause(format!(
                "compilation option {} must be of type {}",
                style(option).italic(),
                style(expected_type).bold()
            )))
    };

    let parse_simple_value = |pair: Pair<Rule>| match pair.as_rule() {
        Rule::number => Value::Int(pair.as_str().parse().unwrap()),
        Rule::float => Value::Float(pair.as_str().parse().unwrap()),
        Rule::str => Value::String(pair.into_inner().next().unwrap().as_str().to_string()),
        Rule::boolean => Value::Boolean(pair.as_str().parse().unwrap()),
        _ => unreachable!(),
    };

    match pair.as_rule() {
        Rule::number | Rule::float | Rule::str | Rule::boolean => {
            // parse value and get it's type
            let value = parse_simple_value(pair);
            let value_ty = value.get_type();

            // check if the type of the provided value is correct
            if value_ty == *expected_type {
                Ok(value)
            } else {
                type_error()
            }
        }
        Rule::optList => {
            let mut opts = vec![];
            let expected_inner_ty = match expected_type {
                ValueType::List(v) => v,
                _ => return type_error(),
            };
            for opt in pair.into_inner() {
                // NOTE: Lists of options can only be composed of simple (non-composed) values
                let value = parse_simple_value(opt);
                let value_ty = value.get_type();

                if value_ty == **expected_inner_ty {
                    opts.push(value);
                } else {
                    return type_error();
                }
            }

            Ok(Value::List(opts, expected_type.clone()))
        }
        _ => unreachable!(format!("{:?}", pair.as_rule())),
    }
}

/// Check if the compilation option really exits.
fn check_option(symbol_type: SymbolType, option: &str) -> Result<(), LogMesg> {
    if symbol_type.get_default().contains_key(option) {
        Ok(())
    } else {
        let valid_opts = symbol_type
            .get_default()
            .keys()
            .map(|v| style(v).italic().to_string())
            .collect::<Vec<String>>();
        Err(LogMesg::err()
            .name("Invalid compilation option")
            .cause(format!(
                "there is no compilation option {} for type {}",
                style(option).italic(),
                style(symbol_type).bold()
            ))
            .help(format!("Valid options are: {}", valid_opts.join(","))))
    }
}

impl CompOpts {
    pub fn default(ty: SymbolType) -> Self {
        CompOpts {
            opts: HashMap::default(),
            ty,
        }
    }

    pub fn get_option(&self, option: &str) -> Value {
        match self.opts.get(option) {
            Some(opt) => opt.clone(),
            None => self.ty.get_default().get(option).unwrap().clone(),
        }
    }
}

impl SymbolType {
    fn get_default(&self) -> &Lazy<HashMap<String, Value>> {
        match self {
            SymbolType::Enum => &DEFAULT_ENUMS,
            SymbolType::Macro => &DEFAULT_MACROS,
            SymbolType::Strct => &DEFAULT_STRUCT,
            SymbolType::Function => &DEFAULT_FUNCTIONS,
        }
    }
}

impl Value {
    pub fn get_type(&self) -> ValueType {
        match self {
            Value::Int(_) => ValueType::Int,
            Value::Float(_) => ValueType::Float,
            Value::String(_) => ValueType::String,
            Value::Boolean(_) => ValueType::Boolean,
            Value::Optional(ty) => ty.clone(),
            Value::List(_, ty) => ValueType::List(Box::new(ty.clone())),
        }
    }

    pub fn into_int(self) -> isize {
        match self {
            Value::Int(v) => v,
            _ => panic!(),
        }
    }

    pub fn into_bool(self) -> bool {
        match self {
            Value::Boolean(v) => v,
            _ => panic!(),
        }
    }

    pub fn into_float(self) -> f64 {
        match self {
            Value::Float(v) => v,
            _ => panic!(),
        }
    }

    pub fn into_string(self) -> String {
        match self {
            Value::String(v) => v,
            _ => panic!(),
        }
    }

    pub fn into_optional(self) -> Option<Value> {
        if matches!(self, Value::Optional(_)) {
            None
        } else {
            Some(self)
        }
    }

    pub fn into_vec(self) -> Vec<Value> {
        match self {
            Value::List(value, _) => value,
            _ => panic!(),
        }
    }
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueType::Int => write!(f, "int"),
            ValueType::Float => write!(f, "float"),
            ValueType::String => write!(f, "string"),
            ValueType::Boolean => write!(f, "boolean"),
            ValueType::List(ty) => write!(f, "list of {}", ty),
        }
    }
}

impl fmt::Display for SymbolType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SymbolType::Enum => write!(f, "enum"),
            SymbolType::Strct => write!(f, "struct"),
            SymbolType::Macro => write!(f, "macro"),
            SymbolType::Function => write!(f, "function"),
        }
    }
}
