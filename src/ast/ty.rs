use pest::iterators::Pair;

use super::parser::*;
use crate::{LogMesg, VarType, current_unit_st};

pub fn parse_ty_or_default(pair: Pair<Rule>, pair_info: Option<(&str, usize)>) -> VarType {
    let (pair_str, pair_loc) = match pair_info {
        Some(v) => v,
        None => (pair.as_str(), pair.as_span().start_pos().line_col().0),
    };
    match parse_var_type(pair) {
        Ok(ty) => ty,
        Err(e) => {
            e.location(pair_loc).lines(pair_str).send().unwrap();
            VarType::Unknown
        }
    }
}

pub fn parse_var_type(pair: Pair<Rule>) -> Result<VarType, LogMesg> {
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::simpleType => parse_simple_ty(inner),
        Rule::arrayType => parse_array_ty(inner),
        Rule::refType => parse_ref_ty(inner),
        _ => unreachable!("{:?}", inner.as_rule()),
    }
}

pub fn parse_simple_ty(pair: Pair<Rule>) -> Result<VarType, LogMesg> {
    match pair.as_str() {
        "i8" => Ok(VarType::Int8),
        "u8" => Ok(VarType::UInt8),
        "i16" => Ok(VarType::Int16),
        "u16" => Ok(VarType::UInt16),
        "i32" => Ok(VarType::Int32),
        "u32" => Ok(VarType::UInt32),
        "i64" => Ok(VarType::Int64),
        "u64" => Ok(VarType::UInt64),
        "bool" => Ok(VarType::Boolean),
        "f32" => Ok(VarType::Float32),
        "f64" => Ok(VarType::Float64),
        // FIX: Any (declared) symbol is a valid type! Only allow structs to do this
        name => {
            current_unit_st!().symbol_type(name).map(|val| {
                if let Some(v) = val {
                    v
                } else {
                    // No error is returned here as any undefined type error will be detected in
                    // the type dependency check pass. At this point all symbols might not be
                    // known. 
                    VarType::Unknown
                }
            })
        },
    }
}

pub fn parse_array_ty(pair: Pair<Rule>) -> Result<VarType, LogMesg> {
    let mut inner = pair.into_inner();
    let ty_rule = inner.next().unwrap();
    let len_rule = inner.next().unwrap();

    Ok(VarType::Array {
        inner: Box::new(parse_var_type(ty_rule)?),
        len: match len_rule.as_str().parse() {
            Ok(v) => v,
            Err(_) => {
                dbg!(inner);
                return Err(LogMesg::err()
                    .name("Wrong value".into())
                    .cause("Invalid length for array, only natural numbers are allowed".into()));
            }
        },
    })
}

pub fn parse_ref_ty(pair: Pair<Rule>) -> Result<VarType, LogMesg> {
    let inner = pair.into_inner().next().unwrap();
    Ok(VarType::Ref(Box::new(parse_var_type(inner)?)))
}
