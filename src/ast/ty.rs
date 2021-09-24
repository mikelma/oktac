use pest::iterators::Pair;

use crate::{VarType, ST, LogMesg};
use super::parser::*;

pub fn parse_var_type(pair: Pair<Rule>) -> VarType {
    let inner = pair.clone().into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::simpleType => match pair.as_str() {
            "i8" => VarType::Int8,
            "u8" => VarType::UInt8,
            "i16" => VarType::Int16,
            "u16" => VarType::UInt16,
            "i32" => VarType::Int32,
            "u32" => VarType::UInt32,
            "i64" => VarType::Int64,
            "u64" => VarType::UInt64,
            "bool" => VarType::Boolean,
            "f32" => VarType::Float32,
            "f64" => VarType::Float64,
            name => match ST.lock().unwrap().symbol_type(name) {
                Ok(t) => t,
                Err(e) => {
                    e.lines(pair.as_str())
                     .location(pair.as_span().start_pos().line_col().0)
                     .send()
                     .unwrap();
                    VarType::Unknown
                },
            },
        },
        Rule::arrayType => {
            let mut inner = inner.into_inner();
            VarType::Array {
                inner: Box::new(parse_var_type(inner.next().unwrap())),
                len: match inner.next().unwrap().as_str().parse() {
                    Ok(v) => v,
                    Err(_) => {
                        LogMesg::err()
                            .name("Wrong value")
                            .cause("Invalid length for array, only natural numbers are allowed")
                            .lines(pair.as_str())
                            .send()
                            .unwrap();
                        0
                    }
                },
            }
        }
        Rule::refType => {
            VarType::Ref(Box::new(parse_var_type(inner.into_inner().next().unwrap())))
        },
        _ => unreachable!(),
    }
}

