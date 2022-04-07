use pest::iterators::Pair;
use pest::Parser;

use super::{expr, parser::*, strct, ty_enum};
use crate::{current_unit_st, AstNode, LogMesg, VarType};

pub fn parse_value_or_type(pair: Pair<Rule>) -> AstNode {
    let inner = pair.into_inner().next().unwrap();

    match inner.as_rule() {
        Rule::strct => strct::parse_struct_value(inner),
        Rule::enm => ty_enum::parse_enum_value(inner, false),
        Rule::varType => {
            let str_val = inner.as_str().to_string();
            match parse_var_type(inner) {
                Ok(ty) => AstNode::Type(ty),
                Err(_) => {
                    let mut parsed = PestParser::parse(Rule::expr, &str_val).unwrap();
                    let pair = parsed.next().unwrap();
                    expr::parse_expr(pair)
                }
            }
        }
        _ => expr::parse_expr(inner),
    }
}

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
        Rule::sliceType => parse_slice_ty(inner),
        Rule::funType => parse_function_ty(inner),
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
        "str" => Ok(VarType::Str),
        "c_voidptr" => Ok(VarType::CVoidRef),
        name => {
            if current_unit_st!().is_type(name) {
                current_unit_st!().symbol_type(name)
            } else {
                Err(LogMesg::err().name("Undefined type".into()).cause(format!(
                    "{} is not a valid type \
                                       or it is not declared",
                    name
                )))
            }
        }
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

pub fn parse_slice_ty(pair: Pair<Rule>) -> Result<VarType, LogMesg> {
    let inner = pair.into_inner().next().unwrap();
    Ok(VarType::Slice(Box::new(parse_var_type(inner)?)))
}

pub fn parse_function_ty(pair: Pair<Rule>) -> Result<VarType, LogMesg> {
    let mut param_ty = vec![];
    let mut ret_ty = None;

    for pair in pair.into_inner() {
        match pair.as_rule() {
            Rule::varType => param_ty.push(parse_var_type(pair)?),
            Rule::funTypeRet => {
                ret_ty = Some(Box::new(parse_var_type(pair.into_inner().next().unwrap())?));
            }
            _ => unreachable!(),
        };
    }
    Ok(VarType::Fun { param_ty, ret_ty })
}
