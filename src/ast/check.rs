use console::style;
use ordered_float::OrderedFloat;
use std::convert::TryInto;

use super::*;
use crate::{current_unit_st, LogMesg, VarType};

/// Checks if the left and right types are compatible considering the binary operator.
/// If types are not compatible, the function returns an error containing the name and
/// the cause of the error.
pub fn binop_resolve_types(l: &VarType, r: &VarType, op: &BinaryOp) -> Result<VarType, LogMesg> {
    if *l == VarType::Unknown || *r == VarType::Unknown {
        return Ok(VarType::Unknown);
    }

    // resolve alias types if needed
    let l = l.resolve_alias();
    let r = r.resolve_alias();

    // check for boolean operations
    if op.is_bool() {
        // only values of the same type can be conpared
        if l == r {
            Ok(VarType::Boolean)
        } else {
            Err(LogMesg::err().name("Mismatched types").cause(format!(
                "values of different types cannot be \
                               compared, left is {} and right is {}",
                l, r
            )))
        }
    } else if op.is_bitwise_op() {
        if l.is_int() && r.is_int() {
            if op.is_shift() {
                if l.size() == r.size() {
                    Ok(l.clone())
                } else {
                    Err(LogMesg::err().name("Mismatched types").cause(format!(
                        "Shift operation expects types to be of the same bit width, \
                        but got types {} and {}",
                        l, r
                    )))
                }
            } else if l == r {
                // for bitwise or, xor, and
                Ok(l.clone())
            } else {
                Err(LogMesg::err().name("Mismatched types").cause(format!(
                    "Bitwise operation expects same types \
                                   of numbers, got {} and {}",
                    l, r
                )))
            }
        } else {
            Err(LogMesg::err().name("Mismatched types").cause(format!(
                "Bitwise operation between non \
                               integer types is unsupported: {} and {}",
                l, r
            )))
        }
    } else {
        // arithmetic operations
        // TODO: Replace `match` with `if`
        match (&l, &r) {
            (_, VarType::Unknown) => Ok(VarType::Unknown),
            (VarType::Unknown, _) => Ok(VarType::Unknown),
            (VarType::Int8, VarType::Int8) => Ok(VarType::Int8),
            (VarType::UInt8, VarType::UInt8) => Ok(VarType::UInt8),
            (VarType::Int16, VarType::Int16) => Ok(VarType::Int16),
            (VarType::UInt16, VarType::UInt16) => Ok(VarType::UInt16),
            (VarType::Int32, VarType::Int32) => Ok(VarType::Int32),
            (VarType::UInt32, VarType::UInt32) => Ok(VarType::UInt32),
            (VarType::Int64, VarType::Int64) => Ok(VarType::Int64),
            (VarType::UInt64, VarType::UInt64) => Ok(VarType::UInt64),
            (VarType::Float32, VarType::Float32) => Ok(VarType::Float32),
            (VarType::Float64, VarType::Float64) => Ok(VarType::Float64),
            (VarType::Boolean, VarType::Boolean) => Err(LogMesg::err()
                .name("Mismatched types".into())
                .cause(format!("cannot apply operator {:?} to booleans", op))),
            _ => Err(LogMesg::err()
                .name("Mismatched types".into())
                .cause(format!("left is {} and right is {}", l, r))),
        }
    }
}

/// Calculates the type of the unary operation. The function returns an error if the operation and
/// the type the operation is applied to are not compatible.
pub fn unop_resolve_type(ty: &VarType, op: &UnaryOp) -> Result<VarType, LogMesg> {
    let error = || {
        LogMesg::err()
            .name("Mismatched types".into())
            .cause(format!("Cannot apply {:?} operator to {} type", op, ty))
    };

    // resolve alias types if needed
    let ty = ty.resolve_alias();

    match op {
        UnaryOp::Not => match ty {
            VarType::Boolean => Ok(VarType::Boolean),
            _ => Err(error()),
        },
        UnaryOp::Deref => match ty {
            VarType::Ref(deref_ty) => Ok(*deref_ty.clone()),
            _ => Err(error()),
        },
        UnaryOp::Minus => {
            if ty.is_signed() {
                Ok(ty.clone())
            } else {
                Err(error())
            }
        }
        UnaryOp::BinaryNot => {
            if ty.is_int() {
                Ok(ty.clone())
            } else {
                Err(error())
            }
        }
        // operations that work with every type
        UnaryOp::Reference => Ok(VarType::Ref(Box::new(ty.clone()))),
    }
}

/// Check if the given types are equal or compatible, if not, return a `LogMesg` error
/// containing the name of the error and the cause. If one of the types is `VarType::Unknown`
/// the function returns an `Ok(())` in order to avoid cascading errors.
pub fn expect_type(expected: VarType, ty: &VarType) -> Result<(), LogMesg> {
    // resove alias types if needed
    let expected = match expected {
        VarType::Alias { .. } => expected.resolve_alias(),
        _ => expected,
    };

    let ty = match ty {
        VarType::Alias { .. } => ty.resolve_alias(),
        _ => ty.clone(),
    };

    if expected == ty || expected == VarType::Unknown || ty == VarType::Unknown {
        Ok(())
    } else {
        Err(LogMesg::err().name("Mismatched types").cause(format!(
            "Expected {} type, got {} type instead",
            expected, ty
        )))
    }
}

/// Given an `AstNode` returns it's `VarType`. However, if there is an expected type for the node,
/// and the node is a literal value, automatic type conversions can be applied to tranform the
/// literal to the expected type value.
pub fn node_type(node: AstNode, expect: Option<VarType>) -> (AstNode, Result<VarType, LogMesg>) {
    let node_ty = match get_node_type_no_autoconv(&node) {
        Ok(ty) => ty,
        Err(e) => return (node, Err(e)),
    };

    // resolve alias type if needed
    let expect = match expect {
        Some(ty) => Some(match ty {
            VarType::Alias { .. } => ty.resolve_alias(),
            _ => ty,
        }),
        None => None,
    };

    // if some type was expected and the node is a literal value, try to convert the literal to the
    // expected type. If conversio is not sucsessfull, return the original type of the node
    if let Some(expected) = expect {
        match expected {
            VarType::Array {
                inner: ref exp_inner_ty,
                ..
            } => match &node {
                AstNode::Array {
                    values: arr_elems,
                    ty: _,
                    is_const,
                } => {
                    let inner_ty = match node_ty {
                        VarType::Array { inner, .. } => inner,
                        _ => unreachable!(),
                    };
                    match *inner_ty {
                        // this case occurs when the array is declared empty (see `parse_value`)
                        VarType::Unknown => (
                            AstNode::Array {
                                values: vec![],
                                ty: *exp_inner_ty.clone(),
                                is_const: *is_const,
                            },
                            Ok(VarType::Array {
                                inner: exp_inner_ty.clone(),
                                len: 0,
                            }),
                        ),
                        _ => {
                            // if the array contains one or more values
                            // extract the type of the first element
                            let (first, new_ty) = match node_type(
                                arr_elems[0].clone(),
                                Some(*exp_inner_ty.clone()),
                            ) {
                                (f, Ok(t)) => (f, t),
                                (_, Err(e)) => {
                                    return (
                                        AstNode::Array {
                                            values: arr_elems.to_vec(),
                                            ty: VarType::Unknown,
                                            is_const: *is_const,
                                        },
                                        Err(e),
                                    );
                                }
                            };

                            let mut new_elems = vec![first]; // list of the new elements of the array
                                                             // skip the first element, as it's type has already been checked
                            for elem in arr_elems.iter().skip(1).cloned() {
                                // try to get the type of the element node
                                new_elems.push(match node_type(elem, Some(new_ty.clone())) {
                                    // all the elements inside the array must have the same type
                                    (elem, Ok(e_ty)) => if e_ty == new_ty {
                                        elem
                                    } else {
                                        return (AstNode::Array { values: arr_elems.to_vec(), ty: VarType::Unknown, is_const: *is_const },
                                            Err(LogMesg::err()
                                            .name("Mismatched types".into())
                                            .cause("All array elements must have the same type".into())));
                                    },
                                    (_, Err(e)) => {
                                        return (AstNode::Array { values: arr_elems.to_vec(), ty: VarType::Unknown, is_const: *is_const }, Err(e));
                                    },
                                });
                            }
                            let len = new_elems.len();
                            (
                                AstNode::Array {
                                    values: new_elems,
                                    ty: new_ty.clone(),
                                    is_const: *is_const,
                                },
                                Ok(VarType::Array {
                                    inner: Box::new(new_ty),
                                    len,
                                }),
                            )
                        }
                    }
                }
                _ => (node, Ok(node_ty)),
            },
            VarType::UInt8 => match node {
                AstNode::Int8(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int8)),
                },
                AstNode::Int16(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int16)),
                },
                AstNode::UInt16(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt16)),
                },
                AstNode::UInt32(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt32)),
                },
                AstNode::Int32(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int32)),
                },
                AstNode::Int64(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int64)),
                },
                AstNode::UInt64(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt64)),
                },
                _ => (node, Ok(node_ty)),
            },
            VarType::Int8 => match node {
                AstNode::UInt8(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt8)),
                },
                AstNode::Int16(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int16)),
                },
                AstNode::UInt16(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt16)),
                },
                AstNode::UInt32(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt32)),
                },
                AstNode::Int32(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int32)),
                },
                AstNode::Int64(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int64)),
                },
                AstNode::UInt64(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int8(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt64)),
                },
                _ => (node, Ok(node_ty)),
            },
            VarType::Int32 => match node {
                AstNode::Int8(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int8)),
                },
                AstNode::UInt8(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt8)),
                },
                AstNode::Int16(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int16)),
                },
                AstNode::UInt16(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt16)),
                },
                AstNode::UInt32(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt32)),
                },
                AstNode::Int64(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int64)),
                },
                AstNode::UInt64(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt64)),
                },
                _ => (node, Ok(node_ty)),
            },
            VarType::UInt32 => match node {
                AstNode::Int8(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int8)),
                },
                AstNode::UInt8(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt8)),
                },
                AstNode::Int16(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int16)),
                },
                AstNode::UInt16(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt16)),
                },
                AstNode::Int32(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int32)),
                },
                AstNode::Int64(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int64)),
                },
                AstNode::UInt64(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt32(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt64)),
                },
                _ => (node, Ok(node_ty)),
            },
            VarType::Int64 => match node {
                AstNode::Int8(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int8)),
                },
                AstNode::UInt8(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt8)),
                },
                AstNode::Int16(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int16)),
                },
                AstNode::UInt16(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt16)),
                },
                AstNode::Int32(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int32)),
                },
                AstNode::UInt32(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int64)),
                },
                AstNode::UInt64(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt64)),
                },
                _ => (node, Ok(node_ty)),
            },
            VarType::UInt64 => match node {
                AstNode::Int8(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int8)),
                },
                AstNode::UInt8(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt8)),
                },
                AstNode::Int16(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int16)),
                },
                AstNode::UInt16(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt16)),
                },
                AstNode::Int32(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int32)),
                },
                AstNode::UInt32(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt32)),
                },
                AstNode::Int64(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt64(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int64)),
                },
                _ => (node, Ok(node_ty)),
            },
            VarType::Int16 => match node {
                AstNode::Int8(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int8)),
                },
                AstNode::UInt8(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt8)),
                },
                AstNode::UInt16(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt16)),
                },
                AstNode::Int32(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int32)),
                },
                AstNode::UInt32(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt32)),
                },
                AstNode::Int64(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int64)),
                },
                AstNode::UInt64(v) => match v.try_into() {
                    Ok(val) => (AstNode::Int16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt64)),
                },
                _ => (node, Ok(node_ty)),
            },
            VarType::UInt16 => match node {
                AstNode::Int8(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int8)),
                },
                AstNode::UInt8(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt8)),
                },
                AstNode::Int32(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int32)),
                },
                AstNode::UInt32(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt32)),
                },
                AstNode::Int64(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::Int64)),
                },
                AstNode::UInt64(v) => match v.try_into() {
                    Ok(val) => (AstNode::UInt16(val), Ok(expected)),
                    Err(_) => (node, Ok(VarType::UInt64)),
                },
                _ => (node, Ok(node_ty)),
            },
            VarType::Float64 => match node {
                AstNode::Float32(v) => (
                    AstNode::Float64(OrderedFloat::from(v.into_inner() as f64)),
                    Ok(expected),
                ),
                _ => (node, Ok(node_ty)),
            },
            _ => (node, Ok(node_ty)),
        }
    } else {
        (node, Ok(node_ty))
    }
}

/// Extracts the `VarType` of a given `AstNode`.
///
/// NOTE: This function does not apply any automatic literal type conversion,
/// you might want to call `node_type` function instead.
pub fn get_node_type_no_autoconv(node: &AstNode) -> Result<VarType, LogMesg> {
    match node {
        AstNode::BinaryExpr { expr_ty, .. } => Ok(expr_ty.clone()),
        AstNode::UnaryExpr { expr_ty, .. } => Ok(expr_ty.clone()),
        AstNode::Int8(_) => Ok(VarType::Int8),
        AstNode::UInt8(_) => Ok(VarType::UInt8),
        AstNode::Int16(_) => Ok(VarType::Int16),
        AstNode::UInt16(_) => Ok(VarType::UInt16),
        AstNode::Int32(_) => Ok(VarType::Int32),
        AstNode::Int64(_) => Ok(VarType::Int64),
        AstNode::UInt32(_) => Ok(VarType::UInt32),
        AstNode::Float32(_) => Ok(VarType::Float32),
        AstNode::Float64(_) => Ok(VarType::Float64),
        AstNode::Boolean(_) => Ok(VarType::Boolean),
        AstNode::Array { values, ty, .. } => Ok(VarType::Array {
            inner: Box::new(ty.clone()),
            len: values.len(),
        }),
        AstNode::Identifyer(id) => match current_unit_st!().search_var(id) {
            Ok(ty) => Ok(ty.0.clone()),
            Err(e) => Err(e),
        },
        AstNode::FunCall { name, ret_ty, .. } => match ret_ty {
            Some(t) => Ok(t.clone()),
            None => Err(LogMesg::err().name("Expected value").cause(format!(
                "Expected value but function {} \
                       has no return type",
                style(name).bold()
            ))),
        },
        AstNode::Strct { name, .. } => Ok(VarType::Struct(name.into())),
        AstNode::MemberAccessExpr { access_types, .. } => Ok(access_types.last().unwrap().clone()),
        AstNode::EnumVariant { enum_name, .. } => Ok(VarType::Enum(enum_name.clone())),
        AstNode::String(_) => Ok(VarType::Str),
        AstNode::Type(ty) => Err(LogMesg::err()
            .name("Expected value")
            .cause(format!("Expected value but got type {} instead", ty))),
        _ => {
            println!("Panic was caused by: {:?}", node);
            unreachable!();
        }
    }
}

pub fn check_function_call_arguments(
    fn_name: &str,
    call_params: &mut [AstNode],
    real_params: &[VarType],
    variadic: bool,
) -> Result<(), LogMesg> {
    if (call_params.len() > real_params.len()) && !variadic {
        return Err(LogMesg::err().name("Too many parameters").cause(format!(
            "Too many arguments for {} function, expected {} but got {}",
            style(fn_name).bold(),
            real_params.len(),
            call_params.len(),
        )));
    }

    if call_params.len() < real_params.len() {
        return Err(LogMesg::err()
            .name("Missing parameters".into())
            .cause(format!(
                "Function {} expects {} parameters, but got {}",
                style(fn_name).bold(),
                real_params.len(),
                call_params.len(),
            )));
    }

    // check the types of the given nodes and the real types the function expects
    for (i, call_param) in call_params.iter_mut().enumerate() {
        let real_ty = match real_params.get(i) {
            Some(t) => t.clone(),
            None => VarType::Unknown, // `call_param` is variadic and does not exist a "correct" `real_ty` for it
        };

        // first check the type of the call param (it could contain an error, if that's the case, return it)
        let (new_call_param, call_param_ty) =
            match check::node_type(call_param.clone(), Some(real_ty.clone())) {
                (node, Ok(ty)) => (node, ty),
                (_, Err(e)) => return Err(e),
            };

        *call_param = new_call_param;

        // check if the function call parameter's type and the
        // actual function argument type match
        check::expect_type(real_ty, &call_param_ty)?;
    }

    Ok(())
}

/// If the given expression depends on a constant variable the function returns a `Some` variant
/// including the name if the constant variable it depends on, else `None`.
///
/// NOTE: This function expects to be called when checking the left hand values of assigment
/// statements, and it only considers `AstNode` varinats relevant to this type of statement.
pub fn check_depends_on_constant_value(node: &AstNode) -> Option<String> {
    match node {
        AstNode::MemberAccessExpr { parent, .. } => check_depends_on_constant_value(parent),
        AstNode::UnaryExpr { value, .. } => check_depends_on_constant_value(value),
        AstNode::Identifyer(id) => match current_unit_st!().search_var(id) {
            Ok((_, is_const)) => {
                if is_const {
                    Some(id.to_string())
                } else {
                    None
                }
            }
            Err(_) => None,
        },
        _ => None,
    }
}
