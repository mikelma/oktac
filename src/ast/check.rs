use ordered_float::OrderedFloat;
use std::convert::TryInto;

use super::*;
use crate::{LogMesg, VarType, current_unit_st};

/// Checks if the left and right types are compatible considering the binary operator.
/// If types are not compatible, the function returns an error containing the name and
/// the cause of the error.
pub fn binop_resolve_types(
    l: &VarType,
    r: &VarType,
    op: &BinaryOp,
) -> Result<VarType, LogMesg> {
    if *l == VarType::Unknown || *r == VarType::Unknown {
        return Ok(VarType::Unknown);
    }

    // check for boolean operations
    if op.is_bool() {
        // only values of the same type can be conpared
        if l == r {
            Ok(VarType::Boolean)
        } else {
            Err(LogMesg::err()
                .name("Mismatched types")
                .cause(format!(
                    "values of different types cannot be compared, left is {:?} and right is {:?}",
                    l, r
                )))
        }
    } else {
        // arithmetic operations
        // TODO: Replace `match` with `if`
        match (l, r) {
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
                .cause(format!("left is {:?} and right is {:?}", l, r))),
        }
    }
}

/// Calculates the type of the unary operation. The function returns an error if the operation and
/// the type the operation is applied to are not compatible.
pub fn unop_resolve_type(ty: &VarType, op: &UnaryOp) -> Result<VarType, LogMesg> {
    let error = || {
        LogMesg::err()
            .name("Mismatched types".into())
            .cause(format!("Cannot apply {:?} operator to {:?} type", op, ty))
    };
    match op {
        UnaryOp::Not => match ty {
            VarType::Boolean => Ok(VarType::Boolean),
            _ => Err(error()),
        },
        UnaryOp::Deref => match ty {
            VarType::Ref(deref_ty) => Ok(*deref_ty.clone()),
            _ => Err(error()),
        },
        // operations that work with every type
        UnaryOp::Reference => Ok(VarType::Ref(Box::new(ty.clone()))),
    }
}

/// Checks if the given types are equal, if not, it returns a `LogMesg` error containing the name
/// of the error and the cause. If the given type is `VarType::Unknown` the function returns an
/// `Ok(())` in order to avoid cascading errors.
pub fn expect_type(expected: VarType, ty: &VarType) -> Result<(), LogMesg> {
    if expected == *ty || expected == VarType::Unknown || *ty == VarType::Unknown {
        Ok(())
    } else {
        Err(LogMesg::err()
            .name("Mismatched types")
            .cause(format!(
                "Expected {:?} type, got {:?} type instead",
                expected, ty
            )))
    }
}

/// Given an `AstNode` returns it's `VarType`. However, if there is an expected type for the node,
/// and the node is a literal value, automatic type conversions can be applied to tranform the
/// literal to the expected type value.
pub fn node_type(
    node: AstNode,
    expect: Option<VarType>,
) -> (AstNode, Result<VarType, LogMesg>) {
    let node_ty = match get_node_type_no_autoconv(&node) {
        Ok(ty) => ty,
        Err(e) => return (node, Err(e)),
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
                AstNode::Float32(v) => (AstNode::Float64(OrderedFloat::from(v.into_inner() as f64)), Ok(expected)),
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
            Ok(Some(ty)) => Ok(ty.clone()),
            Ok(None) => Ok(VarType::Unknown),
            Err(e) => Err(e),
        },
        AstNode::FunCall { name, .. } => match current_unit_st!().search_fun(name) {
            Ok(Some((ty, _))) => match ty {
                Some(t) => Ok(t),
                None => todo!(),
            },
            Ok(None) => Ok(VarType::Unknown),
            Err(e) => Err(e),
        },
        AstNode::Strct { name, .. } => Ok(VarType::Struct(name.into())),
        AstNode::MemberAccessExpr { member_ty, .. } => Ok(member_ty.clone()),
        AstNode::EnumVariant { enum_name, .. } => Ok(VarType::Enum(enum_name.clone())),
        _ => {
            println!("Panic was caused by: {:?}", node);
            unreachable!();
        }
    }
}
