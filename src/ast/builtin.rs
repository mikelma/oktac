use console::style;

use crate::*;
use ast::check;

pub fn check_builtin_fun_call(name: &str, params: &[AstNode]) -> Result<(), LogMesg> {
    match name {
        "@sizeof" => sizeof(params),
        "@bitcast" => bitcast(params),
        "@cstr" => cstr(params),
        "@slice" => slice(params),
        "@len" => len(params),
        "@inttoptr" => inttoptr(params),
        _ => Err(LogMesg::err().name("Undfined function").cause(format!(
            "Builtin function {} does not exist",
            style(name).bold()
        ))),
    }
}

/// NOTE: This function assumes that `check_builtin_fun_call` function has been called on the same
/// function before hand. Else, this function will panic if the passed function does not exist or
/// if there is an error getting the type of a parameter.
pub fn builtin_func_return_ty(name: &str, params: &[AstNode]) -> Option<VarType> {
    match name {
        "@sizeof" => Some(VarType::UInt64),
        "@bitcast" | "@inttoptr" => match &params[1] {
            AstNode::Type(t) => Some(t.clone()),
            _ => unreachable!(),
        },
        "@cstr" => Some(VarType::Ref(Box::new(VarType::UInt8))),
        "@slice" => match check::node_type(params[0].clone(), None).1 {
            Ok(ty) => match ty {
                VarType::Ref(inner_ty) => Some(VarType::Slice(inner_ty)),
                _ => unreachable!(),
            },
            Err(_) => return Some(VarType::Unknown),
        },
        "@len" => Some(VarType::UInt64),
        _ => unreachable!(), // see the comment above this function's prototype
    }
}

/// This builtin function expects a single parameter, containing an okta type.
///
/// It's prototype is the following:
///     `fun sizeof(type): u32`
fn sizeof(params: &[AstNode]) -> Result<(), LogMesg> {
    check_num_params("@sizeof", params.len(), 1)?;

    if !matches!(params[0], AstNode::Type(_)) {
        return Err(LogMesg::err().name("Invalid parameter").cause(format!(
            "Builtin function {} expects a \
                           type argument, but got a value instead",
            style("@sizeof").bold()
        )));
    }

    Ok(())
}

/// A bitcast reinterprets the bits of one value into a value of another type which has the same bit width.
///
/// # Example
/// ```
/// let a: i8 = 10;
/// @bitcast(&a, &16);
/// ```
fn bitcast(params: &[AstNode]) -> Result<(), LogMesg> {
    check_num_params("@bitcast", params.len(), 2)?;

    // if this function returns no error, the parameter is a value
    let lty = check::node_type(params[0].clone(), None).1?;

    let rty = match &params[1] {
        AstNode::Type(t) => t,
        _ => {
            return Err(LogMesg::err().name("Invalid parameter").cause(format!(
                "Builtin function {} expects a \
                           type as second parameter, but got a value instead",
                style("@sizeof").bold()
            )))
        }
    };

    if lty.size() != rty.size() {
        Err(LogMesg::err().name("Invalid parameters").cause(format!(
            "Builtin function {} expects the types of both arguments to be the same bit size.\
            but left is {} and right {} bits width.",
            style("@sizeof").bold(),
            lty.size(),
            rty.size()
        )))
    } else {
        Ok(())
    }
}

/// Convert an okta `str` to C language's str (null terminated `char*`)
fn cstr(params: &[AstNode]) -> Result<(), LogMesg> {
    check_num_params("@cstr", params.len(), 1)?;

    let ty = check::node_type(params[0].clone(), Some(VarType::Str)).1?;

    if ty != VarType::Str {
        Err(LogMesg::err().name("Invalid parameter").cause(format!(
            "Builtin function {} expects a {} as a parameter, got {}",
            style("@cstr").bold(),
            style("str").bold(),
            style(ty).bold()
        )))
    } else {
        Ok(())
    }
}

fn check_num_params(
    fn_name: &str,
    actual_params: usize,
    real_params: usize,
) -> Result<(), LogMesg> {
    if actual_params > real_params {
        return Err(LogMesg::err()
            .name("Too many parameters")
            .cause(format!(
                "Too many arguments for {} function call",
                style("@sizeof").bold()
            ))
            .help(format!(
                "Function {} only takes {} parameters",
                style(fn_name).bold(),
                real_params
            )));
    }

    if real_params > actual_params {
        return Err(LogMesg::err()
            .name("Missing parameters")
            .cause(format!(
                "Function {} expects {} parameters but {} were provided",
                style(fn_name).bold(),
                real_params,
                actual_params
            ))
            .help(format!(
                "Consider providing {} parameters to {}'s call",
                real_params - actual_params,
                style(fn_name).bold()
            )));
    }

    Ok(())
}

/// Returns a slice from the pointer and length arguments given as input.
fn slice(params: &[AstNode]) -> Result<(), LogMesg> {
    check_num_params("@slice", params.len(), 2)?;

    // check first argument type
    let ty = check::node_type(params[0].clone(), None).1?;

    if !matches!(ty, VarType::Ref(_)) {
        return Err(LogMesg::err().name("Invalid parameter").cause(format!(
            "Builtin function {} expects reference as first argument but got {} instead",
            style("@slice").bold(),
            ty
        )));
    }

    // check second argument type
    let ty = check::node_type(params[1].clone(), Some(VarType::UInt64)).1?;

    if ty != VarType::UInt64 {
        return Err(LogMesg::err().name("Invalid parameter").cause(format!(
            "Builtin function {} expects {} as second argument but got {} instead",
            style("@slice").bold(),
            VarType::UInt64,
            ty
        )));
    }

    Ok(())
}

/// Returns the length of the array/slice given as the input.
fn len(params: &[AstNode]) -> Result<(), LogMesg> {
    check_num_params("@len", params.len(), 1)?;

    // check the type of the argument
    let ty = check::node_type(params[0].clone(), None).1?;

    if !matches!(ty, VarType::Array { .. } | VarType::Slice(_)) {
        return Err(LogMesg::err().name("Invalid parameter").cause(format!(
            "Builtin function {} expects an argument of array or slice type, but got {} instead",
            style("@len").bold(),
            ty
        )));
    } else {
        Ok(())
    }
}

/// Converts an integer value to a pointer of a specified type.
fn inttoptr(params: &[AstNode]) -> Result<(), LogMesg> {
    check_num_params("@inttoptr", params.len(), 2)?;

    let val_ty = check::node_type(params[0].clone(), None).1?;

    if !val_ty.is_int() {
        return Err(LogMesg::err().name("Invalid parameter").cause(format!(
            "Builtin function {} expects an integer value as first parameter, got {}",
            style("@inttoptr").bold(),
            style(val_ty).bold(),
        )));
    }

    match &params[1] {
        AstNode::Type(t) if t.is_ref() => Ok(()),
        _ => Err(LogMesg::err().name("Invalid parameter").cause(format!(
            "Builtin function {} expects a ponter type \
                           as second parameter",
            style("@inttoptr").bold()
        ))),
    }
}
