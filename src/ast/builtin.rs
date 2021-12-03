use console::style;

use crate::*;
use ast::check;

pub fn check_builtin_fun_call(name: &str, params: &[AstNode]) -> Result<(), LogMesg> {
    match name {
        "@sizeof" => sizeof(params),
        "@bitcast" => bitcast(params),
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
        "@sizeof" => Some(VarType::UInt16),
        "@bitcast" => match &params[1] {
            AstNode::Type(t) => Some(t.clone()),
            _ => unreachable!(),
        },
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
        _ => return Err(LogMesg::err().name("Invalid parameter").cause(format!(
            "Builtin function {} expects a \
                           type as second parameter, but got a value instead",
            style("@sizeof").bold()
        ))),
    };

    if lty.size() != rty.size() {
        Err(LogMesg::err().name("Invalid parameters").cause(format!(
            "Builtin function {} expects the types of both arguments to be the same bit size.\
            but left is {} and right {} bits width.",
            style("@sizeof").bold(), lty.size(), rty.size())))
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
                style("@sizeof").bold(),
                real_params,
                actual_params
            ))
            .help(format!(
                "Consider removing {} parameters from {}'s call",
                real_params - actual_params,
                style(fn_name).bold()
            )));
    }

    Ok(())
}
