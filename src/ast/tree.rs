use console::Style;
use once_cell::sync::Lazy;
use ordered_float::OrderedFloat;
use ptree::TreeItem;
use serde::{Deserialize, Serialize};

use std::borrow::Cow;
use std::io;
use std::{fmt, path::PathBuf};

use crate::{current_unit_st, VarType};

#[derive(Debug, Clone, PartialEq, Hash, Serialize, Deserialize)]
pub enum AstNode {
    // prototypes
    StructProto {
        name: String,
        visibility: Visibility,
        members: Vec<(String, VarType)>,

        // codegen options:
        packed: bool,
    },
    FuncProto {
        name: String,
        visibility: Visibility,
        ret_type: Option<VarType>,
        params: Vec<(String, VarType)>,

        // codegen options:
        inline: bool,
    },
    ExternFuncProto {
        name: String,
        ret_type: Option<VarType>,
        param_types: Vec<VarType>,
        variadic: bool,
        visibility: Visibility,
    },
    EnumProto {
        name: String,
        visibility: Visibility,
        variants: Vec<(String, Vec<(String, VarType)>)>,
        is_simple: bool,
    },
    AliasProto {
        name: String,
        visibility: Visibility,
        ty: VarType, // real type of the alias
    },

    UseModule(PathBuf),
    Macro {
        id: String,
        visibility: Visibility,
    },

    ConstVarDecl {
        name: String,
        visibility: Visibility,
        ty: VarType,
        value: Box<AstNode>,
        dependencies: Vec<String>,
    },
    FuncDecl {
        name: String,
        visibility: Visibility,
        ret_type: Option<VarType>,
        params: Vec<(String, VarType)>,
        stmts: Box<AstNode>,
    },

    Stmts(Vec<AstNode>),
    MacroResult {
        id: String, // macro name
        stmts: Vec<AstNode>,
    },

    VarDeclStmt {
        id: String,
        var_type: VarType,
        value: Box<AstNode>,
    },
    AssignStmt {
        left: Box<AstNode>,
        right: Box<AstNode>,
    },
    IfStmt {
        cond: Box<AstNode>,
        then_b: Box<AstNode>,
        elif_b: Vec<(AstNode, AstNode)>,
        else_b: Option<Box<AstNode>>,
    },
    IfLetStmt {
        l_enum: Box<AstNode>,
        r_expr: Box<AstNode>,
        then_b: Box<AstNode>,
        else_b: Option<Box<AstNode>>,
    },
    ReturnStmt(Box<AstNode>),
    LoopStmt(Box<AstNode>), // contains an AstNode::Stmts variant inside
    BreakStmt,
    // valued expressions
    BinaryExpr {
        left: Box<AstNode>,
        op: BinaryOp,
        right: Box<AstNode>,
        expr_ty: VarType,
        vars_ty: VarType,
    },
    UnaryExpr {
        op: UnaryOp,
        value: Box<AstNode>,
        expr_ty: VarType,
        var_ty: VarType,
    },
    FunCall {
        name: String,
        params: Vec<AstNode>,
        // return type of the called function
        ret_ty: Option<VarType>,
        // `true` if the called function is function
        // that is builtin in the language itself
        builtin: bool,
    },
    MemberAccessExpr {
        parent: Box<AstNode>,
        members: Vec<MemberAccess>,
        // list of the type produced after each access. The last element of this list is the type
        // that will be produced after the expression is evaluated.
        access_types: Vec<VarType>,
        parent_ty: VarType, // type of the parent
    },

    // values
    Identifier(String), // TODO: Fix typo: Identifier
    Int8(i8),
    UInt8(u8),
    Int16(i16),
    UInt16(u16),
    Int32(i32),
    UInt32(u32),
    Int64(i64),
    UInt64(u64),
    Float32(OrderedFloat<f32>),
    Float64(OrderedFloat<f64>),
    Boolean(bool),
    /// The `Array` variant contains `values`, the elements of the array (can be an empty array)
    /// and `ty`, the `VarType` of the elements inside the array.
    Array {
        values: Vec<AstNode>,
        ty: VarType,
        is_const: bool,
    },
    Strct {
        name: String,
        members: Vec<(String, AstNode)>,
        is_const: bool,
    },
    EnumVariant {
        enum_name: String,
        variant_name: String,
        tag: usize,
        fields: Vec<(usize, VarType, AstNode)>,
        is_const: bool,
    },
    String(Vec<u8>),
    Lambda {
        name: String,
        ret_ty: Option<VarType>,
        stmts: Box<AstNode>,
        params: Vec<(String, VarType)>,
        captured_vars: Vec<(String, VarType)>,
    },
    /// `Type`s are only intended to be used as builtin function parameters
    Type(VarType),
}

impl AstNode {
    pub fn is_const(&self) -> bool {
        match self {
            AstNode::Int64(_)
            | AstNode::UInt64(_)
            | AstNode::Int32(_)
            | AstNode::UInt32(_)
            | AstNode::Int16(_)
            | AstNode::UInt16(_)
            | AstNode::Int8(_)
            | AstNode::UInt8(_)
            | AstNode::Boolean(_)
            | AstNode::Float32(_)
            | AstNode::Float64(_) => true,
            AstNode::Array { is_const, .. } => *is_const,
            // AstNode::Strct { is_const, .. } => *is_const,
            AstNode::Identifier(id) => match current_unit_st!().search_var(id) {
                Ok((_, is_const)) => is_const,
                Err(_e) => {
                    // NOTE: The error isn't reported here, it will be
                    // reported somewhere else
                    false
                }
            },
            // nodes of type `expr`
            AstNode::BinaryExpr { left, right, .. } => left.is_const() && right.is_const(),
            AstNode::MemberAccessExpr {
                parent, members, ..
            } => parent.is_const() && members.iter().all(|m| m.is_const()),
            AstNode::UnaryExpr { value, .. } => value.is_const(),
            // NOTE: For now, function call expression are always considered not to be constant
            _ => false,
        }
    }
}

// required by `petgraph::Graph::extend_with_edges`
impl Default for AstNode {
    fn default() -> Self {
        AstNode::Int8(0)
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Serialize, Deserialize)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    And,
    Or,
    Eq,
    Ne,
    Lt,
    Gt,
    Leq,
    Geq,
    BinaryAnd,
    BinaryOr,
    BinaryXor,
    ShiftLeft,
    ShiftRight,
}

impl BinaryOp {
    pub fn is_bool(&self) -> bool {
        matches!(
            self,
            BinaryOp::And
                | BinaryOp::Or
                | BinaryOp::Eq
                | BinaryOp::Ne
                | BinaryOp::Lt
                | BinaryOp::Gt
                | BinaryOp::Leq
                | BinaryOp::Geq
        )
    }

    pub fn is_bitwise_op(&self) -> bool {
        matches!(
            self,
            BinaryOp::BinaryOr
                | BinaryOp::BinaryAnd
                | BinaryOp::BinaryXor
                | BinaryOp::ShiftLeft
                | BinaryOp::ShiftRight
        )
    }

    pub fn is_shift(&self) -> bool {
        matches!(self, BinaryOp::ShiftLeft | BinaryOp::ShiftRight)
    }

    pub fn is_arithmetic(&self) -> bool {
        matches!(
            self,
            BinaryOp::Add
                | BinaryOp::Subtract
                | BinaryOp::Multiply
                | BinaryOp::Divide
                | BinaryOp::Modulo
        )
    }
}

#[derive(Debug, Clone, PartialEq, Hash, Serialize, Deserialize)]
pub enum UnaryOp {
    Not,
    Reference,
    Deref,
    Minus,
    BinaryNot,
}

#[derive(Debug, Clone, PartialEq, Hash, Serialize, Deserialize)]
pub enum Visibility {
    /// Public, the symbol will be visible from other modules.
    Pub,
    /// Private, the symbol is only visible from the module it
    /// was declared in. This is the default visibility for all symbols.
    Priv,
}

#[derive(Debug, Clone, PartialEq, Hash, Serialize, Deserialize)]
pub enum MemberAccess {
    Index(AstNode),
    MemberId(u32),
    Range {
        start: AstNode,
        end: Option<AstNode>,
    },
}

impl MemberAccess {
    pub fn is_const(&self) -> bool {
        match &self {
            MemberAccess::Index(node) => node.is_const(),
            MemberAccess::MemberId(_) => true,
            MemberAccess::Range { start, end } => {
                start.is_const() && end.as_ref().map_or(true, |v| v.is_const())
            }
        }
    }
}

impl fmt::Display for Visibility {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Visibility::Pub => write!(f, "pub"),
            Visibility::Priv => write!(f, "priv"),
        }
    }
}

const STYLE_PROTO: Lazy<Style> = Lazy::new(|| Style::new().bold().blue());
const STYLE_DECL: Lazy<Style> = Lazy::new(|| Style::new().bold().red());
const STYLE_STMT: Lazy<Style> = Lazy::new(|| Style::new().bold().cyan());
const STYLE_EXPR: Lazy<Style> = Lazy::new(|| Style::new().bold().magenta());
const STYLE_TERM: Lazy<Style> = Lazy::new(|| Style::new().bold());

impl TreeItem for AstNode {
    type Child = Self;

    fn write_self<W: io::Write>(&self, f: &mut W, _style: &ptree::Style) -> io::Result<()> {
        match self {
            AstNode::StructProto {
                name, visibility, ..
            } => write!(
                f,
                "{} {} {}",
                STYLE_PROTO.apply_to("StructProto"),
                visibility,
                name
            ),
            AstNode::EnumProto {
                name, visibility, ..
            } => write!(
                f,
                "{} {} {}",
                STYLE_PROTO.apply_to("EnumProto"),
                visibility,
                name
            ),
            AstNode::FuncProto {
                name,
                visibility,
                ret_type,
                params,
                ..
            } => {
                let params: Vec<String> = params.iter().map(|(_, v)| v.to_string()).collect();
                let mut m = format!(
                    "{} {} {}({})",
                    STYLE_PROTO.apply_to("FuncProto"),
                    visibility,
                    name,
                    params.join(", ")
                );

                if let Some(t) = ret_type {
                    m.push_str(format!(": {}", t).as_str());
                }

                write!(f, "{}", m)
            }
            AstNode::ExternFuncProto {
                name,
                visibility,
                ret_type,
                param_types,
                variadic,
            } => {
                let params: Vec<String> = param_types.iter().map(|v| v.to_string()).collect();

                let mut m = format!(
                    "{} {} {}({}",
                    STYLE_PROTO.apply_to("ExternFuncProto"),
                    visibility,
                    name,
                    params.join(", ")
                );

                if *variadic {
                    m.push_str("...");
                }

                m.push_str(")");

                if let Some(t) = ret_type {
                    m.push_str(format!(": {}", t).as_str());
                }

                write!(f, "{}", m)
            }
            AstNode::AliasProto {
                name,
                visibility,
                ty,
            } => write!(
                f,
                "{} {} {} = {}",
                STYLE_PROTO.apply_to("AliasProto"),
                visibility,
                name,
                ty
            ),
            AstNode::FuncDecl {
                name,
                visibility,
                ret_type,
                params,
                ..
            } => {
                let params: Vec<String> = params.iter().map(|(_, v)| v.to_string()).collect();

                let mut m = format!(
                    "{} {} {}({})",
                    STYLE_DECL.apply_to("FuncDecl"),
                    visibility,
                    name,
                    params.join(", ")
                );

                if let Some(t) = ret_type {
                    m.push_str(format!(": {}", t).as_str());
                }

                write!(f, "{}", m)
            }
            AstNode::ConstVarDecl {
                name,
                visibility,
                ty,
                ..
            } => {
                write!(
                    f,
                    "{} {} {} {}",
                    STYLE_DECL.apply_to("ConstVarDecl"),
                    visibility,
                    ty,
                    name,
                )
            }
            AstNode::Macro { id, visibility, .. } => {
                write!(f, "{} {} {}", STYLE_STMT.apply_to("Macro"), visibility, id)
            }

            AstNode::Stmts(_) => write!(f, "{}", STYLE_STMT.apply_to("Stmts")),
            AstNode::MacroResult { id, .. } => {
                write!(f, "{} {}", STYLE_STMT.apply_to("MacroResult"), id)
            }

            AstNode::VarDeclStmt { id, var_type, .. } => {
                write!(
                    f,
                    "{} {} {}",
                    STYLE_STMT.apply_to("VarDeclStmt"),
                    var_type,
                    id
                )
            }
            AstNode::AssignStmt { .. } => write!(f, "{}", STYLE_STMT.apply_to("AssignStmt")),
            AstNode::IfStmt { .. } => write!(f, "{}", STYLE_STMT.apply_to("IfStmt")),
            AstNode::IfLetStmt { .. } => write!(f, "{}", STYLE_STMT.apply_to("IfLetStmt")),
            AstNode::ReturnStmt(_) => write!(f, "{}", STYLE_STMT.apply_to("ReturnStmt")),
            AstNode::LoopStmt(_) => write!(f, "{}", STYLE_STMT.apply_to("LoopStmt")),
            AstNode::BreakStmt => write!(f, "{}", STYLE_STMT.apply_to("BreakStmt")),
            AstNode::BinaryExpr { op, .. } => {
                write!(f, "{} {:?}", STYLE_EXPR.apply_to("BinaryExpr"), op)
            }
            AstNode::UnaryExpr { op, .. } => {
                write!(f, "{} {:?}", STYLE_EXPR.apply_to("UnaryExpr"), op)
            }
            AstNode::FunCall { name, ret_ty, .. } => match ret_ty {
                Some(t) => write!(f, "{} {}:{}", STYLE_EXPR.apply_to("FunCall"), name, t),
                None => write!(f, "{} {}", STYLE_EXPR.apply_to("FunCall"), name),
            },
            AstNode::MemberAccessExpr { .. } => {
                write!(f, "{}", STYLE_EXPR.apply_to("MemberAccessExpr"))
            }
            AstNode::Array { .. } => write!(f, "{}", STYLE_TERM.apply_to("Array")),
            AstNode::Strct { name, .. } => write!(f, "{} {}", STYLE_TERM.apply_to("Strct"), name),
            AstNode::String(bytes) => write!(
                f,
                "{} {:?}",
                STYLE_TERM.apply_to("String"),
                String::from_utf8_lossy(bytes)
            ),
            AstNode::Lambda { ret_ty, params, .. } => {
                let params: Vec<String> = params.iter().map(|(_, v)| v.to_string()).collect();
                write!(
                    f,
                    "{} fun({})",
                    STYLE_TERM.apply_to("Lambda"),
                    params.join(","),
                )?;
                if let Some(t) = ret_ty {
                    write!(f, ":{}", t)?;
                }
                Ok(())
            }
            AstNode::EnumVariant {
                enum_name,
                variant_name,
                ..
            } => write!(
                f,
                "{} {}:{}",
                STYLE_TERM.apply_to("enumvariant"),
                enum_name,
                variant_name
            ),
            // for other terminal values (i8, str, u322...)
            _ => write!(f, "{}", STYLE_TERM.apply_to(format!("{:?}", self))),
        }
    }

    fn children(&self) -> Cow<[Self::Child]> {
        match self {
            AstNode::FuncDecl { stmts, .. } => Cow::from(vec![*stmts.clone()]),
            AstNode::ConstVarDecl { value, .. } => Cow::from(vec![*value.clone()]),
            AstNode::Stmts(s) => Cow::from(s),
            AstNode::MacroResult { stmts, .. } => Cow::from(stmts),
            AstNode::VarDeclStmt { value, .. } => Cow::from(vec![*value.clone()]),
            AstNode::AssignStmt { left, right } => Cow::from(vec![*left.clone(), *right.clone()]),
            AstNode::IfStmt {
                cond,
                then_b,
                elif_b,
                else_b,
            } => {
                let mut v = vec![*cond.clone(), *then_b.clone()];
                for (cond, block) in elif_b {
                    v.push(cond.clone());
                    v.push(block.clone());
                }

                if let Some(b) = else_b {
                    v.push(*b.clone());
                }
                Cow::from(v)
            }
            AstNode::IfLetStmt {
                l_enum,
                r_expr,
                then_b,
                else_b,
            } => {
                let mut v = vec![*l_enum.clone(), *r_expr.clone(), *then_b.clone()];
                if let Some(b) = else_b {
                    v.push(*b.clone());
                }
                Cow::from(v)
            }
            AstNode::ReturnStmt(inner) => Cow::from(vec![*inner.clone()]),
            AstNode::LoopStmt(inner) => Cow::from(vec![*inner.clone()]),
            AstNode::BinaryExpr { left, right, .. } => {
                Cow::from(vec![*left.clone(), *right.clone()])
            }
            AstNode::UnaryExpr { value, .. } => Cow::from(vec![*value.clone()]),
            AstNode::MemberAccessExpr {
                parent, members, ..
            } => {
                let mut v = vec![*parent.clone()];

                for memb in members {
                    match memb {
                        MemberAccess::Index(node) => v.push(node.clone()),
                        MemberAccess::Range { start, end } => {
                            v.push(start.clone());
                            if let Some(node) = end {
                                v.push(node.clone());
                            }
                        }
                        MemberAccess::MemberId(_) => (),
                    }
                }

                Cow::from(v)
            }
            AstNode::FunCall { params, .. } => Cow::from(params),
            AstNode::Array { values, .. } => Cow::from(values),
            AstNode::Strct { members, .. } => Cow::from(
                members
                    .iter()
                    .map(|(_, v)| v)
                    .cloned()
                    .collect::<Vec<AstNode>>(),
            ),
            AstNode::EnumVariant { fields, .. } => Cow::from(
                fields
                    .iter()
                    .map(|(_, _, v)| v)
                    .cloned()
                    .collect::<Vec<AstNode>>(),
            ),
            AstNode::Lambda { stmts, .. } => Cow::from(vec![*stmts.clone()]),
            _ => Cow::from(vec![]),
        }
    }
}
