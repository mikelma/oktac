use once_cell::sync::Lazy;
use ordered_float::OrderedFloat;
use console::{Style, style};
use ptree::TreeItem;

use std::{fmt, path::PathBuf};
use std::borrow::Cow;
use std::sync::Arc;
use std::io;

use crate::VarType;

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum AstNode {
    // prototypes
    StructProto {
        name: String,
        visibility: Visibility,
        members: Vec<(String, VarType)>,
    },
    FuncProto {
        name: String,
        visibility: Visibility,
        ret_type: Option<VarType>,
        params: Vec<(String, VarType)>,
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

    FuncDecl {
        name: String,
        visibility: Visibility,
        ret_type: Option<VarType>,
        params: Vec<(String, VarType)>,
        stmts: Box<AstNode>,
    },

    Stmts(Vec<AstNode>),

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

    // terminals
    Identifyer(String),
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
            AstNode::Strct { is_const, .. } => *is_const,
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

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    And,
    Or,
    Eq,
    Ne,
    Lt,
    Gt,
    Leq,
    Geq,
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
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum UnaryOp {
    Not,
    Reference,
    Deref,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Visibility {
    /// Public, the symbol will be visible from other modules.
    Pub,
    /// Private, the symbol is only visible from the module it
    /// was declared in. This is the default visibility for all symbols.
    Priv,
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum MemberAccess {
    Index(AstNode),
    MemberId(u32),
    Range {
        start: AstNode,
        end: Option<AstNode>,
    },
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
const STYLE_DECL:  Lazy<Style> = Lazy::new(|| Style::new().bold().red());
const STYLE_STMT:  Lazy<Style> = Lazy::new(|| Style::new().bold().cyan());
const STYLE_EXPR:  Lazy<Style> = Lazy::new(|| Style::new().bold().magenta());
const STYLE_TERM:  Lazy<Style> = Lazy::new(|| Style::new().bold());

/*
impl fmt::Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AstNode::StructProto { name, visibility, .. } 
                => writeln!(f, "{} {} {}", STYLE_PROTO.apply_to("StructProto"), visibility, name),
            AstNode::EnumProto { name, visibility, .. } 
                => writeln!(f, "{} {} {}", STYLE_PROTO.apply_to("EnumProto"), visibility, name),
            AstNode::FuncProto { name, visibility, ret_type, params } => {
                let params: Vec<String> = params.iter().map(|(_, v)| v.to_string()).collect();
                let mut m = format!("{} {} {}({})", STYLE_PROTO.apply_to("FuncProto"), visibility, name, params.join(", "));

                if let Some(t) = ret_type {
                    m.push_str(format!(": {}", t).as_str());
                }

                writeln!(f, "{}", m)
            },
            AstNode::ExternFuncProto { name, visibility, ret_type, param_types, variadic } => {
                let params: Vec<String> = param_types.iter().map(|v| v.to_string()).collect();

                let mut m = format!("{} {} {}({}", STYLE_PROTO.apply_to("ExternFuncProto"), 
                                    visibility, name, params.join(", "));

                if *variadic {
                    m.push_str("...");
                }

                m.push_str(")");

                if let Some(t) = ret_type {
                    m.push_str(format!(": {}", t).as_str());
                }

                writeln!(f, "{}", m)
            },
            AstNode::AliasProto { name, visibility, ty } 
                => writeln!(f, "{} {} {} = {}", STYLE_PROTO.apply_to("AliasProto"), visibility, name, ty),
            AstNode::FuncDecl { name, visibility, ret_type, params, stmts } => {
                let params: Vec<String> = params.iter().map(|(_, v)| v.to_string()).collect();

                let mut m = format!("{} {} {}({})", STYLE_DECL.apply_to("FuncDecl"), 
                                    visibility, name, params.join(", "));

                if let Some(t) = ret_type {
                    m.push_str(format!(": {}", t).as_str());
                }

                writeln!(f, "{}", m)?;
                write!(f, "{}", stmts)
            },
            AstNode::Stmts(stmts) => {
                for s in stmts {
                    writeln!(f, " | {}", s)?;
                }
                Ok(())
            },
            AstNode::VarDeclStmt { id, var_type, value } => {
                writeln!(f, "{} {} {} \n {}", 
                         STYLE_STMT.apply_to("VarDeclStmt"),
                         var_type,
                         id,
                         value)
            },
            AstNode::AssignStmt { left, right } => {
                writeln!(f, "{} \n left:{} \n right: {}", 
                         STYLE_STMT.apply_to("AssignStmt"),
                         left, right)
            },
            _ => write!(f, ""),
        }
    } 
}
*/

impl TreeItem for AstNode {
    type Child = Self;

    fn write_self<W: io::Write>(&self, f: &mut W, _style: &ptree::Style) -> io::Result<()> {
        match self {
            AstNode::StructProto { name, visibility, .. } 
                => write!(f, "{} {} {}", STYLE_PROTO.apply_to("StructProto"), visibility, name),
            AstNode::EnumProto { name, visibility, .. } 
                => write!(f, "{} {} {}", STYLE_PROTO.apply_to("EnumProto"), visibility, name),
            AstNode::FuncProto { name, visibility, ret_type, params } => {
                let params: Vec<String> = params.iter().map(|(_, v)| v.to_string()).collect();
                let mut m = format!("{} {} {}({})", STYLE_PROTO.apply_to("FuncProto"), visibility, name, params.join(", "));

                if let Some(t) = ret_type {
                    m.push_str(format!(": {}", t).as_str());
                }

                write!(f, "{}", m)
            },
            AstNode::ExternFuncProto { name, visibility, ret_type, param_types, variadic } => {
                let params: Vec<String> = param_types.iter().map(|v| v.to_string()).collect();

                let mut m = format!("{} {} {}({}", STYLE_PROTO.apply_to("ExternFuncProto"), 
                                    visibility, name, params.join(", "));

                if *variadic {
                    m.push_str("...");
                }

                m.push_str(")");

                if let Some(t) = ret_type {
                    m.push_str(format!(": {}", t).as_str());
                }

                write!(f, "{}", m)
            },
            AstNode::AliasProto { name, visibility, ty } 
                => write!(f, "{} {} {} = {}", STYLE_PROTO.apply_to("AliasProto"), visibility, name, ty),
            AstNode::FuncDecl { name, visibility, ret_type, params, .. } => {
                let params: Vec<String> = params.iter().map(|(_, v)| v.to_string()).collect();

                let mut m = format!("{} {} {}({})", STYLE_DECL.apply_to("FuncDecl"), 
                                    visibility, name, params.join(", "));

                if let Some(t) = ret_type {
                    m.push_str(format!(": {}", t).as_str());
                }

                write!(f, "{}", m)
            },
            AstNode::Stmts(_) 
                => write!(f, "{}", STYLE_STMT.apply_to("Stmts")),
            AstNode::VarDeclStmt { id, var_type, .. } => {
                write!(f, "{} {} {}", 
                         STYLE_STMT.apply_to("VarDeclStmt"),
                         var_type, 
                         id)
            },
            AstNode::AssignStmt { .. } 
                => write!(f, "{}", STYLE_STMT.apply_to("AssignStmt")),
            AstNode::IfStmt { .. }
                => write!(f, "{}", STYLE_STMT.apply_to("IfStmt")),
            AstNode::IfLetStmt { .. }
                => write!(f, "{}", STYLE_STMT.apply_to("IfLetStmt")),
            AstNode::ReturnStmt(_)
                => write!(f, "{}", STYLE_STMT.apply_to("ReturnStmt")),
            AstNode::LoopStmt(_)
                => write!(f, "{}", STYLE_STMT.apply_to("LoopStmt")),
            AstNode::BreakStmt
                => write!(f, "{}", STYLE_STMT.apply_to("BreakStmt")),
            AstNode::BinaryExpr { op, .. }
                => write!(f, "{} {:?}", STYLE_EXPR.apply_to("BinaryExpr"), op),
            AstNode::UnaryExpr { op, .. }
                => write!(f, "{} {:?}", STYLE_EXPR.apply_to("UnaryExpr"), op),
            AstNode::FunCall { name, ret_ty, .. } => match ret_ty {
                Some(t) => write!(f, "{} {}:{}", STYLE_EXPR.apply_to("FunCall"), name, t),
                None => write!(f, "{} {}", STYLE_EXPR.apply_to("FunCall"), name),
            }, 
            AstNode::MemberAccessExpr { .. } 
                => write!(f, "{}", STYLE_EXPR.apply_to("MemberAccessExpr")),
            AstNode::Array { .. }
                => write!(f, "{}", STYLE_TERM.apply_to("Array")),
            AstNode::Strct { name, .. }
                => write!(f, "{} {}", STYLE_TERM.apply_to("Strct"), name),
            AstNode::EnumVariant { enum_name, variant_name, .. }
                => write!(f, "{} {}:{}", STYLE_TERM.apply_to("enumvariant"), enum_name, variant_name),
            // for other terminal values (i8, str, u322...)
            _ => write!(f, "{}", STYLE_TERM.apply_to(format!("{:?}", self))),
        }
    }

    fn children(&self) -> Cow<[Self::Child]> {
        match self {
            AstNode::FuncDecl {stmts, ..} => Cow::from(vec![*stmts.clone()]), 
            AstNode::Stmts(s) => Cow::from(s), 
            AstNode::VarDeclStmt { value, .. } => Cow::from(vec![*value.clone()]), 
            AstNode::AssignStmt { left, right } => Cow::from(vec![*left.clone(), *right.clone()]), 
            AstNode::IfStmt { cond, then_b, elif_b, else_b } => {
                let mut v = vec![*cond.clone(), *then_b.clone()];
                for (cond, block) in elif_b {
                    v.push(cond.clone());
                    v.push(block.clone());
                }

                if let Some(b) = else_b {
                    v.push(*b.clone());
                }
                Cow::from(v)
            },
            AstNode::IfLetStmt { l_enum, r_expr, then_b, else_b } => {
                let mut v = vec![*l_enum.clone(), *r_expr.clone(), *then_b.clone()];
                if let Some(b) = else_b {
                    v.push(*b.clone());
                }
                Cow::from(v)
            },
            AstNode::ReturnStmt(inner) => Cow::from(vec![*inner.clone()]),
            AstNode::LoopStmt(inner) => Cow::from(vec![*inner.clone()]),
            AstNode::BinaryExpr { left, right, .. } => Cow::from(vec![*left.clone(), *right.clone()]), 
            AstNode::UnaryExpr { value, .. } => Cow::from(vec![*value.clone()]), 
            AstNode::MemberAccessExpr { parent, members, .. } => {
                let mut v = vec![*parent.clone()];

                for memb in members {
                    match memb {
                        MemberAccess::Index(node) => v.push(node.clone()),
                        MemberAccess::Range { start, end } => {
                            v.push(start.clone());
                            if let Some(node) = end {
                                v.push(node.clone());
                            }
                        },
                        MemberAccess::MemberId(_) => (),
                    }
                }

                Cow::from(v)
            },
            AstNode::Array { values, .. } => Cow::from(values),
            AstNode::Strct { members, .. } => {
                Cow::from(members.iter().map(|(_, v)| v).cloned().collect::<Vec<AstNode>>())
            },
            AstNode::EnumVariant { fields, .. } => {
                Cow::from(fields.iter().map(|(_, _, v)| v).cloned().collect::<Vec<AstNode>>())
            },
            _ => Cow::from(vec![]),
        }
    }
}
