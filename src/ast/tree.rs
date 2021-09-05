use crate::VarType;

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    FuncDecl {
        name: String,
        ret_type: Option<VarType>,
        params: Vec<(String, VarType)>,
        stmts: Box<AstNode>,
    },
    ExternFunc {
        name: String,
        ret_type: Option<VarType>,
        param_types: Vec<VarType>,
    },

    Stmts(Vec<AstNode>),

    // expressions
    VarDeclExpr {
        id: String,
        var_type: VarType,
        value: Box<AstNode>,
    },
    AssignExpr {
        left: Box<AstNode>,
        right: Box<AstNode>,
    },
    IfExpr {
        cond: Box<AstNode>,
        then_b: Box<AstNode>,
        elif_b: Vec<(AstNode, AstNode)>,
        else_b: Option<Box<AstNode>>,
    },
    ReturnExpr(Box<AstNode>),
    LoopExpr(Box<AstNode>), // contains an AstNode::Stmts variant inside
    BreakExpr,
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
    },
    IndexationExpr {
        value: Box<AstNode>,
        indexes: Vec<AstNode>,
        ty: VarType,
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
    Float32(f32),
    Float64(f64),
    Boolean(bool),
    /// The `Array` variant contains `values`, the elements of the array (can be an empty array)
    /// and `ty`, the `VarType` of the elements inside the array.
    Array {
        values: Vec<AstNode>,
        ty: VarType,
    },
}

impl AstNode {
    pub fn is_literal(&self) -> bool {
        match self {
            AstNode::Int32(_) | AstNode::UInt32(_) | AstNode::Boolean(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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
        match self {
            BinaryOp::And
            | BinaryOp::Or
            | BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Lt
            | BinaryOp::Gt
            | BinaryOp::Leq
            | BinaryOp::Geq => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
}

// #[derive(Debug, PartialEq)]
// pub enum Iter {
//     IntRange(i64, i64),
// }
