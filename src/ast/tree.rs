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
    StructDef {
        name: String,
        members: Vec<(String, VarType)>,
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
    },
    MemberAccessExpr {
        parent: Box<AstNode>,
        members: Vec<AstNode>, // a list of nodes of type u64
        member_ty: VarType,    // type of: parent[members]
        parent_ty: VarType,
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
        is_const: bool,
    },
    Strct {
        name: String,
        members: Vec<(String, AstNode)>,
        is_const: bool,
    }
}

impl AstNode {
    // TODO: Constant struct detection is missing
    pub fn is_const(&self) -> bool {
        match self {
            AstNode::Int64(_) | AstNode::UInt64(_) 
            |AstNode::Int32(_) | AstNode::UInt32(_) 
            | AstNode::Int16(_) | AstNode::UInt16(_) 
            | AstNode::Int8(_) | AstNode::UInt8(_) 
            | AstNode::Boolean(_)
            | AstNode::Float32(_) 
            | AstNode::Float64(_) => true,
            AstNode::Array { is_const, .. } => *is_const,
            AstNode::Strct {is_const, ..} => *is_const,
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
        matches!(self, 
            BinaryOp::And
            | BinaryOp::Or
            | BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Lt
            | BinaryOp::Gt
            | BinaryOp::Leq
            | BinaryOp::Geq)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
    Reference,
    Deref,
}
