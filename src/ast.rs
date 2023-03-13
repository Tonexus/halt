// ast for halt

//pub const null_kind: TypeExpr = TypeExpr::Variable("Type".to_string());

#[derive(Debug, PartialEq)]
pub enum Definition {
    Type(TypeDef),
    Const(ConstDef),
}

#[derive(Debug, PartialEq)]
pub struct TypeDef {
    pub name:  String,
    pub kexpr: Option<TypeExpr>,
    pub texpr: TypeExpr,
}

#[derive(Debug, PartialEq)]
pub struct ConstDef {
    pub name:  String,
    pub texpr: Option<TypeExpr>,
    pub vexpr: ValueExpr,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeExpr {
    Variable(String),
    TypeParams(Box<TypeExpr>, Vec<TypeExpr>),
    Quantified {
        params:  Vec<(String, TypeExpr)>,
        is_univ: bool,
        subexpr: Box<TypeExpr>,
    },
    Prod(Vec<(String, TypeExpr)>),
    Sum(Vec<(String, TypeExpr)>),
    Function(Box<TypeExpr>, Box<TypeExpr>), // TODO add closure
}

#[derive(Debug, PartialEq)]
pub struct ValueExpr {
    pub variant: ExprVariant,
    pub texpr:   Option<TypeExpr>,
}

// TODO flesh these out
// TODO define Place as type of expr variant that may be on LHS of assignment
#[derive(Debug, PartialEq)]
pub enum ExprVariant {
    Variable(String),
    Literal(LitVariant),
    BinaryOp(BinOpVariant, Box<ValueExpr>, Box<ValueExpr>),
    UnaryOp(UnOpVariant, Box<ValueExpr>),
    Prod(Vec<(String, ValueExpr)>),
    Sum(String, Box<ValueExpr>),
    Closure(Closure),
}

#[derive(Debug, PartialEq)]
pub enum LitVariant {
    Bool(bool),
    Integer(i32),
    Float(f32),
    Ascii(Vec<u8>),
    U8Char(u8),
}

#[derive(Debug, PartialEq)]
pub enum BinOpVariant {
    Pow,
    Log,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Gt,
    Lt,
    Gte,
    Lte,
    Equ,
    Neq,
    And,
    Or,
    Call(Vec<TypeExpr>),
}

// TODO add . field access
// TODO add .. UFCS
// TODO add ~ copy
#[derive(Debug, PartialEq)]
pub enum UnOpVariant {
    Field(String),
    Cast,
    Ref,
    Deref,
    Pos,
    Neg,
    Not,
}

#[derive(Debug, PartialEq)]
pub struct Closure {
    pub params:      Vec<(String, Option<TypeExpr>)>,
    pub type_params: Vec<String>,
    pub returns:     Option<TypeExpr>,
    pub body:        Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expr(ValueExpr),
    Def(Definition),
    Let(ValueExpr, Option<ValueExpr>),
    Assign(ValueExpr, ValueExpr),
    Return(ValueExpr),
    Break,
    Continue,
    Match(Match),
    If(If),
    Loop(Loop),
    With(Vec<Statement>),
}

#[derive(Debug, PartialEq)]
pub struct Match {
    pub value:       ValueExpr,
    pub to_branches: Vec<ToBranch>,
    pub else_branch: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub struct ToBranch {
    pub pattern: Vec<ValueExpr>,
    pub block:   Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub struct If {
    pub value:       ValueExpr,
    pub then_branch: Vec<Statement>,
    pub else_branch: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub struct Loop {
    pub value: ValueExpr,
    pub iter:  ValueExpr,
    pub body:  Vec<Statement>,
}

