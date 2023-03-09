// ast for halt

#[derive(Debug, PartialEq)]
pub enum Definition {
    Type(TypeDef),
    Const(ConstDef),
}

#[derive(Debug, PartialEq)]
pub struct TypeDef {
    pub name:   String,
    pub expr:   TypeExpr,
}

#[derive(Debug, PartialEq)]
pub struct ConstDef {
    pub name:        String,
    pub type_expr:   Option<TypeExpr>,
    pub expr:        ValueExpr,
}

#[derive(Debug, PartialEq)]
pub enum TypeExpr {
    Variable(String),
    TypeParams(Box<TypeExpr>, Vec<TypeExpr>),
    Universal(String, Box<TypeExpr>),
    Existential(String, Box<TypeExpr>),
    Tuple(Vec<TypeExpr>),
    Struct(Vec<(String, TypeExpr)>),
    Choice(Vec<TypeExpr>),
    Tagged(Vec<(String, TypeExpr)>),
    Function(Box<TypeExpr>, Box<TypeExpr>),
}

#[derive(Debug, PartialEq)]
pub struct ValueExpr {
    pub variant:   ExprVariant,
    pub type_expr: Option<TypeExpr>,
}

// TODO flesh these out
// TODO define Place as type of expr variant that may be on LHS of assignment
#[derive(Debug, PartialEq)]
pub enum ExprVariant {
    Variable(String),
    Literal(LitVariant),
    BinaryOp(BinOpVariant, Box<ValueExpr>, Box<ValueExpr>),
    UnaryOp(UnOpVariant, Box<ValueExpr>),
    Tuple(Vec<ValueExpr>),
    Struct(Vec<(String, ValueExpr)>),
    Choice(Box<ValueExpr>),
    Tagged(String, Box<ValueExpr>),
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

