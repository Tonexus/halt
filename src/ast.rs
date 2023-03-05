
#[derive(Debug, PartialEq)]
pub enum Definition {
    Type(TypeDef),
    Const(ConstDef),
}

#[derive(Debug, PartialEq)]
pub struct TypeDef {
    pub name:   String,
    pub params: Vec<String>,
    pub expr:   TypeExpr,
}

#[derive(Debug, PartialEq)]
pub struct ConstDef {
    pub name:        String,
    pub type_expr:   Option<TypeExpr>,
    pub type_params: Vec<String>,
    pub expr:        ValueExpr,
}

#[derive(Debug, PartialEq)]
pub enum TypeExpr {
    Variable(String, Vec<TypeExpr>),
    Universal(String),
    Existential(String),
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
    Closure(Box<Closure>),
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
    Call,
}

#[derive(Debug, PartialEq)]
pub enum UnOpVariant { // TODO add . field access and .. UFCS
    Cast,
    Ref,
    Deref,
    Pos,
    Neg,
    Not,
}

#[derive(Debug, PartialEq)]
pub struct Closure {
    pub params:  Vec<(String, Option<TypeExpr>)>,
    pub returns: Option<TypeExpr>,
    pub body:    Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expr(ValueExpr),
    Def(Definition),
    Let(DeclPattern, Option<ValueExpr>),
    Assign(AsgnPattern, ValueExpr), // TODO replace string with place
    Return(ValueExpr),
    Break,
    Continue,
    Match(ValueExpr, Vec<ToBranch>, Vec<Statement>),
    If(ValueExpr, Vec<Statement>, Vec<Statement>),
    Loop(DeclPattern, ValueExpr, Vec<Statement>),
    With(Vec<Statement>),
}

// TODO consider ref/derefs in patterns?
#[derive(Debug, PartialEq)]
pub struct AsgnPattern {
    pub variant:   AsgnPatVariant,
    pub type_expr: Option<TypeExpr>,
}

#[derive(Debug, PartialEq)]
pub enum AsgnPatVariant {
    Variable(String),
    Deref(ValueExpr),
    Tuple(Vec<AsgnPattern>),
    Struct(Vec<(String, AsgnPattern)>),
}

#[derive(Debug, PartialEq)]
pub struct DeclPattern {
    pub variant:   DeclPatVariant,
    pub type_expr: Option<TypeExpr>,
}

#[derive(Debug, PartialEq)]
pub enum DeclPatVariant {
    Variable(String),
    Tuple(Vec<DeclPattern>),
    Struct(Vec<(String, DeclPattern)>),
}

#[derive(Debug, PartialEq)]
pub struct ToBranch {
    pattern: Vec<ToPattern>,
    block:   Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub struct ToPattern {
    pub variant:   ToPatVariant,
    pub type_expr: Option<TypeExpr>,
}

#[derive(Debug, PartialEq)]
pub enum ToPatVariant {
    Const(ValueExpr), // must be literal or "inline" expression
    Variable(String),
    Tuple(Vec<ToPattern>),
    Struct(Vec<(String, ToPattern)>),
    Choice(Box<ToPattern>),
    Tagged(String, Box<ToPattern>),
}

