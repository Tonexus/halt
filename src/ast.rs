
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
pub enum UnOpVariant {
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
    Expr, // TODO in AST check call must have at least one mutable argument
    Def,
    Let,
    Assign,
    Return,
    Break,
    Continue,
    Match,
    If,
    Loop,
    With,
}

