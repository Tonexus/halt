// ast for halt

use lazy_static::lazy_static;

lazy_static! {
    pub static ref KIND_0: TypeExpr<'static> = TypeExpr::Variable("Type");
    pub static ref KIND_1: TypeExpr<'static> = TypeExpr::Function(Box::new(KIND_0.clone()), Box::new(KIND_0.clone()));
    pub static ref KIND_2: TypeExpr<'static> = TypeExpr::Function(Box::new(KIND_0.clone()), Box::new(KIND_1.clone()));
}

#[derive(Debug, PartialEq)]
pub enum Definition<'a> {
    Type(TypeDef<'a>),
    Const(ConstDef<'a>),
}

#[derive(Debug, PartialEq)]
pub struct TypeDef<'a> {
    pub name:  &'a str,
    pub kexpr: Option<TypeExpr<'a>>,
    pub texpr: TypeExpr<'a>,
}

#[derive(Debug, PartialEq)]
pub struct ConstDef<'a> {
    pub name:  &'a str,
    pub texpr: Option<TypeExpr<'a>>,
    pub vexpr: ValueExpr<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeExpr<'a> {
    Variable(&'a str),
    TypeParams(Box<TypeExpr<'a>>, Vec<TypeExpr<'a>>),
    Quantified {
        params:  Vec<(&'a str, TypeExpr<'a>)>,
        is_univ: bool,
        subexpr: Box<TypeExpr<'a>>,
    },
    Prod(Vec<(&'a str, TypeExpr<'a>)>),
    Sum(Vec<(&'a str, TypeExpr<'a>)>),
    Function(Box<TypeExpr<'a>>, Box<TypeExpr<'a>>), // TODO add closure
}

#[derive(Debug, PartialEq)]
pub struct ValueExpr<'a> {
    pub variant: ExprVariant<'a>,
    pub texpr:   Option<TypeExpr<'a>>,
}

// TODO flesh these out
// TODO define Place as type of expr variant that may be on LHS of assignment
#[derive(Debug, PartialEq)]
pub enum ExprVariant<'a> {
    Variable(&'a str),
    Literal(LitVariant),
    BinaryOp(BinOpVariant<'a>, Box<ValueExpr<'a>>, Box<ValueExpr<'a>>),
    UnaryOp(UnOpVariant<'a>, Box<ValueExpr<'a>>),
    Prod(Vec<(&'a str, ValueExpr<'a>)>),
    Sum(&'a str, Box<ValueExpr<'a>>),
    Closure(Closure<'a>),
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
pub enum BinOpVariant<'a> {
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
    Call(Vec<TypeExpr<'a>>),
}

// TODO add . field access
// TODO add .. UFCS
// TODO add ~ copy
#[derive(Debug, PartialEq)]
pub enum UnOpVariant<'a> {
    Field(&'a str),
    Cast,
    Ref,
    Deref,
    Pos,
    Neg,
    Not,
}

#[derive(Debug, PartialEq)]
pub struct Closure<'a> {
    pub params:      Vec<(&'a str, Option<TypeExpr<'a>>)>,
    pub type_params: Vec<&'a str>,
    pub returns:     Option<TypeExpr<'a>>,
    pub body:        Vec<Statement<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Expr(ValueExpr<'a>),
    Def(Definition<'a>),
    Let(ValueExpr<'a>, Option<ValueExpr<'a>>),
    Assign(ValueExpr<'a>, ValueExpr<'a>),
    Return(ValueExpr<'a>),
    Break,
    Continue,
    Match(Match<'a>),
    If(If<'a>),
    Loop(Loop<'a>),
    With(Vec<Statement<'a>>),
}

#[derive(Debug, PartialEq)]
pub struct Match<'a> {
    pub value:       ValueExpr<'a>,
    pub to_branches: Vec<ToBranch<'a>>,
    pub else_branch: Vec<Statement<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct ToBranch<'a> {
    pub pattern: Vec<ValueExpr<'a>>,
    pub block:   Vec<Statement<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct If<'a> {
    pub value:       ValueExpr<'a>,
    pub then_branch: Vec<Statement<'a>>,
    pub else_branch: Vec<Statement<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Loop<'a> {
    pub value: ValueExpr<'a>,
    pub iter:  ValueExpr<'a>,
    pub body:  Vec<Statement<'a>>,
}

