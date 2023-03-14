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
    Function(Box<TypeExpr<'a>>, Box<TypeExpr<'a>>),
}

#[derive(Debug, PartialEq)]
pub struct ValueExpr<'a> {
    pub variant: ExprVariant<'a>,
    pub texpr:   Option<TypeExpr<'a>>,
}

// TODO define Place as type of expr variant that may be on LHS of assignment
#[derive(Debug, PartialEq)]
pub enum ExprVariant<'a> {
    Variable(&'a str),
    Literal(LitExpr),
    BinOp {
        op:        BinOpExpr<'a>,
        subexpr_1: Box<ValueExpr<'a>>,
        subexpr_2: Box<ValueExpr<'a>>,
    },
    UnOp {
        op:      UnOpExpr<'a>,
        subexpr: Box<ValueExpr<'a>>
    },
    Prod(Vec<(&'a str, ValueExpr<'a>)>),
    Sum(&'a str, Box<ValueExpr<'a>>),
    Closure {
        params:      Vec<(&'a str, Option<TypeExpr<'a>>)>,
        type_params: Vec<&'a str>,
        returns:     Option<TypeExpr<'a>>,
        body:        Vec<Statement<'a>>,
    },
}

#[derive(Debug, PartialEq)]
pub enum LitExpr {
    Bool(bool),
    Integer(i32),
    Float(f32),
    Ascii(Vec<u8>),
    U8Char(u8),
}

#[derive(Debug, PartialEq)]
pub enum BinOpExpr<'a> {
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
pub enum UnOpExpr<'a> {
    Field(&'a str),
    Cast,
    Ref,
    Deref,
    Pos,
    Neg,
    Not,
}

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Expr(ValueExpr<'a>),
    Def(Definition<'a>),
    Let {
        place: ValueExpr<'a>,
        vexpr: Option<ValueExpr<'a>>
    },
    Assign {
        place: ValueExpr<'a>,
        vexpr: ValueExpr<'a>,
    },
    Return(ValueExpr<'a>),
    Break,
    Continue,
    Match {
        vexpr:       ValueExpr<'a>,
        to_branches: Vec<ToBranch<'a>>,
        else_block:  Vec<Statement<'a>>,
    },
    If {
        vexpr:       ValueExpr<'a>,
        then_block: Vec<Statement<'a>>,
        else_block: Vec<Statement<'a>>,
    },
    Loop {
        place:  ValueExpr<'a>,
        iter:   ValueExpr<'a>,
        block:  Vec<Statement<'a>>,
    },
    With(Vec<Statement<'a>>),
}

#[derive(Debug, PartialEq)]
pub struct ToBranch<'a> {
    pub pattern: Vec<ValueExpr<'a>>,
    pub block:   Vec<Statement<'a>>,
}

