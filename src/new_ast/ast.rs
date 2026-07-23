// general expression part of the ast

use std::{
    fmt, fmt::{Display, Formatter},
    collections::{HashSet, HashMap},
};

#[derive(Debug, PartialEq)]
pub struct Def<'a> {
    pub name:     &'a str,
    pub min_tier: u32,
    pub max_tier: u32,
    pub texpr:    Option<Expr<'a>>,
    pub expr:     Expr<'a>,

}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr<'a> {
    // values are tier 0, types tier 1, kinds tier 2, etc...
    pub min_tier: u32,
    pub max_tier: u32,
    pub texpr:    Option<Box<Expr<'a>>>,
    pub var:      ExprVar<'a>,
}

// expression variant
#[derive(Debug, Clone, PartialEq)]
pub enum ExprVar<'a> {
    // expression is a variable
    Var {
        name:    &'a str,
        is_type: bool,
    },
    // expression is a literal
    Lit(LitVar),
    // expression is an application of a function
    App {
        // the function
        fun:   Box<Expr<'a>>,
        // its parameter(s)
        param: Box<Expr<'a>>,
    },
    // expression is a function
    Fun {
        // list of parameter names and optional types
        params: Vec<(&'a str, Option<(u32, Expr<'a>)>)>,
        // optional return type
        bodyt:  Option<Box<(u32, Expr<'a>)>>,
        // function body
        // TODO: allow function body to be imperative for non-type function
        body:   Box<Expr<'a>>,
    },
    // expression is a let expression
    Let {
        // list of variable names and optional types
        // TODO allow value
        vars:  Vec<LetBind<'a>>,
        // body of let expression
        body:  Box<Expr<'a>>,
    },
    // expression is a literal product
    Prod(Vec<(&'a str, Expr<'a>)>),
    // expression is a literal sum
    Sum(&'a str, Box<Expr<'a>>),
    // expression is the type of a function
    TFun(Box<Expr<'a>>, Box<Expr<'a>>),
    // expression is the type of a product
    TPro(HashMap<&'a str, Expr<'a>>),
    // expression is the type of a sum
    TSum(HashMap<&'a str, Expr<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetBind<'a> {
    pub name:  &'a str,
    pub annot: Option<TypeAnnot<'a>>,
    pub value: Option<Expr<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunArg<'a> {
    pub name:  &'a str,
    pub annot: Option<TypeAnnot<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAnnot<'a> {
    pub tier: u32,
    pub expr: Expr<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LitVar {
    Bool(bool),
    Int(i32),
    Float(f32),
    Ascii(Vec<u8>),
    U8Char(u8),
}
