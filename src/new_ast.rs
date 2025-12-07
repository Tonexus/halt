// general expression part of the ast

use std::{
    fmt, fmt::{Display, Formatter},
    collections::{HashSet, HashMap},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Expr<'a> {
    pub tier:  Option<u32>,
    pub texpr: Option<Box<Expr<'a>>>,
    pub var:   ExprVar<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprVar<'a> {
    // expression is a variable
    Var(&'a str),
    // expression is a literal value
    LVal(LitVar),
    // expression is a literal application of a function
    LApp {
        // the function
        fun:   Box<Expr<'a>>,
        // its parameter(s)
        param: Box<Expr<'a>>,
    },
    // expression is a literal function
    LFun {
        // list of parameter names and optional types
        params: Vec<(&'a str, Option<Box<Expr<'a>>>)>,
        // optional return type
        bodyt:  Option<Box<Expr<'a>>>,
        // function body
        // TODO: allow function body to be imperative for non-type function
        body:   Box<Expr<'a>>,
    },
    // expression is a literal let expression
    LLet {
        // list of variable names and optional types
        // TODO allow value
        vars:  Vec<(&'a str, Option<Box<Expr<'a>>>)>,
        // optiobaly body type
        bodyt: Option<Box<Expr<'a>>>,
        // body of let expression
        body:  Box<Expr<'a>>,
    },
    // expression is a literal product
    LPro(Vec<(&'a str, Expr<'a>)>),
    // expression is a literal sum
    LSum(&'a str, Box<Expr<'a>>),
    // expression is the type of a function
    TFun(Box<Expr<'a>>, Box<Expr<'a>>),
    // expression is the type of a product
    TPro(HashMap<&'a str, Expr<'a>>),
    // expression is the type of a sum
    TSum(HashMap<&'a str, Expr<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LitVar {
    Bool(bool),
    Int(i32),
    Float(f32),
    Ascii(Vec<u8>),
    U8Char(u8),
}
