// miscellaneous functions

use std::collections::HashMap;

use super::ast::*;
use super::parser;

// non-numeric types
pub fn get_basic_kword_type_exprs() -> HashMap<&'static str, TypeExpr> {
    let mut out = HashMap::new();
    out.insert("Bool", parser::type_expr("[_0: (), _1: ()]").unwrap());
    out.insert("Res", parser::type_expr("A! B! [okay: A, fail: B]").unwrap());
    out.insert("Opt", parser::type_expr("A! [some: A, none: ()]").unwrap());
    out.insert("Ascii", parser::type_expr("N! Arr{U8, N}").unwrap());
    return out;
}

