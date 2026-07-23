use super::ast::*;
use crate::misc::*;

// functions for making AST expressions

pub fn vexpr_var<'a>(s: &'a str) -> Expr<'a> {
    return Expr {
        min_tier: 0,
        max_tier: 0,
        texpr:    None,
        var:      ExprVar::Var(s),
    };
}

pub fn vexpr_lit_bool(b: bool) -> Expr<'static> {
    return Expr {
        min_tier: 0,
        max_tier: 0,
        texpr: None, // TODO
        var:   ExprVar::LVal(LitVar::Bool(b))
    }
}

pub fn vexpr_lit_int(i: i32) -> Expr<'static> {
    return Expr {
        min_tier: 0,
        max_tier: 0,
        texpr: None, // TODO
        var:   ExprVar::LVal(LitVar::Int(i))
    }
}

pub fn vexpr_lit_float(f: f32) -> Expr<'static> {
    return Expr {
        min_tier: 0,
        max_tier: 0,
        texpr: None, // TODO
        var:   ExprVar::LVal(LitVar::Float(f))
    }
}

pub fn vexpr_lit_ascii(a: Vec<u8>) -> Expr<'static> {
    return Expr {
        min_tier: 0,
        max_tier: 0,
        texpr: None, // TODO
        var:   ExprVar::LVal(LitVar::Ascii(a))
    }
}

pub fn vexpr_lit_u8char(c: u8) -> Expr<'static> {
    return Expr {
        min_tier: 0,
        max_tier: 0,
        texpr: None, // TODO
        var:   ExprVar::LVal(LitVar::U8Char(c))
    }
}

pub fn vexpr_unop<'a>(e: Expr<'a>, s: &'a str) -> Expr<'a> {
    return Expr {
        min_tier: 0,
        max_tier: 0,
        texpr: None,
        // unary op is actuall function application on singleton
        var:   ExprVar::LApp {
            fun:   Box::new(vexpr_var(s)),
            param: Box::new(e)
        }
    };
}

pub fn vexpr_binop<'a>(e1: Expr<'a>, e2: Expr<'a>, s: &'a str) -> Expr<'a> {
    return Expr {
        min_tier: 0,
        max_tier: 0,
        texpr: None,
        // binary op is actuall function application on product
        var:   ExprVar::LApp {
            fun:   Box::new(vexpr_var(s)),
            param: Box::new(Expr {
                min_tier: 0,
                max_tier: 0,
                texpr: None,
                var:   ExprVar::LPro(Vec::from([
                    ("0", e1),
                    ("1", e2),
                ]))
            })
        }
    };
}

pub fn expr_app<'a>(e1: Expr<'a>, e2: Expr<'a>) -> Expr<'a> {
    return Expr {
        min_tier: 0,
        max_tier: MAX_TIER,
        texpr:    None,
        var:      ExprVar::LApp {
            fun:   Box::new(e1),
            param: Box::new(e2)
        }
    }
}

pub fn expr_pro<'a>(l: Vec<(&'a str, Expr<'a>)>) -> Expr<'a> {
    return Expr {
        min_tier: 0,
        max_tier: MAX_TIER,
        texpr:    None,
        var:      ExprVar::LPro(l)
    }
}

pub fn expr_sum<'a>(s: &'a str, e: Expr<'a>) -> Expr<'a> {
    return Expr {
        min_tier: 0,
        max_tier: MAX_TIER,
        texpr:    None,
        var:      ExprVar::LSum(s, Box::new(e))
    }
}

pub fn vexpr_fun<'a>(
    p: Vec<(&'a str, Option<(u32, Expr<'a>)>)>,
    o: Option<(u32, Expr<'a>)>,
    e: Expr<'a>
) -> Expr<'a> {
    return Expr {
        min_tier: 0,
        max_tier: 0,
        texpr: None,
        var:   ExprVar::LFun {
            params: p,
            bodyt:  o.map(Box::new),
            body:   Box::new(e),
        }
    };
}

pub fn texpr_var<'a>(s: &'a str) -> Expr<'a> {
    return Expr {
        min_tier: 1,
        max_tier: MAX_TIER,
        texpr:    None,
        var:      ExprVar::Var(s),
    };
}
