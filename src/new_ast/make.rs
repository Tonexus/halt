use super::ast::*;
use crate::misc::*;

// functions for making AST expressions

pub fn vexpr_var<'a>(s: &'a str) -> Expr<'a> {
    return Expr {
        min_tier: 0,
        max_tier: 0,
        texpr:    None,
        var:      ExprVar::Var {
            name:    s,
            is_type: false
        },
    };
}

pub fn texpr_var<'a>(s: &'a str) -> Expr<'a> {
    return Expr {
        min_tier: 1,
        max_tier: MAX_TIER,
        texpr:    None,
        var:      ExprVar::Var {
            name:    s,
            is_type: true
        },
    };
}

pub fn vexpr_lit_bool(b: bool) -> Expr<'static> {
    return Expr {
        min_tier: 0,
        max_tier: 0,
        texpr:    None, // TODO
        var:      ExprVar::Lit(LitVar::Bool(b))
    }
}

pub fn vexpr_lit_int(i: i32) -> Expr<'static> {
    return Expr {
        min_tier: 0,
        max_tier: 0,
        texpr:    None, // TODO
        var:      ExprVar::Lit(LitVar::Int(i))
    }
}

pub fn vexpr_lit_float(f: f32) -> Expr<'static> {
    return Expr {
        min_tier: 0,
        max_tier: 0,
        texpr:    None, // TODO
        var:      ExprVar::Lit(LitVar::Float(f))
    }
}

pub fn vexpr_lit_ascii(a: Vec<u8>) -> Expr<'static> {
    return Expr {
        min_tier: 0,
        max_tier: 0,
        texpr:    None, // TODO
        var:      ExprVar::Lit(LitVar::Ascii(a))
    }
}

pub fn vexpr_lit_u8char(c: u8) -> Expr<'static> {
    return Expr {
        min_tier: 0,
        max_tier: 0,
        texpr:    None, // TODO
        var:      ExprVar::Lit(LitVar::U8Char(c))
    }
}

pub fn vexpr_unop<'a>(e: Expr<'a>, s: &'a str) -> Expr<'a> {
    return Expr {
        min_tier: 0,
        max_tier: 0,
        texpr:    None,
        // unary op is actuall function application on singleton
        var:      ExprVar::App {
            fun:   Box::new(vexpr_var(s)),
            param: Box::new(e)
        }
    };
}

pub fn vexpr_binop<'a>(e1: Expr<'a>, e2: Expr<'a>, s: &'a str) -> Expr<'a> {
    return Expr {
        min_tier: 0,
        max_tier: 0,
        texpr:    None,
        // binary op is actually function application on product
        var:      ExprVar::App {
            fun:   Box::new(vexpr_var(s)),
            param: Box::new(Expr {
                min_tier: 0,
                max_tier: 0,
                texpr: None,
                var:   ExprVar::Prod(Vec::from([
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
        var:      ExprVar::App {
            fun:   Box::new(e1),
            param: Box::new(e2)
        }
    }
}

pub fn expr_prod<'a>(l: Vec<(&'a str, Expr<'a>)>) -> Expr<'a> {
    return Expr {
        min_tier: 0,
        max_tier: MAX_TIER,
        texpr:    None,
        var:      ExprVar::Prod(l)
    }
}

pub fn expr_sum<'a>(s: &'a str, e: Expr<'a>) -> Expr<'a> {
    return Expr {
        min_tier: 0,
        max_tier: MAX_TIER,
        texpr:    None,
        var:      ExprVar::Sum(s, Box::new(e))
    }
}

pub fn vexpr_fun<'a>(
    l: Vec<(&'a str, Option<(u32, Expr<'a>)>)>,
    o: Option<(u32, Expr<'a>)>,
    e: Expr<'a>
) -> Expr<'a> {
    return Expr {
        min_tier: 0,
        max_tier: 0,
        texpr:    None,
        var:      ExprVar::Fun {
            params: l,
            bodyt:  o.map(Box::new),
            body:   Box::new(e),
        }
    };
}

// TODO fix
pub fn vexpr_let<'a>(
    l: Vec<(&'a str, Option<(u32, Expr<'a>)>)>,
    e: Expr<'a>
) -> Expr<'a> {
    return Expr {
        min_tier: 0,
        max_tier: 0,
        texpr:    None,
        var:      ExprVar::Let {
            vars: l.into_iter().map(
                |(s, t)| LetBind{name: s, annot: t.map(|(n, e)| TypeAnnot{tier: n, expr: e}), value: None}
            ).collect(),
            body: Box::new(e),
        }
    };
}

