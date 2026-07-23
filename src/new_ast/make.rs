use super::ast::*;
use crate::misc::*;

// functions for making AST expressions

pub fn def<'a>(s: &'a str, is_type: bool, t: Option<Annot<'a>>, e: Expr<'a>) -> Def<'a> {
    return Def {
        name:     s,
        is_type:  is_type,
        min_tier: if is_type {1} else {0},
        max_tier: if is_type {MAX_TIER} else {0},
        annot:    t,
        expr:     e,
    }
}

pub fn expr_var<'a>(s: &'a str, is_type: bool) -> Expr<'a> {
    return Expr {
        min_tier: if is_type {1} else {0},
        max_tier: if is_type {MAX_TIER} else {0},
        texpr:    None,
        var:      ExprVar::Var {
            name:    s,
            is_type: is_type
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
            fun:   Box::new(expr_var(s, false)),
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
            fun:   Box::new(expr_var(s, false)),
            param: Box::new(Expr {
                min_tier: 0,
                max_tier: 0,
                texpr: None,
                var:   ExprVar::Prod(Vec::from([
                    (LABELS[0], e1),
                    (LABELS[1], e2),
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
    l: Vec<FunParam<'a>>,
    o: Option<Annot<'a>>,
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

pub fn expr_let<'a>(
    l: Vec<LetBind<'a>>,
    e: Expr<'a>
) -> Expr<'a> {
    return Expr {
        min_tier: 0,
        max_tier: 0,
        texpr:    None,
        var:      ExprVar::Let {
            vars: l,
            body: Box::new(e),
        }
    };
}

pub fn param<'a>(s: &'a str, b: bool, t: Option<Annot<'a>>) -> FunParam<'a> {
    return FunParam {name: s, is_type: b, annot: t};
}

pub fn bind<'a>(s: &'a str, b: bool, t: Option<Annot<'a>>, v: Option<Expr<'a>>) -> LetBind<'a> {
    return LetBind {name: s, is_type: b, annot: t, value: v};
}

pub fn annot<'a>(n: u32, e: Expr<'a>) -> Annot<'a> {
    return Annot {tier: n, expr: e};
}
