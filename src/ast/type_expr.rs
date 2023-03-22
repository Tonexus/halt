// type expression part of the ast

use std::{
    fmt, fmt::{Display, Formatter},
    collections::{HashSet, HashMap},
};

use itertools::Itertools;
use lazy_static::lazy_static;

use super::super::{misc::*, error::*};

lazy_static! {
    pub static ref KIND_0: TypeExpr<'static> = TypeExpr::Variable("Type");
    pub static ref KIND_1: TypeExpr<'static> = TypeExpr::Func(Box::new(KIND_0.clone()), Box::new(KIND_0.clone()));
    pub static ref KIND_2: TypeExpr<'static> = TypeExpr::Func(Box::new(KIND_0.clone()), Box::new(KIND_1.clone()));
}

#[derive(Debug, Clone)]
pub enum TypeExpr<'a> {
    Variable(&'a str),
    TypeParams(Box<TypeExpr<'a>>, Vec<TypeExpr<'a>>),
    Univ(HashMap<&'a str, TypeExpr<'a>>, Box<TypeExpr<'a>>),
    Exis(HashMap<&'a str, TypeExpr<'a>>, Box<TypeExpr<'a>>),
    Prod(HashMap<&'a str, TypeExpr<'a>>),
    Sum(HashMap<&'a str, TypeExpr<'a>>),
    Func(Box<TypeExpr<'a>>, Box<TypeExpr<'a>>),
}

impl PartialEq for TypeExpr<'_> {
    fn eq(&self, other: &Self) -> bool {
        const FIRST: &str = LABELS[0];

        fn inner<'a>(
            one: &TypeExpr<'a>,
            two: &TypeExpr<'a>,
            vars: &mut HashMap<&'a str, &'a str>
        ) -> bool {
            match (one, two) {
                // if singleton, may strip
                (TypeExpr::Prod(h), t1) | (TypeExpr::Sum(h), t1)
                    | (t1, TypeExpr::Prod(h)) | (t1, TypeExpr::Sum(h)) => {
                    if let Ok((&FIRST, t2)) = h.iter().exactly_one() {
                        return inner(t1, t2, vars);
                    }
                },
                _ => {},
            }

            // otherwise, enum and contents must be identical
            match (one, two) {
                (TypeExpr::Variable(s1), TypeExpr::Variable(s2)) => {
                    return match (vars.get(s1), vars.get(s2)) {
                        // local vars must correspond to each other
                        (Some(s3), Some(s4)) => (s1 == s4) && (s2 == s3),
                        // or same global var name
                        (None,     None)     => s1 == s2,
                        _                    => false,
                    };
                },

                // same subexpr and params
                (TypeExpr::TypeParams(t1, l1), TypeExpr::TypeParams(t2, l2)) => {
                    return inner(t1, t2, vars) && l1.iter().zip(l2.iter())
                        .fold(true, |a, (t1, t2)| a && inner(t1, t2, vars));
                },

                // same quantifier
                (TypeExpr::Univ(h1, t1), TypeExpr::Univ(h2, t2))
                    | (TypeExpr::Exis(h1, t1), TypeExpr::Exis(h2, t2)) => {
                    let mut temp_vars = HashMap::new();
                    for ((s1, k1), (s2, k2)) in h1.iter().zip(h2.iter()) {
                        // kinds must be same
                        if k1 != k2 {
                            return false;
                        }
                        // add new quantified vars so they correspond to each other
                        // save old vars for later
                        if let Some(s) = vars.insert(s1, s2) {
                            temp_vars.insert(s1, s);
                        }
                        if let Some(s) = vars.insert(s2, s1) {
                            temp_vars.insert(s2, s);
                        }
                    }

                    // check equality under new quantified vars
                    if !inner(t1, t2, vars) {
                        return false;
                    }

                    // otherwise, restore old vars
                    for ((s1, _), (s2, _)) in h1.iter().zip(h2.iter()) {
                        if let Some(_) = vars.remove(s1) {
                            if let Some(s) = temp_vars.remove(s1) {
                                vars.insert(s1, s);
                            }
                        }
                        if let Some(_) = vars.remove(s2) {
                            if let Some(s) = temp_vars.remove(s2) {
                                vars.insert(s2, s);
                            }
                        }
                    }
                    return true;
                }

                // same types and fields
                // ORDER of keys may be bad
                (TypeExpr::Prod(h1), TypeExpr::Prod(h2))
                    | (TypeExpr::Sum(h1), TypeExpr::Sum(h2)) => {
                        return h1.iter().zip(h2.iter())
                            .fold(true, |a, ((s1, t1), (s2, t2))|
                                a && (s1 == s2) && inner(t1, t2, vars)
                            );
                    },

                // same params and returns
                (TypeExpr::Func(t1, t2), TypeExpr::Func(t3, t4)) => {
                    return inner(t1, t3, vars) && inner(t2, t4, vars);
                }

                _ => {
                    return false;
                },
            }
        }

        return inner(self, other, &mut HashMap::new());
    }
}

impl Eq for TypeExpr<'_> {}

impl Display for TypeExpr<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            TypeExpr::Variable(s) => {
                // just print name
                write!(f, "{}", s)?;
            },

            TypeExpr::TypeParams(t, l) => {
                // print subexpression
                write!(f, "{}{{", *t)?;
                // print all parameters
                l.iter().fold(Ok(true), |a: Result<_, fmt::Error>, t| a
                    .and_then(|first| {
                        if !first {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", *t)?;
                        return Ok(false);
                    })
                )?;
                write!(f, "}}")?;
            },

            TypeExpr::Univ(h, t) => {
                // print quantifier
                write!(f, "!")?;
                // print type vars and their kinds
                h.iter().fold(Ok(true), |a: Result<_, fmt::Error>, (s, k)| a
                    .and_then(|first| {
                        if !first {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}: {}", s, k)?;
                        return Ok(false);
                    })
                )?;
                // write subexpression
                write!(f, " . {}", *t)?;
            },

            TypeExpr::Exis(h, t) => {
                // print quantifier
                write!(f, "?")?;
                // print type vars and their kinds
                h.iter().fold(Ok(true), |a: Result<_, fmt::Error>, (s, k)| a
                    .and_then(|first| {
                        if !first {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}: {}", s, k)?;
                        return Ok(false);
                    })
                )?;
                // write subexpression
                write!(f, " . {}", *t)?;
            },

            TypeExpr::Prod(h) => {
                write!(f, "(")?;
                // print all contents
                h.iter().fold(Ok(true), |a: Result<_, fmt::Error>, (s, t)| a
                    .and_then(|first| {
                        if !first {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}: {}", s, t)?;
                        return Ok(false);
                    })
                )?;
                write!(f, ")")?;
            },

            TypeExpr::Sum(h) => {
                write!(f, "[")?;
                // print all contents
                h.iter().fold(Ok(true), |a: Result<_, fmt::Error>, (s, t)| a
                    .and_then(|first| {
                        if !first {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}: {}", s, t)?;
                        return Ok(false);
                    })
                )?;
                write!(f, "]")?;
            },

            TypeExpr::Func(t1, t2) => {
                write!(f, "{} -> {}", *t1, *t2)?;
            },
        }
        return Ok(());
    }
}

// takes a target type and a constraint type and outputs whether or not the
// target satisfies the constraint
fn satisfy<'a> (
    trgt: &'a TypeExpr<'a>,
    cstr: &'a TypeExpr<'a>,
    ctx: &HashMap<&'a str, (Option<TypeExpr<'a>>, TypeExpr<'a>)>,
) -> Result<(), TypeError> {
    const FIRST: &str = LABELS[0];

    fn inner<'a> (
        trgt: &TypeExpr<'a>,
        cstr: &TypeExpr<'a>,
        // global and local maps from variable name to type expression and kind
        glbl_ctx: &HashMap<&'a str, (Option<TypeExpr<'a>>, TypeExpr<'a>)>,
        trgt_ctx: &mut HashMap<&'a str, (Option<TypeExpr<'a>>, TypeExpr<'a>)>,
        cstr_ctx: &mut HashMap<&'a str, (Option<TypeExpr<'a>>, TypeExpr<'a>)>,
    ) -> Result<(), TypeError> {
        // check target for simplification
        match trgt {
            // if singleton, may strip
            TypeExpr::Prod(h) | TypeExpr::Sum(h) => {
                if let Ok((&FIRST, t)) = h.iter().exactly_one() {
                    return inner(t, cstr, glbl_ctx, trgt_ctx, cstr_ctx);
                }
            },

            // if exis, insert vars into ctx
            TypeExpr::Exis(h, t) => {
                let mut restore = HashMap::new();
                // collect replaced vars
                for (s, k) in h.iter() {
                    if let Some(rep) = trgt_ctx.insert(s, (None, k.clone())) {
                        restore.insert(s, rep);
                    }
                }

                inner(t, cstr, glbl_ctx, trgt_ctx, cstr_ctx)?;

                // restore replaced vars
                for (s, _) in h.iter() {
                    if let Some(rep) = restore.remove(s) {
                        trgt_ctx.insert(s, rep);
                    } else {
                        trgt_ctx.remove(s);
                    }
                }

                return Ok(());
            },

            _ => {},
        }

        // check constraint for simplification
        match cstr {
            // if singleton, may strip
            TypeExpr::Prod(h) | TypeExpr::Sum(h) => {
                if let Ok((&FIRST, t)) = h.iter().exactly_one() {
                    return inner(trgt, t, glbl_ctx, trgt_ctx, cstr_ctx);
                }
            },

            // if exis, insert vars into ctx
            TypeExpr::Exis(h, t) => {
                let mut restore = HashMap::new();
                // collect replaced vars
                for (s, k) in h.iter() {
                    if let Some(rep) = cstr_ctx.insert(s, (None, k.clone())) {
                        restore.insert(s, rep);
                    }
                }

                inner(trgt, t, glbl_ctx, trgt_ctx, cstr_ctx)?;

                // restore replaced vars
                for (s, _) in h.iter() {
                    if let Some(rep) = restore.remove(s) {
                        cstr_ctx.insert(s, rep);
                    } else {
                        cstr_ctx.remove(s);
                    }
                }

                return Ok(());
            },

            _ => {},
        }

        // otherwise, check if enums and contents are identical
        match (trgt, cstr) {
            (TypeExpr::Variable(s1), TypeExpr::Variable(s2)) => {
                // ok if local variables correspond
                // also ok if neither are local and same global type variable
                if s1 == s2 && glbl_ctx.get(s1).is_some() {
                    return Ok(());
                }
            },

            /*
            // same subexpr and params
            (TypeExpr::TypeParams(t1, l1), TypeExpr::TypeParams(t2, l2)) => {
                return inner(t1, t2, vars) && l1.iter().zip(l2.iter())
                    .fold(true, |a, (t1, t2)| a && inner(t1, t2, vars));
            },
            */

            // product down-coercion, h1 keys must be superset of h2 keys
            (TypeExpr::Prod(h1), TypeExpr::Prod(h2)) => {
                for (s, t2) in h2.iter() {
                    if let Some(t1) = h1.get(s) {
                        // if key matches, must satisfy interior constraint
                        inner(t1, t2, glbl_ctx, trgt_ctx, cstr_ctx)?;
                    } else {
                        return Err(TypeError::DefaultErr);
                    }
                }
                return Ok(());
            },

            // sum up-coercion, h1 keys must be subset of h2 keys
            (TypeExpr::Sum(h1), TypeExpr::Sum(h2)) => {
                for (s, t1) in h1.iter() {
                    if let Some(t2) = h2.get(s) {
                        // if key matches, must satisfy interior constraint
                        inner(t1, t2, glbl_ctx, trgt_ctx, cstr_ctx)?;
                    } else {
                        return Err(TypeError::DefaultErr);
                    }
                }
                return Ok(());
            },

            // same params and returns
            (TypeExpr::Func(t1, t2), TypeExpr::Func(t3, t4)) => {
                inner(t1, t3, glbl_ctx, trgt_ctx, cstr_ctx)?;
                return inner(t2, t4, glbl_ctx, trgt_ctx, cstr_ctx);
            }

            _ => {},
        }

        // check target substitutions
        match trgt {
            // replace variable
            TypeExpr::Variable(s) => {
                if let Some((o, _)) = trgt_ctx.get(s) {
                    // TODO check kinds?
                    // type defined with implementation, so substitute
                    if let Some(t) = o {
                        return inner(t, cstr, glbl_ctx, &mut HashMap::new(), cstr_ctx);
                    // type missing implementation, meaning is existential
                    } else {
                        // if not caught above, existential var can never satsify
                        return Err(TypeError::DefaultErr);
                    }
                }

                match glbl_ctx.get(s) {
                    // type defined with implementation, so substitute
                    Some((Some(t), _)) => {
                        return inner(t, cstr, glbl_ctx, &mut HashMap::new(), cstr_ctx);
                    },

                    // type defined, but missing implementation
                    Some((None, _)) => {
                        // TODO lookup base types?
                    },

                    // type undefined
                    None => {
                        return Err(TypeError::DefaultErr);
                    },
                }
            },

            // product down-coercion to singleton
            TypeExpr::Prod(h) => {
                if let Some(t) = h.get(FIRST) {
                    if inner(t, cstr, glbl_ctx, trgt_ctx, cstr_ctx).is_ok() {
                        return Ok(());
                    }
                }
            },

            _ => {},
        }

        // check constraint substitutions
        match cstr {
            // replace variable
            TypeExpr::Variable(s) => {
                if let Some((o, k)) = cstr_ctx.get(s) {
                    // TODO check kinds?
                    // type defined with implementation, so substitute
                    if let Some(t) = o {
                        return inner(trgt, t, glbl_ctx, trgt_ctx, &mut HashMap::new());
                    // type missing implementation, meaning is existential
                    } else {
                        // existential var can always be replaced in constraint
                        cstr_ctx.insert(s, (Some(trgt.clone()), k.clone()));
                        return Ok(());
                    }
                }

                match glbl_ctx.get(s) {
                    // type defined with implementation, so substitute
                    Some((Some(t), _)) => {
                        return inner(trgt, t, glbl_ctx, trgt_ctx, &mut HashMap::new());
                    },

                    // type defined, but missing implementation
                    Some((None, _)) => {
                        // TODO lookup base types?
                    },

                    // type undefined
                    None => {
                        return Err(TypeError::DefaultErr);
                    },
                }
            },

            // sum up-coercion from singleton
            TypeExpr::Sum(h) => {
                if let Some(t) = h.get(FIRST) {
                    if inner(trgt, t, glbl_ctx, trgt_ctx, cstr_ctx).is_ok() {
                        return Ok(());
                    }
                }
            },

            _ => {},
        }

        return Err(TypeError::TypeMismatch(format!("{}", trgt), format!("{}", cstr)));
    }
    // TODO check for universal here? interior naked universal is not allowed


    return inner(trgt, cstr, ctx, &mut HashMap::new(), &mut HashMap::new());
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::super::parser;

    #[test]
    fn basic_satisfy_1() {
        let t1 = parser::type_expr(
            "[U32]"
        ).unwrap();
        let t2 = parser::type_expr(
            "(U32)"
        ).unwrap();
        assert!(satisfy(&t1, &t2, &HashMap::from([("U32", (None, KIND_0.clone()))])).is_ok());
    }

    #[test]
    fn basic_satisfy_2() {
        // down-coerces to B, then up-coerces to [B, B]
        let t1 = parser::type_expr(
            "(U32, U32)"
        ).unwrap();
        let t2 = parser::type_expr(
            "[U32, U32]"
        ).unwrap();
        assert!(satisfy(&t1, &t2, &HashMap::from([("U32", (None, KIND_0.clone()))])).is_ok());
    }

    #[test]
    fn basic_satisfy_3() {
        let t1 = parser::type_expr(
            "(U32, U32)"
        ).unwrap();
        let t2 = parser::type_expr(
            "?A . (A, A)"
        ).unwrap();
        assert!(satisfy(&t1, &t2, &HashMap::from([("U32", (None, KIND_0.clone()))])).is_ok());
    }

    /*#[test]
    fn basic_type_equal_1() {
        let t1 = parser::type_expr(
            "A"
        ).unwrap();
        let t2 = parser::type_expr(
            "A"
        ).unwrap();
        assert!(t1 == t2);
    }

    #[test]
    fn basic_type_equal_2() {
        let t1 = parser::type_expr(
            "([A])"
        ).unwrap();
        let t2 = parser::type_expr(
            "A"
        ).unwrap();
        assert!(t1 == t2);
    }

    #[test]
    fn basic_type_equal_3() {
        let t1 = parser::type_expr(
            "!A . ([A])"
        ).unwrap();
        let t2 = parser::type_expr(
            "!A . A"
        ).unwrap();
        assert!(t1 == t2);
    }

    #[test]
    fn basic_type_equal_4() {
        let t1 = parser::type_expr(
            "!A . A"
        ).unwrap();
        let t2 = parser::type_expr(
            "!B . B"
        ).unwrap();
        assert!(t1 == t2);
    }

    #[test]
    fn basic_type_equal_5() {
        let t1 = parser::type_expr(
            "?A . [(A, A -> A), ()]"
        ).unwrap();
        let t2 = parser::type_expr(
            "[?B . (B, B -> B), ()]"
        ).unwrap();
        assert!(t1 == t2);
    }*/
}

