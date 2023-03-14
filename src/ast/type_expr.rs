// type expression part of the ast

use std::{
    fmt, fmt::{Display, Formatter},
    collections::HashMap,
    hash::{Hash, Hasher},
};

use lazy_static::lazy_static;

use super::super::misc::*;

lazy_static! {
    pub static ref KIND_0: TypeExpr<'static> = TypeExpr::Variable("Type");
    pub static ref KIND_1: TypeExpr<'static> = TypeExpr::Function(Box::new(KIND_0.clone()), Box::new(KIND_0.clone()));
    pub static ref KIND_2: TypeExpr<'static> = TypeExpr::Function(Box::new(KIND_0.clone()), Box::new(KIND_1.clone()));
}

#[derive(Debug, Clone)]
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

impl PartialEq for TypeExpr<'_> {
    fn eq(&self, other: &Self) -> bool {
        const FIRST: &str = LABELS[0];
        let mut vars = HashMap::new();

        fn inner<'a>(
            one: &TypeExpr<'a>,
            two: &TypeExpr<'a>,
            vars: &mut HashMap<&'a str, &'a str>
        ) -> bool {
            // if singleton, may strip
            match (one, two) {
                (TypeExpr::Prod(l), t1) | (TypeExpr::Sum(l), t1)
                    | (t1, TypeExpr::Prod(l)) | (t1, TypeExpr::Sum(l)) => {
                    if let [(FIRST, t2)] = l.as_slice() {
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
                (TypeExpr::Quantified {
                    params: l1, is_univ: b1, subexpr: t1
                }, TypeExpr::Quantified {
                    params: l2, is_univ: b2, subexpr: t2
                }) => {
                    // TODO rename vars so new context doesn't intersect old? i.e. don't have to clone...
                    let mut temp_vars = HashMap::new();
                    // quantifier must be same
                    if b1 != b2 {
                        return false;
                    }

                    for ((s1, k1), (s2, k2)) in l1.iter().zip(l2.iter()) {
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
                    for ((s1, _), (s2, _)) in l1.iter().zip(l2.iter()) {
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
                (TypeExpr::Prod(l1), TypeExpr::Prod(l2))
                    | (TypeExpr::Sum(l1), TypeExpr::Sum(l2)) => {
                        return l1.iter().zip(l2.iter())
                            .fold(true, |a, ((s1, t1), (s2, t2))|
                                a && (s1 == s2) && inner(t1, t2, vars)
                            );
                    },

                // same params and returns
                (TypeExpr::Function(t1, t2), TypeExpr::Function(t3, t4)) => {
                    return inner(t1, t3, vars) && inner(t2, t4, vars);
                }

                _ => {
                    return false;
                },
            }
        }

        return inner(self, other, &mut vars);
    }
}

impl Eq for TypeExpr<'_> {}

impl Hash for TypeExpr<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        const FIRST: &str = LABELS[0];
        // if singleton, may strip
        match self {
            TypeExpr::Prod(l) | TypeExpr::Sum(l)  => {
                if let [(FIRST, t)] = l.as_slice() {
                    t.hash(state);
                    return;
                }
            },
            _ => {},
        }

        // otherwise, hash based on nested structure
        match self {
            TypeExpr::Variable(s) => {
                0.hash(state);
                s.hash(state);
            },

            TypeExpr::TypeParams(t, l) => {
                1.hash(state);
                t.hash(state);
                l.hash(state);
            },

            // same quantifier TODO may have diff param names but still eq
            // (also effects var, since might be a local var)
            TypeExpr::Quantified {params: l, is_univ: b, subexpr: t} => {
                2.hash(state);
                l.hash(state);
                b.hash(state);
                t.hash(state);
            },

            TypeExpr::Prod(l) => {
                3.hash(state);
                l.hash(state);
            },

            TypeExpr::Sum(l) => {
                4.hash(state);
                l.hash(state);
            },

            // same params and returns
            TypeExpr::Function(t1, t2) => {
                5.hash(state);
                t1.hash(state);
                t2.hash(state);
            },
        }
    }
}

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

            TypeExpr::Quantified {
                params:  l,
                is_univ: b,
                subexpr: t,
            } => {
                // print quantifier
                if *b {
                    write!(f, "!")?;
                } else {
                    write!(f, "?")?;
                }
                // print type vars and their kinds
                l.iter().fold(Ok(true), |a: Result<_, fmt::Error>, (s, k)| a
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

            TypeExpr::Prod(l) => {
                write!(f, "(")?;
                // print all contents
                l.iter().fold(Ok(true), |a: Result<_, fmt::Error>, (s, t)| a
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

            TypeExpr::Sum(l) => {
                write!(f, "[")?;
                // print all contents
                l.iter().fold(Ok(true), |a: Result<_, fmt::Error>, (s, t)| a
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

            TypeExpr::Function(t1, t2) => {
                write!(f, "{} -> {}", *t1, *t2)?;
            },
        }
        return Ok(());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::super::parser;
    use std::collections::hash_map::DefaultHasher;

    #[test]
    fn basic_type_equal_1() {
        let t1 = parser::type_expr(
            "A"
        ).unwrap();
        let mut s = DefaultHasher::new();
        t1.hash(&mut s);
        let h1 = s.finish();
        let t2 = parser::type_expr(
            "A"
        ).unwrap();
        let mut s = DefaultHasher::new();
        t2.hash(&mut s);
        let h2 = s.finish();
        assert!(t1 == t2);
        assert!(h1 == h2);
    }

    #[test]
    fn basic_type_equal_2() {
        let t1 = parser::type_expr(
            "([A])"
        ).unwrap();
        let mut s = DefaultHasher::new();
        t1.hash(&mut s);
        let h1 = s.finish();
        let t2 = parser::type_expr(
            "A"
        ).unwrap();
        let mut s = DefaultHasher::new();
        t2.hash(&mut s);
        let h2 = s.finish();
        assert!(t1 == t2);
        assert!(h1 == h2);
    }

    #[test]
    fn basic_type_equal_3() {
        let t1 = parser::type_expr(
            "!A . ([A])"
        ).unwrap();
        let mut s = DefaultHasher::new();
        t1.hash(&mut s);
        let h1 = s.finish();
        let t2 = parser::type_expr(
            "!A . A"
        ).unwrap();
        let mut s = DefaultHasher::new();
        t2.hash(&mut s);
        let h2 = s.finish();
        assert!(t1 == t2);
        assert!(h1 == h2);
    }

    #[test]
    fn basic_type_equal_4() {
        let t1 = parser::type_expr(
            "!A . A"
        ).unwrap();
        let mut s = DefaultHasher::new();
        t1.hash(&mut s);
        let h1 = s.finish();
        let t2 = parser::type_expr(
            "!B . B"
        ).unwrap();
        let mut s = DefaultHasher::new();
        t2.hash(&mut s);
        let h2 = s.finish();
        assert!(t1 == t2);
        assert!(h1 == h2);
    }
}
