// ast for halt

use std::{
    fmt, fmt::{Display, Formatter},
    collections::HashMap,
    hash::{Hash, Hasher},
};

use lazy_static::lazy_static;

use super::misc::*;

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
        let vars = HashMap::new();

        fn inner(one: &TypeExpr, two: &TypeExpr, vars: &HashMap<&str, &str>) -> bool {
            // if singleton, may strip
            match (one, two) {
                (TypeExpr::Prod(l), t1) | (TypeExpr::Sum(l), t1)
                    | (t1, TypeExpr::Prod(l)) | (t1, TypeExpr::Sum(l)) =>
                        if let [(FIRST, t2)] = l.as_slice() { return inner(t1, t2, vars); },
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
                    let mut new_vars = vars.clone();
                    return (b1 == b2) && l1.iter().zip(l2.iter())
                        // add new quantified vars so they correspond to each other
                        // also check equality of kinds
                        .fold(true, |a, ((s1, k1), (s2, k2))| {
                            new_vars.insert(s1, s2);
                            new_vars.insert(s2, s1);
                            return a && (k1 == k2);
                        }) && (b1 == b2) && inner(t1, t2, &new_vars);
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

        return inner(self, other, &vars);
    }
}

impl Eq for TypeExpr<'_> {}

impl Hash for TypeExpr<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        const FIRST: &str = LABELS[0];
        // if singleton, may strip
        match self {
            TypeExpr::Prod(l) | TypeExpr::Sum(l)  =>
                if let [(FIRST, t)] = l.as_slice() { t.hash(state); },
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

#[cfg(test)]
mod tests {
    use super::super::parser;

    #[test]
    fn basic_type_equal_1() {
        let t1 = parser::type_expr(
            "([A])"
        ).unwrap();
        let t2 = parser::type_expr(
            "A"
        ).unwrap();
        assert!(t1 == t2);
    }

    #[test]
    fn basic_type_equal_2() {
        let t1 = parser::type_expr(
            "!A . ([A])"
        ).unwrap();
        let t2 = parser::type_expr(
            "!A . A"
        ).unwrap();
        assert!(t1 == t2);
    }

    #[test]
    fn basic_type_equal_3() {
        let t1 = parser::type_expr(
            "!A . A"
        ).unwrap();
        let t2 = parser::type_expr(
            "!B . B"
        ).unwrap();
        assert!(t1 == t2);
    }
}

