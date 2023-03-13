// tools for checking types

use std::{iter, collections::{HashSet, HashMap}};

use itertools::{Itertools, Either::{Left, Right}};
use petgraph::{Graph, algo::toposort};

use super::ast::*;
use super::error::{CompileError, TypeError, ValueError};
use super::misc;

#[derive(Debug, PartialEq)]
struct Context {
    types:  HashMap<String, Vec<TypeExpr>>,
    values: HashMap<String, Vec<ValueExpr>>,
}

//fn uniquify_names(def: &mut Vec<Definition>) {
//    let mut seen_names = HashMap::new();
//    for def in defs.iter() {
//        match
//    }
//}

/*
fn unify(lhs: Option<TypeExpr>, rhs: Option<TypeExpr>, ctx: Context) -> Result<(), &'static str> {
    Ok(())
}
*/

pub fn check_defs(defs: Vec<Definition>) -> Result<(), CompileError> {
    let mut types: HashMap<&str, &TypeDef> = HashMap::new();
    let mut consts: HashMap<&str, &ConstDef> = HashMap::new();
    // split types, building initial context
    for def in defs.iter() {
        match def {
            Definition::Type(t) => {
                if types.contains_key(t.name.as_str()) {
                    return Err(TypeError::MultiDef(t.name.clone()).into());
                }
                types.insert(t.name.as_str(), &t);
            }
            Definition::Const(c) => {
                if consts.contains_key(c.name.as_str()) {
                    return Err(ValueError::MultiDef(c.name.clone()).into());
                }
                consts.insert(c.name.as_str(), &c);
            }
        }
    }
    // validate types
    validate_type_defs(&types)?;
    return Ok(());
}

// TODO add context param if validating locally defined types
fn validate_type_defs(types: &HashMap<&str, &TypeDef>) -> Result<(), TypeError> {
    // graph of type variable dependencies
    let mut dep_graph: Graph<&str, _> = Graph::new();

    // map of var to dependencies, map of var to indices into graph
    let (deps, node_idx): (HashMap<&str, _>, HashMap<&str, _>) =
        types.iter()
        // get dependencies for each type var and add nodes to graph
        .map(|(s, t)| (
            (*s, get_type_deps(&t.texpr, HashSet::new())),
            (*s, dep_graph.add_node(s))
        ))
        .multiunzip();

    // map of var to num params, set of undefined vars
    let (mut type_kinds, missing_vars): (HashMap<&str, TypeExpr>, HashSet<_>) = deps.iter()
        // merge all dependencies
        .fold(HashSet::new(), |mut a, (_, b)| {a.extend(b); return a})
        .into_iter()
        // only get those that are undefined
        .filter(|s| !deps.contains_key(s))
        // split into keyword types (getting param count) and those that are not
        .partition_map(|s| {
            match get_kword_type_kind(s) {
                Some(n) => Left((s, n)),
                None    => Right(s),
            }
        });

    // if any remaining non-keyword types, fail
    if let Some(name) = missing_vars.iter().next() {
        return Err(TypeError::Undef(name.to_string()));
    }

    // add dependency graph edges
    for (s1, s2) in deps.iter().flat_map(|(s, m)| iter::repeat(s).zip(m.iter())) {
        if let (Some(j), Some(k)) = (node_idx.get(s1), node_idx.get(s2)) {
            dep_graph.add_edge(*k, *j, ());
        }
    }

    // get topological order, error if any recursive definitions
    let nodes = toposort(&dep_graph, None)
        .map_err(|e| TypeError::RecurDef(dep_graph[e.node_id()].to_string()))?;

    // get kind of each type
    for name in nodes.into_iter().map(|x| dep_graph[x]) {
        if let Some(type_def) = types.get(name) {
            let k_inf = infer_kind(&type_def.texpr, &type_kinds)?;
            if let Some(k_annot) = &type_def.kexpr {
                if !valid_kind(&k_annot) {
                    return Err(TypeError::BadKind("TODO: FIXME".to_string())); // TODO
                }
                if k_annot != &k_inf {
                    return Err(TypeError::KindMismatch("TODO: FIXME".to_string())); // TODO
                }
            }
            type_kinds.insert(name, k_inf);
        }
    }
    return Ok(());
}

// gets the number of type parameters for a type
// assumes all input type parameters take no parameters themselves
fn infer_kind<'a>(
    // target type expression
    texpr:      &'a TypeExpr,
    // kind known types
    type_kinds: &HashMap<&str, TypeExpr>,
) -> Result<TypeExpr, TypeError> {
    let null_kind = TypeExpr::Variable("Type".to_string());
    match texpr {
        // look up variable kind
        TypeExpr::Variable(s) => {
            return type_kinds.get(s.as_str()).map(|k| k.clone()).ok_or(TypeError::DefaultErr);
        },

        // must supply params directly to higher-kinded type
        TypeExpr::TypeParams(t, l) => {
            let mut kexpr = infer_kind(t, &type_kinds.clone())?;
            for t in l.into_iter() {
                // check if has slots for params
                match kexpr {
                    TypeExpr::Function(k1, k2) => {
                        if *k1 == infer_kind(t, &type_kinds.clone())? {
                            kexpr = *k2;
                        } else {
                            return Err(TypeError::KindMismatch("TODO: FIXME".to_string())); // TODO
                        }
                    },
                    _ => {
                        return Err(TypeError::TooManyParams("TODO: FIXME".to_string())); // TODO
                    },
                }
            }
            return Ok(kexpr);
        },

        // adds locally-defined type var
        TypeExpr::Quantified {
            params:  l,
            is_univ: b,
            subexpr: t,
        } => {
            let mut new_type_kinds = type_kinds.clone();
            // insert kinds from parameters (checking validity)
            for (name, k) in l.iter() {
                if !valid_kind(k) {
                    return Err(TypeError::BadKind("TODO: FIXME".to_string())); // TODO
                }
                new_type_kinds.insert(name, k.clone());
            }

            // if universal, add parameter kinds
            let mut kexpr = infer_kind(t, &new_type_kinds)?;
            if *b {
                for (_, k) in l.iter() {
                    kexpr = TypeExpr::Function(Box::new(k.clone()), Box::new(kexpr));
                }
            }
            return Ok(kexpr);
        },

        // check all subtrees, kind must each be nullary
        TypeExpr::Prod(l) | TypeExpr::Sum(l) => {
            for (_, t) in l.into_iter() {
                if infer_kind(t, &type_kinds.clone())? != null_kind {
                    return Err(TypeError::MustNullKind("TODO: FIXME".to_string())); // TODO
                }
            }
            return Ok(TypeExpr::Variable("Type".to_string()));
        },

        // check both subtrees, kind must each be nullary
        TypeExpr::Function(t1, t2) => {
            if infer_kind(t1, &type_kinds.clone())? != null_kind {
                return Err(TypeError::MustNullKind("TODO: FIXME".to_string())); // TODO
            }
            if infer_kind(t2, type_kinds)? != null_kind {
                return Err(TypeError::MustNullKind("TODO: FIXME".to_string())); // TODO
            }
            return Ok(TypeExpr::Variable("Type".to_string()));
        },
    }
}

// only allow simple kinds (functions and nullary kind)
fn valid_kind(kexpr: &TypeExpr) -> bool {
    let null_kind = TypeExpr::Variable("Type".to_string());
    if kexpr == &null_kind {
        return true;
    }
    return match kexpr {
        TypeExpr::Function(k1, k2) => valid_kind(k1) && valid_kind(k2),
        _ => false,
    };
}

// get type params for keyword types that are numeric or otherwise special
fn get_kword_type_kind(type_name: &str) -> Option<TypeExpr> {
    use regex::{Regex, Captures};
    use lazy_static::lazy_static;
    // basic kinds
    let null_kind = TypeExpr::Variable("Type".to_string());
    let un   = TypeExpr::Function(Box::new(null_kind.clone()), Box::new(null_kind.clone()));
    let bin  = TypeExpr::Function(Box::new(null_kind.clone()), Box::new(un.clone()));
    match type_name {
        // integer numerics
        "USize" => return Some(null_kind),
        "U32"   => return Some(null_kind),
        "U16"   => return Some(null_kind),
        "U8"    => return Some(null_kind),
        "SSize" => return Some(null_kind),
        "S32"   => return Some(null_kind),
        "S16"   => return Some(null_kind),
        "S8"    => return Some(null_kind),
        // floating point number
        "F32"   => return Some(null_kind),
        // array
        "Arr"   => return Some(bin),
        _       => {
            let f = |c: Option<Captures>| c
                .and_then(|x| x.get(1))
                .and_then(|x| x.as_str().parse::<u32>().ok());
            lazy_static! {
                // enum for up to 2^32 choices
                static ref ENUM: Regex = Regex::new("^N(\\d+)$").unwrap();
            }
            if f(ENUM.captures(type_name)).is_some() {
                return Some(null_kind);
            }
        },
    }
    return None;
}

// gets the type variable dependencies
fn get_type_deps<'a>(
    // target type expression
    texpr:    &'a TypeExpr,
    // known local type variables
    mut vars: HashSet<&'a str>,
) -> HashSet<&'a str> {
    match texpr {
        TypeExpr::Variable(s) => {
            // is a locally-defined type var, not a dependency
            return if vars.contains(s.as_str()) {
                HashSet::new()
            } else {
            // not a locally-defined type var, is a dependency
                HashSet::from([s.as_str()])
            };
        },

        // check all parameters and subexpression
        TypeExpr::TypeParams(t, l) => {
            let mut out = get_type_deps(t, vars.clone());
            for type_expr in l.into_iter() {
                out.extend(get_type_deps(type_expr, vars.clone()));
            }
            return out;
        },

        // adds locally-defined type vars
        TypeExpr::Quantified {
            params: l,
            is_univ: _,
            subexpr: t,
        } => {
            for (s, _) in l.iter() {
                vars.insert(s);
            }
            return get_type_deps(t, vars);
        },

        // must check all subtrees
        TypeExpr::Prod(l) | TypeExpr::Sum(l) => {
            if l.len() == 0 {
                return HashSet::new();
            }
            let mut out = HashSet::new();
            for (_, t) in l.into_iter() {
                out.extend(get_type_deps(t, vars.clone()));
            }
            return out;
        },

        // must check all subtrees
        TypeExpr::Function(t1, t2) => {
            let mut out = get_type_deps(t1, vars.clone());
            out.extend(get_type_deps(t2, vars));
            return out;
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::parser;

    #[test]
    fn basic_type_deps_1() {
        let t = parser::type_expr(
            "([A, B], C -> C)"
        ).unwrap();
        let deps = get_type_deps(&t, HashSet::new());
        for s in ["A", "B", "C"].into_iter() {
            assert!(deps.contains(s));
        }
        assert!(!deps.contains("Bad"));
    }

    #[test]
    fn medium_type_deps_1() {
        let t = parser::type_expr(
            "A! B? ([A, B], C{A} -> C{D})"
        ).unwrap();
        let deps = get_type_deps(&t, HashSet::new());
        for s in ["A", "B"].into_iter() {
            assert!(!deps.contains(s));
        }
        assert!(deps.contains("C"));
        assert!(deps.contains("D"));
    }

    /*#[test]
    fn basic_type_params_1() {
        assert!(get_type_params(
            &parser::type_expr(
                "([A, B], C -> C)"
            ).unwrap(),
            &HashMap::from([("A", 0), ("B", 0), ("C", 0)]),
            HashSet::new()
        ).unwrap() == 0);
    }

    #[test]
    fn basic_type_params_2() {
        assert!(get_type_params(
            &parser::type_expr(
                "(A! B, C)"
            ).unwrap(),
            &HashMap::from([("B", 0), ("C", 1)]),
            HashSet::new()
        ).unwrap() == 1);
    }

    #[test]
    fn basic_type_params_3() {
        assert!(get_type_params(
            &parser::type_expr(
                "(A? A{C}, B)"
            ).unwrap(),
            &HashMap::from([("C", 0)]),
            HashSet::new()
        ).unwrap() == 0);
    }

    #[test]
    fn medium_type_params_1() {
        assert!(get_type_params(
            &parser::type_expr(
                "A{B, C}"
            ).unwrap(),
            &HashMap::from([("A", 2), ("B", 3), ("C", 0)]),
            HashSet::new()
        ).unwrap() == 3);
    }

    #[test]
    fn medium_type_params_2() {
        assert!(get_type_params(
            &parser::type_expr(
                "(A! A, (B! C! [B, C]){D})"
            ).unwrap(),
            &HashMap::from([("D", 0)]),
            HashSet::new()
        ).unwrap() == 1);
    }

    #[test]
    fn medium_type_params_3() {
        assert!(get_type_params(
            &parser::type_expr(
                "(A! (A, A, A)){[B! C! ()]{D! E! F! ()}, D}"
            ).unwrap(),
            &HashMap::from([("D", 0)]),
            HashSet::new()
        ).unwrap() == 3);
    }

    #[test]
    fn basic_type_params_fail_1() {
        assert!(matches!(get_type_params(
            &parser::type_expr(
                "(A! B, C)"
            ).unwrap(),
            &HashMap::from([("B", 0), ("C", 0)]),
            HashSet::new()
        ), Err(TypeError::InconsParams(_))));
    }

    #[test]
    fn basic_type_params_fail_2() {
        assert!(matches!(get_type_params(
            &parser::type_expr(
                "(A, B{C! D})"
            ).unwrap(),
            &HashMap::from([("A", 0), ("B", 0), ("D", 0)]),
            HashSet::new()
        ), Err(TypeError::TooManyParams)));
    }

    #[test]
    fn basic_type_params_fail_3() {
        assert!(matches!(get_type_params(
            &parser::type_expr(
                "(){A}"
            ).unwrap(),
            &HashMap::from([("A", 0)]),
            HashSet::new()
        ), Err(TypeError::TooManyParams)));
    }

    #[test]
    fn basic_type_params_fail_4() {
        assert!(matches!(get_type_params(
            &parser::type_expr(
                "(Foo, ()){A}"
            ).unwrap(),
            &HashMap::from([("Foo", 0), ("A", 0)]),
            HashSet::new()
        ), Err(TypeError::TooManyParams)));
    }

    #[test]
    fn basic_type_params_fail_5() {
        assert!(matches!(get_type_params(
            &parser::type_expr(
                "[A, ()]"
            ).unwrap(),
            &HashMap::from([("A", 1)]),
            HashSet::new()
        ), Err(TypeError::InconsParams(_))));
    }*/

    #[test]
    fn basic_type_defs_1() {
        assert!(check_defs(parser::defs(
            "Foo := U32;"
        ).unwrap()).is_ok());
    }

    #[test]
    fn basic_type_defs_2() {
        assert!(check_defs(parser::defs(
            "Foo := U32; Bar := Foo;"
        ).unwrap()).is_ok());
    }

    #[test]
    fn basic_type_defs_3() {
        assert!(check_defs(parser::defs(
            "Foo := Arr{Bar, N47}; Bar := Arr{(N1, USize), N13};"
        ).unwrap()).is_ok());
    }

    #[test]
    fn medium_type_defs_1() {
        assert!(check_defs(parser::defs(
            "Foo := A! Bar{A}; Bar := B! C! [B, C]; Baz := Foo{U32};"
        ).unwrap()).is_ok());
    }

    #[test]
    fn basic_type_defs_fail_1() {
        assert!(check_defs(parser::defs(
            "Foo := Foo;"
        ).unwrap()).is_err());
    }

    #[test]
    fn basic_type_defs_fail_2() {
        assert!(check_defs(parser::defs(
            "Foo := Bar; Bar := Foo;"
        ).unwrap()).is_err());
    }

    #[test]
    fn basic_type_defs_fail_3() {
        assert!(check_defs(parser::defs(
            "Foo := U32{U16};"
        ).unwrap()).is_err());
    }
}


