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
    let mut types: HashMap<&str, &TypeExpr> = HashMap::new();// = misc::get_basic_kword_type_exprs();
    let mut consts: HashMap<&str, (&Option<TypeExpr>, &ValueExpr)> = HashMap::new();
    // split types, building initial context
    for def in defs.iter() {
        match def {
            Definition::Type(t) => {
                if types.contains_key(t.name.as_str()) {
                    return Err(TypeError::MultiDef(t.name.clone()).into());
                }
                types.insert(t.name.as_str(), &t.expr);
            }
            Definition::Const(c) => {
                if consts.contains_key(c.name.as_str()) {
                    return Err(ValueError::MultiDef(c.name.clone()).into());
                }
                consts.insert(c.name.as_str(), (&c.type_expr, &c.expr));
            }
        }
    }
    // validate types
    validate_type_defs(&types)?;
    return Ok(());
}

// TODO add context param if validating locally defined types
fn validate_type_defs(types: &HashMap<&str, &TypeExpr>) -> Result<(), TypeError> {
    // graph of type variable dependencies
    let mut dep_graph: Graph<&str, _> = Graph::new();

    // map of var to dependencies, map of var to indices into graph
    let (deps, node_idx): (HashMap<&str, _>, HashMap<&str, _>) =
        types.iter()
        // get dependencies for each type var and add nodes to graph
        .map(|(s, t)| (
            (*s, get_type_deps(t, HashSet::new())),
            (*s, dep_graph.add_node(s))
        ))
        .multiunzip();

    // map of var to num params, set of undefined vars
    let (mut type_params, missing_vars): (HashMap<&str, Option<usize>>, HashSet<_>) = deps.iter()
        // merge all dependencies
        .fold(HashSet::new(), |mut a, (_, b)| {a.extend(b); return a})
        .into_iter()
        // only get those that are undefined
        .filter(|s| !deps.contains_key(s))
        // split into keyword types (getting param count) and those that are not
        .partition_map(|s| {
            match get_kword_type_params(s) {
                Some(n) => Left((s, Some(n))),
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

    // get number of type parameters for each type
    for name in nodes.into_iter().map(|x| dep_graph[x]) {
        if let Some(type_expr) = types.get(name) {
            type_params.insert(name, get_type_params(
                type_expr,
                &type_params,
                HashSet::new(),
                HashSet::new(),
            )?);
        }
    }
    return Ok(());
}

// get type params for keyword types that are numeric or otherwise special
fn get_kword_type_params(type_name: &str) -> Option<usize> {
    use lazy_static::lazy_static;
    use regex::{Regex, Captures};
    match type_name {
        // integer numerics
        "USize" => return Some(0),
        "U32"   => return Some(0),
        "U16"   => return Some(0),
        "U8"    => return Some(0),
        "SSize" => return Some(0),
        "S32"   => return Some(0),
        "S16"   => return Some(0),
        "S8"    => return Some(0),
        // floating point number
        "F32"   => return Some(0),
        // array
        "Arr"   => return Some(2),
        _       => {
            let f = |c: Option<Captures>| c
                .and_then(|x| x.get(1))
                .and_then(|x| x.as_str().parse::<u32>().ok());
            lazy_static! {
                // enum for up to 2^32 choices
                static ref ENUM: Regex = Regex::new("^N(\\d+)$").unwrap();
            }
            if f(ENUM.captures(type_name)).is_some() {
                return Some(0);
            }
        },
    }
    return None;
}

// gets the number of type parameters for a type
// assumes all input type parameters take no parameters themselves
fn get_type_params<'a>(
    // target type expression
    type_expr:  &'a TypeExpr,
    // number of params for known types
    num_params: &HashMap<&str, Option<usize>>,
    // known universal type variables
    mut univ:   HashSet<&'a str>,
    // known existential type variables
    mut exis:   HashSet<&'a str>,
) -> Result<Option<usize>, TypeError> {
    match type_expr {
        TypeExpr::Variable(s) => {
            // is a locally-defined universal type var, 0 params
            if univ.contains(s.as_str()) {
                return Ok(Some(0));
            }
            // is a locally-defined existential type var, arbitrary params
            if exis.contains(s.as_str()) {
                return Ok(None);
            }
            // not a locally-defined type var, look up params
            return Ok(*num_params.get(s.as_str()).ok_or(TypeError::DefaultErr)?);
        },

        // check all parameters exactly
        TypeExpr::TypeParams(t, l) => {
            let mut acc = get_type_params(t, num_params, univ.clone(), exis.clone())?;
            for t in l.into_iter() {
                // has no more slots for params
                if let Some(0) = acc {
                    return Err(TypeError::TooManyParams);
                }
                // loses 1 slot per param, but might be a param that itself has params
                acc = get_type_params(
                    t,
                    num_params,
                    univ.clone(),
                    exis.clone(),
                )?.and_then(|n| acc.map(|m| n + m - 1));
            }
            return Ok(acc);
        },

        // adds locally-defined type var from parameter
        TypeExpr::Universal(s, t) => {
            univ.insert(s);
            return Ok(get_type_params(t, num_params, univ, exis)?.map(|n| n + 1));
        },

        // adds locally-defined type var
        TypeExpr::Existential(s, t) => {
            exis.insert(s);
            return get_type_params(t, num_params, univ, exis);
        },

        // must check all subtrees, number of parameters to each must be the same
        TypeExpr::Prod(l) | TypeExpr::Sum(l) => {
            if l.len() == 0 {
                return Ok(Some(0));
            }
            let mut out = None;
            for (_, t) in l.into_iter() {
                let o = get_type_params(t, num_params, univ.clone(), exis.clone())?;
                // param count must be same across branches or none
                if out != o {
                    out = out.xor(o);
                    // only get here if both are some and are not equal
                    out.ok_or(TypeError::InconsParams("Bob".to_string()))?; // TODO fix
                }
            }
            return Ok(out);
        },

        // must check all subtrees, number of parameters to each must be the same
        TypeExpr::Function(t1, t2) => {
            let o = get_type_params(t1, num_params, univ.clone(), exis.clone())?;
            let mut out = get_type_params(t2, num_params, univ, exis)?;
            // param count must be same across branches or none
            if out != o {
                out = out.xor(o);
                // only get here if both are some and are not equal
                out.ok_or(TypeError::InconsParams("Bob".to_string()))?; // TODO fix
            }
            return Ok(out);
        },
    }
}

// gets the type variable dependencies
fn get_type_deps<'a>(
    // target type expression
    type_expr: &'a TypeExpr,
    // known local type variables
    mut vars:  HashSet<&'a str>,
) -> HashSet<&'a str> {
    match type_expr {
        TypeExpr::Variable(s) => {
            // is a locally-defined type var, not a dependency
            return if vars.contains(s.as_str()) {
                HashSet::new()
            } else {
            // not a locally-defined type var, is a dependency
                HashSet::from([s.as_str()])
            };
        },

        // check all parameters exactly
        TypeExpr::TypeParams(t, l) => {
            let mut out = get_type_deps(t, vars.clone());
            for type_expr in l.into_iter() {
                out.extend(get_type_deps(type_expr, vars.clone()));
            }
            return out;
        },

        // adds locally-defined type var from parameter
        TypeExpr::Universal(s, t) => {
            vars.insert(s);
            return get_type_deps(t, vars);
        },

        // adds locally-defined type var
        TypeExpr::Existential(s, t) => {
            vars.insert(s);
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

    #[test]
    fn basic_type_params_1() {
        assert!(get_type_params(
            &parser::type_expr(
                "([A, B], C -> C)"
            ).unwrap(),
            &HashMap::from([("A", Some(0)), ("B", Some(0)), ("C", Some(0))]),
            HashSet::new(),
            HashSet::new(),
        ).unwrap().unwrap() == 0);
    }

    #[test]
    fn basic_type_params_2() {
        assert!(get_type_params(
            &parser::type_expr(
                "(A! B, C)"
            ).unwrap(),
            &HashMap::from([("B", Some(0)), ("C", Some(1))]),
            HashSet::new(),
            HashSet::new(),
        ).unwrap().unwrap() == 1);
    }

    #[test]
    fn basic_type_params_3() {
        assert!(get_type_params(
            &parser::type_expr(
                "(A? A{C}, B)"
            ).unwrap(),
            &HashMap::from([("B", Some(0)), ("C", Some(0))]),
            HashSet::new(),
            HashSet::new(),
        ).unwrap().unwrap() == 0);
    }

    #[test]
    fn medium_type_params_1() {
        assert!(get_type_params(
            &parser::type_expr(
                "A{B, C}"
            ).unwrap(),
            &HashMap::from([("A", Some(2)), ("B", Some(3)), ("C", Some(0))]),
            HashSet::new(),
            HashSet::new(),
        ).unwrap().unwrap() == 3);
    }

    #[test]
    fn medium_type_params_2() {
        assert!(get_type_params(
            &parser::type_expr(
                "(A! A, (B! C! [B, C]){D})"
            ).unwrap(),
            &HashMap::from([("D", Some(0))]),
            HashSet::new(),
            HashSet::new(),
        ).unwrap().unwrap() == 1);
    }

    #[test]
    fn medium_type_params_3() {
        assert!(get_type_params(
            &parser::type_expr(
                "(A! (A, A, A)){[B! C! ()]{D! E! F! ()}, D}"
            ).unwrap(),
            &HashMap::from([("D", Some(0))]),
            HashSet::new(),
            HashSet::new(),
        ).unwrap().unwrap() == 3);
    }

    #[test]
    fn basic_type_params_fail_1() {
        assert!(matches!(get_type_params(
            &parser::type_expr(
                "(A! B, C)"
            ).unwrap(),
            &HashMap::from([("B", Some(0)), ("C", Some(0))]),
            HashSet::new(),
            HashSet::new(),
        ), Err(TypeError::InconsParams(_))));
    }

    #[test]
    fn basic_type_params_fail_2() {
        assert!(matches!(get_type_params(
            &parser::type_expr(
                "(A, B{C! D})"
            ).unwrap(),
            &HashMap::from([("A", Some(0)), ("B", Some(0)), ("D", Some(0))]),
            HashSet::new(),
            HashSet::new(),
        ), Err(TypeError::TooManyParams)));
    }

    #[test]
    fn basic_type_params_fail_3() {
        assert!(matches!(get_type_params(
            &parser::type_expr(
                "(){A}"
            ).unwrap(),
            &HashMap::from([("A", Some(0))]),
            HashSet::new(),
            HashSet::new(),
        ), Err(TypeError::TooManyParams)));
    }

    #[test]
    fn basic_type_params_fail_4() {
        assert!(matches!(get_type_params(
            &parser::type_expr(
                "(Foo, ()){A}"
            ).unwrap(),
            &HashMap::from([("Foo", Some(0)), ("A", Some(0))]),
            HashSet::new(),
            HashSet::new(),
        ), Err(TypeError::TooManyParams)));
    }

    #[test]
    fn basic_type_params_fail_5() {
        assert!(matches!(get_type_params(
            &parser::type_expr(
                "[A, ()]"
            ).unwrap(),
            &HashMap::from([("A", Some(1))]),
            HashSet::new(),
            HashSet::new(),
        ), Err(TypeError::InconsParams(_))));
    }

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


