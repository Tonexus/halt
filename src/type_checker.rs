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

// indicates which types this type depends on and their possible number of
// type parameters
struct TypeDepParams {
    // dependencies for which the relative number of type parameters is known
    relat:      HashMap<String, i32>,
    // dependencies for which the exact number of type parameters is known
    exact:      HashMap<String, i32>,
    // minimum number of parameters to complete this type
    req_params: i32,
}

/*
fn unify(lhs: Option<TypeExpr>, rhs: Option<TypeExpr>, ctx: Context) -> Result<(), &'static str> {
    Ok(())
}
*/

pub fn check_defs(defs: Vec<Definition>) -> Result<(), CompileError> {
    let mut types: HashMap<String, TypeExpr> = misc::get_basic_kword_type_exprs();
    let mut consts: HashMap<String, (Option<TypeExpr>, ValueExpr)> = HashMap::new();
    // split types, building initial context
    for def in defs.into_iter() {
        match def {
            Definition::Type(t) => {
                if types.contains_key(&t.name) {
                    return Err(TypeError::MultiDef(t.name).into());
                }
                types.insert(t.name.clone(), t.expr);
            }
            Definition::Const(c) => {
                if consts.contains_key(&c.name) {
                    return Err(ValueError::MultiDef(c.name).into());
                }
                consts.insert(c.name, (c.type_expr, c.expr));
            }
        }
    }
    // validate types
    validate_type_defs(&types)?;
    return Ok(());
}

// TODO add context param if validating locally defined types
fn validate_type_defs(types: &HashMap<String, TypeExpr>) -> Result<(), TypeError> {
    let mut deps: HashMap<String, HashSet<String>> = HashMap::new();
    let mut dep_graph: Graph<&String, ()> = Graph::new();
    let mut node_idx = HashMap::new();

    // for each type var get dependencies
    for (name, type_expr) in types.iter() {
        deps.insert(name.to_string(), get_type_deps(&type_expr, HashSet::new()));
        // also insert nodes into dependency graph
        node_idx.insert(name, dep_graph.add_node(name));
    }

    let (mut type_params, missing_vars): (HashMap<String, usize>, HashSet<&String>) = deps.iter()
        // merge all dependencies
        .fold(HashSet::<&String>::new(), |mut a, (_, b)| {a.extend(b); return a})
        .into_iter()
        // only get those that are undefined
        .filter(|s| !deps.contains_key(*s))
        // split into keyword types (getting param count) and those that are not
        .partition_map(|s| {
            match get_kword_type_params(s) {
                Some(n) => Left((s.clone(), n)),
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
        .map_err(|e| TypeError::RecurDef(dep_graph[e.node_id()].clone()))?;

    // get number of type parameters for each type
    for name in nodes.into_iter().map(|x| dep_graph[x]) {
        if let Some(type_expr) = types.get(name) {
            type_params.insert(name.to_string(), get_type_params(
                &type_expr,
                &type_params,
                HashSet::new(),
            )?);
        }
    }
    return Ok(());
}

// get type params for keyword types that are numeric or otherwise special
fn get_kword_type_params(type_name: &String) -> Option<usize> {
    use lazy_static::lazy_static;
    use regex::{Regex, Captures};
    match type_name.as_str() {
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
fn get_type_params(
    // target type expression
    type_expr:  &TypeExpr,
    // number of params for known types
    num_params: &HashMap<String, usize>,
    // known local type variables
    mut vars:   HashSet<String>,
) -> Result<usize, TypeError> {
    match type_expr {
        TypeExpr::Variable(s) => {
            // is a locally-defined type var, 0 params
            if vars.contains(s) {
                return Ok(0);
            }
            // not a locally-defined type var, look up params
            return Ok(*num_params.get(s).ok_or(TypeError::DefaultErr)?);
        },

        // check all parameters exactly
        TypeExpr::TypeParams(t, l) => {
            let mut acc = get_type_params(t, num_params, vars.clone())?;
            for t in l.into_iter() {
                // has no more slots for params
                if acc == 0 {
                    return Err(TypeError::TooManyParams);
                }
                // loses 1 slot per param, but might be a param that is itself generic
                acc = acc - 1 + get_type_params(t, num_params, vars.clone())?;
            }
            return Ok(acc);
        },

        // adds locally-defined type var from parameter
        TypeExpr::Universal(s, t) => {
            vars.insert(s.to_string());
            return Ok(get_type_params(t, num_params, vars)? + 1);
        },

        // adds locally-defined type var
        // TODO existential may have arbitrary number of type variables (for now assume not)
        // fix by allow option?
        TypeExpr::Existential(s, t) => {
            vars.insert(s.to_string());
            return get_type_params(t, num_params, vars);
        },

        // must check all subtrees, number of parameters to each must be the same
        TypeExpr::Prod(l) | TypeExpr::Sum(l) => {
            if l.len() == 0 {
                return Ok(0);
            }
            let mut one = HashSet::new();
            for (_, t) in l.into_iter() {
                one.insert(get_type_params(t, num_params, vars.clone())?);
            }
            // param count must be same across branches
            return one
                .into_iter()
                .exactly_one()
                .map_err(|_| TypeError::InconsistParams {name: "Bob".to_string()}); // TODO fix
        },

        // must check all subtrees, number of parameters to each must be the same
        TypeExpr::Function(t1, t2) => {
            let n = get_type_params(t1, num_params, vars.clone())?;
            let m = get_type_params(t2, num_params, vars)?;
            // number of used params must be same across branches
            if n != m {
                return Err(TypeError::InconsistParams {name: "Bob".to_string()}); // TODO fix
            }
            return Ok(n);
        },
    }
}

// gets the type variable dependencies
fn get_type_deps(
    // target type expression
    type_expr: &TypeExpr,
    // known local type variables
    mut vars:  HashSet<String>,
) -> HashSet<String> {
    match type_expr {
        TypeExpr::Variable(s) => {
            // is a locally-defined type var, not a dependency
            return if vars.contains(s) {
                HashSet::new()
            } else {
            // not a locally-defined type var, is a dependency
                HashSet::from([s.to_string()])
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
            vars.insert(s.to_string());
            return get_type_deps(t, vars);
        },

        // adds locally-defined type var
        TypeExpr::Existential(s, t) => {
            vars.insert(s.to_string());
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

   /* #[test]
    fn basic_type_1() {
        let deps = get_type_deps(&parser::type_expr(
            "([A, B], C -> C)"
        ).unwrap(), HashSet::new()).unwrap();
        assert!(deps.req_params == 0);
        for s in ["A", "B", "C"].into_iter() {
            let val = deps.relat.get(s);
            assert!(val.is_some());
            assert!(*val.unwrap() == 0);
        }
    }

    #[test]
    fn medium_type_1() {
        let deps = get_type_deps(&parser::type_expr(
            "A! B? ([A, B], C{A} -> C{B})"
        ).unwrap(), HashSet::new()).unwrap();
        assert!(deps.req_params == 1);
        assert!(deps.relat.len() == 1);
        for s in ["A", "B"].into_iter() {
            let val = deps.relat.get(s);
            assert!(val.is_none());
        }
        let val = deps.relat.get("C");
        assert!(*val.unwrap() == 0);
    }

    #[test]
    fn medium_type_2() {
        let deps = get_type_deps(&parser::type_expr(
            "(A! A, (B! C! [B, C]){D})"
        ).unwrap(), HashSet::new()).unwrap();
        assert!(deps.req_params == 1);
        assert!(deps.relat.len() == 0);
        assert!(deps.exact.len() == 1);
        for s in ["A", "B", "C"].into_iter() {
            let val = deps.relat.get(s);
            assert!(val.is_none());
            let val = deps.exact.get(s);
            assert!(val.is_none());
        }
        let val = deps.exact.get("D");
        assert!(*val.unwrap() == 0);
    }

    #[test]
    fn basic_type_fail_1() {
        assert!(get_type_deps(&parser::type_expr(
            "(A! B, C)"
        ).unwrap(), HashSet::new()).is_err());
    }

    #[test]
    fn basic_type_fail_2() {
        assert!(get_type_deps(&parser::type_expr(
            "(A, B{C! D})"
        ).unwrap(), HashSet::new()).is_err());
    }

    #[test]
    fn basic_type_fail_3() {
        assert!(get_type_deps(&parser::type_expr(
            "(){A}"
        ).unwrap(), HashSet::new()).is_err());
    }

    #[test]
    fn basic_type_fail_4() {
        assert!(get_type_deps(&parser::type_expr(
            "(Foo, ()){A}"
        ).unwrap(), HashSet::new()).is_err());
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


