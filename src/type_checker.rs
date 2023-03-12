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
    let mut dep_graph: Graph<String, ()> = Graph::new();
    let mut node_idx = HashMap::new();

    // for each type var get dependencies
    for (name, type_expr) in types.iter() {
        deps.insert(name.to_string(), get_type_deps(&type_expr, HashSet::new()));
        // also insert nodes into dependency graph
        node_idx.insert(name.to_string(), dep_graph.add_node(name.to_string()));
    }

    let (type_params, missing_vars): (HashMap<String, usize>, HashSet<&String>) = deps.iter()
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
        match (node_idx.get(s1), node_idx.get(s2)) {
            (Some(j), Some(k)) => {dep_graph.add_edge(*j, *k, ());},
            _ => {},
        }
    }

    // get topological order, error if any recursive definitions
    let nodes = toposort(&dep_graph, None)
        .map_err(|e| TypeError::RecurDef(dep_graph[e.node_id()].clone()))?;

    // get number of type parameters for each type
    /*for name in nodes.into_iter().map(|x| dep_graph[x]) {
        types.get(name).map(get_num_params(name, ))
    }*/
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

/*
// gets the type variable dependencies and type parameter information
fn get_type_deps(
    // type expression to parse
    expr:       &TypeExpr,
    // the net number of parameters required at this depth
    net_params: i32,
    // context of defined type variables
    mut ctxt:   HashSet<String>,
) -> Result<TypeDepParams, TypeError> {
    // helper function to merge two dependency sets
    fn merge(
        one: &mut HashMap<String, i32>,
        two: HashMap<String, i32>
    ) -> Result<(), TypeError> {
        for (name, m) in two.into_iter() {
            // make sure params count for same type variable is same
            match one.insert(name.clone(), m) {
                Some(n) if n != m => {
                    return Err(TypeError::InconsistParams {name: name});
                },
                _ => {},
            }
        }
        return Ok(());
    }

    match expr {
        TypeExpr::Variable(s) => {
            // is a locally-defined type var, not a dependency
            if ctxt.contains(s) {
                return Ok(TypeDepParams {
                    relat:      HashMap::new(),
                    exact:      HashMap::new(),
                    req_params: net_params,
                });
            }
            // not a locally-defined type var, is a dependency
            return Ok(TypeDepParams {
                relat:      HashMap::from([(s.to_string(), net_params)]),
                exact:      HashMap::new(),
                req_params: net_params,
            });
        },

        // check all parameters exactly
        TypeExpr::TypeParams(t, l) => {
            let mut out = get_type_deps(t, net_params - l.len() as i32, ctxt.clone())?;
            out.req_params = i32::max(out.req_params, net_params - l.len() as i32);
            for type_expr in l.into_iter() {
                let deps = get_type_deps(type_expr, 0, ctxt.clone())?;
                // TODO allowed?
                /*if deps.req_params > 0 {
                    return Err("Cannot supply type parameters to type parameter");
                }*/
                // combine dependency sets, BOTH into exact, since can take no params
                merge(&mut out.exact, deps.relat)?;
                merge(&mut out.exact, deps.exact)?;
            }
            return Ok(out);
        },

        // adds locally-defined type var from parameter
        TypeExpr::Universal(s, t) => {
            ctxt.insert(s.to_string());
            let mut out = get_type_deps(t, net_params + 1, ctxt)?;
            out.req_params = i32::max(out.req_params, net_params + 1);
            return Ok(out);
        },

        // adds locally-defined type var
        // TODO existential may have arbitrary number of type variables (for now assume not)
        TypeExpr::Existential(s, t) => {
            ctxt.insert(s.to_string());
            let mut out = get_type_deps(t, net_params, ctxt)?;
            out.req_params = i32::max(out.req_params, net_params);
            return Ok(out);
        },

        // must check all subtrees, number of parameters to each must be the same
        TypeExpr::Prod(l) | TypeExpr::Sum(l) => {
            if l.len() == 0 {
                // empty type cannot have any type parameters
                if net_params < 0 {
                    return Err(TypeError::CannotHaveParams);
                }
                return Ok(TypeDepParams {
                    relat:       HashMap::new(),
                    exact:       HashMap::new(),
                    req_params: net_params,
                })
            }
            let mut out = TypeDepParams {
                relat:       HashMap::new(),
                exact:       HashMap::new(),
                req_params: net_params,
            };
            let mut req_params_set = HashSet::new();
            for (_, t) in l.into_iter() {
                let deps = get_type_deps(t, net_params, ctxt.clone())?;
                req_params_set.insert(deps.req_params);
                // combine dependency sets
                merge(&mut out.relat, deps.relat)?;
                merge(&mut out.exact, deps.exact)?;
            }
            // number of used params must be same across branches
            match req_params_set.into_iter().exactly_one() {
                Ok(n) => {
                    // set used params to max of net params
                    out.req_params = i32::max(out.req_params, n);
                    return Ok(out);
                },
                _ => {
                    return Err(TypeError::InconsistParams {name: "Bob".to_string()}); // TODO fix
                }
            }
        },

        // must check all subtrees, number of parameters to each must be the same
        TypeExpr::Function(t1, t2) => {
            let mut out = get_type_deps(t1, net_params, ctxt.clone())?;
            let deps = get_type_deps(t2, net_params, ctxt)?;
            // number of used params must be same across branches
            if out.req_params != deps.req_params {
                return Err(TypeError::InconsistParams {name: "Bob".to_string()}); // TODO fix
            }
            // set used params to max of net params
            out.req_params = i32::max(out.req_params, net_params);
            // combine dependency sets
            merge(&mut out.relat, deps.relat)?;
            merge(&mut out.exact, deps.exact)?;
            return Ok(out);
        },
    }
}
*/

// gets the type variable dependencies
fn get_type_deps(
    // type expression to parse
    expr:       &TypeExpr,
    // context of defined type variables
    mut ctxt:   HashSet<String>,
) -> HashSet<String> {
    match expr {
        TypeExpr::Variable(s) => {
            // is a locally-defined type var, not a dependency
            return if ctxt.contains(s) {
                HashSet::new()
            } else {
            // not a locally-defined type var, is a dependency
                HashSet::from([s.to_string()])
            };
        },

        // check all parameters exactly
        TypeExpr::TypeParams(t, l) => {
            let mut out = get_type_deps(t, ctxt.clone());
            for type_expr in l.into_iter() {
                out.extend(get_type_deps(type_expr, ctxt.clone()));
            }
            return out;
        },

        // adds locally-defined type var from parameter
        TypeExpr::Universal(s, t) => {
            ctxt.insert(s.to_string());
            return get_type_deps(t, ctxt);
        },

        // adds locally-defined type var
        TypeExpr::Existential(s, t) => {
            ctxt.insert(s.to_string());
            return get_type_deps(t, ctxt);
        },

        // must check all subtrees
        TypeExpr::Prod(l) | TypeExpr::Sum(l) => {
            if l.len() == 0 {
                return HashSet::new();
            }
            let mut out = HashSet::new();
            for (_, t) in l.into_iter() {
                out.extend(get_type_deps(t, ctxt.clone()));
            }
            return out;
        },

        // must check all subtrees
        TypeExpr::Function(t1, t2) => {
            let mut out = get_type_deps(t1, ctxt.clone());
            out.extend(get_type_deps(t2, ctxt));
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


