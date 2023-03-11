// tools for checking types

use std::collections::HashSet;
use std::collections::HashMap;

use itertools::Itertools;

use super::ast::*;

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

// gets the type variable dependencies and type parameter information
fn get_type_deps(
    expr:       TypeExpr,
    net_params: i32,
    mut ctxt:   HashSet<String>,
) -> Result<TypeDepParams, &'static str> {
    // helper function to merge two dependency sets
    fn merge(
        one: &mut HashMap<String, i32>,
        two: HashMap<String, i32>
    ) -> Result<(), &'static str> {
        for (name, m) in two.into_iter() {
            // make sure params count for same type variable is same
            match one.insert(name, m) {
                Some(n) if n != m => {
                    return Err("Inconsistent number of type parameters");
                },
                _ => {},
            }
        }
        return Ok(());
    }

    match expr {
        TypeExpr::Variable(s) => {
            // is a locally-defined type var, not a dependency
            if ctxt.contains(&s) {
                return Ok(TypeDepParams {
                    relat:      HashMap::new(),
                    exact:      HashMap::new(),
                    req_params: net_params,
                });
            }
            // not a locally-defined type var, is a dependency
            return Ok(TypeDepParams {
                relat:      HashMap::from([(s, net_params)]),
                exact:      HashMap::new(),
                req_params: net_params,
            });
        },

        // check all parameters exactly
        TypeExpr::TypeParams(t, l) => {
            let mut out = get_type_deps(*t, net_params - l.len() as i32, ctxt.clone())?;
            out.req_params = i32::max(out.req_params, net_params - l.len() as i32);
            for type_expr in l.into_iter() {
                let deps = get_type_deps(type_expr, 0, ctxt.clone())?;
                if deps.req_params > 0 {
                    return Err("Cannot supply type parameters to type parameter");
                }
                // combine dependency sets, BOTH into exact, since can take no params
                merge(&mut out.exact, deps.relat)?;
                merge(&mut out.exact, deps.exact)?;
            }
            return Ok(out);
        },

        // adds locally-defined type var from parameter
        TypeExpr::Universal(s, t) => {
            ctxt.insert(s);
            let mut out = get_type_deps(*t, net_params + 1, ctxt)?;
            out.req_params = i32::max(out.req_params, net_params + 1);
            return Ok(out);
        },

        // adds locally-defined type var
        // TODO existential may have arbitrary number of type variables (for now assume not)
        TypeExpr::Existential(s, t) => {
            ctxt.insert(s);
            let mut out = get_type_deps(*t, net_params, ctxt)?;
            out.req_params = i32::max(out.req_params, net_params);
            return Ok(out);
        },

        // must check all subtrees, number of parameters to each must be the same
        TypeExpr::Prod(l) | TypeExpr::Sum(l) => {
            if l.len() == 0 {
                // empty type cannot have any type parameters
                if net_params < 0 {
                    return Err("Too many supplied type parameters");
                }
                return Ok(TypeDepParams {
                    relat:       HashMap::new(),
                    exact:       HashMap::from([("$Terminal".to_string(), net_params)]),
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
                    return Err("Inconsistent number of type parameters");
                }
            }
        },

        // must check all subtrees, number of parameters to each must be the same
        TypeExpr::Function(t1, t2) => {
            let mut out = get_type_deps(*t1, net_params, ctxt.clone())?;
            let deps = get_type_deps(*t2, net_params, ctxt)?;
            // number of used params must be same across branches
            if out.req_params != deps.req_params {
                return Err("Inconsistent number of type parameters");
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

/*
fn unify(lhs: Option<TypeExpr>, rhs: Option<TypeExpr>, ctx: Context) -> Result<(), &'static str> {
    Ok(())
}
*/

pub fn type_check(defs: Vec<Definition>) -> Result<(), &'static str> {
    let mut types: HashMap<String, TypeExpr> = HashMap::new();
    let mut consts: HashMap<String, (Option<TypeExpr>, ValueExpr)> = HashMap::new();
    //let mut type_ctxt = HashMap::new();
    //let mut ctxt   = Context{types: HashMap::new(), values: HashMap::new()};
    // split types and consts, building initial context
    for def in defs.into_iter() {
        match def {
            Definition::Type(t) => {
                if types.contains_key(&t.name) {
                    return Err("Type is already defined.");
                }
                types.insert(t.name.clone(), t.expr);
                //type_ctxt.insert(t.name, TypeVarState::Global);
                //ctxt.types.insert(c.name, Vec::from([(t.expr)]));
            }
            Definition::Const(c) => {
                if consts.contains_key(&c.name) {
                    return Err("Constant is already defined.");
                }
                consts.insert(c.name, (c.type_expr, c.expr));
                //ctxt.values.insert(c.name, Vec::from([(c.type_expr, c.expr)]));
            }
        }
    }
    for (name, type_expr) in types.into_iter() {
        println!("parsed type {}!", name);
        match get_type_deps(type_expr, 0, HashSet::new()) {
            Ok(deps) => {
                for (dep_name, count) in deps.relat.into_iter() {
                    println!("{} has {} parameters!", dep_name, count);
                }
            }
            Err(out) => {
                println!("check failed with error \"{}\"", out)
            }
        }
    }
    // build initial context
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::parser::program_parser::*;

    #[test]
    fn basic_type() {
        let deps = get_type_deps(type_expr(
            "([A, B], C -> C)"
        ).unwrap(), 0, HashSet::new()).unwrap();
        assert!(deps.req_params == 0);
        for s in ["A", "B", "C"].into_iter() {
            let val = deps.relat.get(s);
            assert!(val.is_some());
            assert!(*val.unwrap() == 0);
        }
    }

    #[test]
    fn medium_type() {
        let deps = get_type_deps(type_expr(
            "A! B? ([A, B], C{A} -> C{B})"
        ).unwrap(), 0, HashSet::new()).unwrap();
        assert!(deps.req_params == 1);
        assert!(deps.relat.len() == 1);
        for s in ["A", "B"].into_iter() {
            let val = deps.relat.get(s);
            assert!(val.is_none());
        }
        let val = deps.relat.get("C");
        assert!(*val.unwrap() == 0);

        let deps = get_type_deps(type_expr(
            "(A! A, (B! C! [B, C]){D})"
        ).unwrap(), 0, HashSet::new()).unwrap();
        assert!(deps.req_params == 1);
        //assert!(deps.relat.length() == 1);
        for s in ["A", "B", "C"].into_iter() {
            let val = deps.relat.get(s);
            assert!(val.is_none());
        }
        //let val = deps.relat.get("D");
        //assert!(*val.unwrap() == 0);
    }

    #[test]
    fn basic_type_fail() {
        assert!(get_type_deps(type_expr(
            "(A! B, C)"
        ).unwrap(), 0, HashSet::new()).is_err());
        assert!(get_type_deps(type_expr(
            "(A, B{C! D})"
        ).unwrap(), 0, HashSet::new()).is_err());
    }
}


