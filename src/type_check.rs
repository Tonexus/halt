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
    depends:     HashMap<String, i32>,
    // dependencies for which the number of type parameters is exactly known
    //strict_deps: HashMap<String, i32>,
    // total allowed parameters for this type
    used_params: i32,
}

// gets the type variable dependencies and type parameter information
fn get_type_deps(
    expr:       TypeExpr,
    net_params: i32,
    mut ctxt:   HashSet<String>,
) -> Result<TypeDepParams, &'static str> {
    match expr {
        TypeExpr::Variable(s) => {
            // is a locally-defined type var, not a dependency
            if ctxt.contains(&s) {
                return Ok(TypeDepParams {
                    depends:     HashMap::new(),
                    used_params: net_params,
                });
            }
            // not a locally-defined type var, is a dependency
            return Ok(TypeDepParams {
                depends:     HashMap::from([(s, net_params)]),
                used_params: net_params,
            });
        },
        TypeExpr::TypeParams(t, l) => {
            let mut out = get_type_deps(*t, net_params - l.len() as i32, ctxt.clone())?;
            out.used_params = i32::max(out.used_params, net_params - l.len() as i32);
            for type_expr in l.into_iter() {
                let deps = get_type_deps(type_expr, 0, ctxt.clone())?;
                if deps.used_params > 0 {
                    return Err("Cannot supply type parameters to type parameter");
                }
                /*for (dep_name, params) in deps.strict_deps.into_iter() {
                    if let Ok(params2) = out.strict_deps.get(&dep_name) {
                        if params != params2 {
                            return Err("Inconsistent number of type parameters");
                        }
                    } else {
                        out.strict_deps.insert(dep_name, params);
                    }
                }
                for (dep_name, params) in deps.weak_deps.into_iter() {
                    if let Ok(params2) = out.strict_deps.get(&dep_name) {
                        if params != params2 {
                            return Err("Inconsistent number of type parameters");
                        }
                    } else {
                        out.strict_deps.insert(dep_name, params);
                    }
                }*/
            }
            return Ok(out);
        },
        // adds locally-defined type var from parameter
        TypeExpr::Universal(s, t) => {
            ctxt.insert(s);
            let mut out = get_type_deps(*t, net_params + 1, ctxt)?;
            out.used_params = i32::max(out.used_params, net_params + 1);
            return Ok(out);
        },
        // adds locally-defined type var
        TypeExpr::Existential(s, t) => {
            ctxt.insert(s);
            let mut out = get_type_deps(*t, net_params, ctxt)?;
            out.used_params = i32::max(out.used_params, net_params);
            return Ok(out);
        },
        TypeExpr::Prod(l) | TypeExpr::Sum(l) => {
            // is terminal type, so cannot be given any parameters
            if l.len() == 0 {
                if net_params < 0 {
                    return Err("Too many supplied type parameters");
                }
                return Ok(TypeDepParams {
                    depends:     HashMap::from([("$Terminal".to_string(), net_params)]),
                    used_params: net_params,
                })
            }
            let mut out = TypeDepParams {
                depends:     HashMap::new(),
                used_params: net_params,
            };
            let mut used_params_set = HashSet::new();
            for (_, t) in l.into_iter() {
                let deps = get_type_deps(t, net_params, ctxt.clone())?;
                used_params_set.insert(deps.used_params);
                // combine dependency sets
                for (name, m) in deps.depends.into_iter() {
                    // make sure params count for same type variable is same
                    match out.depends.insert(name, m) {
                        Some(n) if n != m => {
                            return Err("Inconsistent number of type parameters");
                        },
                        _ => {},
                    }
                }
            }
            // number of used params must be same across branches
            match used_params_set.into_iter().exactly_one() {
                Ok(n) => {
                    // set used params to max of net params
                    out.used_params = i32::max(out.used_params, n);
                    return Ok(out);
                },
                _ => {
                    return Err("Inconsistent number of type parameters");
                }
            }
        },
        TypeExpr::Function(t1, t2) => {
            let mut out = get_type_deps(*t1, net_params, ctxt.clone())?;
            let deps = get_type_deps(*t2, net_params, ctxt)?;
            // number of used params must be same across branches
            if out.used_params != deps.used_params {
                return Err("Inconsistent number of type parameters");
            }
            // set used params to max of net params
            out.used_params = i32::max(out.used_params, net_params);
            // combine dependency sets
            for (name, m) in deps.depends.into_iter() {
                // make sure params count for same type variable is same
                match out.depends.insert(name, m) {
                    Some(n) if n != m => {
                        return Err("Inconsistent number of type parameters");
                    },
                    _ => {},
                }
            }
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
                for (dep_name, count) in deps.depends.into_iter() {
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
        assert!(deps.used_params == 0);
        for s in ["A", "B", "C"].into_iter() {
            let val = deps.depends.get(s);
            assert!(val.is_some());
            assert!(*val.unwrap() == 0);
        }
    }

    #[test]
    fn medium_type() {
        let deps = get_type_deps(type_expr(
            "A! B? ([A, B], C{A} -> C{B})"
        ).unwrap(), 0, HashSet::new()).unwrap();
        assert!(deps.used_params == 1);
        assert!(deps.depends.len() == 1);
        for s in ["A", "B"].into_iter() {
            let val = deps.depends.get(s);
            assert!(val.is_none());
        }
        let val = deps.depends.get("C");
        assert!(*val.unwrap() == 0);

        let deps = get_type_deps(type_expr(
            "(A! A, (B! C! [B, C]){D})"
        ).unwrap(), 0, HashSet::new()).unwrap();
        assert!(deps.used_params == 1);
        //assert!(deps.depends.length() == 1);
        for s in ["A", "B", "C"].into_iter() {
            let val = deps.depends.get(s);
            assert!(val.is_none());
        }
        //let val = deps.depends.get("D");
        //assert!(*val.unwrap() == 0);
    }

    #[test]
    fn basic_type_fail() {
        assert!(get_type_deps(type_expr(
            "(A! B, C)"
        ).unwrap(), 0, HashSet::new()).is_err())
    }
}


