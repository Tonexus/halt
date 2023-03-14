// tools for checking types

use std::{iter, collections::{HashSet, HashMap}};

use itertools::{Itertools, Either::{Left, Right}};
use petgraph::{Graph, algo::toposort};

use super::ast::*;
use super::error::{CompileError, TypeError, ValueError};

/*#[derive(Debug, PartialEq)]
struct Context {
    types:  HashMap<String, Vec<TypeExpr>>,
    values: HashMap<String, Vec<ValueExpr>>,
}*/

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
                if types.contains_key(t.name) {
                    return Err(TypeError::MultiDef(t.name.to_string()).into());
                }
                types.insert(t.name, &t);
            }
            Definition::Const(c) => {
                if consts.contains_key(c.name) {
                    return Err(ValueError::MultiDef(c.name.to_string()).into());
                }
                consts.insert(c.name, &c);
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
                    return Err(TypeError::InvalidKind(format!("{}", k_annot)));
                }
                if k_annot != &k_inf {
                    return Err(TypeError::KindMismatch(
                        format!("{}", &type_def.texpr),
                        format!("{}", k_annot),
                    ));
                }
            }
            type_kinds.insert(name, k_inf);
        }
    }
    return Ok(());
}

// TODO allow inference of kinds of quantified variables (unify kind annotation)
// ONLY use defs in type kinds (dependencies), kind should not change based on
// who has this kind as its dependency

// gets the kind of a type
fn infer_kind<'a>(
    // target type expression
    texpr:      &'a TypeExpr,
    // kind known types
    type_kinds: &HashMap<&str, TypeExpr<'a>>,
) -> Result<TypeExpr<'a>, TypeError> {
    match texpr {
        // look up variable kind
        TypeExpr::Variable(s) => {
            return type_kinds.get(s).map(|k| k.clone()).ok_or(TypeError::DefaultErr);
        },

        // must supply params directly to higher-kinded type
        TypeExpr::TypeParams(t1, l) => {
            let mut kexpr = infer_kind(t1, &type_kinds.clone())?;
            for t2 in l.into_iter() {
                // check if has slots for params
                if let TypeExpr::Function(k1, k2) = kexpr {
                    if *k1 == infer_kind(t2, &type_kinds.clone())? {
                        kexpr = *k2;
                    } else {
                        return Err(TypeError::KindMismatch(
                            format!("{}", t2),
                            format!("{}", k1),
                        ));
                    }
                } else {
                    return Err(TypeError::TooManyParams(format!("{}", t1)));
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
                    return Err(TypeError::InvalidKind(format!("{}", k)));
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

        // if not singleton check all subtrees, kind must each be nullary
        TypeExpr::Prod(l) | TypeExpr::Sum(l) => {
            // singleton is always same kind as nested
            if let Ok((_, t)) = l.iter().exactly_one() {
                return infer_kind(t, &type_kinds.clone());
            }

            for (_, t) in l.into_iter() {
                if infer_kind(t, &type_kinds.clone())? != *KIND_0 {
                    return Err(TypeError::MustNullKind(format!("{}", t)));
                }
            }
            return Ok(KIND_0.clone());
        },

        // check both subtrees, kind must each be nullary
        TypeExpr::Function(t1, t2) => {
            if infer_kind(t1, &type_kinds.clone())? != *KIND_0 {
                return Err(TypeError::MustNullKind(format!("{}", t1)));
            }
            if infer_kind(t2, type_kinds)? != *KIND_0 {
                return Err(TypeError::MustNullKind(format!("{}", t2)));
            }
            return Ok(KIND_0.clone());
        },
    }
}

// only allow simple kinds (functions and nullary kind)
fn valid_kind(kexpr: &TypeExpr) -> bool {
    if kexpr == &*KIND_0 {
        return true;
    }
    return match kexpr {
        TypeExpr::Function(k1, k2) => valid_kind(k1) && valid_kind(k2),
        TypeExpr::Prod(l) | TypeExpr::Sum(l) => l.iter()
            .exactly_one()
            .map(|(_, k)| valid_kind(k))
            .unwrap_or(false),
        _ => false,
    };
}

// get type params for keyword types that are numeric or otherwise special
fn get_kword_type_kind(type_name: &str) -> Option<TypeExpr> {
    use regex::{Regex, Captures};
    use lazy_static::lazy_static;
    match type_name {
        // integer numerics
        "USize" => return Some(KIND_0.clone()),
        "U32"   => return Some(KIND_0.clone()),
        "U16"   => return Some(KIND_0.clone()),
        "U8"    => return Some(KIND_0.clone()),
        "SSize" => return Some(KIND_0.clone()),
        "S32"   => return Some(KIND_0.clone()),
        "S16"   => return Some(KIND_0.clone()),
        "S8"    => return Some(KIND_0.clone()),
        // floating point number
        "F32"   => return Some(KIND_0.clone()),
        // array
        "Arr"   => return Some(KIND_2.clone()),
        "Ascii" => return Some(KIND_1.clone()),
        _       => {
            let f = |c: Option<Captures>| c
                .and_then(|x| x.get(1))
                .and_then(|x| x.as_str().parse::<u32>().ok());
            lazy_static! {
                // enum for up to 2^32 choices
                static ref ENUM: Regex = Regex::new("^N(\\d+)$").unwrap();
            }
            if f(ENUM.captures(type_name)).is_some() {
                return Some(KIND_0.clone());
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
            return if vars.contains(s) {
                HashSet::new()
            } else {
            // not a locally-defined type var, is a dependency
                HashSet::from([*s])
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
            "!A . ?B . ([A, B], C{A} -> C{D})"
        ).unwrap();
        let deps = get_type_deps(&t, HashSet::new());
        for s in ["A", "B"].into_iter() {
            assert!(!deps.contains(s));
        }
        assert!(deps.contains("C"));
        assert!(deps.contains("D"));
    }

    #[test]
    fn basic_type_kind_1() {
        assert!(infer_kind(
            &parser::type_expr(
                "([A, B], C -> C)"
            ).unwrap(),
            &HashMap::from([
                ("A", KIND_0.clone()),
                ("B", KIND_0.clone()),
                ("C", KIND_0.clone()),
            ]),
        ).unwrap() == *KIND_0);
    }

    #[test]
    fn basic_type_kind_2() {
        assert!(infer_kind(
            &parser::type_expr(
                "!A . (A, B, C)"
            ).unwrap(),
            &HashMap::from([
                ("B", KIND_0.clone()),
                ("C", KIND_0.clone()),
            ]),
        ).unwrap() == *KIND_1);
    }

    #[test]
    fn basic_type_kind_3() {
        assert!(infer_kind(
            &parser::type_expr(
                "(?A: Type -> Type . A{C}, B)"
            ).unwrap(),
            &HashMap::from([
                ("B", KIND_0.clone()),
                ("C", KIND_0.clone()),
            ]),
        ).unwrap() == *KIND_0);
    }

    #[test]
    fn basic_type_kind_4() {
        assert!(infer_kind(
            &parser::type_expr(
                "A{B, C}"
            ).unwrap(),
            &HashMap::from([
                ("A", KIND_2.clone()),
                ("B", KIND_0.clone()),
                ("C", KIND_0.clone()),
            ]),
        ).unwrap() == *KIND_0);
    }

    #[test]
    fn medium_type_kind_1() {
        assert!(infer_kind(
            &parser::type_expr(
                "!A, B . [A, B]"
            ).unwrap(),
            &HashMap::new(),
        ).unwrap() == *KIND_2);
    }

    #[test]
    fn medium_type_kind_2() {
        assert!(infer_kind(
            &parser::type_expr(
                "!A: Type -> Type -> Type . A{(!B . !C . [B, C]){D, D}}"
            ).unwrap(),
            &HashMap::from([
                ("D", KIND_0.clone()),
            ]),
        ).unwrap() == TypeExpr::Function(
            Box::new(KIND_2.clone()),
            Box::new(KIND_1.clone()),
        ));
    }

    #[test]
    fn medium_type_kind_3() {
        assert!(infer_kind(
            &parser::type_expr(
                "(!A: Type -> Type -> Type . A){B, C}"
            ).unwrap(),
            &HashMap::from([
                ("B", KIND_2.clone()),
                ("C", KIND_0.clone()),
            ]),
        ).unwrap() == *KIND_1);
    }

    #[test]
    fn basic_type_kind_fail_1() {
        assert!(matches!(infer_kind(
            &parser::type_expr(
                "(!A . B, C)"
            ).unwrap(),
            &HashMap::from([
                ("B", KIND_0.clone()),
                ("C", KIND_0.clone()),
            ]),
        ), Err(TypeError::MustNullKind(_))));
    }

    #[test]
    fn basic_type_kind_fail_2() {
        assert!(matches!(infer_kind(
            &parser::type_expr(
                "(A, B{C})"
            ).unwrap(),
            &HashMap::from([
                ("A", KIND_0.clone()),
                ("B", KIND_0.clone()),
                ("C", KIND_0.clone()),
            ]),
        ), Err(TypeError::TooManyParams(_))));
    }

    #[test]
    fn basic_type_kind_fail_3() {
        assert!(matches!(infer_kind(
            &parser::type_expr(
                "(){A}"
            ).unwrap(),
            &HashMap::from([
                ("A", KIND_0.clone()),
            ]),
        ), Err(TypeError::TooManyParams(_))));
    }

    #[test]
    fn basic_type_kind_fail_4() {
        assert!(matches!(infer_kind(
            &parser::type_expr(
                "[A, ()]"
            ).unwrap(),
            &HashMap::from([
                ("A", KIND_1.clone()),
            ]),
        ), Err(TypeError::MustNullKind(_))));
    }

    #[test]
    fn basic_type_kind_fail_5() {
        assert!(matches!(infer_kind(
            &parser::type_expr(
                "A{B}"
            ).unwrap(),
            &HashMap::from([
                ("A", KIND_1.clone()),
                ("B", KIND_2.clone()),
            ]),
        ), Err(TypeError::KindMismatch(_, _))));
    }

    #[test]
    fn basic_type_kind_fail_6() {
        assert!(matches!(infer_kind(
            &parser::type_expr(
                "!A: InvalidKind . A"
            ).unwrap(),
            &HashMap::new(),
        ), Err(TypeError::InvalidKind(_))));
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
    fn basic_type_defs_4() {
        assert!(check_defs(parser::defs(
            "Foo: [Type] := Arr{N12, N47};"
        ).unwrap()).is_ok());
    }

    #[test]
    fn basic_type_defs_5() {
        assert!(check_defs(parser::defs(
            "Foo: (Type -> Type) -> Type := !A: Type -> Type . A{U32};"
        ).unwrap()).is_ok());
    }

    #[test]
    fn medium_type_defs_1() {
        assert!(check_defs(parser::defs(
            "Foo := !A . Bar{A}; Bar := !B, C . [B, C]; Baz := Foo{U32};"
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


