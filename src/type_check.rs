// tools for checking types
use std::collections::HashSet;
use std::collections::HashMap;

use super::ast::*;

#[derive(Debug, PartialEq)]
struct Context {
    types:  HashMap<String, Vec<TypeExpr>>,
    values: HashMap<String, Vec<ValueExpr>>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum TypeVarState {
    Global,  // variable is defined (external to the expression)
    Local,   // variable is defined (internal to the expression)
}

//fn uniquify_names(def: &mut Vec<Definition>) {
//    let mut seen_names = HashMap::new();
//    for def in defs.iter() {
//        match
//    }
//}

// validates a type expression and gets its dependencies
fn get_type_deps(
    expr:       TypeExpr,
    num_params: usize,
    mut ctxt:   HashMap<String, TypeVarState>,
) -> Result<HashSet<String>, &'static str> {
    match (expr, num_params) {
        (TypeExpr::Variable(s), 0) => {
            return match ctxt.get(&s) {
                Some(TypeVarState::Global) => Ok(HashSet::from([s])),
                Some(TypeVarState::Local)  => Ok(HashSet::new()),
                None                       => Err("Type not defined"),
            };
        },
        (TypeExpr::TypeParams(t, l), mut n) => {
            let mut out: HashSet<String> = HashSet::new();
            n = n + l.len();
            for t2 in l.into_iter() {
                out.extend(get_type_deps(t2, 0, ctxt.clone())?);
            }
            out.extend(get_type_deps(*t, n, ctxt)?);
            return Ok(out);
        },
        (TypeExpr::Universal(s, t), n) => {
            if n == 0 {
                return Err("Too few type parameters");
            }
            ctxt.insert(s, TypeVarState::Local);
            return get_type_deps(*t, n - 1, ctxt);
        },
        _ => {
            return Err("Too many type parameters");
        },
    }
}

fn unify(lhs: Option<TypeExpr>, rhs: Option<TypeExpr>, ctx: Context) -> Result<(), &'static str> {
    Ok(())
}

pub fn type_check(defs: Vec<Definition>) -> Result<(), &'static str> {
    let mut types: HashMap<String, TypeExpr> = HashMap::new();
    let mut consts: HashMap<String, (Option<TypeExpr>, ValueExpr)> = HashMap::new();
    let mut type_ctxt = HashMap::new();
    //let mut ctxt   = Context{types: HashMap::new(), values: HashMap::new()};
    // split types and consts, building initial context
    for def in defs.into_iter() {
        match def {
            Definition::Type(t) => {
                if types.contains_key(&t.name) {
                    return Err("Type is already defined.");
                }
                types.insert(t.name.clone(), t.expr);
                type_ctxt.insert(t.name, TypeVarState::Global);
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
    for (_, t) in types.into_iter() {
        dbg!(get_type_deps(t, 0, type_ctxt));
        break;
    }
    // build initial context
    Ok(())
}
