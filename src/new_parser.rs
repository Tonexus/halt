// parser for halt. uses peg

use std::collections::HashMap;

use super::new_ast::*;
use super::misc::*;

pub use program_parser::*;

fn make_vexpr_var<'a>(s: &'a str) -> Expr<'a> {
    return Expr {
        tier:  Some(0),
        texpr: None,
        var:   ExprVar::Var(s),
    };
}

fn make_vexpr_lit_bool(b: bool) -> Expr<'static> {
    return Expr {
        tier:  Some(0),
        texpr: None, // TODO
        var:   ExprVar::LVal(LitVar::Bool(b))
    }
}

fn make_vexpr_lit_int(i: i32) -> Expr<'static> {
    return Expr {
        tier:  Some(0),
        texpr: None, // TODO
        var:   ExprVar::LVal(LitVar::Int(i))
    }
}

fn make_vexpr_lit_float(f: f32) -> Expr<'static> {
    return Expr {
        tier:  Some(0),
        texpr: None, // TODO
        var:   ExprVar::LVal(LitVar::Float(f))
    }
}

fn make_vexpr_lit_ascii(a: Vec<u8>) -> Expr<'static> {
    return Expr {
        tier:  Some(0),
        texpr: None, // TODO
        var:   ExprVar::LVal(LitVar::Ascii(a))
    }
}

fn make_vexpr_lit_u8char(c: u8) -> Expr<'static> {
    return Expr {
        tier:  Some(0),
        texpr: None, // TODO
        var:   ExprVar::LVal(LitVar::U8Char(c))
    }
}

fn make_vexpr_unop<'a>(e: Expr<'a>, s: &'a str) -> Expr<'a> {
    return Expr {
        tier:  Some(0),
        texpr: None,
        var:   ExprVar::LApp {
            fun:   Box::new(make_vexpr_var(s)),
            param: Box::new(e)
        }
    };
}

fn make_vexpr_binop<'a>(e1: Expr<'a>, e2: Expr<'a>, s: &'a str) -> Expr<'a> {
    return Expr {
        tier:  Some(0),
        texpr: None,
        var:   ExprVar::LApp {
            fun:   Box::new(make_vexpr_var(s)),
            param: Box::new(Expr {
                tier:  Some(0),
                texpr: None,
                var:   ExprVar::LPro(Vec::from([
                    ("0", e1),
                    ("1", e2),
                ]))
            })
        }
    };
}

peg::parser!{
    grammar program_parser() for str {
        // **************
        // CHARACTER SETS
        // **************

        rule num()        = ['0'..='9']
        rule upper()      = ['A'..='Z']
        rule lower()      = ['a'..='z']
        rule alpha()      = ['A'..='Z' | 'a'..='z']
        rule alphanum()   = ['A'..='Z' | 'a'..='z' | '0'..='9']
        // valid characters in value variable names and labels
        rule value_char() = ['a'..='z' | '0'..='9' | '_']
        // valid characters in type variable names
        rule type_char()  = ['A'..='Z' | 'a'..='z' | '0'..='9']
        // valid characters in all variable names
        rule name_char()  = ['A'..='Z' | 'a'..='z' | '0'..='9' | '_']

        // ***************
        // GENERAL PARSING
        // ***************

        // any white space or comment
        rule _            = quiet!{([' ' | '\t' | '\r' | '\n'] / ("//" [^ '\n']*))*}

        // ********
        // KEYWORDS
        // ********

        // TODO have true/false/i/inline/yield/enter just be unoverwritable vars?

        // statements
        rule kw_break()    = "break" !name_char()
        rule kw_continue() = "continue" !name_char()
        rule kw_do()       = "do" !name_char()
        rule kw_else()     = "else" !name_char()
        rule kw_from()     = "from" !name_char()
        rule kw_if()       = "if" !name_char()
        rule kw_let()      = "let" !name_char()
        rule kw_loop()     = "loop" !name_char()
        rule kw_match()    = "match" !name_char()
        rule kw_return()   = "return" !name_char()
        rule kw_to()       = "to" !name_char()

        // **************
        // VALUE LITERALS
        // **************

        // booleans
        rule literal_boolean() -> bool = n: value_name() {?
            match n {
                KW_TRUE  => Ok(true),
                KW_FALSE => Ok(false),
                _        => Err("boolean literal"),
            }
        }
        // signed integer
        rule literal_integer() -> i32 =
            s: $("-"? num()+) !(name_char() / ".") {?
                s.parse::<i32>().map_err(|_| "integer literal")
            }
        // floating point number
        rule literal_float() -> f32 =
            s: $("-"? num()+ "." num()+) !(name_char() / ".") {?
                s.parse::<f32>().map_err(|_| "floating point literal")
            }
        // string TODO fix escape sequences
        rule literal_string() -> &'input str =
            "\"" s: $(([^'\"'] / "\\\"")*) "\"" { s }

        // *************
        // MISCELLANEOUS
        // *************

        /*
        // rule for type annotation
        rule type_annot() -> Expr<'input> =
            _ ":" _ t: type_expr() {t}
        // labeled type
        rule labeled_type() -> (&'input str, Expr<'input>) =
            n: label_name() t: type_annot() {(n, t)}
        // labeled value
        rule labeled_value() -> (&'input str, Expr<'input>) =
            n: label_name() _ "=" _ v: value_expr() {(n, v)}
        // optionally kinded type name
        rule opt_kinded_type_name() -> (&'input str, Option<Expr<'input>>) =
            n: type_name() o: type_annot()? {(n, o)}
        // optionally typed value name
        rule opt_typed_value_name() -> (&'input str, Option<Expr<'input>>) =
            n: value_name() o: type_annot()? {(n, o)}
        // type list
        rule type_list() -> Vec<Expr<'input>> =
            l: (type_expr() ++ (_ "," _)) (_ ",")? {l}
        // labeled type list
        rule labeled_type_list() -> Vec<(&'input str, Expr<'input>)> =
            l: (labeled_type() ++ (_ "," _)) (_ ",")? {l}
        // type list with implicit labels
        rule type_list_labeled() -> Vec<(&'input str, Expr<'input>)> =
            l: type_list() {?
                if l.len() > 10 {
                    return Err("Too many labels");
                }
                return Ok(l.into_iter().enumerate().map(
                    |(n, t)| (LABELS[n], t)
                ).collect());
            }
        // no types in type list
        rule empty_type() -> Vec<(&'input str, Expr<'input>)> =
            "" {Vec::new()}
        // value list
        rule value_list() -> Vec<Expr<'input>> =
            l: (value_expr() ++ (_ "," _)) (_ ",")? {l}
        // labeled value list
        rule labeled_value_list() -> Vec<(&'input str, Expr<'input>)> =
            l: (labeled_value() ++ (_ "," _)) (_ ",")? {l}
        // value list with implicit labels
        rule value_list_labeled() -> Vec<(&'input str, Expr<'input>)> =
            l: value_list() {?
                if l.len() > 10 {
                    return Err("Too many labels");
                }
                return Ok(l.into_iter().enumerate().map(
                    |(n, v)| (LABELS[n], v)
                ).collect());
            }
        // no values in value list
        rule empty_value() -> Vec<(&'input str, Expr<'input>)> =
            "" {Vec::new()}
        */

        // ************************
        // PROGRAMMER-DEFINED NAMES
        // ************************

        // user defined variable names
        rule value_name() -> &'input str = // TODO later check for leading _ in var name
            quiet!{
                n: $(lower() value_char()*) !value_char() {
                    n
                }
            } / expected!("value variable name")
        // user defined label names for product fields or sum tags
        rule label_name() -> &'input str =
            quiet!{
                n: $(value_char()+) !value_char() {
                    n
                }
            } / expected!("label name")
        // user defined type names
        rule type_name() -> &'input str =
            quiet!{
                n: $(upper() alphanum()*) !type_char() {
                    n
                }
            } / expected!("type variable name")

        // *********************
        // TOP-LEVEL DEFINITIONS
        // *********************

        /*
        // collect all top-level definitions
        pub rule defs() -> Vec<Definition<'input>> =
            _ d: (def() **  _) _ {d}
        rule def() -> Definition<'input> = type_def() / const_def()
        // definition of a type
        rule type_def() -> Definition<'input> =
            n: opt_kinded_type_name() _ ":=" _ t: type_expr() _ ";" {
                Definition::Type(TypeDef {
                    name:  n.0,
                    kexpr: n.1,
                    texpr: t,
                })
            }
        // definition of a constant variable
        rule const_def() -> Definition<'input> =
            n: opt_typed_value_name() _ ":=" _ v: value_expr() _ ";" {
                Definition::Const(ConstDef{
                    name:  n.0,
                    texpr: n.1,
                    vexpr: v,
                })
            }
          */

        // ****************
        // TYPE EXPRESSIONS
        // ****************

        /*
        // type expressions // TODO add plus and mul for combining sums and products?
        pub rule type_expr() -> TypeExpr<'input> = precedence!{
            // function type is only binary op
            t1: @ _ "->" _ t2: (@) {
                TypeExpr::Func(Box::new(t1), Box::new(t2))
            }
            --
            // declare new universal or existential type variable
            "!" _ l: (opt_kinded_type_name() ++ (_ "," _) ) _ "." _ t: @ {
                TypeExpr::Univ(
                    l.into_iter()
                        .map(|(n, o)| (n, o.unwrap_or(KIND_0.clone())))
                        .collect(),
                    Box::new(t),
                )
            }
            // declare new universal or existential type variable
            "?" _ l: (opt_kinded_type_name() ++ (_ "," _) ) _ "." _ t: @ {
                TypeExpr::Exis(
                    l.into_iter()
                        .map(|(n, o)| (n, o.unwrap_or(KIND_0.clone())))
                        .collect(),
                    Box::new(t),
                )
            }
            --
            // inserting universal type parameters
            t: @ _ "{" _ l: type_list() _ "}" {
                TypeExpr::TypeParams(Box::new(t), l)
            }
            --
            // atoms
            t: variable_type() {t}
            t: prod_type() {t}
            t: sum_type() {t}
        }
        // type variable
        rule variable_type() -> TypeExpr<'input> =
            n: type_name() {TypeExpr::Variable(n)}
        // product type, implicit fields, explicit fields, or empty
        rule prod_type() -> TypeExpr<'input> =
            "(" _ l: (type_list_labeled() / labeled_type_list() / empty_type()) _ ")" {
                TypeExpr::Prod(l.into_iter().collect())
            }
        // sum type, implicit fields, explicit fields, or empty
        rule sum_type() -> TypeExpr<'input> =
            "[" _ l: (type_list_labeled() / labeled_type_list() / empty_type()) _ "]" {
                TypeExpr::Sum(l.into_iter().collect())
            }
        */

        // *****************
        // VALUE EXPRESSIONS
        // *****************

        pub rule vexpr() -> Expr<'input> = precedence!{
            // equality
            e1: (@) _ "==" _ e2: @ {
                make_vexpr_binop(e1, e2, "_eq")
            }
            e1: (@) _ "!=" _ e2: @ {
                make_vexpr_binop(e1, e2, "_neq")
            }
            --
            // comparison / shift
            e1: (@) _ ">" _ e2: @ {
                make_vexpr_binop(e1, e2, "_gt")
            }
            e1: (@) _ "<" _ e2: @ {
                make_vexpr_binop(e1, e2, "_lt")
            }
            e1: (@) _ ">=" _ e2: @ {
                make_vexpr_binop(e1, e2, "_gte")
            }
            e1: (@) _ "<=" _ e2: @ {
                make_vexpr_binop(e1, e2, "_lte")
            }
            --
            // or
            e1: (@) _ "\\/" _ e2: @ {
                make_vexpr_binop(e1, e2, "_or")
            }
            --
            // and
            e1: (@) _ "/\\" _ e2: @ {
                make_vexpr_binop(e1, e2, "_and")
            }
            --
            // addition and subtraction
            e1: (@) _ "+" _ e2: @ {
                make_vexpr_binop(e1, e2, "_add")
            }
            e1: (@) _ "-" _ e2: @ {
                make_vexpr_binop(e1, e2, "_sub")
            }
            --
            // multiplication, division, and modulo
            e1: (@) _ "*" _ e2: @ {
                make_vexpr_binop(e1, e2, "_mul")
            }
            e1: (@) _ "/" _ e2: @ {
                make_vexpr_binop(e1, e2, "_div")
            }
            e1: (@) _ "%" _ e2: @ {
                make_vexpr_binop(e1, e2, "_mod")
            }
            --
            // exponent and logarithm TODO: check associativity
            e1: (@) _ "^" _ e2: @ {
                make_vexpr_binop(e1, e2, "_pow")
            }
            e1: (@) _ "@" _ e2: @ {
                make_vexpr_binop(e1, e2, "_log")
            }
            --
            // suffix neg, ref, deref
            e: @ _ "~" {
                make_vexpr_unop(e, "_neg")
            }
            e: @ _ "&" {
                make_vexpr_unop(e, "_ref")
            }
            e: @ _ "$" {
                make_vexpr_unop(e, "_deref")
            }/*
            --
            // function application (right associative) with optional type params
            e1: @ _ o: ("{" _ l: type_list() _ "}" _ {l})? !['+' | '-'] e2: (@) { ValueExpr {
                variant: ExprVariant::BinOp {
                    op:        BinOpExpr::Call(
                        match o {
                            Some(l) => l,
                            None    => Vec::new(),
                        }
                    ),
                    subexpr_1: Box::new(e1),
                    subexpr_2: Box::new(e2),
                },
                texpr: None,
            }}
            */
            --
            // atoms / non-direct recursion
            e: vexpr_lit() {e}
            e: vexpr_var() {e}
            /*
            e: prod_expr() {e}
            e: sum_expr() {e}
            e: closure_expr() {e}
            */
        }

        // any literal value
        rule vexpr_lit() -> Expr<'input> =
            quiet!{
                lit_bool_expr() / lit_int_expr() /
                lit_float_expr() / lit_ascii_expr()
            } / expected!("literal expression")
        // boolean literal
        rule lit_bool_expr() -> Expr<'input> =
            b: literal_boolean() {
                make_vexpr_lit_bool(b)
            }
        // signed integer literal
        rule lit_int_expr() -> Expr<'input> =
            n: literal_integer() {
                make_vexpr_lit_int(n)
            }
        // floating point literal
        rule lit_float_expr() -> Expr<'input> =
            x: literal_float() {
                make_vexpr_lit_float(x)
            }
        // ascii string literal
        rule lit_ascii_expr() -> Expr<'input> =
            s: literal_string() {
                make_vexpr_lit_ascii(s.as_bytes().to_vec())
            }
        // value variable
        rule vexpr_var() -> Expr<'input> =
            n: value_name() {?
                (!is_kw_value(n) && !is_kw_statement(n))
                    .then_some(make_vexpr_var(n)).ok_or("value variable")
            }
        /*
        // closure expression (functions are closures with no closed-over vars)
        rule closure_expr() -> ValueExpr<'input> =
            o1: ("!" _ l: (type_name() ++ (_ ","  _)) _ "." _ {l})?
            "(" _ l: (opt_typed_value_name() ** (_ "," _)) _ ("," _)? ")"
            _ "->" _
            o2: (t: type_expr() _ ":" _ {t})?
            b: block() { ValueExpr {
                variant:   ExprVariant::Closure {
                    params:      l,
                    type_params: match o1 {
                        Some(l) => l,
                        None    => Vec::new(),
                    },
                    returns:     o2,
                    body:        b,
                },
                texpr: None,
            }}
        // product expression TODO: allow typed fields?
        rule prod_expr() -> ValueExpr<'input> =
            "(" _ l: (labeled_value_list() / value_list_labeled()) _ ")" {
                ValueExpr {
                    variant: ExprVariant::Prod(l),
                    texpr:   None,
                }
            }
        // choice expression TODO: allow types to imply position?
        // tagged expression TODO: allow typed tags?
        rule sum_expr() -> ValueExpr<'input> =
            "[" _ e: (labeled_value() / (e: value_expr() {("_0", e)})) _ "]" {
                ValueExpr {
                    variant: ExprVariant::Sum(e.0, Box::new(e.1)),
                    texpr:   None,
                }
            }
        */
        /* TODO UFCS
        rule ufcs_call_exp() -> Expression
            = e1: exp_specifier "." n: var_name() e2: tuple_exp() {
                
            }
        */

        // **********
        // STATEMENTS
        // **********

        /*
        // a brace-enclosed block of statements
        rule block() -> Vec<Statement<'input>> =
            "{" _ s: (stmt() ** _) _ "}" {s}
        rule stmt() -> Statement<'input> =
            return_stmt() / break_stmt() / continue_stmt() / match_stmt() /
            if_stmt() / loop_stmt() / let_stmt() / def_stmt() / assign_stmt() /
            expr_stmt()
        // return statement
        rule return_stmt() -> Statement<'input> =
            kw_return() _ o: (e: value_expr() _ {e})? ";" { Statement::Return(
                match o {
                    Some(e) => e,
                    None    => ValueExpr {
                        variant: ExprVariant::Prod(Vec::new()),
                        texpr:   Some(TypeExpr::Prod(HashMap::new())),
                    },
                }
            )}
        // break statement
        rule break_stmt() -> Statement<'input> =
            kw_break() _ ";" { Statement::Break }
        // continue statement
        rule continue_stmt() -> Statement<'input> =
            kw_continue() _ ";" { Statement::Continue }
        // match statement (is not exhaustive)
        rule match_stmt() -> Statement<'input> =
            kw_match() _
            e: value_expr() _
            l1: (to_branch() ++ _)
            o: (_ l2: else_branch() {l2})? {
                Statement::Match {
                    vexpr:       e,
                    to_branches: l1,
                    else_block:  match o {
                        Some(l2) => l2,
                        None     => Vec::new(),
                    },
                }
            }
        // if statement
        rule if_stmt() -> Statement<'input> =
            kw_if() _
            e: value_expr() _
            l1: block()
            o: (_ l2: else_branch() {l2})? {
                Statement::If {
                    vexpr:      e,
                    then_block: l1,
                    else_block: match o {
                        Some(l2) => l2,
                        None     => Vec::new(),
                    }
                }
            }
        // to branch of a match statement TODO expr must const TODO sum destructure
        rule to_branch() -> ToBranch<'input> =
            l1: (kw_to() _ e: value_expr() _ {e})+ l2: block() {
                ToBranch {pattern: l1, block: l2}
            }
        // else branch of match or if
        rule else_branch() -> Vec<Statement<'input>> =
            kw_else() _ l: (block() / (s: (match_stmt() / if_stmt()) {
                Vec::from([s])
            })) {l}
        // loop statement
        rule loop_stmt() -> Statement<'input> =
            kw_loop() _ e1: value_expr() _ kw_from() _ e2: value_expr() _
            l: block() {
                Statement::Loop {place: e1, iter: e2, block: l}
            }
        // let statement TODO LHS destructure
        rule let_stmt() -> Statement<'input> =
            kw_let() _ e1: value_expr() _ o: ("=" _ e2: value_expr() _ ";" {e2})? {
                Statement::Let {place: e1, vexpr: o}
            }
        // local definition statement
        rule def_stmt() -> Statement<'input> =
            d: def() { Statement::Def(d) }
        // assignment statement TODO LHS destructure
        rule assign_stmt() -> Statement<'input> =
            e1: value_expr() _ "=" _ e2: value_expr() _ ";" {
                Statement::Assign {place: e1, vexpr: e2}
            }
        // expression statement
        rule expr_stmt() -> Statement<'input> =
            e: value_expr() _ ";" { Statement::Expr(e) }
        */
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn basic_vexpr_1() {
        assert_eq!(vexpr("foo"), Ok(make_vexpr_var("foo")));
    }

    #[test]
    fn basic_vexpr_2() {
        assert_eq!(
            vexpr("1 + foo * 2.1"),
            Ok(make_vexpr_binop(
                make_vexpr_lit_int(1),
                make_vexpr_binop(
                    make_vexpr_var("foo"),
                    make_vexpr_lit_float(2.1),
                    "_mul",
                ),
                "_add"
            ))
        );
    }

    #[test]
    fn basic_vexpr_3() {
        assert_eq!(
            vexpr("cat == \"dog\"~"),
            Ok(make_vexpr_binop(
                make_vexpr_var("cat"),
                make_vexpr_unop(
                    make_vexpr_lit_ascii("dog".as_bytes().to_vec()),
                    "_neg"
                ),
                "_eq"
            ))
        );
    }

    /*#[test]
    fn basic_type_def_1() {
        assert_eq!(defs("Foo := Int;"), Ok(
            Vec::from([Definition::Type(TypeDef {
                name:  "Foo",
                kexpr: None,
                texpr: TypeExpr::Variable("Int"),
            })])
        ));
    }

    #[test]
    fn medium_type_def_1() {
        assert_eq!(defs("Foo := !A. (A, [int: Int, float: Float]);"), Ok(
            Vec::from([Definition::Type(TypeDef {
                name:  "Foo",
                kexpr: None,
                texpr: TypeExpr::Univ(
                    HashMap::from([("A", KIND_0.clone())]),
                    Box::new(TypeExpr::Prod(HashMap::from([
                        ("_0", TypeExpr::Variable("A")),
                        ("_1", TypeExpr::Sum(HashMap::from([
                            ("int", TypeExpr::Variable("Int")),
                            ("float", TypeExpr::Variable("Float")),
                        ]))),
                    ]))),
                ),
            })])
        ));
    }

    #[test]
    fn basic_const_def_1() {
        assert_eq!(defs("foo := bar + false;"), Ok(
            Vec::from([Definition::Const(ConstDef {
                name:  "foo",
                texpr: None,
                vexpr: ValueExpr {
                    variant: ExprVariant::BinOp {
                        op:        BinOpExpr::Add,
                        subexpr_1: Box::new(ValueExpr {
                            variant:   ExprVariant::Variable("bar"),
                            texpr: None,
                        }),
                        subexpr_2: Box::new(ValueExpr {
                            variant:   ExprVariant::Literal(LitExpr::Bool(false)),
                            texpr: Some(TypeExpr::Variable("Bool")),
                        }),
                    },
                    texpr:   None,
                }
            })])
        ));
    }

    #[test]
    fn basic_def_fail_1() {
        assert!(defs("fL%u").is_err())
    }*/

    /*
    #[test]
    fn basic_type_expr_1() {
        assert_eq!(type_expr("A -> Foo(A)"), Ok(Expr {
            tier:  None,
            texpr: None, // TODO should be Tfun
            var:   ExprVar::LFun {
                params: Vec::from(["A", None]),
                bodyt:  None,
                body:   Box::new(Expr {
                   tier:  None,
                   texpr: None,
                   var:   ExprVar::LApp {
                       fun:   Box::new(Expr {
                           tier:  None,
                           texpr: None,
                           var:   ExprVar::Var("Foo")
                       }),
                       param: Box::new(Expr {
                           tier:  None,
                           texpr: None,
                           var:   ExprVar::Var("A")
                       })
                   }
               }),
            }
        }));
    }

    #[test]
    fn basic_value_expr_1() {
        assert_eq!(value_expr("[some = 1 + 1]"), Ok(Expr {
            tier:  None,
            texpr: None,
            var:   ExprVar::LSum(
                "some",
                Box::new(value_expr("1 + 1").unwrap())
            )
        }));
    }

    #[test]
    fn basic_value_expr_order_1() {
        // parenthesize as (foo())$
        assert_eq!(value_expr("foo()$"), Ok(
            ValueExpr {
                variant: ExprVariant::UnOp {
                    op:      UnOpExpr::Deref,
                    subexpr: Box::new(value_expr("foo()").unwrap()),
                },
                texpr:   None,
            }
        ));
    }

    #[test]
    fn basic_value_expr_order_2() {
        // parenthesize as (a * b) - c
        assert_eq!(value_expr("a * b - c"), Ok(
            ValueExpr {
                variant: ExprVariant::BinOp {
                    op:        BinOpExpr::Sub,
                    subexpr_1: Box::new(value_expr("a * b").unwrap()),
                    subexpr_2: Box::new(value_expr("c").unwrap()),
                },
                texpr:   None,
            }
        ));
    }

    #[test]
    fn basic_value_expr_order_3() {
        // parenthesize as (foo(a)) * (bar(b))
        assert_eq!(value_expr("foo(a) * bar(b)"), Ok(
            ValueExpr {
                variant: ExprVariant::BinOp {
                    op:        BinOpExpr::Mul,
                    subexpr_1: Box::new(value_expr("foo(a)").unwrap()),
                    subexpr_2: Box::new(value_expr("bar(b)").unwrap()),
                },
                texpr:   None,
            }
        ));
    }
    /*assert_eq!(
        parser::function("fn foo:(a:Int)->():={}"),
        Ok(Function{
            name: "foo",
            params: Vec::from([("a", TypeExpr::Variable("Int"))]),
            returns: TypeExpr::Tuple(Vec::new()),
            body: Vec::new(),
        })
    );
    assert_eq!(
        parser::function("fn bar:()->(Int,Int):={let m:()=();}"),
        Ok(Function{
            name: "bar",
            params: Vec::new(),
            returns: TypeExpr::Tuple(Vec::from([
                TypeExpr::Variable("Int"),
                TypeExpr::Variable("Int"),
            ])),
            body: Vec::from([Statement::Let]),
        })
    );*/*/

}

