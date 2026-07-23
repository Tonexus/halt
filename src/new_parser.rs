// parser for halt. uses peg

use std::collections::HashMap;

use super::new_ast::*;
use super::misc::*;

pub use program_parser::*;


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

        // rule for type annotation
        rule type_annot() -> (u32, Expr<'input>) =
            _ l: (":"*<1, 9>) _ t: vexpr() {(l.len() as u32 - 1, t)}
        // rule for reverse type annotation (function output)
        rule type_annot_rev() -> (u32, Expr<'input>) =
            _ t: vexpr() _ l: (":"*<1, 9>) {(l.len() as u32 - 1, t)}
        /*
        // labeled type
        rule labeled_type() -> (&'input str, Expr<'input>) =
            n: label_name() t: type_annot() {(n, t)}
        */
        // optionally typed value name // TODO used as param names, allow types as params
        rule opt_typed_value_name() -> (&'input str, Option<(u32, Expr<'input>)>) =
            n: value_name() o: type_annot()? {(n, o)}
        /*
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
        */
        // value list with implicit labels (instantiating tuple)
        rule tuple_value_list() -> Vec<(&'input str, Expr<'input>)> =
            l: (vexpr() ++ (_ "," _)) (_ ",")? {?
                if l.len() > 10 {
                    return Err("Too many labels");
                }
                return Ok(l.into_iter().enumerate().map(
                    |(n, v)| (LABELS[n], v)
                ).collect());
            }
        // label with value (instantiating a product or sum)
        rule labeled_value() -> (&'input str, Expr<'input>) =
            n: label_name() _ "=" _ v: vexpr() {(n, v)}
        // labeled value list (instantiating product)
        rule prod_value_list() -> Vec<(&'input str, Expr<'input>)> =
            l: (labeled_value() ++ (_ "," _)) (_ ",")? {l}
        // no values in value list
        rule empty_value_list() -> Vec<(&'input str, Expr<'input>)> =
            "" {Vec::new()}

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

        // collect all top-level definitions
        pub rule defs() -> Vec<Def<'input>> =
            _ d: (def() **  _) _ {d}
        rule def() -> Def<'input> = type_def() / val_def()
        // definition of a type
        rule type_def() -> Def<'input> =
            n: type_name() _ ":=" _ t: vexpr() _ ";" {
                Def {
                    name:     n,
                    min_tier: 1,
                    max_tier: MAX_TIER,
                    texpr:    None, // TODO allow annotation
                    expr:     t,
                }
            }
        // definition of a constant variable
        rule val_def() -> Def<'input> =
            n: value_name() _ ":=" _ v: vexpr() _ ";" {
                Def{
                    name:  n,
                    min_tier: 0,
                    max_tier: 0,
                    texpr:    None, // TODO allow annotation
                    expr:     v,
                }
            }

        // ****************
        // TYPE EXPRESSIONS
        // ****************

        // type expressions // TODO add plus and mul for combining sums and products?
        /*
        pub rule texpr() -> Expr<'input> = precedence!{
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
            t: texpr_var() {t}
            t: prod_type() {t}
            t: sum_type() {t}
        }
        */
        /*
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
                make::vexpr_binop(e1, e2, "_eq")
            }
            e1: (@) _ "!=" _ e2: @ {
                make::vexpr_binop(e1, e2, "_neq")
            }
            --
            // comparison / shift
            e1: (@) _ ">" _ e2: @ {
                make::vexpr_binop(e1, e2, "_gt")
            }
            e1: (@) _ "<" _ e2: @ {
                make::vexpr_binop(e1, e2, "_lt")
            }
            e1: (@) _ ">=" _ e2: @ {
                make::vexpr_binop(e1, e2, "_gte")
            }
            e1: (@) _ "<=" _ e2: @ {
                make::vexpr_binop(e1, e2, "_lte")
            }
            --
            // or
            e1: (@) _ "\\/" _ e2: @ {
                make::vexpr_binop(e1, e2, "_or")
            }
            --
            // and
            e1: (@) _ "/\\" _ e2: @ {
                make::vexpr_binop(e1, e2, "_and")
            }
            --
            // addition and subtraction
            e1: (@) _ "+" _ e2: @ {
                make::vexpr_binop(e1, e2, "_add")
            }
            e1: (@) _ "-" _ e2: @ {
                make::vexpr_binop(e1, e2, "_sub")
            }
            --
            // multiplication, division, and modulo
            e1: (@) _ "*" _ e2: @ {
                make::vexpr_binop(e1, e2, "_mul")
            }
            e1: (@) _ "/" _ e2: @ {
                make::vexpr_binop(e1, e2, "_div")
            }
            e1: (@) _ "%" _ e2: @ {
                make::vexpr_binop(e1, e2, "_mod")
            }
            --
            // exponent and logarithm TODO: check associativity
            // TODO log as | instead, @ as access?
            e1: (@) _ "^" _ e2: @ {
                make::vexpr_binop(e1, e2, "_pow")
            }
            e1: (@) _ "@" _ e2: @ {
                make::vexpr_binop(e1, e2, "_log")
            }
            --
            // suffix neg, ref, deref
            e: @ _ "~" {
                make::vexpr_unop(e, "_neg")
            }
            e: @ _ "&" {
                make::vexpr_unop(e, "_ref")
            }
            e: @ _ "$" {
                make::vexpr_unop(e, "_deref")
            }
            --
            // function application (right associative)
            e1: @ _ e2: (@) {
                make::expr_app(e1, e2)
            }
            --
            // atoms / non-direct recursion
            e: vexpr_lit() {e}
            e: vexpr_var() {e}
            e: texpr_var() {e}
            e: vexpr_fun() {e}
            e: expr_let() {e}
            e: expr_prod() {e}
            e: expr_sum() {e}
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
                make::vexpr_lit_bool(b)
            }
        // signed integer literal
        rule lit_int_expr() -> Expr<'input> =
            n: literal_integer() {
                make::vexpr_lit_int(n)
            }
        // floating point literal
        rule lit_float_expr() -> Expr<'input> =
            x: literal_float() {
                make::vexpr_lit_float(x)
            }
        // ascii string literal
        rule lit_ascii_expr() -> Expr<'input> =
            s: literal_string() {
                make::vexpr_lit_ascii(s.as_bytes().to_vec())
            }
        // value variable
        rule vexpr_var() -> Expr<'input> =
            n: value_name() {?
                (!is_kw_value(n) && !is_kw_statement(n))
                    .then_some(make::vexpr_var(n)).ok_or("value variable")
            }
        // type variable
        rule texpr_var() -> Expr<'input> =
            n: type_name() {make::texpr_var(n)}
        // function expression (also closures)
        // distinguish vexpr func from expr func as vexpr allows imperative block
        rule vexpr_fun() -> Expr<'input> =
            "(" _ l: (opt_typed_value_name() ** (_ "," _)) _ ("," _)? ")" _ "->"
            o: type_annot_rev()? _ // TODO only needed for block/vexpr version?
            b: vexpr() {
            //b: block() { // TODO
                make::vexpr_fun(l, o, b)
            }
        // let expression TODO allow optionally assigning var to expression as well
        rule expr_let() -> Expr<'input> =
            "(" _ l: (opt_typed_value_name() ** (_ "," _)) _ ("," _)? ")" _ "|>"
            e: vexpr() {
                make::vexpr_let(l, e)
            }
        // product expression
        rule expr_prod() -> Expr<'input> =
            "(" _ l: (tuple_value_list() / prod_value_list() / empty_value_list()) _ ")" {
                make::expr_prod(l)
            }
        // choice expression
        // tagged expression
        rule expr_sum() -> Expr<'input> =
            "[" _ e: labeled_value() _ "]" {
                make::expr_sum(e.0, e.1)
            }
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
        assert_eq!(vexpr("foo"), Ok(make::vexpr_var("foo")));
    }

    #[test]
    fn basic_vexpr_2() {
        assert_eq!(
            vexpr("1 + foo * 2.1"),
            Ok(make::vexpr_binop(
                make::vexpr_lit_int(1),
                make::vexpr_binop(
                    make::vexpr_var("foo"),
                    make::vexpr_lit_float(2.1),
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
            Ok(make::vexpr_binop(
                make::vexpr_var("cat"),
                make::vexpr_unop(
                    make::vexpr_lit_ascii("dog".as_bytes().to_vec()),
                    "_neg"
                ),
                "_eq"
            ))
        );
    }

    #[test]
    fn basic_vexpr_4(){
        assert_eq!(
            vexpr("(a, b,) -> (c, d: U8) -> 5"),
            Ok(make::vexpr_fun(
                [("a", None), ("b", None)].to_vec(),
                None,
                make::vexpr_fun(
                    [("c", None), ("d", Some((0, make::texpr_var("U8"))))].to_vec(),
                    None,
                    make::vexpr_lit_int(5),
                )
            ))
        );
    }

    #[test]
    fn basic_vexpr_5(){
        assert_eq!(
            vexpr("(a:: U32) -> Str: 7 + a"),
            Ok(make::vexpr_fun(
                [("a", Some((1, make::texpr_var("U32"))))].to_vec(),
                Some((0, make::texpr_var("Str"))),
                make::vexpr_binop(
                    make::vexpr_lit_int(7),
                    make::vexpr_var("a"),
                    "_add"
                )
            ))
        );
    }

    #[test]
    fn basic_vexpr_6(){
        assert_eq!(
            vexpr("foo bar baz 10"),
            Ok(make::expr_app(
                make::vexpr_var("foo"),
                make::expr_app(
                    make::vexpr_var("bar"),
                    make::expr_app(
                        make::vexpr_var("baz"),
                        make::vexpr_lit_int(10)
                    )
                )
            ))
        );
    }

    #[test]
    fn basic_vexpr_7(){
        assert_eq!(
            vexpr("(foo, [bar = 1], baz, ())"),
            Ok(make::expr_prod([
                (LABELS[0], make::vexpr_var("foo")),
                (LABELS[1], make::expr_sum("bar", make::vexpr_lit_int(1))),
                (LABELS[2], make::vexpr_var("baz")),
                (LABELS[3], make::expr_prod(Vec::new()))
            ].to_vec()))
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

