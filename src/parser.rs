// parser for halt. uses peg

use super::ast::*;

pub use program_parser::*;

peg::parser!{
    grammar program_parser() for str {
        // **************
        // CHARACTER SETS
        // **************

        // 1 or 0 space (all other whitespace already removed)
        rule _            = quiet!{[' ']?}
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
        rule name_char()  = ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']

        // ********
        // KEYWORDS
        // ********

        // TODO have true/false/i/inline/yield/enter just be unoverwritable vars?
        rule kw_arr()      = "Arr" !name_char()
        rule kw_ascii()    = "Ascii" !name_char()
        rule kw_bool( )    = "Bool" !name_char()
        rule kw_break()    = "break" !name_char()
        rule kw_continue() = "continue" !name_char()
        rule kw_else()     = "else" !name_char()
        rule kw_enum()     = "N" (s: $(num()+) {? s.parse::<u32>().map_err(|_| "u32")}) !name_char()
        rule kw_false()    = "false" !name_char()
        rule kw_float()    = "F32" !name_char()
        rule kw_from()     = "from" !name_char()
        rule kw_i()        = "i" !name_char()
        rule kw_if()       = "if" !name_char()
        rule kw_int()      = ['S' | 'U'] ("8" / "16" / "32" / "Size") !name_char()
        rule kw_let()      = "let" !name_char()
        rule kw_loop()     = "loop" !name_char()
        rule kw_match()    = "match" !name_char()
        rule kw_opt()      = "Opt" !name_char()
        rule kw_res()      = "Res" !name_char()
        rule kw_return()   = "return" !name_char()
        rule kw_to()       = "to" !name_char()
        rule kw_true()     = "true" !name_char()
        rule kw() = kw_arr() / kw_ascii() / kw_bool() / kw_break() /
            kw_continue() / kw_else() / kw_enum() / kw_false() / kw_float() /
            kw_from() / kw_i() / kw_if() / kw_int() / kw_let() / kw_loop() /
            kw_match() / kw_opt() / kw_res() / kw_return() / kw_to() / kw_true()

        // **************
        // VALUE LITERALS
        // **************

        // booleans
        rule literal_true() -> bool = kw_true() { true }
        rule literal_false() -> bool = kw_false() { false }
        // signed integer
        rule literal_integer() -> i32 =
            s: $("-"? num()+) !(name_char() / ".") {?
                s.parse::<i32>().map_err(|_| "failed to parse int")
            }
        // floating point number
        rule literal_float() -> f32 =
            s: $("-"? num()+ "." num()+) !(name_char() / ".") {?
                s.parse::<f32>().map_err(|_| "failed to parse float")
            }
        // string TODO fix escape sequences
        rule literal_string() -> String =
            "\"" s: $(([^'\"'] / "\\\"")*) "\"" { s.to_string() }

        // *************
        // MISCELLANEOUS
        // *************

        // rule for type annotation/casting
        rule type_annot() -> TypeExpr =
            _ ":" _ t: type_expr() {t}
        // labeled type
        rule labeled_type() -> (String, TypeExpr) =
            n: label_name() t: type_annot() {(n, t)}
        // labeled value
        rule labeled_value() -> (String, ValueExpr) =
            n: label_name() _ "=" _ v: value_expr() {(n, v)}
        // optionally typed value name
        rule opt_typed_value_name() -> (String, Option<TypeExpr>) =
            n: value_name() o: type_annot()? {(n, o)}
        // type list
        rule type_list() -> Vec<TypeExpr> =
            l: (type_expr() ++ (_ "," _)) (_ ",")? {l}
        // labeled type list
        rule labeled_type_list() -> Vec<(String, TypeExpr)> =
            l: (labeled_type() ++ (_ "," _)) (_ ",")? {l}
        // type list with implicit labels
        rule type_list_labeled() -> Vec<(String, TypeExpr)> =
            l: type_list() {
                l.into_iter().enumerate().map(
                    |(n, t)| ("_".to_owned() + &n.to_string(), t)
                ).collect()
            }
        // no types in type list
        rule empty_type() -> Vec<(String, TypeExpr)> =
            "" {Vec::new()}
        // value list
        rule value_list() -> Vec<ValueExpr> =
            l: (value_expr() ++ (_ "," _)) (_ ",")? {l}
        // labeled value list
        rule labeled_value_list() -> Vec<(String, ValueExpr)> =
            l: (labeled_value() ++ (_ "," _)) (_ ",")? {l}
        // value list with implicit labels
        rule value_list_labeled() -> Vec<(String, ValueExpr)> =
            l: value_list() {
                l.into_iter().enumerate().map(
                    |(n, t)| ("_".to_owned() + &n.to_string(), t)
                ).collect()
            }
        // no values in value list
        rule empty_value() -> Vec<(String, ValueExpr)> =
            "" {Vec::new()}

        // ************************
        // PROGRAMMER-DEFINED NAMES
        // ************************

        // user defined variable names
        rule value_name() -> String = // TODO later check for leading _ in var name
            quiet!{
                !kw() n: $(lower() value_char()*) !value_char() {
                    n.to_string()
                }
            } / expected!("value variable name")
        // user defined label names for product fields or sum tags
        rule label_name() -> String =
            quiet!{
                !kw() n: $(value_char()+) !value_char() {
                    n.to_string()
                }
            } / expected!("label name")
        // user defined type names
        rule type_name() -> String =
            quiet!{
                !kw() n: $(upper() alphanum()*) !type_char() {
                    n.to_string()
                }
            } / expected!("type variable name")

        // *********************
        // TOP-LEVEL DEFINITIONS
        // *********************

        // collect all top-level definitions
        pub rule defs() -> Vec<Definition> =
            _ d: (def() **  _) _ {d}
        rule def() -> Definition = type_def() / const_def()
        // definition of a type
        rule type_def() -> Definition =
            n: type_name() _ ":=" _ t: type_expr() _ ";" {
                Definition::Type(TypeDef {
                    name: n,
                    expr: t,
                })
            }
        // definition of a constant variable
        rule const_def() -> Definition =
            n: value_name() o: type_annot()? _ ":=" _ v: value_expr() _ ";" {
                Definition::Const(ConstDef{
                    name:      n,
                    type_expr: o,
                    expr:      v,
                })
            }

        // ****************
        // TYPE EXPRESSIONS
        // ****************

        // type expressions // TODO add plus and mul for combining sums and products?
        pub rule type_expr() -> TypeExpr = precedence!{
            // function type is only binary op
            t1: (@) _ "->" _ t2: @ {
                TypeExpr::Function(Box::new(t1), Box::new(t2))
            }
            --
            // inserting universal type parameters
            t: @ _ "{" _ l: type_list() _ "}" {
                TypeExpr::TypeParams(Box::new(t), l)
            }
            --
            // declare new universal type variable
            n: type_name() _ "!" _ t: @ {TypeExpr::Universal(n, Box::new(t))}
            // declare new existential type variable
            n: type_name() _ "?" _ t: @ {TypeExpr::Existential(n, Box::new(t))}
            --
            // atoms
            t: keyword_type() {t}
            t: variable_type() {t}
            t: prod_type() {t}
            t: sum_type() {t}
        }
        // keyword type
        rule keyword_type() -> TypeExpr =
            n: $(kw_bool() / kw_enum() / kw_int() / kw_float() / kw_opt() /
            kw_res() / kw_arr() / kw_ascii()) { 
            TypeExpr::Variable(n.to_string())
        }
        // type variable
        rule variable_type() -> TypeExpr =
            n: type_name() {TypeExpr::Variable(n)}
        // product type, implicit fields, explicit fields, or empty
        rule prod_type() -> TypeExpr =
            "(" _ l: (type_list_labeled() / labeled_type_list() / empty_type()) _ ")" {
                TypeExpr::Prod(l)
            }
        // sum type, implicit fields, explicit fields, or empty
        rule sum_type() -> TypeExpr =
            "[" _ l: (type_list_labeled() / labeled_type_list() / empty_type()) _ "]" {
                TypeExpr::Sum(l)
            }

        // *****************
        // VALUE EXPRESSIONS
        // *****************

        pub rule value_expr() -> ValueExpr = precedence!{
            // equality
            e1: (@) _ "==" _ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Equ,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            e1: (@) _ "!=" _ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Neq,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            --
            // comparison / shift
            e1: (@) _ ">" _ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Gt,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            e1: (@) _ "<" _ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Lt,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            e1: (@) _ ">=" _ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Gte,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            e1: (@) _ "<=" _ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Lte,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            --
            // or
            e1: (@) _ "\\/" _ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Or,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            --
            // and
            e1: (@) _ "/\\" _ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::And,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            --
            // addition and subtraction
            e1: (@) _ "+" _ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Add,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            e1: (@) _ "-" _ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Sub,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            --
            // multiplication, division, and modulo
            e1: (@) _ "*" _ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Mul,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            e1: (@) _ "/" _ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Div,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            e1: (@) _ "%" _ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Mod,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            --
            // exponent and logarithm TODO: check associativity
            e1: (@) _ "^" _ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Pow,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            e1: (@) _ "@" _ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Log,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            --
            // prefix positive, negative, and not
            "+" _ e: @ { ValueExpr {
                variant: ExprVariant::UnaryOp(
                    UnOpVariant::Pos,
                    Box::new(e),
                ),
                type_expr: None,
            }}
            "-" _ e: @ { ValueExpr {
                variant: ExprVariant::UnaryOp(
                    UnOpVariant::Neg,
                    Box::new(e),
                ),
                type_expr: None,
            }}
            "!" _ e: @ { ValueExpr {
                variant: ExprVariant::UnaryOp(
                    UnOpVariant::Not,
                    Box::new(e),
                ),
                type_expr: None,
            }}
            --
            // suffix type annotation / cast, reference, dereference
            e: @ t: type_annot() { ValueExpr {
                variant: ExprVariant::UnaryOp(
                    UnOpVariant::Cast,
                    Box::new(e),
                ),
                type_expr: Some(t),
            }}
            e: @ _ "&" { ValueExpr {
                variant: ExprVariant::UnaryOp(
                    UnOpVariant::Ref,
                    Box::new(e),
                ),
                type_expr: None,
            }}
            e: @ _ "$" { ValueExpr {
                variant: ExprVariant::UnaryOp(
                    UnOpVariant::Deref,
                    Box::new(e),
                ),
                type_expr: None,
            }}
            --
            // function application (right associative) with optional type params
            e1: @ _ o: ("{" _ l: type_list() _ "}" _ {l})? !['+' | '-'] e2: (@) { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Call(
                        match o {
                            Some(l) => l,
                            None    => Vec::new(),
                        }
                    ),
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            --
            // atoms / non-direct recursion
            e: lit_expr() {e}
            e: closure_expr() {e}
            e: variable_expr() {e}
            e: prod_expr() {e}
            e: sum_expr() {e}
        }
        // any literal
        rule lit_expr() -> ValueExpr =
            quiet!{
                lit_unit_expr() / lit_bool_expr() / lit_int_expr() /
                lit_float_expr() / lit_ascii_expr()
            } / expected!("literal expression")
        // unit literal
        rule lit_unit_expr() -> ValueExpr =
            "(" _ ")" { ValueExpr {
                variant:   ExprVariant::Prod(Vec::new()),
                type_expr: Some(TypeExpr::Prod(Vec::new())),
            }}
        // boolean literal
        rule lit_bool_expr() -> ValueExpr =
            b: (literal_true() / literal_false()) { ValueExpr {
                variant:   ExprVariant::Literal(LitVariant::Bool(b)),
                type_expr: Some(TypeExpr::Variable("Bool".to_string())),
            }}
        // signed integer literal
        rule lit_int_expr() -> ValueExpr =
            n: literal_integer() { ValueExpr {
                variant:   ExprVariant::Literal(LitVariant::Integer(n)),
                type_expr: Some(TypeExpr::Variable("S32".to_string())),
            }}
        // floating point literal
        rule lit_float_expr() -> ValueExpr =
            x: literal_float() { ValueExpr {
                variant:   ExprVariant::Literal(LitVariant::Float(x)),
                type_expr: Some(TypeExpr::Variable("F32".to_string())),
            }}
        // ascii string literal
        rule lit_ascii_expr() -> ValueExpr =
            s: literal_string() { ValueExpr {
                variant:   ExprVariant::Literal(LitVariant::Ascii(s.as_bytes().to_vec())),
                type_expr: Some(TypeExpr::Variable("Ascii".to_string())),
            }}
        // value variable
        rule variable_expr() -> ValueExpr =
            n: value_name() { ValueExpr {
                variant:   ExprVariant::Variable(n),
                type_expr: None,
            }}
        // closure expression (functions are closures with no closed-over vars)
        rule closure_expr() -> ValueExpr =
            o1: ((n: type_name() _ "!" _ {n})+)?
            "(" _ l: (opt_typed_value_name() ** (_ "," _)) _ ("," _)? ")"
            _ "->" _
            o2: (t: type_expr() _ ":" _ {t})?
            b: block() { ValueExpr {
                variant:   ExprVariant::Closure(Closure {
                    params:      l,
                    type_params: match o1 {
                        Some(l) => l,
                        None    => Vec::new(),
                    },
                    returns:     o2,
                    body:        b,
                }),
                type_expr: None,
            }}
        // product expression TODO: allow typed fields?
        rule prod_expr() -> ValueExpr =
            "(" _ l: (value_list_labeled() / labeled_value_list()) _ ")" {
                ValueExpr {
                    variant:   ExprVariant::Prod(l),
                    type_expr: None,
                }
            }
        // choice expression TODO: allow types to imply position?
        // tagged expression TODO: allow typed tags?
        rule sum_expr() -> ValueExpr =
            "[" _ e: ((e: value_expr() {("_0".to_string(), e)}) / labeled_value()) _ "]" {
                ValueExpr {
                    variant:   ExprVariant::Sum(e.0, Box::new(e.1)),
                    type_expr: None,
                }
            }
        /* TODO UFCS
        rule ufcs_call_exp() -> Expression
            = e1: exp_specifier "." n: var_name() e2: tuple_exp() {
                
            }
        */

        // **********
        // STATEMENTS
        // **********

        // a brace-enclosed block of statements
        rule block() -> Vec<Statement> =
            "{" _ s: (stmt() ** _) _ "}" {s}
        rule stmt() -> Statement =
            return_stmt() / break_stmt() / continue_stmt() / match_stmt() /
            if_stmt() / loop_stmt() / let_stmt() / def_stmt() / assign_stmt() /
            expr_stmt()
        // return statement
        rule return_stmt() -> Statement =
            kw_return() _ o: (e: value_expr() _ {e})? ";" { Statement::Return(
                match o {
                    Some(e) => e,
                    None    => ValueExpr {
                        variant:   ExprVariant::Prod(Vec::new()),
                        type_expr: Some(TypeExpr::Prod(Vec::new())),
                    },
                }
            )}
        // break statement
        rule break_stmt() -> Statement =
            kw_break() _ ";" { Statement::Break }
        // continue statement
        rule continue_stmt() -> Statement =
            kw_continue() _ ";" { Statement::Continue }
        // match statement (is not exhaustive)
        rule match_stmt() -> Statement =
            kw_match() _
            e: value_expr() _
            l1: (to_branch() ++ _)
            o: (_ l2: else_branch() {l2})? {
                Statement::Match(Match {
                    value:       e,
                    to_branches: l1,
                    else_branch: match o {
                        Some(l2) => l2,
                        None     => Vec::new(),
                    },
                })
            }
        // if statement
        rule if_stmt() -> Statement =
            kw_if() _
            e: value_expr() _
            l1: block()
            o: (_ l2: else_branch() {l2})? {
                Statement::If(If {
                    value:       e,
                    then_branch: l1,
                    else_branch: match o {
                        Some(l2) => l2,
                        None     => Vec::new(),
                    }
                })
            }
        // to branch of a match statement TODO expr must const TODO sum destructure
        rule to_branch() -> ToBranch =
            l1: (kw_to() _ e: value_expr() _ {e})+ l2: block() {
                ToBranch{pattern: l1, block: l2}
            }
        // else branch of match or if
        rule else_branch() -> Vec<Statement> =
            kw_else() _ l: (block() / (s: (match_stmt() / if_stmt()) {
                Vec::from([s])
            })) {l}
        // loop statement
        rule loop_stmt() -> Statement =
            kw_loop() _ e1: value_expr() _ kw_from() _ e2: value_expr() _
            l: block() {
                Statement::Loop(Loop{value: e1, iter: e2, body: l})
            }
        // let statement TODO LHS destructure
        rule let_stmt() -> Statement =
            kw_let() _ e1: value_expr() _ o: ("=" _ e2: value_expr() _ ";" {e2})? {
                Statement::Let(e1, o)
            }
        // local definition statement
        rule def_stmt() -> Statement =
            d: def() { Statement::Def(d) }
        // assignment statement TODO LHS destructure
        rule assign_stmt() -> Statement =
            e1: value_expr() _ "=" _ e2: value_expr() _ ";" {
                Statement::Assign(e1, e2)
            }
        // expression statement
        rule expr_stmt() -> Statement =
            e: value_expr() _ ";" { Statement::Expr(e) }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn basic_type_def() {
        assert_eq!(defs("Foo := Int;"), Ok(
            Vec::from([Definition::Type(TypeDef{
                name: "Foo".to_string(),
                expr: TypeExpr::Variable("Int".to_string()),
            })])
        ));
    }

    #[test]
    fn medium_type_def() {
        assert_eq!(defs("Foo := A! (A, [int: Int, float: Float]);"), Ok(
            Vec::from([Definition::Type(TypeDef{
                name: "Foo".to_string(),
                expr: TypeExpr::Universal(
                    "A".to_string(),
                    Box::new(TypeExpr::Prod(Vec::from([
                        ("_0".to_string(), TypeExpr::Variable("A".to_string())),
                        ("_1".to_string(), TypeExpr::Sum(Vec::from([
                            ("int".to_string(), TypeExpr::Variable("Int".to_string())),
                            ("float".to_string(), TypeExpr::Variable("Float".to_string())),
                        ]))),
                    ]))),
                ),
            })])
        ));
    }

    #[test]
    fn basic_const_def() {
        assert_eq!(defs("foo := bar + false;"), Ok(
            Vec::from([Definition::Const(ConstDef{
                name:        "foo".to_string(),
                type_expr:   None,
                expr:        ValueExpr {
                    variant:   ExprVariant::BinaryOp(
                        BinOpVariant::Add,
                        Box::new(ValueExpr{
                            variant:   ExprVariant::Variable("bar".to_string()),
                            type_expr: None,
                        }),
                        Box::new(ValueExpr{
                            variant:   ExprVariant::Literal(LitVariant::Bool(false)),
                            type_expr: Some(TypeExpr::Variable("Bool".to_string())),
                        }),
                    ),
                    type_expr: None,
                }
            })])
        ));
    }

    #[test]
    fn basic_def_fail() {
        assert!(defs("fL%u").is_err())
    }

    #[test]
    fn basic_expr_order_of_ops_1() {
        // parenthesize as (foo())$
        assert_eq!(value_expr("foo()$"), Ok(
            ValueExpr {
                variant:   ExprVariant::UnaryOp(
                    UnOpVariant::Deref,
                    Box::new(value_expr("foo()").unwrap()),
                ),
                type_expr: None,
            }
        ));
    }

    #[test]
    fn basic_expr_order_of_ops_2() {
        // parenthesize as (a * b) - c
        assert_eq!(value_expr("a * b - c"), Ok(
            ValueExpr {
                variant:   ExprVariant::BinaryOp(
                    BinOpVariant::Sub,
                    Box::new(value_expr("a * b").unwrap()),
                    Box::new(value_expr("c").unwrap()),
                ),
                type_expr: None,
            }
        ));
    }

    #[test]
    fn basic_expr_order_of_ops_3() {
        // parenthesize as (foo(a)) * (bar(b))
        assert_eq!(value_expr("foo(a) * bar(b)"), Ok(
            ValueExpr {
                variant:   ExprVariant::BinaryOp(
                    BinOpVariant::Mul,
                    Box::new(value_expr("foo(a)").unwrap()),
                    Box::new(value_expr("bar(b)").unwrap()),
                ),
                type_expr: None,
            }
        ));
    }
    /*assert_eq!(
        parser::function("fn foo:(a:Int)->():={}"),
        Ok(Function{
            name: "foo".to_string(),
            params: Vec::from([("a".to_string(), TypeExpr::Variable("Int".to_string()))]),
            returns: TypeExpr::Tuple(Vec::new()),
            body: Vec::new(),
        })
    );
    assert_eq!(
        parser::function("fn bar:()->(Int,Int):={let m:()=();}"),
        Ok(Function{
            name: "bar".to_string(),
            params: Vec::new(),
            returns: TypeExpr::Tuple(Vec::from([
                TypeExpr::Variable("Int".to_string()),
                TypeExpr::Variable("Int".to_string()),
            ])),
            body: Vec::from([Statement::Let]),
        })
    );*/

}

