use super::ast::*;

peg::parser!{
    pub grammar program_parser() for str {
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
        rule type_char()  = ['A'..='Z' | 'a'..='z' | '0'..='9' | '!' | '?']
        // valid characters in all variable names
        rule name_char()  = ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']

        // ********
        // KEYWORDS
        // ********

        // TODO have true/false/i/inline/yield/enter just be unoverwritable vars?
        rule kw_break()    = "break"
        rule kw_continue() = "continue"
        rule kw_else()     = "else"
        rule kw_false()    = "false"
        rule kw_from()     = "from"
        rule kw_i()        = "i"
        rule kw_if()       = "if"
        rule kw_let()      = "let"
        rule kw_loop()     = "loop"
        rule kw_match()    = "match"
        rule kw_return()   = "return"
        rule kw_to()       = "to"
        rule kw_true()     = "true"
        rule kw() = (kw_break() / kw_continue() / kw_else() / kw_false() /
            kw_from() / kw_i() / kw_if() / kw_let() / kw_loop() / kw_match() /
            kw_return() / kw_to() / kw_true()) !name_char()

        // ********
        // LITERALS
        // ********

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
        // label/type pair
        rule label_type_pair() -> (String, TypeExpr) =
            n: label_name() t: type_annot() {(n, t)}
        // label/value pair
        rule label_value_pair() -> (String, ValueExpr) =
            n: label_name() _ "=" _ v: value_expr() {(n, v)}
        // optionally typed value name
        rule opt_typed_value_name() -> (String, Option<TypeExpr>) =
            n: value_name() o: type_annot()? {(n, o)}
        // brace-enclosed universal type variable names
        rule type_params() -> (Vec<String>) =
            _ "{" _ l: (univ_type_name() ++ (_ "," _)) _ ("," _)? "}" {l}

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
        // user defined universal type names
        rule univ_type_name() -> String =
            quiet!{
                !kw() n: $(upper() alphanum()* "!") !type_char() {
                    n.to_string()
                }
            } / expected!("universal type variable name")
        // user defined existential type names
        rule exis_type_name() -> String =
            quiet!{
                !kw() n: $(upper() alphanum()* "?") !type_char() {
                    n.to_string()
                }
            } / expected!("existential type variable name")

        // *********************
        // TOP-LEVEL DEFINITIONS
        // *********************

        // collect all top-level definitions
        pub rule defs() -> Vec<Definition> =
            _ d: (def() **  _) _ {d}
        rule def() -> Definition = type_def() / const_def()
        // definition of a type
        rule type_def() -> Definition =
            n: type_name() o: type_params()? _ ":=" _ t: type_expr() _ ";" {
                Definition::Type(TypeDef {
                    name:   n,
                    params: match o {
                        Some(l) => l,
                        None    => Vec::new(),
                    },
                    expr:   t,
                })
            }
        // definition of a constant variable
        rule const_def() -> Definition =
            n: value_name() o1: type_params()? o2: type_annot()? _ ":=" _ v: value_expr() _ ";" {
                Definition::Const(ConstDef{
                    name:        n,
                    type_expr:   o2,
                    type_params: match o1 {
                        Some(l) => l,
                        None    => Vec::new(),
                    },
                    expr:        v,
                })
            }

        // ****************
        // TYPE EXPRESSIONS
        // ****************

        // type expressions // TODO add plus and mul for combining sums and products?
        pub rule type_expr() -> TypeExpr = precedence!{
            // function type is only binary op
            t1: (@) _ "->" _ t2: @ {TypeExpr::Function(Box::new(t1), Box::new(t2))}
            --
            // atoms
            t: univ_type() {t}
            t: exis_type() {t}
            t: variable_type() {t}
            t: tuple_type() {t}
            t: struct_type() {t}
            t: choice_type() {t}
            t: tagged_type() {t}
        }
        // type variable (may be generic)
        rule variable_type() -> TypeExpr =
            n: type_name()
            o: ("{" _ l: (type_expr() ++ (_ "," _)) _ ("," _)? "}" {l})? {
                TypeExpr::Variable(
                    n,
                    match o {
                        Some(l) => l,
                        None    => Vec::new(),
                    },
                )
            }
        // universal type variable
        rule univ_type() -> TypeExpr =
            n: univ_type_name() {TypeExpr::Universal(n.to_string())}
        // existential type variable
        rule exis_type() -> TypeExpr =
            n: exis_type_name() {TypeExpr::Existential(n.to_string())}
        // tuple type (product with implcit field names)
        rule tuple_type() -> TypeExpr =
            "(" _ o: (l: (type_expr() ++ (_ "," _)) _ ("," _)? {l})? ")" {
                TypeExpr::Tuple(match o {
                    Some(l) => l,
                    None    => Vec::new(),
                })
            }
        // struct type (product with explicit field names)
        rule struct_type() -> TypeExpr =
            "(" _ l: (label_type_pair() ++ (_ "," _)) _ ("," _)? ")" {
                TypeExpr::Struct(l)
            }
        // choice type (sum with implcit tag names)
        rule choice_type() -> TypeExpr =
            "[" _ o: (l: (type_expr() ++ (_ "," _)) _ ("," _)? {l})? "]" {
                TypeExpr::Choice(match o {
                    Some(l) => l,
                    None    => Vec::new(),
                })
            }
        // tagged type (product with explicit tag names)
        rule tagged_type() -> TypeExpr =
            "[" _ l: (label_type_pair() ++ (_ "," _)) _ ("," _)? "]" {
                TypeExpr::Tagged(l)
            }

        // *****************
        // VALUE EXPRESSIONS
        // *****************

        pub rule value_expr() -> ValueExpr = precedence!{
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
            // function application (right associative)
            e1: @ _ e2: (@) { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Call,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            --
            // true/false should be literals, not variables
            e: lit_expr() {e}
            // should try closures over tuples
            e: closure_expr() {e}
            --
            // other atoms
            e: variable_expr() {e}
            e: tuple_expr() {e}
            e: struct_expr() {e}
            e: choice_expr() {e}
            e: tagged_expr() {e}
        }
        // any literal
        rule lit_expr() -> ValueExpr =
            quiet!{
                lit_bool_expr() / lit_int_expr() / lit_float_expr() / lit_ascii_expr()
            } / expected!("literal expression")
        // boolean literal
        rule lit_bool_expr() -> ValueExpr =
            b: (literal_true() / literal_false()) { ValueExpr {
                variant:   ExprVariant::Literal(LitVariant::Bool(b)),
                type_expr: Some(TypeExpr::Variable("Bool".to_string(), Vec::new())),
            }}
        // signed integer literal
        rule lit_int_expr() -> ValueExpr =
            n: literal_integer() { ValueExpr {
                variant:   ExprVariant::Literal(LitVariant::Integer(n)),
                type_expr: Some(TypeExpr::Variable("S32".to_string(), Vec::new())),
            }}
        // floating point literal
        rule lit_float_expr() -> ValueExpr =
            x: literal_float() { ValueExpr {
                variant:   ExprVariant::Literal(LitVariant::Float(x)),
                type_expr: Some(TypeExpr::Variable("F32".to_string(), Vec::new())),
            }}
        // ascii string literal
        rule lit_ascii_expr() -> ValueExpr =
            s: literal_string() { ValueExpr {
                variant:   ExprVariant::Literal(LitVariant::Ascii(s.as_bytes().to_vec())),
                type_expr: Some(TypeExpr::Variable("Ascii".to_string(), Vec::new())),
            }}
        // value variable
        rule variable_expr() -> ValueExpr =
            n: value_name() { ValueExpr {
                variant:   ExprVariant::Variable(n),
                type_expr: None,
            }}
        // closure expression (functions are closures with no closed-over vars)
        rule closure_expr() -> ValueExpr =
            "(" _ l: (opt_typed_value_name() ** (_ "," _)) _ ("," _)? ")" _ "->" _
            o: (t: type_expr() _ ":" _ {t})? b: block() { ValueExpr {
                variant:   ExprVariant::Closure(Box::new(Closure {
                    params:  l,
                    returns: o,
                    body:    b,
                })),
                type_expr: None,
            }}
        // tuple expression
        rule tuple_expr() -> ValueExpr =
            "(" _ l: (value_expr() ** (_ "," _)) _ ("," _)? ")" { ValueExpr {
                variant:   ExprVariant::Tuple(l),
                type_expr: None,
            }}
        // struct expression TODO: allow typed fields?
        rule struct_expr() -> ValueExpr =
            "(" _ l: (label_value_pair() ** (_ "," _)) _ ("," _)? ")" { ValueExpr {
                variant:   ExprVariant::Struct(l),
                type_expr: None,
            }}
        // choice expression TODO: allow types to imply position?
        rule choice_expr() -> ValueExpr =
            "[" _ e: value_expr() _ "]" { ValueExpr {
                variant:   ExprVariant::Choice(Box::new(e)),
                type_expr: None,
            }}
        // tagged expression TODO: allow typed tags?
        rule tagged_expr() -> ValueExpr =
            "[" _ e: label_value_pair() _ "]" { ValueExpr {
                variant:   ExprVariant::Tagged(e.0, Box::new(e.1)),
                type_expr: None,
            }}
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
                        variant:   ExprVariant::Tuple(Vec::new()),
                        type_expr: None, // TODO set to unit type
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
            kw_match() _ value_expr() _ (to_branch() ++ _) (_ else_branch())? {
                //Statement::Match
                Statement::Break
            }
        // if statement
        rule if_stmt() -> Statement =
            kw_if() _ e: value_expr() _ l1: block() o: (_ l2: else_branch() {l2})? {
                Statement::If(e, l1, match o {
                    Some(l2) => l2,
                    None     => Vec::new(),
                })
            }
        // to branch of a match statement TODO expr must const TODO sum destructure
        rule to_branch() -> () =
            kw_to() _ (value_expr() / ("[" _ label_name() _ "=" _ value_name() _ "]")) _ block()
        // else branch of match or if
        rule else_branch() -> Vec<Statement> =
            kw_else() _ l: (block() / (s: (match_stmt() / if_stmt()) {
                Vec::from([s])
            })) {l}
        rule loop_stmt() -> Statement =
            kw_loop() _ n: opt_typed_value_name() _ kw_from() _ value_expr() _
            block() {
                //Statement::Loop
                Statement::Break
            }
        // let statement TODO: allow no initialize?
        rule let_stmt() -> Statement =
            kw_let() _ n: opt_typed_value_name() _ "=" _ e: value_expr()
            _ ";" {
                //Statement::Let
                Statement::Break
            }
        // local definition statement
        rule def_stmt() -> Statement =
            d: def() { Statement::Def(d) }
        // assignment statement TODO: allow more interesting LHS
        rule assign_stmt() -> Statement =
            n: value_name() _ "=" _ e: value_expr() _ ";" {
                //Statement::Assign
                Statement::Break
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
        assert_eq!(program_parser::defs("Foo := Int;"), Ok(
            Vec::from([Definition::Type(TypeDef{
                name:   "Foo".to_string(),
                params: Vec::new(),
                expr:   TypeExpr::Variable("Int".to_string(), Vec::new()),
            })])
        ));
    }

    #[test]
    fn medium_type_def() {
        assert_eq!(program_parser::defs("Foo{A!} := (A!, [int: Int, float: Float]);"), Ok(
            Vec::from([Definition::Type(TypeDef{
                name:   "Foo".to_string(),
                params: Vec::from(["A!".to_string()]),
                expr:   TypeExpr::Tuple(Vec::from([
                    TypeExpr::Universal("A!".to_string()),
                    TypeExpr::Tagged(Vec::from([
                        ("int".to_string(), TypeExpr::Variable("Int".to_string(), Vec::new())),
                        ("float".to_string(), TypeExpr::Variable("Float".to_string(), Vec::new())),
                    ])),
                ])),
            })])
        ));
    }

    #[test]
    fn basic_const_def() {
        assert_eq!(program_parser::defs("foo := bar + false;"), Ok(
            Vec::from([Definition::Const(ConstDef{
                name:        "foo".to_string(),
                type_expr:   None,
                type_params: Vec::new(),
                expr:        ValueExpr {
                    variant:   ExprVariant::BinaryOp(
                        BinOpVariant::Add,
                        Box::new(ValueExpr{
                            variant:   ExprVariant::Variable("bar".to_string()),
                            type_expr: None,
                        }),
                        Box::new(ValueExpr{
                            variant:   ExprVariant::Literal(LitVariant::Bool(false)),
                            type_expr: Some(TypeExpr::Variable("Bool".to_string(), Vec::new())),
                        }),
                    ),
                    type_expr: None,
                }
            })])
        ));
    }

    #[test]
    fn basic_def_fail() {
        assert!(program_parser::defs("fL%u").is_err())
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

