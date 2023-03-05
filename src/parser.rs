use super::ast::*;

peg::parser!{
    pub grammar program_parser() for str {
        // ****************************
        // RULES FOR USER-DEFINED NAMES
        // ****************************

        // user defined variable names
        rule value_name() -> String = // TODO later check for leading _ in var name
            quiet!{
                n: $(['a'..='z']['a'..='z' | '0'..='9' | '_']*) !name_char() {
                    n.to_string()
                }
            } / expected!("value variable name")
        // user defined label names for product fields or sum tags
        rule label_name() -> String =
            quiet!{
                n: $(['a'..='z' | '0'..='9']['a'..='z' | '0'..='9' | '_']*) !name_char() {
                    n.to_string()
                }
            } / expected!("label name")
        // user defined type names
        rule type_name() -> String =
            quiet!{
                n: $(['A'..='Z']['A'..='Z' | 'a'..='z' | '0'..='9']*) !name_char() {
                    n.to_string()
                }
            } / expected!("type variable name")
        // user defined universal type names
        rule univ_type_name() -> String =
            quiet!{
                n: $(type_name() "!") !(name_char() / ['!' | '?']){
                    n.to_string()
                }
            } / expected!("universal type variable name")
        // user defined existential type names
        rule exis_type_name() -> String =
            quiet!{
                n: $(type_name() "?") !(name_char() / ['!' | '?']){
                    n.to_string()
                }
            } / expected!("existential type variable name")

        // ******************
        // OTHER HELPER RULES
        // ******************

        // 1 or 0 space (all other whitespace already removed)
        rule _ = quiet!{[' ']?}
        // valid characters in variable names
        rule name_char() = ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']
        // rule for type annotation/casting
        rule type_annot() -> TypeExpr =
            _ ":" _ t: type_expr() {t}
        // label/type pair helper
        rule label_type_pair() -> (String, TypeExpr) =
            n: label_name() t: type_annot() {(n, t)}
        // label/value pair helper
        rule label_value_pair() -> (String, ValueExpr) =
            n: label_name() _ "=" _ v: value_expr() {(n, v)}
        // optionally typed value name helper
        rule opt_typed_value_name() -> (String, Option<TypeExpr>) =
            n: value_name() o: type_annot()? {(n, o)}

        // *******************************
        // RULES FOR TOP-LEVEL DEFINITIONS
        // *******************************

        // collect all top-level definitions
        pub rule defs() -> Vec<Definition> =
            _ d: (def() **  _) _ {d}
        rule def() -> Definition = type_def() / const_def()
        // definition of a type
        rule type_def() -> Definition =
            n: type_name()
            o: ("{" _ l: (univ_type_name() ++ (_ "," _)) _ ("," _)? "}" {l})?
            _ ":=" _
            t: type_expr()
            _ ";" {
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
            n: value_name() _ o: type_annot()? _ ":=" _ v: value_expr() _ ";" {
                Definition::Const(ConstDef{
                    name: n,
                    type_expr: o,
                    type_params: Vec::new(), // TODO fix once decide syntax
                    expr: v,
                })
            }

        // **************************
        // RULES FOR TYPE EXPRESSIONS
        // **************************

        // type expressions
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
            "(" _ t: (type_expr() ** (_ "," _)) _ ("," _)? ")" {
                TypeExpr::Tuple(t)
            }
        // struct type (product with explicit field names)
        rule struct_type() -> TypeExpr =
            "(" _ l: (label_type_pair() ++ (_ "," _)) _ ("," _)? ")" {
                TypeExpr::Struct(l)
            }
        // choice type (sum with implcit tag names)
        rule choice_type() -> TypeExpr =
            "[" _ t: (type_expr() ** (_ "," _)) _ ("," _)? "]" {
                TypeExpr::Choice(t)
            }
        // tagged type (product with explicit tag names)
        rule tagged_type() -> TypeExpr =
            "[" _ l: (label_type_pair() ++ (_ "," _)) _ ("," _)? "]" {
                TypeExpr::Tagged(l)
            }

        // *********************
        // RULES FOR EXPRESSIONS
        // *********************

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
            // literal atoms (true/false should be literals, not variables)
            e: literal_bool_expr() {e}
            e: literal_ascii_expr() {e} // TODO more literals, also imaginary
            --
            // other atoms
            e: variable_expr() {e}
            e: tuple_expr() {e}
        }
        // value variable
        rule variable_expr() -> ValueExpr =
            n: value_name() {
                ValueExpr {
                    variant:   ExprVariant::Variable(n),
                    type_expr: None,
                }
            }
        // boolean literal
        rule literal_bool_expr() -> ValueExpr =
            ("true" { ValueExpr {
                variant:   ExprVariant::Literal(LitVariant::Bool(true)),
                type_expr: Some(TypeExpr::Variable("Bool".to_string(), Vec::new())),
            }}) /
            ("false" { ValueExpr {
                variant:   ExprVariant::Literal(LitVariant::Bool(false)),
                type_expr: Some(TypeExpr::Variable("Bool".to_string(), Vec::new())),
            }})
        // ascii string literal
        rule literal_ascii_expr() -> ValueExpr =
            "\"" s: $(([^'\"'] / "\\\"")*) "\"" { ValueExpr {
                variant:   ExprVariant::Literal(LitVariant::Ascii(s.as_bytes().to_vec())),
                type_expr: Some(TypeExpr::Variable("Ascii".to_string(), Vec::new())),
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
        /* TODO UFCS
        rule ufcs_call_exp() -> Expression
            = e1: exp_specifier "." n: var_name() e2: tuple_exp() {
                
            }
        */

        // ********************
        // RULES FOR STATEMENTS
        // ********************

        // a brace-enclosed block of statements
        rule block() -> Vec<Statement> =
            "{" _ s: (stmt() ** _) _ "}" {s}
        rule stmt() -> Statement =
            return_stmt() / break_stmt() / continue_stmt() / let_stmt() /
            def_stmt() / assign_stmt() / expr_stmt()
        // return statement
        rule return_stmt() -> Statement =
            "return" _ o: (e: value_expr() _ {e}) ";" { Statement::Return }
        // break statement
        rule break_stmt() -> Statement =
            "break" _ ";" { Statement::Break }
        // continue statement
        rule continue_stmt() -> Statement =
            "continue" _ ";" { Statement::Continue }
        // let statement TODO: allow no initialize?
        rule let_stmt() -> Statement =
            "let" _ n: opt_typed_value_name() _ "=" _ e: value_expr()
            _ ";" {
                Statement::Let
            }
        // local definition statement
        rule def_stmt() -> Statement =
            d: def() { Statement::Def }
        // assignment statement TODO: allow more interesting LHS
        rule assign_stmt() -> Statement =
            n: value_name() _ "=" _ e: value_expr() _ ";" { Statement::Assign }
        // expression statement
        rule expr_stmt() -> Statement =
            e: value_expr() _ ";" { Statement::Expr }
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

