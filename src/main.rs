#[derive(Debug, PartialEq)]
pub enum Definition {
    Type(TypeDef),
    Const(ConstDef),
}

#[derive(Debug, PartialEq)]
pub struct TypeDef {
    name:   String,
    params: Vec<String>,
    expr:   TypeExpr,
}

#[derive(Debug, PartialEq)]
pub struct ConstDef {
    name:        String,
    type_expr:   Option<TypeExpr>,
    type_params: Vec<String>,
    expr:        ValueExpr,
}

#[derive(Debug, PartialEq)]
pub enum TypeExpr {
    Variable(String, Vec<TypeExpr>),
    Universal(String),
    Existential(String),
    Tuple(Vec<TypeExpr>),
    Struct(Vec<(String, TypeExpr)>),
    Choice(Vec<TypeExpr>),
    Tagged(Vec<(String, TypeExpr)>),
    Function(Box<TypeExpr>, Box<TypeExpr>),
}

#[derive(Debug, PartialEq)]
pub struct ValueExpr {
    variant:   ExprVariant,
    type_expr: Option<TypeExpr>,
}

#[derive(Debug, PartialEq)]
pub enum ExprVariant { // TODO flesh these out
    Variable(String),
    Literal(LitVariant),
    BinaryOp(BinOpVariant, Box<ValueExpr>, Box<ValueExpr>),
    UnaryOp(UnOpVariant, Box<ValueExpr>),
    Tuple(Vec<ValueExpr>),
    Struct(Vec<(String, ValueExpr)>),
    Choice(Box<ValueExpr>),
    Tagged(String, Box<ValueExpr>),
    Closure,
}

#[derive(Debug, PartialEq)]
pub enum LitVariant {
    Bool(bool),
    Integer(i32),
    Float(f32),
    Ascii(Vec<u8>),
    U8Char(u8),
}

#[derive(Debug, PartialEq)]
pub enum BinOpVariant {
    Pow,
    Log,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Gt,
    Lt,
    Equ,
    Neq,
    And,
    Or,
    Call,
}

#[derive(Debug, PartialEq)]
pub enum UnOpVariant {
    Cast,
    Ref,
    Deref,
    Pos,
    Neg,
    Not,
}

#[derive(Debug, PartialEq)]
pub enum Statement { // TODO flesh these out
    Let,
    Assignment,
    Expression, // TODO in AST check call must have at least one mutable argument
    Return,
    Condition,
    Loop,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    name:    String,
    params:  Vec<(String, TypeExpr)>,
    returns: TypeExpr,
    body:    Vec<Statement>,
}

peg::parser!{
    grammar parser() for str {
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

        // single whitespace
        rule _ = quiet!{[' ' | '\n' | '\t']}
        // 1 or 0 whitespace
        rule __ = quiet!{_?}
        // valid characters in variable names
        rule name_char() = ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']
        // rule for type annotation/casting
        rule type_annot() -> TypeExpr =
            __ ":" __ t: type_expr() {t}

        // *******************************
        // RULES FOR TOP-LEVEL DEFINITIONS
        // *******************************

        // collect all top-level definitions
        pub rule defs() -> Vec<Definition> =
            (d: (type_def() / const_def()) __ ";" __ {d})*
        // definition of a type
        rule type_def() -> Definition =
            n: type_name()
            o: ("{" __ l: (univ_type_name() ++ (__ "," __)) __ "}" {l})?
            __ ":=" __
            t: type_expr() {
                Definition::Type(TypeDef{
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
            n: value_name() __ o: type_annot()? __ ":=" __ v: value_expr() {
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
            t1: (@) __ "->" __ t2: @ {TypeExpr::Function(Box::new(t1), Box::new(t2))}
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
        // label type pair helper
        rule label_type_pair() -> (String, TypeExpr) =
            n: label_name() __ ":" __ t: type_expr() {(n, t)}
        // type variable (may be generic)
        rule variable_type() -> TypeExpr =
            n: type_name()
            o: ("{" __ l: (type_expr() ++ (__ "," __)) __ "}" {l})? {
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
            "(" __ t: (type_expr() ** (__ "," __)) __ ")" {
                TypeExpr::Tuple(t)
            }
        // struct type (product with explicit field names)
        rule struct_type() -> TypeExpr =
            "(" __ l: (label_type_pair() ++ (__ "," __)) __ ")" {
                TypeExpr::Struct(l)
            }
        // choice type (sum with implcit tag names)
        rule choice_type() -> TypeExpr =
            "[" __ t: (type_expr() ** (__ "," __)) __ "]" {
                TypeExpr::Choice(t)
            }
        // tagged type (product with explicit tag names)
        rule tagged_type() -> TypeExpr =
            "[" __ l: (label_type_pair() ++ (__ "," __)) __ "]" {
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
            e: @ __ "&" { ValueExpr {
                variant: ExprVariant::UnaryOp(
                    UnOpVariant::Ref,
                    Box::new(e),
                ),
                type_expr: None,
            }}
            e: @ __ "$" { ValueExpr {
                variant: ExprVariant::UnaryOp(
                    UnOpVariant::Deref,
                    Box::new(e),
                ),
                type_expr: None,
            }}
            --
            // prefix positive, negative, and not
            "+" __ e: @ { ValueExpr {
                variant: ExprVariant::UnaryOp(
                    UnOpVariant::Pos,
                    Box::new(e),
                ),
                type_expr: None,
            }}
            "-" __ e: @ { ValueExpr {
                variant: ExprVariant::UnaryOp(
                    UnOpVariant::Neg,
                    Box::new(e),
                ),
                type_expr: None,
            }}
            "!" __ e: @ { ValueExpr {
                variant: ExprVariant::UnaryOp(
                    UnOpVariant::Not,
                    Box::new(e),
                ),
                type_expr: None,
            }}
            --
            // exponent and logarithm TODO: check associativity
            e1: (@) __ "^" __ e2: @ { ValueExpr{
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Pow,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            e1: (@) __ "@" __ e2: @ { ValueExpr{
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Log,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            --
            // multiplication, division, and modulo
            e1: (@) __ "*" __ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Mul,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            e1: (@) __ "/" __ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Div,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            e1: (@) __ "%" __ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Mod,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            --
            // addition and subtraction
            e1: (@) __ "+" __ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Add,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            e1: (@) __ "-" __ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Sub,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            --
            // comparison / shift
            e1: (@) __ ">" __ e2: @ {ValueExpr{
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Gt,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            e1: (@) __ "<" __ e2: @ {ValueExpr{
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Lt,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            --
            // equality
            e1: (@) __ "==" __ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Equ,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            e1: (@) __ "!=" __ e2: @ { ValueExpr {
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Neq,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            --
            // and
            e1: (@) __ "/\\" __ e2: @ { ValueExpr{
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::And,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            --
            // or
            e1: (@) __ "\\/" __ e2: @ { ValueExpr{
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Or,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            --
            // function application (right associative)
            e1: @ __ e2: (@) { ValueExpr {
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
                ValueExpr{
                    variant:   ExprVariant::Variable(n),
                    type_expr: None,
                }
            }
        // boolean literals
        rule literal_bool_expr() -> ValueExpr =
            ("true" {ValueExpr{
                variant:   ExprVariant::Literal(LitVariant::Bool(true)),
                type_expr: Some(TypeExpr::Variable("Bool".to_string(), Vec::new())),
            }}) /
            ("false" {ValueExpr{
                variant:   ExprVariant::Literal(LitVariant::Bool(false)),
                type_expr: Some(TypeExpr::Variable("Bool".to_string(), Vec::new())),
            }})
        // ascii string literals
        rule literal_ascii_expr() -> ValueExpr =
            "\"" s: $(([^'\"'] / "\\\"")*) "\"" {ValueExpr{
                variant:   ExprVariant::Literal(LitVariant::Ascii(s.as_bytes().to_vec())),
                type_expr: Some(TypeExpr::Variable("Ascii".to_string(), Vec::new())),
            }}
        // tuple expressions
        rule tuple_expr() -> ValueExpr =
            "(" __ l: (value_expr() ** (__ "," __)) __ ")" {ValueExpr{
                variant:  ExprVariant::Tuple(l),
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

        rule let_stmt() -> Statement
            = "let " n: value_name() ":" t: type_expr() "=" e: value_expr() ";" {
                Statement::Let
            }
        rule expr_stmt() -> Statement // TODO should be expression that may mutate rather than just an expression
            = e: value_expr() ";" {Statement::Expression}
        rule scope() -> Vec<Statement>
            = "{" s: (stmt_specifier() *) "}" {s}
        rule stmt_specifier() -> Statement
            = let_stmt() / expr_stmt()

        // one parameter in a parameter list
        rule param() -> (String, TypeExpr)
            = n: value_name() ":" t: type_expr() {(n, t)}
        // parameter list for function definition
        rule params() -> Vec<(String, TypeExpr)>
            = "(" p: (param() ** ",") ")" {p}

        /*pub rule function() -> Function
            = "fn " name: var_name() ":" params: params() "->" returns: type_expr() ":=" body: scope() {
                Function{name, params, returns, body}
            }*/
    }
}



fn main() {
    assert_eq!(parser::defs("Foo := Int;"), Ok(
        Vec::from([Definition::Type(TypeDef{
            name:   "Foo".to_string(),
            params: Vec::new(),
            expr:   TypeExpr::Variable("Int".to_string(), Vec::new()),
        })])
    ));
    assert_eq!(parser::defs("Foo{A!} := (A!, [int: Int, float: Float]);"), Ok(
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
    assert_eq!(parser::defs("foo := bar + false;"), Ok(
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
    assert!(parser::defs("fL%u").is_err())
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
