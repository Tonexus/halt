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
    Call,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Log,
    Equ,
    Neq,
    Gt,
    Lt,
    And,
    Or,
}

#[derive(Debug, PartialEq)]
pub enum UnOpVariant {
    Cast,
    Neg,
    Not,
    Ref,
    Deref,
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
        // RULES FOR USER DEFINED NAMES
        // ****************************

        // user defined variable names
        rule value_name() -> String = // TODO later check for leading _ in var name
            n: $(['a'..='z']['a'..='z' | '0'..='9' | '_']*) {n.to_string()}
        // user defined label names for product fields or sum tags
        rule label_name() -> String =
            n: $(['a'..='z' | '0'..='9']['a'..='z' | '0'..='9' | '_']*) {n.to_string()}
        // user defined type names
        rule type_name() -> String =
            n: $(['A'..='Z']['A'..='Z' | 'a'..='z' | '0'..='9']*) {n.to_string()}

        // *******************************
        // RULES FOR TOP-LEVEL DEFINITIONS
        // *******************************

        // collect all top-level definitions
        pub rule defs() -> Vec<Definition> =
            (d: (type_def() / const_def()) ";" {d})*
        // definition of a type
        rule type_def() -> Definition =
            n: type_name()
            o: ("{" l: ($(univ_type()) ++ ",") "}" {
                l.into_iter().map(|s| s.to_string()).collect()
            })?
            ":="
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
            //n: value_name() o: (":" t: type_expr() {t})? ":=" v: value_expr() {
            n: value_name() ":=" v: value_expr() {
                Definition::Const(ConstDef{
                    name: n,
                    //type_expr: o,
                    type_expr: None,
                    type_params: Vec::new(), // TODO fix once decide syntax
                    expr: v,
                })
            }

        // **************************
        // RULES FOR TYPE EXPRESSIONS
        // **************************

        // type expressions
        rule type_expr() -> TypeExpr = precedence!{
            // function type is only binary op
            t1: (@) "->" t2: @ {TypeExpr::Function(Box::new(t1), Box::new(t2))}
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
        // label type pair
        rule label_type_pair() -> (String, TypeExpr) =
            n: label_name() ":" t: type_expr() {(n, t)}
        // type variable (may be generic)
        rule variable_type() -> TypeExpr =
            n: type_name()
            o: ("{" l: (type_expr() ++ ",") "}" {l})? {
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
            n: $(type_name() "!") {TypeExpr::Universal(n.to_string())}
        // existential type variable
        rule exis_type() -> TypeExpr =
            n: $(type_name() "?") {TypeExpr::Existential(n.to_string())}
        // tuple type (product with implcit field names)
        rule tuple_type() -> TypeExpr =
            "(" t: (type_expr() ** ",") ")" {
                TypeExpr::Tuple(t)
            }
        // struct type (product with explicit field names)
        rule struct_type() -> TypeExpr =
            "(" l: (label_type_pair() ++ ",") ")" {
                TypeExpr::Struct(l)
            }
        // choice type (sum with implcit tag names)
        rule choice_type() -> TypeExpr =
            "[" t: (type_expr() ** ",") "]" {
                TypeExpr::Choice(t)
            }
        // tagged type (product with explicit tag names)
        rule tagged_type() -> TypeExpr =
            "[" l: (label_type_pair() ++ ",") "]" {
                TypeExpr::Tagged(l)
            }

        // *********************
        // RULES FOR EXPRESSIONS
        // *********************

        rule value_expr() -> ValueExpr = precedence!{
            // type annotation / cast
            e: @ ":" t: type_expr() {ValueExpr{
                variant: ExprVariant::UnaryOp(
                    UnOpVariant::Cast,
                    Box::new(e),
                ),
                type_expr: Some(t),
            }}
            --
            // addition and subtraction
            e1: (@) "+" e2: @ {ValueExpr{
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Add,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            e1: (@) "-" e2: @ {ValueExpr{
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Sub,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            --
            // function application (right associative)
            e1: @ "" e2: (@) {ValueExpr{
                variant: ExprVariant::BinaryOp(
                    BinOpVariant::Call,
                    Box::new(e1),
                    Box::new(e2),
                ),
                type_expr: None,
            }}
            --
            // literal atoms (true/false should be ltierals, not variables)
            e: literal_bool_expr() {e}
            e: literal_ascii_expr() {e} // TODO more literals
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
            "(" l: (value_expr() ** ",") ")" {ValueExpr{
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
    assert_eq!(parser::defs("Foo:=Int;"), Ok(
        Vec::from([Definition::Type(TypeDef{
            name:   "Foo".to_string(),
            params: Vec::new(),
            expr:   TypeExpr::Variable("Int".to_string(), Vec::new()),
        })])
    ));
    assert_eq!(parser::defs("Foo{A!}:=(A!,[int:Int,float:Float]);"), Ok(
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
    assert_eq!(parser::defs("foo:=bar+false;"), Ok(
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
