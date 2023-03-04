#[derive(Debug, PartialEq)]
pub enum Definition {
    Type(TypeVar),
    Value(ValueVar),
}

#[derive(Debug, PartialEq)]
pub struct TypeVar {
    name:   String,
    params: Vec<String>,
    expr:   TypeExpr,
}

#[derive(Debug, PartialEq)]
pub struct ValueVar {
    name:        String,
    type_expr:   Option<TypeExpr>,
    type_params: Vec<String>,
    const_expr:  Option<ValueExpr>,
}

#[derive(Debug, PartialEq)]
pub enum TypeExpr {
    Named(String),
    Universal(String),
    Existential(String),
    Generic((String, Vec<TypeExpr>)),
    Tuple(Vec<TypeExpr>),
    Struct(Vec<(String, TypeExpr)>),
    Choice(Vec<TypeExpr>),
    Tagged(Vec<(String, TypeExpr)>),
    Function(Box<(TypeExpr, TypeExpr)>),
}

#[derive(Debug, PartialEq)]
pub struct ValueExpr {
    variant:   ExpressionVariant,
    type_expr: Option<TypeExpr>,
}


#[derive(Debug, PartialEq)]
pub enum LiteralExpression {
    Unit,
    Boolean,
    Integer,
    Float,
}

#[derive(Debug, PartialEq)]
pub struct CallExpression {
    name:     String,
    argument: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub enum ExpressionVariant { // TODO flesh these out
    Literal(LiteralExpression),
    Variable(String),
    Arithmetic,
    Call(CallExpression),
}


#[derive(Debug, PartialEq)]
pub struct Expression {
    variant:  ExpressionVariant,
    exp_type: Option<TypeExpr> // TODO: one day, generics...
}

#[derive(Debug, PartialEq)]
pub enum Statement { // TODO flesh these out
    Let,
    Assignment,
    Call, // TODO in AST check call must have at least one mutable argument
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
        rule bool_true() -> bool
            = b: "true" {true}
        rule bool_false() -> bool
            = b: "false" {false}
        pub rule bool_value() -> bool
            = bool_true() / bool_false()

        // ****************************
        // RULES FOR USER DEFINED NAMES
        // ****************************

        // user defined variable names
        rule var_name() -> String // TODO later check for leading _ in var name
            = n: (['a'..='z' | '0'..='9' | '_']+) {n.into_iter().collect()}
        // user defined label names for product fields or sum tags
        rule label_name() -> String
            = n: (['a'..='z' | '0'..='9' | '_']+) {n.into_iter().collect()}
        // user defined type names
        rule type_name() -> String
            = n: (['A'..='Z' | 'a'..='z' | '0'..='9']+) {n.into_iter().collect()}

        // **************************
        // RULES FOR TYPE EXPRESSIONS
        // **************************

        // type expressions
        rule type_expr() -> TypeExpr = precedence!{
            // function type
            t1: (@) "->" t2: @ {TypeExpr::Function(Box::new((t1, t2)))}
            --
            t: nonfunc_type() {t}
        }
        // any non-function base type (to avoid recursion)
        rule nonfunc_type() -> TypeExpr
            = univ_type() / exis_type() / generic_type() / named_type()
            / tuple_type() / struct_type() / choice_type() / tagged_type()
        // label type pair
        rule label_type_pair() -> (String, TypeExpr)
            = n: label_name() ":" t: type_expr() {(n, t)}
        // named type
        rule named_type() -> TypeExpr
            = n: type_name() {TypeExpr::Named(n)}
        // universal type variable
        rule univ_type() -> TypeExpr
            = n: type_name() "!" {TypeExpr::Universal(n)}
        // existential type variable
        rule exis_type() -> TypeExpr
            = n: type_name() "?" {TypeExpr::Existential(n)}
        // generic named type
        rule generic_type() -> TypeExpr
            = n: type_name() "{" l: (type_expr() ** ",") "}" {
                TypeExpr::Generic((n, l))
            }
        // tuple type (product with implcit field names)
        rule tuple_type() -> TypeExpr
            = "(" t: (type_expr() ** ",") ")" {
                TypeExpr::Tuple(t)
            }
        // struct type (product with explicit field names)
        rule struct_type() -> TypeExpr
            = "(" l: (label_type_pair() ++ ",") ")" {
                TypeExpr::Struct(l)
            }
        // choice type (sum with implcit tag names)
        rule choice_type() -> TypeExpr
            = "[" t: (type_expr() ** ",") "]" {
                TypeExpr::Choice(t)
            }
        // tagged type (product with explicit tag names)
        rule tagged_type() -> TypeExpr
            = "[" l: (label_type_pair() ++ ",") "]" {
                TypeExpr::Struct(l)
            }

        // *********************
        // RULES FOR EXPRESSIONS
        // *********************

        rule unit_exp() -> Expression
            = "()" {
                Expression{
                    variant:  ExpressionVariant::Literal(LiteralExpression::Unit),
                    exp_type: Some(TypeExpr::Tuple(Vec::new())),
                }
            }
        rule literal_exp() -> Expression // TODO other literals, boolean, integers
            = unit_exp()
        rule var_exp() -> Expression
            = n: var_name() {
                Expression{
                    variant:  ExpressionVariant::Variable(n),
                    exp_type: None,
                }
            }
        rule basic_call_exp() -> Expression
            = n: var_name() " "*<,1> e: exp_specifier() {
                Expression{
                    variant:  ExpressionVariant::Call(
                        CallExpression{name: n, argument: Box::new(e)}
                    ),
                    exp_type: None,
                }
            }
        /* TODO UFCS
        rule ufcs_call_exp() -> Expression
            = e1: exp_specifier "." n: var_name() e2: tuple_exp() {
                
            }
        */
        rule call_exp() -> Expression
            = basic_call_exp()
        rule exp_specifier() -> Expression
            = literal_exp() / var_exp() / call_exp()

        // ********************
        // RULES FOR STATEMENTS
        // ********************

        rule let_stmt() -> Statement
            = "let " n: var_name() ":" t: type_expr() "=" e: exp_specifier() ";" {
                Statement::Let
            }
        rule call_stmt() -> Statement // TODO should be function call rather than just an expression
            = e: call_exp() ";" {Statement::Call}
        rule scope() -> Vec<Statement>
            = "{" s: (stmt_specifier() *) "}" {s}
        rule stmt_specifier() -> Statement
            = let_stmt() / call_stmt()

        // *******************************
        // RULES FOR TOP-LEVEL DEFINITIONS
        // *******************************

        // one parameter in a parameter list
        rule param() -> (String, TypeExpr)
            = n: var_name() ":" t: type_expr() {(n, t)}
        // parameter list for function definition
        rule params() -> Vec<(String, TypeExpr)>
            = "(" p: (param() ** ",") ")" {p}

        pub rule function() -> Function
            = "fn " name: var_name() ":" params: params() "->" returns: type_expr() ":=" body: scope() {
                Function{name, params, returns, body}
            }
    }
}



fn main() {
    assert_eq!(parser::bool_value("false"), Ok(false));
    assert_eq!(
        parser::function("fn foo:(a:Int)->():={}"),
        Ok(Function{
            name: "foo".to_string(),
            params: Vec::from([("a".to_string(), TypeExpr::Named("Int".to_string()))]),
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
                TypeExpr::Named("Int".to_string()),
                TypeExpr::Named("Int".to_string()),
            ])),
            body: Vec::from([Statement::Let]),
        })
    );
}
