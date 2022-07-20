
#[derive(Debug, PartialEq)]
pub enum PrimitiveType {
    Unit,
}

#[derive(Debug, PartialEq)]
pub enum StructuralType {
    Product(Vec<StructuralType>), // TODO add labels/field names
    Sum(Vec<StructuralType>),     // TODO add labels/variant names
    Primitive(PrimitiveType),
    Named(String),
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
    exp_type: Option<StructuralType> // TODO: one day, generics...
}

#[derive(Debug, PartialEq)]
pub enum Statement { // TODO flesh these out
    Let,
    Assignment,
    Call,
    Return,
    Condition,
    Loop,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    name:    String,
    params:  Vec<(String, StructuralType)>,
    returns: StructuralType,
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
        // user defined type names
        rule type_name() -> String
            = n: (['A'..='Z' | 'a'..='z' | '0'..='9']+) {n.into_iter().collect()}

        // ******************************
        // RULES FOR DATA TYPE SPECIFIERS
        // ******************************

        rule unit_type() -> StructuralType
            = "()" {StructuralType::Primitive(PrimitiveType::Unit)}
        rule prim_type() -> StructuralType // TODO other primitive types
            = unit_type()
        rule named_type() -> StructuralType
            = t: type_name() {StructuralType::Named(t)}
        // tuple type specifiers
        rule tuple_type() -> StructuralType
            = "(" t: (type_specifier() ** ",") ")" {
                StructuralType::Product(t)
            }
        // type specifiers, including anonymous structural types
        rule type_specifier() -> StructuralType
            = prim_type() / named_type() / tuple_type() // TODO add other structural types

        // *********************
        // RULES FOR EXPRESSIONS
        // *********************

        rule unit_exp() -> Expression
            = "()" {
                Expression{
                    variant:  ExpressionVariant::Literal(LiteralExpression::Unit),
                    exp_type: Some(StructuralType::Primitive(PrimitiveType::Unit)),
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
            = "let " n: var_name() ":" t: type_specifier() "=" e: exp_specifier() ";" {
                Statement::Let
            }
        rule call_stmt() -> Statement // TODO should be function call rather than just an expression
            = e: call_exp() ";" {Statement::Call}
        rule scope() -> Vec<Statement>
            = "{" s: (stmt_specifier() *) "}" {s}
        rule stmt_specifier() -> Statement
            = let_stmt() / call_stmt()

        // ******************************
        // RULES FOR FUNCTION DEFINITIONS
        // ******************************

        // one parameter in a parameter list
        rule param() -> (String, StructuralType)
            = n: var_name() ":" t: type_specifier() {(n, t)}
        // parameter list for function definition
        rule params() -> Vec<(String, StructuralType)>
            = "(" p: (param() ** ",") ")" {p}

        pub rule function() -> Function
            = "fn " name: var_name() ":" params: params() "->" returns: type_specifier() ":=" body: scope() {
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
            params: Vec::from([("a".to_string(), StructuralType::Named("Int".to_string()))]),
            returns: StructuralType::Primitive(PrimitiveType::Unit),
            body: Vec::new(),
        })
    );
    assert_eq!(
        parser::function("fn bar:()->(Int,Int):={let m:()=();}"),
        Ok(Function{
            name: "bar".to_string(),
            params: Vec::new(),
            returns: StructuralType::Product(Vec::from([
                StructuralType::Named("Int".to_string()),
                StructuralType::Named("Int".to_string()),
            ])),
            body: Vec::from([Statement::Let]),
        })
    );
}
