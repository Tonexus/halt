// error type

use thiserror::Error;


#[derive(Error, Debug)]
pub enum CompileError {
    #[error("Parsing failed at {}. Expected {}", .source.location, .source.expected)]
    Parse {
        #[from]
        source: peg::error::ParseError<peg::str::LineCol>,
    },
    #[error(transparent)]
    Type(#[from] TypeError),
    #[error(transparent)]
    Value(#[from] ValueError),
    #[error("You are bad and should feel bad.")]
    DefaultErr,
}

#[derive(Error, Debug)]
pub enum TypeError {
    #[error("Type {0} has multiple definitions.")]
    MultiDef(String),
    #[error("Type {0} is undefined.")]
    Undef(String),
    #[error("Type {0} is recursively defined.")]
    RecurDef(String),
    #[error("Type {check} has an inconsistent number of type parameters ({m}, {n}) in the definition of {main}.")]
    InconsistParams {
        main:  String,
        check: String,
        m:     i32,
        n:     i32,
    },
    #[error("You are bad and should feel bad.")]
    DefaultErr,
}

#[derive(Error, Debug)]
pub enum ValueError {
    #[error("Const {0} has multiple definitions.")]
    MultiDef(String),
}
