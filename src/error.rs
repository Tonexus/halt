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
    MultiTypeDef(String),
}

#[derive(Error, Debug)]
pub enum ValueError {
    #[error("Const {0} has multiple definitions.")]
    MultiConstDef(String),
}
