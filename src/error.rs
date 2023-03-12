// error type

use thiserror::Error;


#[derive(Error, Debug)]
pub enum CompileError {
    #[error(
        "Parsing failed at {} (around \"{}\"). Expected {}",
        .source.location,
        .source.location, // TODO actually print the line
        .source.expected,
    )]
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
    #[error("Type {0} cannot be recursively defined.")]
    RecurDef(String),
    // TODO in which def
    #[error("Type {name} has an inconsistent number of type parameters.")]
    InconsistParams {name: String},
    // TODO type name, in which def
    #[error("Type has too many parameters.")]
    TooManyParams,
    #[error("You are bad and should feel bad.")]
    DefaultErr,
}

#[derive(Error, Debug)]
pub enum ValueError {
    #[error("Const {0} has multiple definitions.")]
    MultiDef(String),
}
