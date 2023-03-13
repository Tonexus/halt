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
    #[error("Kind {0} is an invalid kind.")]
    BadKind(String),
    #[error("Composite type {0} must have kind \"Type\".")]
    MustNullKind(String),
    // TODO in which def
    #[error("Kind of type parameter {0} does not match its usage.")]
    KindMismatch(String),
    // TODO type name, in which def
    #[error("Type {0} has too many parameters.")]
    TooManyParams(String),
    #[error("You are bad and should feel bad.")]
    DefaultErr,
}

#[derive(Error, Debug)]
pub enum ValueError {
    #[error("Const {0} has multiple definitions.")]
    MultiDef(String),
}
