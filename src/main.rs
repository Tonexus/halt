#![allow(dead_code)]

use std::{env, fs::File, io::Read};

mod ast;
mod parser;

use ast::{
    Definition, TypeDef, ConstDef, TypeExpr, ValueExpr, ExprVariant, LitVariant,
    BinOpVariant, UnOpVariant, Closure, Statement,
};

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let mut file = File::open(args[1].clone())?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    dbg!(contents);
    Ok(())
}

