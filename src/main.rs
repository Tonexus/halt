#![allow(dead_code)]

use std::{env, fs::File, io::Read};
use regex::Regex;

mod ast;
mod parser;

fn main() -> std::io::Result<()> {
    // open argument as file
    let args: Vec<String> = env::args().collect();
    let mut file = File::open(args[1].clone())?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    // remove comments
    let comment_remover = Regex::new("//.*\n").unwrap();
    let contents = comment_remover.replace_all(&contents, "");
    dbg!(&contents);

    // remove excess whitespace
    let whitespace_remover = Regex::new("\\p{White_Space}+").unwrap();
    let contents = whitespace_remover.replace_all(&contents, " ");
    dbg!(&contents);

    let out = parser::program_parser::defs(&contents);
    assert!(out.is_ok());
    dbg!("Parse success");
    Ok(())
}

