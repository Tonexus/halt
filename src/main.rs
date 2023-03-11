#![allow(dead_code)]

use std::{env, fs::File, io::Read};
use regex::Regex;

mod ast;
mod parser;
mod type_check;

fn main() -> Result<(), &'static str> {
    // open argument as file
    let args: Vec<String> = env::args().collect();
    let mut file = File::open(args[1].clone()).map_err(|_| "io error")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents).map_err(|_| "io error")?;

    // remove comments
    let comment_remover = Regex::new("//.*\n").unwrap();
    let contents = comment_remover.replace_all(&contents, "");

    // remove excess whitespace
    let whitespace_remover = Regex::new("\\p{White_Space}+").unwrap();
    let contents = whitespace_remover.replace_all(&contents, " ");

    let out = parser::program_parser::defs(&contents);
    dbg!(&out);
    if let Ok(defs) = out {
            println!("Parse success");
            return type_check::type_check(defs);
    } else {
        println!("{}", &contents);
        println!("Parse failed");
        return Ok(());
    }
}

