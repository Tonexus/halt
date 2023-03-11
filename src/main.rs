#![allow(dead_code)]

mod ast;
mod misc;
mod parser;
mod type_checker;

use std::{env, fs::File, io::Read};
use regex::Regex;

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

    let out = parser::defs(&contents);
    dbg!(&out);
    if let Ok(defs) = out {
            println!("Parse success");
            return type_checker::check_defs(defs);
    } else {
        println!("{}", &contents);
        println!("Parse failed");
        return Ok(());
    }
}

