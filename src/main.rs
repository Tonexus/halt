#![allow(dead_code)]

mod ast;
mod error;
mod misc;
mod parser;
mod type_checker;

use std::{env, fs::File, io::Read};

use regex::Regex;
use anyhow::{Context, Result};

use error::CompileError;

fn main() -> Result<()> {
    // open argument as file
    let args: Vec<String> = env::args().collect();
    let mut file = File::open(args[1].clone()).context("Failed to open file")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents).context("Failed to read file")?;

    // remove comments
    let comment_remover = Regex::new("//.*\n").context("Bad regex")?;
    let contents = comment_remover.replace_all(&contents, "");

    // remove excess whitespace
    let whitespace_remover = Regex::new("\\p{White_Space}+").context("Bad regex")?;
    let contents = whitespace_remover.replace_all(&contents, " ");

    let defs = parser::defs(&contents).map_err(|e| CompileError::from(e))?;
    return Ok(type_checker::check_defs(defs)?);
    /*dbg!(&out);
    if let Ok(defs) = out {
            println!("Parse success");
            return type_checker::check_defs(defs);
    } else {
        println!("{}", &contents);
        println!("Parse failed");
        return Ok(());
    }*/
}

