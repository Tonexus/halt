#![allow(dead_code)]

mod ast;
mod error;
mod misc;
mod parser;
//mod type_checker;

use std::{env, fs::File, io::Read};

use anyhow::{Context, Result};

use error::CompileError;

fn main() -> Result<()> {
    // open argument as file
    let args: Vec<String> = env::args().collect();
    let mut file = File::open(args[1].clone()).context("Failed to open file")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents).context("Failed to read file")?;

    let defs = parser::defs(&contents).map_err(|e| CompileError::from(e))?;
    //type_checker::check_defs(defs)?;
    println!("Program ok!");
    return Ok(());
}

