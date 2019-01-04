#![feature(slice_patterns)]
#![feature(box_syntax, box_patterns)]

use std::io;
use std::collections::HashMap;
use std::collections::HashSet;
use std::io::prelude::*;
use std::fs::File;
use std::env;
use std::process;

#[macro_use]
extern crate log;

mod util;
mod lexer;
mod parser;
// mod anf;

use util::get_unique_varname;

use parser::{Parser};

fn read_input(filename: &str, mut input_buffer: &mut String)
              -> io::Result<()> {
    let mut f = try!(File::open(filename));
    try!(f.read_to_string(&mut input_buffer));
    Ok(())
}
pub fn main() {
    let args : Vec<_> = env::args().collect();
    if args.len() < 2 {
        panic!("usage: {} filename", args[0].clone());
    }

    let mut input = String::new();

    let _ = read_input(&args[1], &mut input);
    let parser = Parser::new(&input);
    let toplevel : Vec<_> = parser.collect();

    println!("{:?}", toplevel);
}
