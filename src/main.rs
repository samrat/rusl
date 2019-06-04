#![feature(slice_patterns)]
#![feature(box_syntax, box_patterns)]

use std::io;
use std::collections::HashMap;
use std::collections::HashSet;
use std::io::prelude::*;
use std::fs::File;
use std::env;
use std::process;
use std::rc::Rc;

#[macro_use]
extern crate log;

mod util;
mod lexer;
mod ast;
mod parser;
// mod anf;

use parser::Parser;
use ast::Ast;

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

    let mut uniquify_mapping : HashMap<Rc<String>, Rc<String>> = HashMap::new();
    for prim in ["+", "-", "tuple-ref", "tuple"].iter() {
        uniquify_mapping.insert(Rc::new(prim.to_string()), Rc::new(prim.to_string()));
    }

    let uniquified = Ast::Prog(toplevel[..toplevel.len()-1].to_vec(),
                       Box::new(toplevel[toplevel.len()-1].clone()))
             .uniquify(&mut uniquify_mapping);
    println!("Uniquify: {:?}",
             uniquified);

    let (closures_converted, _) = uniquified
        .convert_to_closures(&HashSet::new(),
                             &HashSet::new());

    println!("Closures converted {:?}",
             closures_converted);

    println!("{:?}", toplevel);
}
