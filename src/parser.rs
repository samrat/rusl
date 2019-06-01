#![feature(bind_by_move_pattern_guards)]
// mod lexer;
use lexer::{Token, Lexer};

use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum CC {
    // condition codes
    E, L, LE, G, GE,
}

#[derive(Debug, Clone)]
pub enum Ast {
    Symbol(String),
    Number(i64),
    Bool(bool),
    List(Vec<Ast>),
    FuncName(String),  // for closure-conversion

    Define(String, Vec<String>, Box<Ast>),
    If(Box<Ast>,     // cnd
       Box<Ast>,     // thn
       Box<Ast>),    // els
    Let(Vec<(String, Ast)>, Box<Ast>),
    Lambda(Vec<String>, Box<Ast>),
    Tuple(Vec<Ast>),
    Cmp(CC, Box<Ast>, Box<Ast>),
    App(Box<Ast>, Vec<Ast>),
    Prog(Vec<Ast>, Box<Ast>),
    Nil,
}

static mut VAR_COUNTER : i32 = 0;
pub fn get_unique_varname(stem: &str) -> String {
    unsafe {
        VAR_COUNTER += 1;
        return stem.to_string() + &VAR_COUNTER.to_string();
    }
}


impl Ast {
    pub fn uniquify(&self, mapping: &mut HashMap<String, String>) -> Self {
        match self {
            Ast::Symbol(name) => {
                let uniq_name = mapping.get(&name[..]).expect("unbound symbol");
                Ast::Symbol(uniq_name.to_string())
            },
            Ast::Number(_) | Ast::Bool(_) => self.clone(),
            Ast::Tuple(elts) => {
                let elts =
                    elts.iter().map(|e| e.uniquify(mapping)).collect();
                Ast::Tuple(elts)
            },
            Ast::Let(bindings, body) => {
                let mut new_bindings = vec![];
                for (k, v) in bindings {
                    let uniq_k = get_unique_varname(&k).to_string();
                    mapping.insert(k.to_string(), uniq_k.clone());
                    new_bindings.push((uniq_k,
                                       v.uniquify(mapping)));
                }
                Ast::Let(new_bindings,
                         Box::new(body.uniquify(mapping)))
            },
            Ast::List(elts) => {
                let new_elts : Vec<_> = elts.iter().map(|e| e.uniquify(mapping)).collect();
                Ast::List(new_elts)
            },
            Ast::Cmp(cc, left, right) =>
                return Ast::Cmp(cc.clone(),
                                box left.uniquify(mapping),
                                box right.uniquify(mapping)),
            Ast::Lambda(args, body) => {
                let mut new_args = vec![];
                for arg in args {
                    let new_arg = get_unique_varname(&arg);
                    new_args.push(new_arg.clone());
                    mapping.insert(arg.to_string(), new_arg);
                }

                 Ast::Lambda(new_args,
                             box body.uniquify(mapping))
            },
            Ast::Define(name, args, val) => {
                let uniq_fname = get_unique_varname(&name);
                mapping.insert(name.to_string(), uniq_fname.clone());

                let mut new_args = vec![];
                for arg in args {
                    let new_arg = get_unique_varname(&arg);
                    new_args.push(new_arg.clone());
                    mapping.insert(arg.to_string(), new_arg);
                }

                Ast::Define(uniq_fname,
                            new_args,
                            box val.uniquify(mapping))
            },
            Ast::If(cond, thn, els) =>
                Ast::If(box (cond.uniquify(mapping)),
                        box (thn.uniquify(mapping)),
                        box (els.uniquify(mapping))),
            Ast::App(f, args) => {
                let new_args : Vec<_> = args.iter()
                    .map(|a| a.uniquify(mapping)).collect();
                Ast::App(box f.uniquify(mapping),
                         new_args)
            },
            Ast::Prog(defs, e) => {
                let new_defs : Vec<_> = defs.iter()
                    .map(|def| def.uniquify(mapping)).collect();
                Ast::Prog(new_defs, box e.uniquify(mapping))
            },
            _ => panic!("NYI"),
        }
    }
}

pub struct Parser<'input> {
    lexer: Lexer<'input>
}

impl<'input> Parser<'input> {
    pub fn new(source: &'input str) -> Parser<'input> {
        Parser {
            lexer: Lexer::new(source)
        }
    }

    pub fn get_list(&mut self) -> Vec<Ast> {
        match self.get_expr() {
            Some(expr) => match self.lexer.next() {
                None => vec![],
                Some(Token::RParen) => vec![expr],
                Some(tok) => {
                    self.lexer.unread(tok);
                    let mut seq = self.get_list();
                    seq.insert(0, expr);

                    return seq;
                }
            },
            None => vec![],
        }
    }

    pub fn get_expr(&mut self) -> Option<Ast> {
        match self.lexer.next() {
            Some(Token::Symbol(s)) => Some(Ast::Symbol(s.to_string())),
            Some(Token::Number(n)) => Some(Ast::Number(n)),
            Some(Token::LParen) => Some(Ast::List(self.get_list())),
            Some(Token::RParen) => panic!("line {}: {} unmatched ')'",
                                          self.lexer.line_num,
                                          self.lexer.col),
            _ => None,
        }
    }

    pub fn get_arg_names(args: &[Ast]) -> Vec<String> {
        let arg_names : Vec<String> =
            args.iter().map(|arg| match arg {
                Ast::Symbol(name) => name.to_string(),
                _ => panic!("invalid arg"),
            }).collect();

        arg_names
    }

    pub fn get_ast(expr: &Ast) -> Ast {
        match expr {
            Ast::Symbol(sym) =>
                match &sym[..] {
                    "#f" => Ast::Bool(false),
                    "#t" => Ast::Bool(true),
                    _ => Ast::Symbol(sym.to_string())
                },
            Ast::List(elts) =>
                match &elts[..] {
                    [Ast::Symbol(k), Ast::List(defelts), body]
                        if k == "define" => {
                            let name = &defelts[0];
                            let args = &defelts[1..];

                            if let Ast::Symbol(name) = name {
                                Ast::Define(name.to_string(),
                                            Parser::get_arg_names(args),
                                            Box::new(Parser::get_ast(body)))
                            } else {
                                panic!("invalid function prototype");
                            }
                        },
                    [Ast::Symbol(k), cnd, thn, els]
                        if k == "if" => {
                            Ast::If(Box::new(Parser::get_ast(cnd)),
                                    Box::new(Parser::get_ast(thn)),
                                    Box::new(Parser::get_ast(els)))
                        },
                    [Ast::Symbol(k), Ast::List(bindings), body]
                        if k == "let" => {
                            let mut astified_bindings = vec![];
                            for bind_pair in bindings {
                                if let Ast::List(kv) = bind_pair {
                                    let (key, val) =
                                        if kv.len() == 2 {
                                            let astified_val = Parser::get_ast(&kv[1]);
                                            let keyname =
                                                if let Ast::Symbol(k) = kv[0].clone() {
                                                    k
                                                } else {
                                                    panic!("let binding key is not symbol");
                                                };
                                            (keyname, astified_val)
                                        } else {
                                            panic!("invalid let binding syntax");
                                        };

                                    astified_bindings.push((key, val));
                                }
                            }

                            Ast::Let(astified_bindings,
                                     Box::new(Parser::get_ast(body)))
                        },
                    [Ast::Symbol(k), Ast::List(args), body]
                        if k == "lambda" => {
                            Ast::Lambda(Parser::get_arg_names(&args),
                                        Box::new(Parser::get_ast(body)))
                        },
                    [Ast::Symbol(k), _..]
                        if k == "tuple" => {
                            let tuple_elts = elts[1..].iter()
                            // TODO: move?
                                .map(|e| Parser::get_ast(e))
                                .collect();
                            Ast::Tuple(tuple_elts)
                        },
                    [Ast::Symbol(cmp), left, right]
                        if (cmp == ">" || cmp == "<" ||
                            cmp == "<=" || cmp == ">=" ||
                            cmp == "=") => {
                            let cc = match &cmp[..] {
                                ">" => CC::G,
                                "<" => CC::L,
                                ">=" => CC::GE,
                                "<=" => CC::LE,
                                "=" => CC::E,
                                &_ => panic!("invalid cmp op"),
                            };

                            Ast::Cmp(cc,
                                     Box::new(Parser::get_ast(left)),
                                     Box::new(Parser::get_ast(right)))
                        },
                    [f, _..] => {
                        let args = elts[1..].iter().map(|e| Parser::get_ast(e)) // TODO: move
                            .collect();

                        Ast::App(Box::new(f.clone()), args)
                    }
                    _ => Ast::Nil,
                },
            _ => expr.clone(),
        }
    }

    pub fn read(&mut self) -> Option<Ast> {
        match self.get_expr() {
            Some(expr) => Some(Parser::get_ast(&expr)),
            None => None,
        }
    }
}

impl<'input> Iterator for Parser<'input> {
    type Item = Ast;

    fn next(&mut self) -> Option<Ast> {
        self.read()
    }
}

fn main() {
    let mut p = Parser::new("(if #t 42 (define (foo x y) (+ (* 1 2) 2)))");
    println!("{:?}", Parser::get_ast(&p.get_expr().unwrap()));

    let mut p2 = Parser::new("(let ((x 10) (y 2)) (+ x y))");
    println!("{:?}", Parser::get_ast(&p2.get_expr().unwrap()));

    let mut p3 = Parser::new("(lambda (x y) (+ x y))");
    println!("{:?}", Parser::get_ast(&p3.get_expr().unwrap()));

    let mut p4 = Parser::new("(tuple 1 2 3)");
    println!("{:?}", Parser::get_ast(&p4.get_expr().unwrap()));

    let mut p5 = Parser::new("(> 1 2)");
    println!("{:?}", Parser::get_ast(&p5.get_expr().unwrap()));

    let mut p6 = Parser::new("(+ 1 2)");
    println!("{:?}", Parser::get_ast(&p6.get_expr().unwrap()));


    let ps = Parser::new("(> 1 2) (+ 1 2)");
    for a in ps {
        println!("{:?}", a);
    }
}
