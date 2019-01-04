#![feature(slice_patterns)]
mod lexer;
use lexer::{Token, Lexer};

#[derive(Debug, PartialEq, Clone)]
pub enum CC {
    // condition codes
    E, L, LE, G, GE,
}

#[derive(Debug, Clone)]
pub enum Ast<'input> {
    Symbol(&'input str),
    Number(i64),
    Bool(bool),
    List(Vec<Ast<'input>>),
    FuncName(&'input str),  // for closure-conversion

    Define(&'input str, Vec<&'input str>, Box<Ast<'input>>),
    If(Box<Ast<'input>>,     // cnd
       Box<Ast<'input>>,     // thn
       Box<Ast<'input>>),    // els
    Let(Vec<(&'input str, Ast<'input>)>, Box<Ast<'input>>),
    Lambda(Vec<&'input str>, Box<Ast<'input>>),
    Tuple(Vec<Ast<'input>>),
    Cmp(CC, Box<Ast<'input>>, Box<Ast<'input>>),
    App(Box<Ast<'input>>, Vec<Ast<'input>>)
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

    pub fn get_list(&mut self) -> Vec<Ast<'input>> {
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

    pub fn get_expr(&mut self) -> Option<Ast<'input>> {
        match self.lexer.next() {
            Some(Token::Symbol(s)) => Some(Ast::Symbol(s)),
            Some(Token::Number(n)) => Some(Ast::Number(n)),
            Some(Token::LParen) => Some(Ast::List(self.get_list())),
            Some(Token::RParen) => panic!("line {}: {} unmatched ')'",
                                          self.lexer.line_num,
                                          self.lexer.col),
            _ => None,
        }
    }

    pub fn get_arg_names(args: &[Ast<'input>]) -> Vec<&'input str> {
        let arg_names : Vec<&'input str> = args.iter().map(|arg| match arg {
            Ast::Symbol(name) => *name,
            _ => panic!("invalid arg"),
        }).collect();

        arg_names
    }

    pub fn get_ast(expr: &Ast<'input>) -> Ast<'input> {
        match expr {
            &Ast::Symbol(sym) =>
                match &sym[..] {
                    "#f" => Ast::Bool(false),
                    "#t" => Ast::Bool(true),
                    _ => Ast::Symbol(sym)
                },
            &Ast::List(ref elts) =>
                match &elts[..] {
                    &[Ast::Symbol(k), Ast::List(ref defelts), ref body]
                        if k == "define" => {
                            let name = &defelts[0];
                            let args = &defelts[1..];

                            if let Ast::Symbol(name) = name {
                                Ast::Define(name,
                                            Parser::get_arg_names(args),
                                            Box::new(Parser::get_ast(&body)))
                            } else {
                                panic!("invalid function prototype");
                            }
                        },
                    &[Ast::Symbol(k), ref cnd, ref thn, ref els]
                        if k == "if" => {
                            Ast::If(Box::new(Parser::get_ast(cnd)),
                                    Box::new(Parser::get_ast(thn)),
                                    Box::new(Parser::get_ast(els)))
                        },
                    &[Ast::Symbol(k), Ast::List(ref bindings), ref body]
                        if k == "let" => {
                            let mut astified_bindings = vec![];
                            for bind_pair in bindings {
                                if let Ast::List(kv) = bind_pair {
                                    let (key, val) =
                                        if kv.len() == 2 {
                                            let astified_val = Parser::get_ast(&kv[1]);
                                            let keyname =
                                                if let Ast::Symbol(k) = kv[0] {
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
                                     Box::new(Parser::get_ast(&body)))
                        },
                    &[Ast::Symbol(k), Ast::List(ref args), ref body]
                        if k == "lambda" => {
                            Ast::Lambda(Parser::get_arg_names(args),
                                        Box::new(Parser::get_ast(body)))
                        },
                    &[Ast::Symbol(k), _..]
                        if k == "tuple" => {
                            let tuple_elts = elts[1..].iter()
                                .map(|e| Parser::get_ast(e))
                                .collect();
                            Ast::Tuple(tuple_elts)
                        },
                    &[Ast::Symbol(cmp), ref left, ref right]
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
                    &[ref f, _..] => {
                        let args = elts[1..].iter().map(|e| Parser::get_ast(e))
                            .collect();

                        Ast::App(Box::new(f.clone()), args)
                    }
                    _ => Ast::Number(42),
                },
            _ => expr.clone(),
        }
    }

    pub fn read(&mut self) -> Option<Ast<'input>> {
        match self.get_expr() {
            Some(expr) => Some(Parser::get_ast(&expr)),
            None => None,
        }
    }
}

impl<'input> Iterator for Parser<'input> {
    type Item = Ast<'input>;

    fn next(&mut self) -> Option<Ast<'input>> {
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


    let mut ps = Parser::new("(> 1 2) (+ 1 2)");
    for a in ps {
        println!("{:?}", a);
    }
}
