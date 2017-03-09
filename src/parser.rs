use std::process;

use lexer::Token;
use lexer::LexerState;
use lexer::get_token;

use log;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CC {
    // condition codes
    E, L, LE, G, GE,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum SExpr {
    Symbol(String),
    Number(i32),
    Bool(bool),
    List(Vec<SExpr>),

    Define(String, Vec<String>, Box<SExpr>),
    Let(Vec<(String, SExpr)>, Box<SExpr>),
    If(Box<SExpr>, Box<SExpr>, Box<SExpr>),
    Cmp(CC, Box<SExpr>, Box<SExpr>),
    App(Box<SExpr>, Vec<SExpr>),
    Prog(Vec<SExpr>, Box<SExpr>),
    EOF,
}


fn unread(ls: &mut LexerState, tok: Token) {
    if let Some(_) = ls.tok_buf {
        println!("error: unread buffer full");
    }
    else {
        ls.tok_buf = Some(tok)
    }
}



fn get_list(ls: &mut LexerState) -> Vec<SExpr> {
    match get_expr(ls) {
        exp => match get_token(ls) {
            Token::RParen => return vec![exp],
            tok => {
                unread(ls, tok);
                let mut seq = get_list(ls);
                seq.insert(0, exp);
                return seq;
            },
        },
    }
}

pub fn get_expr(ls: &mut LexerState) -> SExpr {
    match get_token(ls) {
        Token::Symbol(s) => return SExpr::Symbol(s),
        Token::Number(n) => return SExpr::Number(n),
        Token::LParen => {
            return SExpr::List(get_list(ls));
        },
        Token::RParen => panic!("line {}:{} unmatched ')'",
                                ls.line_num, ls.col),
        Token::EOF => return SExpr::EOF,
    }
}

pub fn get_arg_names(args: &Vec<SExpr>) -> Vec<String> {
    let mut arg_names = vec![];
    for arg in args {
        match arg {
            &SExpr::Symbol(ref name) => arg_names.push(name.clone()),
            _ => {
                error!("arg should be a Symbol");
                process::exit(0);
            }
        }
    }

    return arg_names;
}

pub fn get_ast(expr: &SExpr) -> SExpr {
    match expr {
        &SExpr::Symbol(ref sym) => {
            match &sym[..] {
                "#f" => SExpr::Bool(false),
                "#t" => SExpr::Bool(true),
                _ => SExpr::Symbol(sym.clone()),
            }
        },
        &SExpr::List(ref elts) =>
            match &elts[..] {
                &[SExpr::Symbol(ref k), SExpr::List(ref defelts), ref body] 
                    if k == "define" => {
                    let ref name = defelts[0];
                    let args = defelts[1..].to_vec();
                    
                    match name {
                        &SExpr::Symbol(ref name) => {
                            return SExpr::Define(name.clone(), get_arg_names(&args.to_vec()),
                                                 Box::new(get_ast(body)));
                        },
                        _ => panic!("invalid function prototype"),
                    }
                },
                &[SExpr::Symbol(ref k), ref cnd, ref thn, ref els] 
                    if k == "if" => {
                    return SExpr::If(Box::new(get_ast(cnd)),
                                     Box::new(get_ast(thn)),
                                     Box::new(get_ast(els)));
                    },
                &[SExpr::Symbol(ref k), SExpr::List(ref bindings), ref body]
                    if k == "let" => {
                        let mut astified_bindings = vec![];
                        for bind_pair in bindings {
                            let (key, val) = match bind_pair {
                                // TODO: check length
                                &SExpr::List(ref kv) => (kv[0].clone(), kv[1].clone()),
                                _ => panic!("non-list in let-binding"),
                            };

                            let keyname = match key {
                                SExpr::Symbol(k) => k,
                                _ => panic!("let binding key is not symbol"),
                            };
                            astified_bindings.push((keyname, get_ast(&val)));
                        }
                        return SExpr::Let(astified_bindings, Box::new(get_ast(&body)));  
                    },
                &[SExpr::Symbol(ref cmp), ref left, ref right]
                    if (cmp == ">" || cmp == "<" || 
                        cmp == "<=" || cmp == ">=" ||
                        cmp == "=") => {
                        let cc = match &cmp[..] {
                            ">" => CC::G,
                            "<" => CC::L,
                            ">=" => CC::GE,
                            "<=" => CC::LE,
                            "=" => CC::E,
                            &_ => panic!("NYI"),
                        };

                        return SExpr::Cmp(cc, box left.clone(), box right.clone());
                },
                &[ref f, _..] => {
                    let mut astified_args = vec![];
                    for arg in elts[1..].to_vec() {
                        astified_args.push(get_ast(&arg));
                    }

                    return SExpr::App(Box::new(f.clone()), astified_args);
                },
                &_ => panic!("NYI: {:?}", elts),
            }
        ,
        &_ => expr.clone(),
    }
}

pub fn read(ls: &mut LexerState) -> SExpr {
    return get_ast(&get_expr(ls));
}

#[test]
fn test_parser() {
    let mut input = String::from("(if #f (+ 42 (foo 12)) 17) 
                                  (define (foo x y z) (+ x 10))
                                  (+ 1 2)");
    let mut lexer = LexerState {
        s: input,
        pos: 0,
        col: 1,
        line_num: 1,
        tok_buf: None,
    };
    assert_eq!(SExpr::If(Box::new(SExpr::Bool(false)),
                         Box::new(SExpr::App(Box::new(SExpr::Symbol("+".to_string())),
                                             vec![SExpr::Number(42),
                                                  SExpr::App(Box::new(SExpr::Symbol("foo".to_string())),
                                                             vec![SExpr::Number(12)])])),
                         Box::new(SExpr::Number(17))),
               read(&mut lexer));

    // Second top-level s-expression
    assert_eq!(SExpr::Define("foo".to_string(), vec!["x".to_string(), "y".to_string(), "z".to_string()], 
                             Box::new(SExpr::App(Box::new(SExpr::Symbol("+".to_string())), 
                                        vec![SExpr::Symbol("x".to_string()), SExpr::Number(10)]))),
               read(&mut lexer));

    // Third top-level s-expression
    assert_eq!(SExpr::App(Box::new(SExpr::Symbol("+".to_string())),
                          vec![SExpr::Number(1), 
                               SExpr::Number(2)]),
               read(&mut lexer));
    // nothing left in string
    assert_eq!(SExpr::EOF, read(&mut lexer));
}
