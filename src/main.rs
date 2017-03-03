use std::io;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
enum Token {
    LParen,
    RParen,
    Symbol(String),
    Number(i32),
    EOF,
}

pub struct LexerState {
    s: String,
    pos: usize,
    tok_buf: Option<Token>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum SExpr {
    Symbol(String),
    Number(i32),
    Bool(bool),
    List(Vec<SExpr>),

    // TODO: change define to be special-form for function definition
    Define(String, Box<SExpr>),
    Let(Vec<(String, SExpr)>, Box<SExpr>),
    If(Box<SExpr>, Box<SExpr>, Box<SExpr>),
    App(Box<SExpr>, Vec<SExpr>),
}

fn unread(ls: &mut LexerState, tok: Token) {
    if let Some(_) = ls.tok_buf {
        println!("error: unread buffer full");
    }
    else {
        ls.tok_buf = Some(tok)
    }
}

fn is_valid_symbol_start(c: char) -> bool {
    // TODO: avoid allocatiing this in each call
    let SymbolStartChars = vec!['+', '-', '*', '/', '#'];

    let mut ret = false;
    if c.is_alphabetic() { ret = true; }
    else {
        for s in SymbolStartChars {
            if c == s { ret = true; break; }
            else { continue; }
        }
    }

    return ret;
}

fn get_token(ls: &mut LexerState) -> Token {
    if let Some(tok) = ls.tok_buf.clone() {
        ls.tok_buf = None;
        return tok;
    }
    else {
        let mut iter = ls.s[ls.pos..].chars().peekable();
        while let Some(&c) = iter.peek() {
            if c.is_numeric() {
                let mut acc = String::new();
                let mut n = c;
                while n.is_numeric() {
                    acc.push(n);
                    iter.next();
                    ls.pos += 1;
                    n = match iter.peek() {
                        Some(&x) => x,
                        None => break,
                    };
                }
                return Token::Number(acc.parse().unwrap());
            }
            else if is_valid_symbol_start(c) {
                let mut acc = String::new();
                let mut s = c;
                while (s.is_alphanumeric() || 
                       is_valid_symbol_start(s)) {
                    acc.push(s);
                    iter.next();
                    ls.pos += 1;
                    s = match iter.peek() {
                        Some(&x) => x,
                        None => break,
                    };
                }
                return Token::Symbol(acc);
            }
            else {
                match c {
                    ' ' => {
                        iter.next();
                        ls.pos += 1;
                        continue
                    },
                    '(' => {
                        iter.next();
                        ls.pos += 1;
                        return Token::LParen
                    },
                    ')' => {
                        iter.next();
                        ls.pos += 1;
                        return Token::RParen
                    },
                    _ => panic!("unexpected char: {}", c),
                }
            }
        }
        return Token::EOF;
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

fn get_expr(ls: &mut LexerState) -> SExpr {
    match get_token(ls) {
        Token::Symbol(s) => return SExpr::Symbol(s),
        Token::Number(n) => return SExpr::Number(n),
        Token::LParen => {
            return SExpr::List(get_list(ls));
        },
        Token::RParen => panic!("unmatched ')'"),
        _ => return SExpr::Number(32),
    }
}

fn get_ast(expr: SExpr) -> SExpr {
    match expr {
        SExpr::Symbol(sym) => {
            match &sym[..] {
                "#f" => SExpr::Bool(false),
                "#t" => SExpr::Bool(true),
                _ => SExpr::Symbol(sym),
            }
        },
        SExpr::List(elts) =>
            match elts[0].clone() {
                SExpr::Symbol(sym) => {
                    match &sym[..] {
                        "define" => {
                            // TODO: Check that elts has correct length
                            if let SExpr::Symbol(name) = elts[1].clone() {
                                let e = elts[2].clone();
                                return SExpr::Define(name, Box::new(e));
                            }
                            else {
                                panic!("expected `name` to be a symbol");
                            }
                        },
                        "if" => {
                            return SExpr::If(Box::new(get_ast(elts[1].clone())),
                                             Box::new(get_ast(elts[2].clone())),
                                             Box::new(get_ast(elts[3].clone())));
                        },
                        "let" => {
                            let bindings = match elts[1] {
                                SExpr::List(ref bs) => bs,
                                _ => panic!("let bindings"),
                            };

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
                                astified_bindings.push((keyname, get_ast(val)));
                            }
                            return SExpr::Let(astified_bindings, Box::new(get_ast(elts[2].clone())));
                        },
                        _ => SExpr::App(Box::new(elts[0].clone()), elts[1..].to_vec()),
                    }
                }
                _ => panic!("NYI"),
            },
        _ => expr,
    }
}

fn read_input() -> io::Result<()> {
    let mut input = String::new();

    try!(io::stdin().read_line(&mut input));

    let mut lexer = LexerState {
        s: input,
        pos: 0,
        tok_buf: None,
    };

    let mut uniquify_mapping = HashMap::new();

    println!("{:?}", uniquify(&mut uniquify_mapping,
                              get_ast(get_expr(&mut lexer))));

    Ok(())
}

#[test]
fn test_parser() {
    let mut input = String::from("(if #f (+ 42 (foo 12)) 17)");
    let mut lexer = LexerState {
        s: input,
        pos: 0,
        tok_buf: None,
    };
    assert_eq!(SExpr::If(Box::new(SExpr::Bool(false)),
                         Box::new(SExpr::App(Box::new(SExpr::Symbol("+".to_string())),
                                             vec![SExpr::Number(42),
                                                  SExpr::List(vec![SExpr::Symbol("foo".to_string()),
                                                                   SExpr::Number(12)])])),
                         Box::new(SExpr::Number(17))),
               get_ast(get_expr(&mut lexer)));
}


static mut var_counter : i32 = 0;
fn get_unique_varname(stem: &str) -> String {
    unsafe {
        var_counter += 1;
        return stem.to_string() + &"." + &var_counter.to_string();
    }
}    

fn uniquify(mapping: &mut HashMap<String, String>, expr: SExpr) 
            -> SExpr {
    match expr {
        SExpr::Symbol(name) => 
            SExpr::Symbol(mapping.get(&name).unwrap().to_string()),
        SExpr::Number(_) => expr,
        SExpr::Bool(_) => expr,
        SExpr::Let(bindings, body) => {
            let mut new_bindings = vec![];
            for (k,v) in bindings {
                let uniq_k = get_unique_varname(&k);
                mapping.insert(k.clone(), uniq_k.clone());
                new_bindings.push((uniq_k, v));
            }
            return SExpr::Let(new_bindings, Box::new(uniquify(mapping, *body)));
        },
        SExpr::List(elts) => {
            let mut new_elts = vec![];
            for e in elts {
                new_elts.push(uniquify(mapping, e));
            }
            
            return SExpr::List(new_elts);
        }
        SExpr::Define(name, val) => {
            return SExpr::Define(name,
                                 Box::new(uniquify(mapping, *val)));
        },
        SExpr::If(cond, thn, els) => {
            return SExpr::If(Box::new(uniquify(mapping, *cond)),
                             Box::new(uniquify(mapping, *thn)),
                             Box::new(uniquify(mapping, *els)));
        },
        SExpr::App(f, args) => {
            let mut new_args = vec![];
            for a in args {
                new_args.push(uniquify(mapping, a));
            }
            return SExpr::App(f, new_args);
        },
        //_ => expr,
    }
}

fn main() {
    read_input();
}
