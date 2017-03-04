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

#[derive(Clone, Debug)]
enum Arg {
    Number(i32),
    Var(String),
}

#[derive(Clone, Debug)]
enum Flat {
    Symbol(String),
    Number(i32),
    Bool(bool),
    Assign(String, Box<Flat>),
    Return(Arg),
    If(Box<Flat>, Vec<Flat>, Vec<Flat>),
    EqP(Box<Flat>, Box<Flat>),
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
    let symbol_start_chars = vec!['+', '-', '*', '/', '#'];

    let mut ret = false;
    if c.is_alphabetic() { ret = true; }
    else {
        for s in symbol_start_chars {
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
                        _ => {
                            let mut astified_args = vec![];
                            for arg in elts[1..].to_vec() {
                                astified_args.push(get_ast(arg));
                            }

                            return SExpr::App(Box::new(elts[0].clone()), astified_args);
                        },
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

    println!("{:?}", flatten(uniquify(&mut uniquify_mapping,
                                      get_ast(get_expr(&mut lexer)))));

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


static mut VAR_COUNTER : i32 = 0;
fn get_unique_varname(stem: &str) -> String {
    unsafe {
        VAR_COUNTER += 1;
        return stem.to_string() + &"." + &VAR_COUNTER.to_string();
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

fn flatten(expr: SExpr) -> (Flat, Vec<Flat>, Vec<String>) {
    match expr {
        SExpr::Symbol(name) => (Flat::Symbol(name.clone()),
                                vec![], 
                                vec![name]),
        SExpr::Number(n) => (Flat::Number(n), vec![], vec![]),
        SExpr::Bool(b) => (Flat::Bool(b), vec![], vec![]),
        SExpr::Let(bindings, body) => {
            let (flat_body, body_assigns, body_vars) = flatten(*body);

            let mut bindings_assigns = vec![];
            let mut bindings_vars = vec![];
            for (k, v) in bindings {
                let (flat_v, v_assigns, v_vars) = flatten(v);
                println!("{:?}", flat_v);
                match flat_v.clone() {
                    Flat::Symbol(name) => bindings_vars.push(name),
                    _ => (),
                };
                bindings_assigns.extend_from_slice(&v_assigns);
                bindings_assigns.extend_from_slice(
                    &[Flat::Assign(k.clone(), Box::new(flat_v))]
                    );
                bindings_vars.extend_from_slice(&v_vars);
                bindings_vars.push(k);
            }
            bindings_assigns.extend_from_slice(&body_assigns);
            bindings_vars.extend_from_slice(&body_vars);
            return (flat_body, 
                    bindings_assigns, 
                    bindings_vars);
        },
        SExpr::List(elts) => {
            panic!("NYI");
        },
        SExpr::Define(name, val) => {
            // TODO: FIXME
            return (Flat::Symbol("".to_string()), vec![], vec![]);
        },
        SExpr::If(cnd, thn, els) => {
            let (flat_cnd, mut cnd_assigns, mut cnd_vars) = 
                flatten(*cnd);
            let (flat_thn, mut thn_assigns, mut thn_vars) = 
                flatten(*thn);
            let (flat_els, mut els_assigns, mut els_vars) = 
                flatten(*els);

            let if_temp = get_unique_varname("if");

            thn_assigns.extend_from_slice(&[Flat::Assign(if_temp.clone(),
                                                         Box::new(flat_thn))]);
            els_assigns.extend_from_slice(&[Flat::Assign(if_temp.clone(),
                                                         Box::new(flat_els))]);
            let flat_if = Flat::If(Box::new(Flat::EqP(Box::new(flat_cnd),
                                                      Box::new(Flat::Bool(true)))),
                                   thn_assigns,
                                   els_assigns);

            cnd_assigns.extend_from_slice(&[flat_if]);
            cnd_vars.append(&mut thn_vars);
            cnd_vars.append(&mut els_vars);
            cnd_vars.extend_from_slice(&[if_temp.clone()]);
            return (Flat::Symbol(if_temp),
                    cnd_assigns,
                    cnd_vars);
                                   
        },
        SExpr::App(f, args) => {
            panic!("NYI");
        },
        // _ => (SExpr::Symbol("".to_string()), vec![], vec![]),
    }
}

fn main() {
    read_input();
}
