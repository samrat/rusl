use lexer::Token;
use lexer::LexerState;
use lexer::get_token;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum SExpr {
    Symbol(String),
    Number(i32),
    Bool(bool),
    List(Vec<SExpr>),

    // TODO: change define to be special-form for function definition
    Define(String, Box<SExpr>),
    Let(Vec<(String, SExpr)>, Box<SExpr>),
    If(Box<SExpr>, Box<SExpr>, Box<SExpr>),
    App(Box<SExpr>, Vec<SExpr>),
    Prog(Box<SExpr>),
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
        // TODO: show line num/column
        Token::RParen => panic!("unmatched ')'"),
        Token::EOF => return SExpr::EOF,
    }
}

pub fn get_ast(expr: SExpr) -> SExpr {
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

pub fn read(ls: &mut LexerState) -> SExpr {
    return get_ast(get_expr(ls));
}

#[test]
fn test_parser() {
    let mut input = String::from("(if #f (+ 42 (foo 12)) 17) 
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
    assert_eq!(SExpr::App(Box::new(SExpr::Symbol("+".to_string())),
                          vec![SExpr::Number(1), 
                               SExpr::Number(2)]),
               read(&mut lexer));
    // nothing left in string
    assert_eq!(SExpr::EOF, read(&mut lexer));
}
