use std::io;

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

#[derive(Debug)]
enum SExpr {
    Symbol(String),
    Number(i32),
    List(Vec<SExpr>),
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
    let SymbolStartChars = vec!['+', '-', '*', '/'];

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

fn read_input() -> io::Result<()> {
    let mut input = String::new();

    try!(io::stdin().read_line(&mut input));

    let mut lexer = LexerState {
        s: input,
        pos: 0,
        tok_buf: None,
    };

    println!("{:?}", get_expr(&mut lexer));

    Ok(())
}

fn main() {
    read_input();
}
