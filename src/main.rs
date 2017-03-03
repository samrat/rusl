use std::io;

#[derive(Debug, PartialEq)]
enum Token {
    LParen,
    RParen,
    Plus,
    Minus,
    
    Symbol(String),
    Number(i32),
    EOF,
}

pub struct LexerState {
    s: String,
    pos: usize,
}

fn get_token(ls: &mut LexerState) -> Token {
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
        else if c.is_alphabetic() {
            let mut acc = String::new();
            let mut s = c;
            while s.is_alphanumeric() {
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
                '+' => {
                    iter.next();
                    ls.pos += 1;
                    return Token::Plus
                },
                '-' => {
                    iter.next();
                    ls.pos += 1;
                    return Token::Minus
                },
                _ => return Token::EOF,
            }
        }
    }
    return Token::EOF;
}

fn read_input() -> io::Result<()> {
    let mut input = String::new();

    try!(io::stdin().read_line(&mut input));

    let mut lexer = LexerState {
        s: input,
        pos: 0};

    let mut tok = get_token(&mut lexer);
    while tok != Token::EOF {
        println!("get_token: {:?}", tok);
        tok = get_token(&mut lexer);
    }

    Ok(())
}

fn main() {
    read_input();
}
