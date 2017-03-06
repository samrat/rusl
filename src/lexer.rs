#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    LParen,
    RParen,
    Symbol(String),
    Number(i32),
    EOF,
}

pub struct LexerState {
    pub s: String,
    pub pos: usize,
    pub col: usize,
    pub line_num: usize,
    pub tok_buf: Option<Token>,
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

pub fn get_token(ls: &mut LexerState) -> Token {
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
                    ls.col += 1;
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
                    ls.col += 1;
                    s = match iter.peek() {
                        Some(&x) => x,
                        None => break,
                    };
                }
                return Token::Symbol(acc);
            }
            else {
                match c {
                    '\n' => {
                        iter.next();
                        ls.pos += 1;
                        ls.col = 0;
                        ls.line_num += 1;
                        continue;
                    },
                    ' ' => {
                        iter.next();
                        ls.pos += 1;
                        ls.col += 1;
                        continue
                    },
                    '(' => {
                        iter.next();
                        ls.pos += 1;
                        ls.col += 1;
                        return Token::LParen
                    },
                    ')' => {
                        iter.next();
                        ls.pos += 1;
                        ls.col += 1;
                        return Token::RParen
                    },
                    _ => panic!("line {}:{} unexpected char: {}", ls.line_num, ls.col, c),
                }
            }
        }
        return Token::EOF;
    }
}
