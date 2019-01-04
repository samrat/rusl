#[derive(Debug, PartialEq, Clone)]
pub enum Token<'input> {
    LParen,
    RParen,
    Symbol(&'input str),
    Number(i64),
    EOF,
}

pub struct Lexer<'input> {
    pub s: &'input str,
    pub pos: usize,
    pub col: usize,
    pub line_num: usize,
    pub tok_buf: Option<Token<'input>>,
}

fn is_valid_in_symbol(c: char) -> bool {
    c.is_alphabetic() ||
    match c {
        '+' | '-' | '*' | '/' | '#' | '<' | '>' | '=' => true,
        _ => false,
    }
}

impl<'input> Lexer<'input> {
    pub fn new(source: &'input str) -> Lexer<'input> {
        Lexer { s: source,
                pos: 0,
                col: 0,
                line_num: 1,
                tok_buf: None}
    }

    pub fn unread(&mut self, tok: Token<'input>) {
        match self.tok_buf {
            Some(_) => panic!("error: unread buffer full"),
            None => self.tok_buf = Some(tok),
        }
    }

    pub fn get_token(&mut self) -> Token<'input> {
        if let Some(tok) = self.tok_buf.clone() {
            self.tok_buf = None;
            return tok;
        }
        else {
            let mut iter = self.s[self.pos..].chars().peekable();
            while let Some(&c) = iter.peek() {
                if c.is_numeric() {
                    let mut n = c;
                    let start = self.pos;
                    while n.is_numeric() {
                        iter.next();
                        self.pos += 1;
                        self.col += 1;
                        n = match iter.peek() {
                            Some(&x) => x,
                            None => break,
                        };
                    }
                    return Token::Number(self.s[start..self.pos].parse().unwrap());
                }
                else if is_valid_in_symbol(c) {
                    let mut s = c;
                    let start = self.pos;
                    while s.is_alphanumeric() || is_valid_in_symbol(s) {
                        iter.next();
                        self.pos += 1;
                        self.col += 1;
                        s = match iter.peek() {
                            Some(&x) => x,
                            None => break,
                        };
                    }
                    return Token::Symbol(&self.s[start..self.pos]);
                }
                else {
                    match c {
                        '\n' => {
                            iter.next();
                            self.pos += 1;
                            self.col = 0;
                            self.line_num += 1;
                            continue;
                        },
                        ';' => {
                            iter.next();
                            self.pos += 1;
                            self.col += 1;
                            while let Some(c) = iter.next() {
                                self.pos += 1;
                                if c.clone() == '\n' {
                                    break;
                                }
                            }
                            self.line_num += 1;
                            self.col = 0;
                        },
                        ' ' => {
                            iter.next();
                            self.pos += 1;
                            self.col += 1;
                            continue
                        },
                        '(' => {
                            iter.next();
                            self.pos += 1;
                            self.col += 1;
                            return Token::LParen
                        },
                        ')' => {
                            iter.next();
                            self.pos += 1;
                            self.col += 1;
                            return Token::RParen
                        },

                        _ => panic!("line {}:{} unexpected char: {}", self.line_num, self.col, c),
                    }
                }
            }
            return Token::EOF;
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Token<'input>;

    fn next(&mut self) -> Option<Token<'input>> {
        match self.get_token() {
            Token::EOF => None,
            t => Some(t),
        }
    }
}

fn main() {
}
