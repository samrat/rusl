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
    #[derive(Debug)]
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
                                                vec![],
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
                                                (kv[0], kv[1])
                                            } else {
                                                panic!("invalid let binding syntax");
                                            };

                                        let keyname =
                                            if let Ast::Symbol(k) = key {
                                                k
                                            } else {
                                                panic!("let binding key is not symbol");
                                            };

                                        astified_bindings.push((keyname, Parser::get_ast(&val)));
                                    }
                                }

                                Ast::Let(astified_bindings,
                                         Box::new(Parser::get_ast(&body)))
                            },
                        _ => Ast::Number(42),
                    },
                _ => Ast::Number(42),
            }
        }
    }

    let mut p = Parser::new("(if #t 42 (define (foo x y) (+ (* 1 2) 2)))");
    // println!("{:?}", p.get_expr());
    println!("{:?}", Parser::get_ast(&p.get_expr().unwrap()));
}
