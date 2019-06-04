#![feature(bind_by_move_pattern_guards)]

use lexer::{Token, Lexer};

use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum CC {
    // condition codes
    E, L, LE, G, GE,
}

#[derive(Debug, Clone)]
pub enum Ast {
    Symbol(Rc<String>),
    Number(i64),
    Bool(bool),
    List(Vec<Ast>),
    FuncName(Rc<String>),  // for closure-conversion

    Define(Rc<String>, Vec<Rc<String>>, Box<Ast>),
    If(Box<Ast>,     // cnd
       Box<Ast>,     // thn
       Box<Ast>),    // els
    Let(Vec<(Rc<String>, Ast)>, Box<Ast>),
    Lambda(Vec<Rc<String>>, Box<Ast>),
    Tuple(Vec<Ast>),
    Cmp(CC, Box<Ast>, Box<Ast>),
    App(Box<Ast>, Vec<Ast>),
    Prog(Vec<Ast>, Box<Ast>),
    Nil,
}

static mut VAR_COUNTER : i32 = 0;
pub fn get_unique_varname(stem: &str) -> String {
    unsafe {
        VAR_COUNTER += 1;
        return stem.to_string() + &VAR_COUNTER.to_string();
    }
}

impl Ast {
    pub fn uniquify(&self, mapping: &mut HashMap<Rc<String>, Rc<String>>) -> Self {
        match self {
            Ast::Symbol(name) => {
                let uniq_name = mapping.get(name)
                    .expect("unbound symbol");
                Ast::Symbol(uniq_name.clone())
            },
            Ast::Number(_) | Ast::Bool(_) => self.clone(),
            Ast::Tuple(elts) => {
                let elts =
                    elts.iter().map(|e| e.uniquify(mapping)).collect();
                Ast::Tuple(elts)
            },
            Ast::Let(bindings, body) => {
                let mut new_bindings = vec![];
                for (k, v) in bindings {
                    let uniq_k = Rc::new(get_unique_varname(&k));
                    new_bindings.push((uniq_k.clone(),
                                       v.uniquify(mapping)));
                    mapping.insert(k.clone(), uniq_k.clone());

                }
                Ast::Let(new_bindings,
                         Box::new(body.uniquify(mapping)))
            },
            Ast::List(elts) => {
                let new_elts : Vec<_> = elts.iter().map(|e| e.uniquify(mapping)).collect();
                Ast::List(new_elts)
            },
            Ast::Cmp(cc, left, right) =>
                return Ast::Cmp(cc.clone(),
                                box left.uniquify(mapping),
                                box right.uniquify(mapping)),
            Ast::Lambda(args, body) => {
                let mut new_args = vec![];
                for arg in args {
                    let new_arg = Rc::new(get_unique_varname(arg));
                    new_args.push(new_arg.clone());
                    mapping.insert(arg.clone(), new_arg.clone());
                }

                 Ast::Lambda(new_args,
                             box body.uniquify(mapping))
            },
            Ast::Define(name, args, val) => {
                let uniq_fname = Rc::new(get_unique_varname(name));
                mapping.insert(name.clone(), uniq_fname.clone());

                let mut new_args = vec![];
                for arg in args {
                    let new_arg = Rc::new(get_unique_varname(arg));
                    new_args.push(new_arg.clone());
                    mapping.insert(arg.clone(), new_arg.clone());
                }

                Ast::Define(uniq_fname.clone(),
                            new_args,
                            box val.uniquify(mapping))
            },
            Ast::If(cond, thn, els) =>
                Ast::If(box (cond.uniquify(mapping)),
                        box (thn.uniquify(mapping)),
                        box (els.uniquify(mapping))),
            Ast::App(f, args) => {
                let new_args : Vec<_> = args.iter()
                    .map(|a| a.uniquify(mapping)).collect();
                Ast::App(box f.uniquify(mapping),
                         new_args)
            },
            Ast::Prog(defs, e) => {
                let new_defs : Vec<_> = defs.iter()
                    .map(|def| def.uniquify(mapping)).collect();
                Ast::Prog(new_defs, box e.uniquify(mapping))
            },
            _ => panic!("NYI"),
        }
    }

    fn get_free_variables(&self,
                          env: &HashSet<Rc<String>>,
                          parent_env: &HashSet<Rc<String>>) -> Vec<Rc<String>> {
        match self {
            Ast::Number(_) => vec![],
            Ast::Symbol(name) => {
                match env.get(name) {
                    Some(_) => vec![],
                    None => {
                        match parent_env.get(name) {
                            Some(_) => vec![name.clone()],
                            None => panic!("no binding found in parent env for free-variable {} env: {:?} parent-env: {:?}", name, env, parent_env),
                        }
                    }
                }
            },
            Ast::If(cnd, thn, els) => {
                let mut cnd_freevars =
                    cnd.get_free_variables(env, parent_env);
                let thn_freevars =
                    thn.get_free_variables(env, parent_env);
                let els_freevars =
                    els.get_free_variables(env, parent_env);

                cnd_freevars.extend_from_slice(&thn_freevars);
                cnd_freevars.extend_from_slice(&els_freevars);

                return cnd_freevars;
            },
            Ast::Define(_, args, body) |
            Ast::Lambda(args, body) => {
                let mut new_parent_env : HashSet<Rc<String>> = HashSet::new()
                    .union(&env).cloned().collect();
                new_parent_env = new_parent_env
                    .union(&parent_env).cloned().collect();

                let new_env : HashSet<Rc<String>> = args.iter().map(|a| a.clone()).collect();

                println!("[lambda] new_env: {:?} new_parent_env: {:?}",
                         new_env,
                         new_parent_env);
                
                body.get_free_variables(&new_env, &new_parent_env)
            },
            Ast::Let(bindings, body) => {
                let mut new_env = HashSet::new();
                let mut bindings_free_vars = vec![];
                for (k, v) in bindings {
                    let v_freevars : Vec<_> =
                        v.get_free_variables(env, parent_env);
                    new_env.insert(k.clone());
                    bindings_free_vars.extend_from_slice(&v_freevars);
                }

                new_env = new_env.union(&env).cloned().collect();
                let body_freevars : Vec<_> = 
                    body.get_free_variables(&new_env,
                                            parent_env);
                bindings_free_vars.extend_from_slice(&body_freevars);

                return bindings_free_vars;
            },
            Ast::App(_, args) => {
                let mut args_freevars = vec![];
                for arg in args {
                    let arg_freevars : Vec<_> =
                        arg.get_free_variables(env,
                                               parent_env);
                    args_freevars.extend_from_slice(&arg_freevars);
                }

                return args_freevars;
            },
            _ => panic!("NYI: {:?}", self),
        }
    }


    fn symbol_is_primitive(sym: &str) -> bool {
        match sym {
            "+" | "-" | "tuple-ref" | "tuple" => true,
            _ => false,
        }
    }

    fn get_define_name(def: &Ast) -> Rc<String> {
        match def {
            Ast::Define(name, _, _) => name.clone(),
            _ => panic!("not a Define"),
        }
    }

    pub fn convert_to_closures(&self, env: &HashSet<Rc<String>>, toplevel_funs: &HashSet<Rc<String>>)
                           -> (Ast, Vec<Ast>) {
        match self {
            Ast::Cmp(_, _, _) |
            Ast::Bool(_) |
            Ast::Symbol(_) |
            Ast::FuncName(_) |
            Ast::Number(_) => (self.clone(), vec![]),
            Ast::If(cnd, thn, els) => {
                let (converted_cnd, mut cnd_defines) =
                    cnd.convert_to_closures(env, toplevel_funs);
                let (converted_thn, thn_defines) =
                    thn.convert_to_closures(env, toplevel_funs);
                let (converted_els, els_defines) =
                    els.convert_to_closures(env, toplevel_funs);

                let converted = Ast::If(box converted_cnd,
                                        box converted_thn,
                                        box converted_els);

                cnd_defines.extend_from_slice(&thn_defines);
                cnd_defines.extend_from_slice(&els_defines);

                return (converted, cnd_defines);
            },
            Ast::Define(name, args, body) => {
                let mut new_args = args.clone();
                let mut new_env = env.clone();
                
                for arg in args {
                    new_env.insert(arg.clone());
                }
                let (converted_body, body_defines) =
                    body.convert_to_closures(&new_env, toplevel_funs);

                new_args.insert(0, Rc::new("clos".to_string()));

                let converted = Ast::Define(name.clone(),
                                            new_args,
                                            box converted_body);

                return (converted, body_defines);

            },
            Ast::Lambda(args, body) => {
                let mut new_env = HashSet::new();
                let mut new_args = args.clone();
                for arg in args {
                    new_env.insert(arg.clone());
                }

                let (converted_body, mut new_defines) =
                    body.convert_to_closures(&new_env, toplevel_funs);

                let lambda_name = Rc::new(get_unique_varname("lam"));
                let free_vars : Vec<_> = body.get_free_variables(&new_env,
                                                                 env);
                let mut load_free_vars = converted_body;
                for (i, fvar) in free_vars.iter().enumerate() {
                    let bindings = vec![(fvar.clone(),
                                         Ast::App(box Ast::Symbol(Rc::new("tuple-ref".to_string())),
                                                  vec![Ast::Symbol(Rc::new("clos".to_string())),
                                                       Ast::Number((i+1) as i64)]))];
                    load_free_vars =
                        Ast::Let(bindings,
                                 box load_free_vars);
                }

                new_args.insert(0, Rc::new("clos".to_string()));

                let mut closure_elts =
                    vec![Ast::FuncName(lambda_name.clone())];

                let free_vars : Vec<Ast> =
                    free_vars.iter().map(|fv| Ast::Symbol(fv.clone())).collect();
                closure_elts.extend_from_slice(&free_vars[..]);
                let closure = Ast::Tuple(closure_elts);

                new_defines.extend_from_slice(&[
                    Ast::Define(lambda_name.clone(),
                                new_args, 
                                box load_free_vars)
                ]);

                return (closure, new_defines);
            },
            Ast::Tuple(elts) => {
                let mut converted_elts = vec![];
                let mut elts_defines = vec![];

                for elt in elts {
                    let (conv_elt, elt_defines) : (Ast, Vec<Ast>) =
                        elt.convert_to_closures(env, toplevel_funs);
                    converted_elts.push(conv_elt);
                    elts_defines.extend_from_slice(&elt_defines);
                }

                let converted = Ast::Tuple(converted_elts);
                return (converted, elts_defines);

            },
            Ast::App(box Ast::Symbol(f), ref args)
                if Self::symbol_is_primitive(f) => {
                    let mut converted_args = vec![];
                    let mut args_defines = vec![];

                    for arg in args.clone() {
                        let (conv_arg, arg_defines) : (Ast, Vec<Ast>) =
                            arg.convert_to_closures(env, toplevel_funs);
                        converted_args.push(conv_arg);
                        args_defines.extend_from_slice(&arg_defines);
                    }

                    let converted = Ast::App(box Ast::Symbol(f.clone()),
                                             converted_args);
                    return (converted, args_defines);
                },
            Ast::App(box Ast::Symbol(f), ref args)
                if !Self::symbol_is_primitive(f) => {
                    let fname = match toplevel_funs.get(f) {
                        Some(_) => Ast::Tuple(vec![Ast::FuncName(f.clone())]),
                        None => Ast::Symbol(f.clone()),
                    };

                    let (fclos, fdefines) : (Ast, Vec<Ast>) =
                        fname.convert_to_closures(env, toplevel_funs);
                    let f_temp = Rc::new(get_unique_varname("tmp"));

                    let mut converted_args =
                        vec![Ast::Symbol(f_temp.clone())];
                    let mut args_defines = vec![];

                    for arg in args {
                        let (conv_arg, arg_defines) : (Ast, Vec<Ast>) =
                            arg.convert_to_closures(env, toplevel_funs);
                        converted_args.push(conv_arg);
                        args_defines.extend_from_slice(&arg_defines);
                    }

                    args_defines.extend_from_slice(&fdefines);

                    let converted = Ast::Let(vec![(f_temp.clone(), fclos)],
                                               box Ast::App(
                                                   box Ast::App(box Ast::Symbol(Rc::new("tuple-ref".to_string())),
                                                                vec![Ast::Symbol(f_temp),
                                                                     Ast::Number(0)]),
                                                   converted_args));
                    return (converted, args_defines);
                },
            Ast::Let(bindings, body) => {
                let mut new_bindings = vec![];
                let mut bindings_defines = vec![];
                let mut new_env = env.clone();

                for (k, v) in bindings {
                    let (converted_v, v_defines) : (Ast, Vec<Ast>) =
                        v.convert_to_closures(env, toplevel_funs);

                    new_env.insert(k.clone());
                    new_bindings.push((k.clone(), converted_v));
                    bindings_defines.extend_from_slice(&v_defines);
                };

                let (converted_body, body_defines) : (Ast, Vec<Ast>) =
                    body.convert_to_closures(&new_env, toplevel_funs);

                bindings_defines.extend_from_slice(&body_defines);

                let converted = Ast::Let(new_bindings,
                                         box converted_body);
                return (converted, bindings_defines);
            },
            Ast::Prog(defines, main) => {
                let mut converted_defines = vec![];
                let mut defines_new_defines = vec![];
                let mut toplevel_funs = toplevel_funs.clone();

                for def in defines {
                    toplevel_funs.insert(Self::get_define_name(&def));
                    let (converted_define, new_defines) : (Ast, Vec<Ast>) =
                        def.convert_to_closures(env, &toplevel_funs);

                    converted_defines.push(converted_define);
                    defines_new_defines.extend_from_slice(&new_defines);
                }

                let (converted_main, main_defines) : (Ast, Vec<Ast>) =
                    main.convert_to_closures(env, &toplevel_funs);

                converted_defines.extend_from_slice(&defines_new_defines);
                converted_defines.extend_from_slice(&main_defines);

                let converted = Ast::Prog(converted_defines,
                                            box converted_main);

                return (converted, vec![]);
            },
            _ => panic!("NYI: {:?}", self),
        }
    }
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

    pub fn get_list(&mut self) -> Vec<Ast> {
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

    pub fn get_expr(&mut self) -> Option<Ast> {
        match self.lexer.next() {
            Some(Token::Symbol(s)) => Some(Ast::Symbol(Rc::new(s.to_string()))),
            Some(Token::Number(n)) => Some(Ast::Number(n)),
            Some(Token::LParen) => Some(Ast::List(self.get_list())),
            Some(Token::RParen) => panic!("line {}: {} unmatched ')'",
                                          self.lexer.line_num,
                                          self.lexer.col),
            _ => None,
        }
    }

    pub fn get_arg_names(args: &[Ast]) -> Vec<Rc<String>> {
        let arg_names : Vec<Rc<String>> =
            args.iter().map(|arg| match arg {
                Ast::Symbol(name) => name.clone(),
                _ => panic!("invalid arg"),
            }).collect();

        arg_names
    }

    pub fn get_ast(expr: &Ast) -> Ast {
        match expr {
            Ast::Symbol(sym) =>
                match &sym[..] {
                    "#f" => Ast::Bool(false),
                    "#t" => Ast::Bool(true),
                    _ => Ast::Symbol(Rc::new(sym.to_string()))
                },
            Ast::List(elts) =>
                match &elts[..] {
                    [Ast::Symbol(k), Ast::List(defelts), body]
                        if &k[..] == "define" => {
                            let name = &defelts[0];
                            let args = &defelts[1..];

                            if let Ast::Symbol(name) = name {
                                Ast::Define(name.clone(),
                                            Parser::get_arg_names(args),
                                            Box::new(Parser::get_ast(body)))
                            } else {
                                panic!("invalid function prototype");
                            }
                        },
                    [Ast::Symbol(k), cnd, thn, els]
                        if &k[..] == "if" => {
                            Ast::If(Box::new(Parser::get_ast(cnd)),
                                    Box::new(Parser::get_ast(thn)),
                                    Box::new(Parser::get_ast(els)))
                        },
                    [Ast::Symbol(k), Ast::List(bindings), body]
                        if &k[..] == "let" => {
                            let mut astified_bindings = vec![];
                            for bind_pair in bindings {
                                if let Ast::List(kv) = bind_pair {
                                    let (key, val) =
                                        if kv.len() == 2 {
                                            let astified_val = Parser::get_ast(&kv[1]);
                                            let keyname =
                                                if let Ast::Symbol(k) = kv[0].clone() {
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
                                     Box::new(Parser::get_ast(body)))
                        },
                    [Ast::Symbol(k), Ast::List(args), body]
                        if &k[..] == "lambda" => {
                            Ast::Lambda(Parser::get_arg_names(&args),
                                        Box::new(Parser::get_ast(body)))
                        },
                    [Ast::Symbol(k), _..]
                        if &k[..] == "tuple" => {
                            let tuple_elts = elts[1..].iter()
                            // TODO: move?
                                .map(|e| Parser::get_ast(e))
                                .collect();
                            Ast::Tuple(tuple_elts)
                        },
                    [Ast::Symbol(cmp), left, right]
                        if (&cmp[..] == ">" || &cmp[..] == "<" ||
                            &cmp[..] == "<=" || &cmp[..] == ">=" ||
                            &cmp[..] == "=") => {
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
                    [f, _..] => {
                        let args = elts[1..].iter().map(|e| Parser::get_ast(e)) // TODO: move
                            .collect();

                        Ast::App(Box::new(f.clone()), args)
                    }
                    _ => Ast::Nil,
                },
            _ => expr.clone(),
        }
    }

    pub fn read(&mut self) -> Option<Ast> {
        match self.get_expr() {
            Some(expr) => Some(Parser::get_ast(&expr)),
            None => None,
        }
    }
}

impl<'input> Iterator for Parser<'input> {
    type Item = Ast;

    fn next(&mut self) -> Option<Ast> {
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


    let ps = Parser::new("(> 1 2) (+ 1 2)");
    for a in ps {
        println!("{:?}", a);
    }
}
