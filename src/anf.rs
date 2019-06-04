use util::get_unique_varname;
use std::rc::Rc;
use ast::{Ast, CC};

#[derive(Clone, Debug, PartialEq)]
pub enum Flat {
    Symbol(Rc<String>),
    FuncName(Rc<String>),           // for closure-conversion
    Number(i64),
    Bool(bool),
    Tuple(Vec<Flat>),
    Assign(Rc<String>, Box<Flat>),
    Return(Box<Flat>),
    If(Box<Flat>, Vec<Flat>, Vec<Flat>),
    Cmp(CC, Box<Flat>, Box<Flat>),
    App(Rc<String>, Vec<Flat>),
    Prim(Rc<String>, Vec<Flat>),
}

#[derive(Debug, PartialEq)]
pub enum FlatResult {
    Prog(Vec<FlatResult>, Vec<Flat>, Vec<Rc<String>>),
    Define(Rc<String>, Vec<Rc<String>>, Vec<Flat>, Vec<Rc<String>>),
    Flat(Flat, Vec<Flat>, Vec<Rc<String>>),
}

fn flatten_args(args: &[Ast])
                -> (Vec<Flat>, Vec<Flat>, Vec<Rc<String>>) {
    let mut flat_args = vec![];
    let mut args_assigns : Vec<Flat> = vec![];
    let mut args_vars = vec![];

    for arg in args {
        match flatten(arg) {
            FlatResult::Flat(flat, assigns, vars) => {
                flat_args.push(flat);
                args_assigns.extend_from_slice(&assigns);
                args_vars.extend_from_slice(&vars);
            },
            _ => panic!("unreachable"),
        };
    }

    return (flat_args, args_assigns, args_vars);
}


// This function does and ANF transformation. The output is a Flat
// expression.
pub fn flatten(expr: &Ast) -> FlatResult {
    match expr {
        Ast::Symbol(name) => FlatResult::Flat(Flat::Symbol(name.clone()),
                                                vec![],
                                                vec![name.clone()]),
        Ast::FuncName(name) => FlatResult::Flat(Flat::FuncName(name.clone()),
                                                vec![],
                                                vec![name.clone()]),
        Ast::Number(n) => FlatResult::Flat(Flat::Number(*n),
                                             vec![],
                                             vec![]),
        Ast::Bool(b) => FlatResult::Flat(Flat::Bool(*b),
                                           vec![],
                                           vec![]),
        Ast::Lambda(_, _) =>
            panic!("closure conversion should happen before flatten"),
        Ast::Tuple(elts) => {
            let tup_temp = Rc::new(get_unique_varname("tmp"));
            let mut flat_elts = vec![];
            let mut elts_assigns : Vec<Flat> = vec![];
            let mut elts_vars = vec![];

            for elt in elts {
                if let FlatResult::Flat(flat, assigns, vars) = flatten(elt) {
                    flat_elts.push(flat);
                    elts_assigns.extend_from_slice(&assigns);
                    elts_vars.extend_from_slice(&vars);
                } else {
                    panic!("unreachable")
                }
            }

            elts_assigns.extend_from_slice(&[
                Flat::Assign(tup_temp.clone(),
                             box Flat::Tuple(flat_elts))
            ]);
            elts_vars.extend_from_slice(&[tup_temp.clone()]);

            return FlatResult::Flat(Flat::Symbol(tup_temp),
                                    elts_assigns,
                                    elts_vars)
        },
        Ast::Let(bindings, body) => {
            if let FlatResult::Flat(flat_body, body_assigns, body_vars) = flatten(body) {
                let mut bindings_assigns = vec![];
                let mut bindings_vars = vec![];
                for (k, v) in bindings {
                    if let FlatResult::Flat(flat_v, v_assigns, v_vars) = flatten(v) {
                        match flat_v.clone() {
                            Flat::Symbol(name) => bindings_vars.push(name),
                            _ => (),
                        };
                        bindings_assigns.extend_from_slice(&v_assigns);
                        bindings_assigns.extend_from_slice(
                            &[Flat::Assign(k.clone(), Box::new(flat_v))]
                        );
                        bindings_vars.extend_from_slice(&v_vars);
                        bindings_vars.push(k.clone());
                    }
                    else {
                        panic!("unreachable");
                    }
                }
                bindings_assigns.extend_from_slice(&body_assigns);
                bindings_vars.extend_from_slice(&body_vars);
                return FlatResult::Flat(flat_body,
                                        bindings_assigns,
                                        bindings_vars);

            } else {
                panic!("NYI");
            }
        },
        Ast::List(_) => {
            panic!("NYI");
        },
        Ast::Define(name, args, body) => {
            let (flat_body, mut body_assigns, mut body_vars) =
                match flatten(body) {
                    FlatResult::Flat(flat_body, body_assigns, body_vars) =>
                        (flat_body, body_assigns, body_vars),
                    _ => panic!("unreachable"),
                };
            body_assigns.extend_from_slice(&[
                Flat::Return(Box::new(flat_body))
            ]);

            // Remove args from body_vars
            for arg in args {
                body_vars = body_vars.iter().filter(|v| v != &arg).cloned().collect();
            }

            return FlatResult::Define(name.clone(),
                                      args.to_vec(),
                                      body_assigns,
                                      body_vars);
        },
        Ast::If(cnd, thn, els) => {
            let (flat_cnd, mut cnd_assigns, mut cnd_vars) =
                match flatten(cnd) {
                    FlatResult::Flat(flat_cnd, cnd_assigns, cnd_vars) =>
                        (flat_cnd, cnd_assigns, cnd_vars),
                    _ => panic!("unreachable"),
                };
            let (flat_thn, mut thn_assigns, mut thn_vars) =
                match flatten(thn) {
                    FlatResult::Flat(flat_thn, thn_assigns, thn_vars) =>
                        (flat_thn, thn_assigns, thn_vars),
                    _ => panic!("unreachable"),
                };
            let (flat_els, mut els_assigns, mut els_vars) =
                match flatten(els) {
                    FlatResult::Flat(flat_els, els_assigns, els_vars) =>
                        (flat_els, els_assigns, els_vars),
                    _ => panic!("unreachable"),
                };

            let if_temp = Rc::new(get_unique_varname("if"));

            thn_assigns.extend_from_slice(&[Flat::Assign(if_temp.clone(),
                                                         Box::new(flat_thn))]);
            els_assigns.extend_from_slice(&[Flat::Assign(if_temp.clone(),
                                                         Box::new(flat_els))]);
            let flat_if = Flat::If(Box::new(flat_cnd),
                                   thn_assigns,
                                   els_assigns);

            cnd_assigns.extend_from_slice(&[flat_if]);
            cnd_vars.append(&mut thn_vars);
            cnd_vars.append(&mut els_vars);
            cnd_vars.extend_from_slice(&[if_temp.clone()]);
            return FlatResult::Flat(Flat::Symbol(if_temp),
                                    cnd_assigns,
                                    cnd_vars);

        },
        Ast::Cmp(cc, left, right) => {
            let (flat_left, mut left_assigns, mut left_vars) =
                match flatten(left) {
                    FlatResult::Flat(flat, assigns, vars) => (flat, assigns, vars),
                    _ => panic!("unreachable"),
                };
            let (flat_right, mut right_assigns, mut right_vars) =
                match flatten(right) {
                    FlatResult::Flat(flat, assigns, vars) => (flat, assigns, vars),
                    _ => panic!("unreachable"),
                };
            let cmp_temp = Rc::new(get_unique_varname("tmp"));
            left_assigns.append(&mut right_assigns);
            left_assigns.extend_from_slice(&[
                Flat::Assign(cmp_temp.clone(), box Flat::Cmp(cc.clone(),
                                                             box flat_left,
                                                             box flat_right))
            ]);
            left_vars.append(&mut right_vars);
            left_vars.push(cmp_temp.clone());

            return FlatResult::Flat(Flat::Symbol(cmp_temp),
                                    left_assigns,
                                    left_vars);
        },
        Ast::App(f, args) => {
            match f {
                box Ast::Symbol(fname) => {
                    match &fname[..] {
                        "-" => {
                            let arg1 = match &args[..] {
                                &[ref arg1] => arg1,
                                _ => panic!("Wrong no. of args to `-`: {:?}", args),
                            };
                            let (flat_e, mut e_assigns, mut e_vars) =
                                match flatten(arg1) {
                                    FlatResult::Flat(flat_e, e_assigns, e_vars) =>
                                        (flat_e, e_assigns, e_vars),
                                    _ => panic!("unreachable"),
                                };
                            let neg_temp = Rc::new(get_unique_varname("tmp"));
                            let flat_neg = Flat::Assign(neg_temp.clone(),
                                                        Box::new(Flat::Prim(Rc::new("-".to_string()), vec![flat_e])));
                            e_assigns.extend_from_slice(&[flat_neg]);
                            e_vars.extend_from_slice(&[neg_temp.clone()]);
                            return FlatResult::Flat(Flat::Symbol(neg_temp),
                                                    e_assigns,
                                                    e_vars);
                        },
                        "+" => {
                            let (arg1, arg2) = match &args[..] {
                                &[ref arg1, ref arg2] => (arg1, arg2),
                                _ => panic!("Wrong no. of args to `+`"),
                            };
                            let (flat_e1, mut e1_assigns, mut e1_vars) =
                                match flatten(arg1) {
                                    FlatResult::Flat(flat_e1, e1_assigns, e1_vars) =>
                                        (flat_e1, e1_assigns, e1_vars),
                                    _ => panic!("unreachable"),
                                };
                            let (flat_e2, mut e2_assigns, mut e2_vars) =
                                match flatten(arg2) {
                                    FlatResult::Flat(flat_e2, e2_assigns, e2_vars) =>
                                        (flat_e2, e2_assigns, e2_vars),
                                    _ => panic!("unreachable"),
                                };

                            let plus_temp = Rc::new(get_unique_varname("tmp"));

                            let flat_plus = Flat::Assign(plus_temp.clone(),
                                                         Box::new(Flat::Prim(Rc::new("+".to_string()), vec![flat_e1, flat_e2])));
                            e1_assigns.append(&mut e2_assigns);
                            e1_assigns.extend_from_slice(&[flat_plus]);

                            e1_vars.append(&mut e2_vars);
                            e1_vars.extend_from_slice(&[plus_temp.clone()]);

                            return FlatResult::Flat(Flat::Symbol(plus_temp),
                                                    e1_assigns,
                                                    e1_vars);
                        },
                        "tuple-ref" => {
                            let (tuple, index) = match &args[..] {
                                &[ref tuple, ref index] => (tuple, index),
                                _ => panic!("Wrong no. of args to `tuple-ref`: {:?}", args),
                            };
                            let index = match index {
                                &Ast::Number(n) => Flat::Number(n),
                                &_ => panic!("index to tuple-ref must be a literal number"),
                            };
                            let (flat_tuple, mut tup_assigns, mut tup_vars) =
                                match flatten(tuple) {
                                    FlatResult::Flat(flat, assigns, vars) =>
                                        (flat, assigns, vars),
                                    _ => panic!("unreachable"),
                                };

                            let ref_temp = Rc::new(get_unique_varname("tmp"));
                            let flat_ref = Flat::Assign(ref_temp.clone(),
                                                        Box::new(Flat::Prim(Rc::new("tuple-ref".to_string()),
                                                                            vec![flat_tuple, index])));
                            tup_assigns.extend_from_slice(&[flat_ref]);

                            tup_vars.extend_from_slice(&[ref_temp.clone()]);

                            return FlatResult::Flat(Flat::Symbol(ref_temp),
                                                    tup_assigns,
                                                    tup_vars);
                        },
                        f => {
                            return flatten(&Ast::App(box Ast::Symbol(Rc::new("tuple-ref".to_string())),
                                                     vec![Ast::Tuple(vec![Ast::FuncName(fname.clone())]),
                                                          Ast::Number(0)]));
                        },
                    }
                },
                box Ast::App(_, _) => {
                    if let FlatResult::Flat(flat_fref,
                                            mut fref_assigns,
                                            mut fref_vars) = flatten(f) {
                        let flat_fref = match flat_fref {
                            Flat::Symbol(fname) => fname,
                            _ => panic!("unreachable: {:?}", flat_fref),
                        };

                        let app_temp = Rc::new(get_unique_varname("tmp"));
                        let (flat_args, args_assigns, args_vars) =
                            flatten_args(&args);
                        let flat_app = Flat::Assign(app_temp.clone(),
                                                    box Flat::App(flat_fref,
                                                                  flat_args));

                        fref_assigns.extend_from_slice(&args_assigns);
                        fref_assigns.extend_from_slice(&[flat_app]);

                        fref_vars.extend_from_slice(&[app_temp.clone()]);
                        fref_vars.extend_from_slice(&args_vars[..]);

                        return FlatResult::Flat(Flat::Symbol(app_temp.clone()),
                                                fref_assigns,
                                                fref_vars);
                    } else {
                        panic!("unreachable");
                    }
                    
                },
                _ => panic!("not a function: {:?}", f),
            }
        },
        Ast::Prog(defs, e) => {
            let (flat_e, mut e_assigns, mut e_vars) =
                match flatten(e) {
                    FlatResult::Flat(flat_e, e_assigns, e_vars) =>
                        (flat_e, e_assigns, e_vars),
                    _ => panic!("unreachable"),
                };
            let return_e = Flat::Return(Box::new(flat_e));

            e_assigns.extend_from_slice(&[return_e]);
            e_vars.dedup();

            let mut flat_defs = vec![];
            for def in defs {
                flat_defs.push(flatten(def));
            }

            return FlatResult::Prog(flat_defs,
                                    e_assigns,
                                    e_vars);
        },
        _ => unimplemented!(),
    }
}

#[test]
fn test_flatten() {
    use parser::Parser;

    let input = String::from("(+ 12 (+ 13 14))");
    let parser = Parser::new(&input);
    let toplevel : Vec<_> = parser.collect();

    assert_eq!(
        flatten(&Ast::Prog(vec![], Box::new(toplevel[0].clone()))),
        FlatResult::Prog(vec![],
                         vec![Flat::Assign(Rc::new("tmp1".to_string()), Box::new(Flat::Prim(Rc::new("+".to_string()),
                                                                                   vec![Flat::Number(13), Flat::Number(14)]))),
                              Flat::Assign(Rc::new("tmp2".to_string()),
                                           Box::new(Flat::Prim(Rc::new("+".to_string()),
                                                               vec![Flat::Number(12),
                                                                    Flat::Symbol(Rc::new("tmp1".to_string()))]))),
                              Flat::Return(Box::new(Flat::Symbol(Rc::new("tmp2".to_string()))))],
                         vec![Rc::new("tmp1".to_string()),
                              Rc::new("tmp2".to_string())])
    );

    // TODO: Reset start(var counter) so that these asserts are
    // independent.
    assert_eq!(
        flatten(&Ast::Define(Rc::new("foo".to_string()),
                             vec![Rc::new("x".to_string()),
                                  Rc::new("y".to_string()),
                                  Rc::new("z".to_string())],
                             box (Ast::App(box (Ast::Symbol(Rc::new("+".to_string()))),
                                           vec![Ast::Symbol(Rc::new("x".to_string())),
                                                Ast::Number(10)])))),
        FlatResult::Define(Rc::new("foo".to_string()),
                           vec![Rc::new("x".to_string()),
                                Rc::new("y".to_string()),
                                Rc::new("z".to_string())],
                           vec![Flat::Assign(Rc::new("tmp3".to_string()),
                                             Box::new(Flat::Prim(Rc::new("+".to_string()),
                                                                 vec![Flat::Symbol(Rc::new("x".to_string())), Flat::Number(10)]))),
                                Flat::Return(Box::new(Flat::Symbol(Rc::new("tmp3".to_string()))))],
                           vec![Rc::new("tmp3".to_string())])
    );
}
