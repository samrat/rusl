use util::get_unique_varname;
use parser::SExpr;

#[derive(Clone, Debug, PartialEq)]
pub enum Flat {
    Symbol(String),
    Number(i32),
    Bool(bool),
    Assign(String, Box<Flat>),
    Return(Box<Flat>),
    If(Box<Flat>, Vec<Flat>, Vec<Flat>),
    EqP(Box<Flat>, Box<Flat>),
    Prim(String, Vec<Flat>),
}


// This function does and ANF transformation. The output is a Flat
// expression.
pub fn flatten(expr: SExpr) -> (Flat, Vec<Flat>, Vec<String>) {
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
            match *f {
                SExpr::Symbol(fname) => {
                    match &fname[..] {
                        "-" => {
                            // TODO: check no. of args
                            let (flat_e, mut e_assigns, mut e_vars) = flatten(args[0].clone());
                            let neg_temp = get_unique_varname("tmp");
                            let flat_neg = Flat::Assign(neg_temp.clone(),
                                                        Box::new(Flat::Prim("-".to_string(), vec![flat_e])));
                            e_assigns.extend_from_slice(&[flat_neg]);
                            e_vars.extend_from_slice(&[neg_temp.clone()]);
                            return (Flat::Symbol(neg_temp),
                                    e_assigns,
                                    e_vars);
                        },
                        "+" => {
                            let (flat_e1, mut e1_assigns, mut e1_vars) = flatten(args[0].clone());
                            let (flat_e2, mut e2_assigns, mut e2_vars) = flatten(args[1].clone());

                            let plus_temp = get_unique_varname("tmp");

                            let flat_plus = Flat::Assign(plus_temp.clone(),
                                                         Box::new(Flat::Prim("+".to_string(), vec![flat_e1, flat_e2])));
                            e1_assigns.append(&mut e2_assigns);
                            e1_assigns.extend_from_slice(&[flat_plus]);

                            e1_vars.append(&mut e2_vars);
                            e1_vars.extend_from_slice(&[plus_temp.clone()]);

                            return (Flat::Symbol(plus_temp),
                                    e1_assigns,
                                    e1_vars);
                        },
                        &_ => panic!("NYI!"),
                    }
                },
                _ => panic!("not a function!"),
            }
        },
        SExpr::Prog(e) => {
            let (flat_e, mut e_assigns, mut e_vars) = flatten(*e);
            let return_e = Flat::Return(Box::new(flat_e));

            e_assigns.extend_from_slice(&[return_e]);
            e_vars.dedup();

            return (Flat::Symbol("<PROGRAM>".to_string()),
                    e_assigns,
                    e_vars);
        },
        SExpr::EOF => panic!("Don't know what to do with EOF"),
    }
}

#[test]
fn test_flatten() {
    use lexer::LexerState;
    use parser::read;
    
    let mut input = String::from("(+ 12 (+ 13 14))");
    let mut lexer = LexerState {
        s: input,
        pos: 0,
        col: 1,
        line_num: 1,
        tok_buf: None,
    };

    assert_eq!(
        flatten(SExpr::Prog(Box::new(read(&mut lexer)))),
        (Flat::Symbol("<PROGRAM>".to_string()), 
         vec![Flat::Assign("tmp1".to_string(), Box::new(Flat::Prim("+".to_string(), vec![Flat::Number(13), Flat::Number(14)]))), 
              Flat::Assign("tmp2".to_string(), Box::new(Flat::Prim("+".to_string(), vec![Flat::Number(12), Flat::Symbol("tmp1".to_string())]))),
              Flat::Return(Box::new(Flat::Symbol("tmp2".to_string())))
         ], vec!["tmp1".to_string(), "tmp2".to_string()])
    );
}
