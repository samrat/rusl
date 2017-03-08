use std::io;
use std::collections::HashMap;
use std::collections::HashSet;
use std::io::prelude::*;
use std::fs::File;
use std::env;
use std::process;

#[macro_use]
extern crate log;

mod util;
mod lexer;
mod parser;
mod anf;

use util::get_unique_varname;

use lexer::Token;
use lexer::LexerState;

use parser::SExpr;
use parser::read;

use anf::{Flat,FlatResult};
use anf::flatten;


#[derive(Debug, Clone)]
enum CC {
    // condition codes
    E, L, LE, G, GE,
}

#[derive(Debug, Clone)]
enum Reg {
    RAX, RBX, RBP, RCX, RDX,
}

#[derive(Debug, Clone)]
enum X86Arg {
    Reg(Reg),
    Imm(i32),
    RegOffset(Reg, i32),
    Var(String),     // pseudo-x86
}

#[derive(Debug, Clone)]
enum X86 {
    Mov(X86Arg, X86Arg),
    Add(X86Arg, X86Arg),
    Cmp(X86Arg, X86Arg),
    EqP(X86Arg, X86Arg),          // pseudo-X86
    If(Box<X86>, Vec<X86>, Vec<X86>), // pseudo-X86

    // pseudo-X86
    IfWithLives(Box<X86>,                      // cond
                Vec<X86>,                      // then
                Vec<HashSet<String>>,          // then-live-sets
                Vec<X86>,                      // else
                Vec<HashSet<String>>           // else-live-sets
    ),
    Prog(Vec<X86>, Vec<String>),

    ProgWithLives(Vec<X86>, Vec<String>, Vec<HashSet<String>>),
    JmpIf(CC, String),
    Jmp(String),
    Label(String),
}



// uniquify variable names. This function simply adds a monotonically
// increasing counter(VAR_COUNTER) to each and every variable.
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
                new_bindings.push((uniq_k,
                                   uniquify(mapping, v)));
            }
            return SExpr::Let(new_bindings, 
                              Box::new(uniquify(mapping, *body)));
        },
        SExpr::List(elts) => {
            let mut new_elts = vec![];
            for e in elts {
                new_elts.push(uniquify(mapping, e));
            }
            
            return SExpr::List(new_elts);
        }
        SExpr::Define(name, args, val) => {
            return SExpr::Define(name,
                                 args,
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
        SExpr::Prog(defs, e) =>
            return SExpr::Prog(defs, Box::new(uniquify(mapping, *e))),
        SExpr::EOF => {
            error!("Don't know what to do with EOF");
            process::exit(0);
        },
    }
}


fn flat_arg_type(v: Flat) -> X86Arg {
    match v {
        Flat::Symbol(name) => X86Arg::Var(name),
        Flat::Number(n) => X86Arg::Imm(n),
        Flat::Bool(b) => {
            match b {
                true => X86Arg::Imm(1),
                false => X86Arg::Imm(0),
            }
        },
        _ => {
            error!("flat_arg_type: compound expression");
            process::exit(0);
        },
    }
}

// convert one Flat instruction to pseudo-x86
fn flat_to_px86(instr: Flat) -> Vec<X86> {
    match instr {
        Flat::Assign(dest, e) => {
            match *e {
                Flat::Symbol(name) => vec![X86::Mov(X86Arg::Var(dest), X86Arg::Var(name))],
                Flat::Number(n) => vec![X86::Mov(X86Arg::Var(dest), X86Arg::Imm(n))],
                Flat::Bool(b) => {
                    let bval = match b {
                        true => 1,
                        false => 0,
                    };
                    return vec![X86::Mov(X86Arg::Var(dest), 
                                         X86Arg::Imm(bval))];
                },
                // https://github.com/rust-lang/rust/issues/16223
                x => match x {
                    Flat::Prim(f, args) => {
                        match &f[..] {
                            "+" => {
                                // TODO: check arg count
                                let arg1 = args[0].clone();
                                let arg2 = args[1].clone();
                                return vec![
                                    X86::Mov(X86Arg::Var(dest.clone()),
                                             flat_arg_type(arg1)),
                                    X86::Add(X86Arg::Var(dest),
                                             flat_arg_type(arg2))
                                ];
                            },
                            _ => panic!("primitive not defined"),
                        }
                    },
                    _ => {
                        println!("{:?}", x);
                        panic!("NYI")
                    },
                },
            }
        },
        Flat::Return(v) => {
            let val = flat_arg_type(*v);
            return vec![X86::Mov(X86Arg::Reg(Reg::RAX), 
                                 val)]
        },
        Flat::If(cnd, thn, els) => {
            let (eq_left, eq_right) = match *cnd {
                x => match x {
                    // https://github.com/rust-lang/rust/issues/16223
                    Flat::EqP(left, right) => (left, right),
                    _ => panic!("if cond needs to be Flat::EqP"),
                },
            };
            let mut thn_instrs = vec![];
            for i in thn {
                let mut i_instrs = flat_to_px86(i);
                thn_instrs.append(&mut i_instrs);
            }
            let mut els_instrs = vec![];
            for i in els {
                let mut i_instrs = flat_to_px86(i);
                els_instrs.append(&mut i_instrs);
            }
            return vec![X86::If(Box::new(X86::EqP(flat_arg_type(*eq_left),
                                                  flat_arg_type(*eq_right))),
                                thn_instrs,
                                els_instrs)];
        },
        _ => panic!("NYI"),
    }
}

// convert a Flat expression into pseudo-x86 instructions. pseudo-x86
// is like x86 but with if's and temporaries. It is also
// "unpatched" (see `patch_instructions`)
fn select_instructions(flat_prog: FlatResult) -> X86 {
    match flat_prog {
        FlatResult::Prog(defs, main_assigns, main_vars) => {
            // TODO: select-instructions for defs
            
            let mut x86_instrs = vec![];
            for i in main_assigns {
                let mut i_instrs = flat_to_px86(i);
                x86_instrs.append(&mut i_instrs);
            }
            return X86::Prog(x86_instrs, main_vars);
        },
        _ => panic!("flat_prog is not a top-level Prog"),
    }
}

// For an instruction, returns a 3-tuple:
// (variables used in instruction, variables read, variables written to)
fn instruction_rw(instr: X86) -> (Vec<String>, Vec<String>, Vec<String>) {
    match instr {
        X86::Mov(X86Arg::Var(dest), X86Arg::Var(src)) => {
            return (vec![dest.clone(), src.clone()],
                    vec![src],
                    vec![dest]);
        },
        X86::Mov(X86Arg::Var(dest), _) => {
            return (vec![dest.clone()],
                    vec![],
                    vec![dest]);
        },
        X86::Mov(X86Arg::Reg(Reg::RAX), X86Arg::Var(src)) => {
            return (vec![src.clone()],
                    vec![src],
                    vec![])
        },
        X86::Cmp(left, right) => {
            match (left, right) {
                (X86Arg::Var(l), X86Arg::Var(r)) => (vec![l.clone(),
                                                          r.clone()],
                                                     vec![l, r],
                                                     vec![]),
                (X86Arg::Var(l), _) => (vec![l.clone()],
                                        vec![l],
                                        vec![]),
                (_, X86Arg::Var(r)) => (vec![r.clone()],
                                        vec![r],
                                        vec![]),
                (_, _) => (vec![], vec![], vec![]),
            }
        },
        X86::Add(X86Arg::Var(dest), X86Arg::Var(src)) => {
            return (vec![dest.clone(), src.clone()],
                    vec![dest.clone(), src],
                    vec![dest]);
        },
        X86::Add(X86Arg::Var(dest), _) => {
            return (vec![dest.clone()],
                    vec![dest.clone()],
                    vec![dest]);
        },
        _ => panic!("NYI: {:?}", instr),
    }
}


// Find live variables during each instruction. For `if`s, the live
// sets are embedded in the new list of instructions
fn get_live_after_sets(mut instrs: Vec<X86>, lives: HashSet<String>) 
                   -> (HashSet<String>, Vec<HashSet<String>>, Vec<X86>) {
    let mut live_of_next = lives.clone();
    let mut live_after_sets = vec![];
    let mut new_instrs = vec![];
    
    instrs.reverse();
    for instr in instrs {
        match instr {
            X86::If(cnd, thns, elss) => {
                let (thn_lives, thn_live_sets, new_thns) =
                    get_live_after_sets(thns.clone(), live_of_next.clone());
                let (els_lives, els_live_sets, new_elss) =
                    get_live_after_sets(elss.clone(), live_of_next.clone());
                let cond_vars = match *cnd.clone() {
                    x => match x {
                        // https://github.com/rust-lang/rust/issues/16223
                        X86::EqP(left, right) => {
                            match (left, right) {
                                (X86Arg::Var(l), X86Arg::Var(r)) => vec![l, r],
                                (X86Arg::Var(l), _) => vec![l],
                                (_, X86Arg::Var(r)) => vec![r],
                                _ => vec![],
                            }
                        },
                        _ => panic!("if cond needs to be EqP"),
                    }
                };

                let mut live = lives.clone();
                live = live.union(&lives).cloned().collect();
                live = live.union(&thn_lives).cloned().collect();
                live = live.union(&els_lives).cloned().collect();

                live_of_next = live.clone();
                live_after_sets.push(live);

                new_instrs.push(X86::IfWithLives(
                    cnd,
                    new_thns, thn_live_sets,
                    new_elss, els_live_sets));
            },

            _ => {
                let (all_vars, read_vars, written_vars) =
                    instruction_rw(instr.clone());
                let mut live = live_of_next.clone();
                let written_vars_set : HashSet<_> = 
                    written_vars.iter().cloned().collect();
                live = live.difference(&written_vars_set).cloned().collect();
                let read_vars_set : HashSet<_> = read_vars.iter().cloned().collect();
                live = live.union(&read_vars_set).cloned().collect();
                
                live_of_next = live.clone();
                live_after_sets.push(live);
                new_instrs.push(instr);
            },
        }
    };

    live_after_sets.reverse();
    new_instrs.reverse();
    return (live_of_next, live_after_sets, new_instrs);
}

fn uncover_live(prog: X86) -> X86 {
    match prog {
        X86::Prog(instrs, vars) => {
            let (_, live_sets, new_instrs) = get_live_after_sets(instrs, HashSet::new());
            return X86::ProgWithLives(new_instrs, vars, live_sets);
        },
        _ => panic!("prog is not a top-level Prog"),
    }
}

// For each variable, figure out the interval when it is live. Results
// are inserted into live_intervals.
fn compute_live_intervals(instrs: Vec<X86>, live_sets: Vec<HashSet<String>>,
                          live_intervals: &mut HashMap<String, (i32, i32)>,
                          init_line_num: i32) {
    let mut line_num = init_line_num;
    let instr_live_sets : Vec<_> = instrs.iter().zip(live_sets).collect();
    for (instr, live_set) in instr_live_sets {
        match (instr.clone(), live_set.clone()) {
            (X86::IfWithLives(cnd, thns, thn_lives,
                              elss, els_lives), _) => {
                compute_live_intervals(thns.clone(), thn_lives, live_intervals, line_num);
                compute_live_intervals(elss.clone(), els_lives, live_intervals, line_num);
                line_num = line_num + thns.len() as i32 + elss.len() as i32;
            },
            (_, _) => {
                for v in live_set {
                    match live_intervals.get(&v) {
                        Some(&(start, end)) => {
                            live_intervals.insert(v, (start, line_num));
                        },
                        None => {
                            live_intervals.insert(v, (line_num-1, line_num));
                        },
                    }
                }
                line_num = line_num + 1;
            },
        }
    }
}

// Allocate registers for variables. If it can't find a free register,
// the variable won't be present as a key in the returned hash-map
fn allocate_registers(live_intervals: HashMap<String, (i32, i32)>)
                      -> HashMap<String, i32> {
    let mut live_intervals_vec = vec![];
    for (v, live_interval) in live_intervals {
        live_intervals_vec.push((v, live_interval));
    }
    live_intervals_vec.sort_by_key(|interval| interval.clone().0);
    let mut mapping : HashMap<String, i32> = HashMap::new();
    let mut free = vec![1,2];
    let mut alloc : HashSet<i32> = HashSet::new();
    let mut active_intervals : Vec<(String, (i32, i32))> = vec![];

    for (v, (start, end)) in live_intervals_vec.clone() {
        // clear done intervals from alloc, and free registers
        // allocated to them
        for (i, &(ref a, (astart, aend))) in active_intervals.clone().iter().enumerate() {
            if aend < start {
                active_intervals.remove(i);

                match mapping.get(a) {
                    Some(reg) => {
                        free.push(reg.clone());
                    },
                    None => (),
                }
            }
        }

        // allocate free register, if any.
        if free.len() > 0 {
            mapping.insert(v.clone(), free.pop().unwrap());
        }

        // add current to active_intervals
        active_intervals.push((v.clone(), (start, end)));
    }
    return mapping;
}

fn assign_homes_to_op2(locs: &HashMap<String, X86Arg>,
                       src: X86Arg, dest: X86Arg) -> (X86Arg, X86Arg) {
    match (dest.clone(), src.clone()) {
        (X86Arg::Var(d), X86Arg::Var(s)) =>
            (locs.get(&d).unwrap().clone(),
             locs.get(&s).unwrap().clone()),
        (X86Arg::Var(d), _) =>
            (locs.get(&d).unwrap().clone(),
             src),
        (_, X86Arg::Var(s)) =>
            (dest, locs.get(&s).unwrap().clone()),
        (X86Arg::Reg(reg), _) =>
            (dest, src) ,
        _ => panic!("unreachable"),
    }
}

// Given a list of instructions and mapping from vars to
// "homes"(register/stack location), return a new list of instructions
// with vars replaced with their assigned homes.
fn assign_homes_to_instrs(instrs: Vec<X86>, locs: HashMap<String, X86Arg>) -> Vec<X86> {
    let mut new_instrs = vec![];
    for i in instrs {
        match i {
            X86::IfWithLives(cnd, thn, thn_lives, els, els_lives) => {
                let new_cnd = match *cnd {
                    x => match x {
                        // https://github.com/rust-lang/rust/issues/16223
                        X86::EqP(left, right) => {
                        let new_left = match left {
                            X86Arg::Var(v) => locs.get(&v).unwrap().clone(),
                            _ => left,
                        };
                        X86::EqP(new_left, right)
                        },
                        _ => panic!("if cond should be an EqP"),
                    },
                };
                let new_thn = assign_homes_to_instrs(thn, locs.clone());
                let new_els = assign_homes_to_instrs(els, locs.clone());
                new_instrs.push(
                    X86::If(Box::new(new_cnd), new_thn, new_els)
                );
            },
            X86::Mov(dest, src) => {
                let (new_dest, new_src) = assign_homes_to_op2(&locs, src, dest);
                new_instrs.push(X86::Mov(new_dest, new_src))
            },
            X86::Add(dest, src) => {
                let (new_dest, new_src) = assign_homes_to_op2(&locs, src, dest);
                new_instrs.push(X86::Add(new_dest, new_src))
            },
            _ => panic!("NYI: {:?}", i),
        }
    };
    
    return new_instrs;
}

fn assign_homes(prog: X86) -> X86 {
    let regs = vec![Reg::RBX, Reg::RDX, Reg::RCX];
    match prog {
        X86::ProgWithLives(instrs, vars, live_sets) => {
            let mut live_intervals = HashMap::new();
            compute_live_intervals(instrs.clone(),
                                   live_sets, 
                                   &mut live_intervals, 1);
            let reg_alloc = allocate_registers(live_intervals);
            let mut locs = HashMap::new();
            let mut stack_size = 0;
            for var in vars.clone() {
                locs.insert(
                    var.clone(),
                    match reg_alloc.get(&var) {
                        Some(reg) => X86Arg::Reg(regs[reg.clone() as usize].clone()),
                        None => {
                            stack_size += 1;
                            X86Arg::RegOffset(Reg::RBP, stack_size * -8)
                        },
                    }
                    );
                };
            return X86::Prog(assign_homes_to_instrs(instrs, locs), vars);
        },
        _ => panic!("assign_homes: not top level prog"),
    }
}

fn lower_if (instr: X86) -> Vec<X86> {
    match instr {
        X86::If(cnd, thn, els) => {
            let (eqp_left, eqp_right) = match *cnd {
                x => match x {
                    // https://github.com/rust-lang/rust/issues/16223
                    X86::EqP(left, right) => (left, right),
                    _ => panic!("if cond is always EqP"),
                },
            };
            let thn_label = get_unique_varname("then");
            let end_label = get_unique_varname("endif");

            let mut new_elss = vec![];
            for i in els {
                new_elss.extend_from_slice(&lower_if(i));
            }
            let mut new_thns = vec![];
            for i in thn {
                new_thns.extend_from_slice(&lower_if(i));
            }

            let mut if_instrs = vec![
                X86::Cmp(eqp_right, eqp_left),
                X86::JmpIf(CC::E, thn_label.clone()),
            ];
            if_instrs.append(&mut new_elss);
            if_instrs.extend_from_slice(&[
                X86::Jmp(end_label.clone()),
                X86::Label(thn_label),
            ]);
            if_instrs.append(&mut new_thns);
            if_instrs.extend_from_slice(&[
                X86::Label(end_label),
            ]);

            return if_instrs;
        },
        _ => vec![instr],
    }
}

fn lower_conditionals(prog: X86) -> X86 {
    match prog {
        X86::Prog(instrs, vars) => {
            let mut new_instrs = vec![];
            for i in instrs {
                new_instrs.extend_from_slice(&lower_if(i));
            }

            return X86::Prog(new_instrs, vars);
        }
        _ => panic!("lower_conditionals: not top-level Prog"),
    }
}

fn patch_single_instr(instr: X86) -> Vec<X86> {
    match instr {
        // both source and dest are indirect addresses
        X86::Mov(X86Arg::RegOffset(Reg::RBP, dest), 
                 X86Arg::RegOffset(Reg::RBP, src)) => {
            vec![X86::Mov(X86Arg::Reg(Reg::RAX),
                          X86Arg::RegOffset(Reg::RBP, src)),
                 X86::Mov(X86Arg::RegOffset(Reg::RBP, dest), 
                          X86Arg::Reg(Reg::RAX))]
        },
        // both source and dest are indirect addresses
        X86::Add(X86Arg::RegOffset(Reg::RBP, dest), 
                 X86Arg::RegOffset(Reg::RBP, src)) => {
            vec![X86::Mov(X86Arg::Reg(Reg::RAX),
                          X86Arg::RegOffset(Reg::RBP, dest)),
                 X86::Add(X86Arg::Reg(Reg::RAX),
                          X86Arg::RegOffset(Reg::RBP, src)),
                 X86::Mov(X86Arg::RegOffset(Reg::RBP, dest),
                          X86Arg::Reg(Reg::RAX))
            ]
        },
        _ => vec![instr],
    }
}

fn patch_instructions(prog: X86) -> X86 {
    match prog {
        X86::Prog(instrs, vars) => {
            let mut patched_instrs = vec![];
            for i in instrs {
                patched_instrs.extend_from_slice(&patch_single_instr(i));
            }
            return X86::Prog(patched_instrs, vars);
        },
        _ => panic!("patch_instructions: not top-level Prog"),
    }
}


fn display_reg(reg: Reg) -> String {
    match reg {
        Reg::RAX => "rax",
        Reg::RBX => "rbx",
        Reg::RBP => "rbp",
        Reg::RDX => "rdx",
        Reg::RCX => "rcx",
    }.to_string()
}

fn print_x86_arg(arg: X86Arg) -> String {
    match arg {
        X86Arg::Reg(r) => format!("{}", display_reg(r)),
        X86Arg::Imm(n) => format!("{}", n),
        X86Arg::RegOffset(r, offset) => format!("QWORD [{}{}]", 
                                                display_reg(r), offset),
        _ => panic!("invalid arg type"),
    }
}

fn print_CC(cc: CC) -> String {
    match cc {
        CC::E => "e", 
        CC::L => "l", 
        CC::LE => "le", 
        CC::G => "g", 
        CC::GE => "ge",
    }.to_string()
}

fn print_instr(instr: X86) -> String {
    let instr_string = match instr.clone() {
        X86::Mov(dest, src) => format!("mov {}, {}", 
                                       print_x86_arg(dest), 
                                       print_x86_arg(src)),
        X86::Add(dest, src) => format!("add {}, {}", 
                                       print_x86_arg(dest), 
                                       print_x86_arg(src)),
        X86::Cmp(left, right) => format!("cmp {}, {}",
                                        print_x86_arg(left), 
                                        print_x86_arg(right)),
        X86::JmpIf(cc, label) => format!("j{} {}",
                                         print_CC(cc),
                                         label),
        X86::Jmp(label) => format!("jmp {}", label),
        X86::Label(label) => format!("{}:", label),

        _ => panic!("invalid op"),
    };

    match instr {
        X86::Label(_) => return format!("{}\n", instr_string),
        _ => return format!("    {}\n", instr_string),
    }
}

fn print_x86(prog: X86) -> String {
    let prelude = "section .text
extern print_int
global main
main:
    push rbp
    mov rbp, rsp\n";
    // TODO: save/restore registers
    let postlude = "    mov rdi, rax
    call print_int

    mov rsp, rbp
    pop rbp
    ret\n";
    let mut instrs_str = match prog {
        X86::Prog(instrs, vars) => {
            let mut instrs_str = String::from(prelude);
            for i in instrs {
                instrs_str.push_str(&print_instr(i));
            }
            instrs_str
        },
        _ => panic!("print_x86: not top-level Prog"),
    };

    instrs_str.push_str(postlude);
    return instrs_str;
}


fn read_input() -> io::Result<()> {
    let args : Vec<_> = env::args().collect();
    if args.len() < 2 {
        panic!("usage: {} filename", args[0].clone());
    }

    let mut f = try!(File::open(args[1].clone()));
    let mut input = String::new();
    try!(f.read_to_string(&mut input));

    let mut lexer = LexerState {
        s: input,
        pos: 0,                 // absolute position
        col: 1,                 // column in line
        line_num: 1,
        tok_buf: None,
    };

    let mut uniquify_mapping = HashMap::new();

    let mut toplevel = vec![];
    let mut sexpr = read(&mut lexer);
    while sexpr != SExpr::EOF {
        toplevel.push(sexpr);
        sexpr = read(&mut lexer);
    }

    let uniquified = uniquify(&mut uniquify_mapping,
                              SExpr::Prog(toplevel[..toplevel.len()-1].to_vec(),
                                          Box::new(toplevel[toplevel.len()-1].clone())));
    let flattened = flatten(uniquified);
    
    // let (flat_prog, prog_assigns, prog_vars) =
    //     match  {
    //         FlatResult::Prog(defs, main_assigns, main_vars) =>
    //             (main_assigns, main_vars),
    //         _ => panic!("unreachable"),
    //     };
    let instrs = select_instructions(flattened);
    let instrs = uncover_live(instrs);

    println!("{}", print_x86(patch_instructions(lower_conditionals(assign_homes(instrs)))));

    Ok(())
}

fn main() {
    read_input();
}
