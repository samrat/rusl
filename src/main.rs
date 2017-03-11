#![feature(advanced_slice_patterns, slice_patterns)]
#![feature(box_syntax)]

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

use lexer::LexerState;

use parser::{SExpr, CC};
use parser::read;

use anf::{Flat,FlatResult};
use anf::flatten;



#[derive(Debug, Clone)]
enum Reg {
    AL,
    
    RAX, RBX, RBP, RCX, RDX, RDI, RSI,
    R8, R9, R10, R11, R12, R13, R14, R15,
}

#[derive(Debug, Clone)]
enum X86Arg {
    Reg(Reg),
    Imm(u64),
    RegOffset(Reg, i64),
    GlobalVal(String),
    Var(String),     // pseudo-x86
}


// TODO: It might be a good idea to pull the constructors pertaining
// to pseudo-x86 into a separate datatype.
#[derive(Debug, Clone)]
enum X86 {
    Mov(X86Arg, X86Arg),
    Add(X86Arg, X86Arg),
    Sub(X86Arg, X86Arg),
    Neg(X86Arg),
    Cmp(X86Arg, X86Arg),
    Push(Reg),
    Pop(Reg),
    Set(X86Arg, CC),
    MovZx(X86Arg, X86Arg),
    EqP(X86Arg, X86Arg),          // pseudo-X86
    If(Box<X86>, Vec<X86>, Vec<X86>), // pseudo-X86

    // pseudo-X86
    IfWithLives(Box<X86>,                      // cond
                Vec<X86>,                      // then
                Vec<HashSet<String>>,          // then-live-sets
                Vec<X86>,                      // else
                Vec<HashSet<String>>           // else-live-sets
    ),
    Define(String, Vec<String>, Vec<X86>),
    DefineWithLives(String,               //  name
                    Vec<String>,          // vars
                    Vec<HashSet<String>>, // live_sets 
                    Vec<X86>,             // instrs
    ),
    DefineWithStackSize(String, // name
                        i64,    // stack size
                        Vec<X86>, // instrs
                        ),
    
    Prog(Vec<X86>,              // defines
         Vec<X86>,              // main-instructions
         Vec<String>            // main-vars
    ),

    ProgWithLives(Vec<X86>,     // defines
                  Vec<X86>,     // main-instructions
                  Vec<String>,  // main-vars
                  Vec<HashSet<String>> // live-sets
    ),

    ProgWithStackSize(Vec<X86>,     // defines
                      Vec<X86>,     // main-instructions
                      i64,          // stack size
    ),
    Call(String),
    JmpIf(CC, String),
    Jmp(String),
    Label(String),
}

const CONST_TRUE : u64  = 0xffffffffffffffff;
const CONST_FALSE : u64 = 0x7fffffffffffffff;

// R15 is used to point to rootstack
// R11 is used to point to heap
const CALLEE_SAVE_REGS : [Reg;4] =
    [Reg::RBX, Reg::R12, Reg::R13, Reg::R14, // Reg::R15
    ];
const CALLER_SAVE_REGS : [Reg;7] =
    [Reg::RDX, Reg::RCX, Reg::RSI, Reg::RDI, 
     Reg::R8, Reg::R9, Reg::R10, // Reg::R11
    ];
// order of registers in which to place first 6 arguments
const ARG_REG_ORDER : [Reg; 6] = [Reg::RDI,
                                  Reg::RSI,
                                  Reg::RDX,
                                  Reg::RCX,
                                  Reg::R8,
                                  Reg::R9];
const REGS : [Reg;11] = [
    // callee-save
    Reg::RBX, Reg::R12, Reg::R13, Reg::R14, // Reg::R15,

    // caller-save
    Reg::RDX, Reg::RCX, Reg::RSI, Reg::RDI, 
    Reg::R8, Reg::R9, Reg::R10, // Reg::R11
];

// uniquify variable names. This function simply adds a monotonically
// increasing counter(VAR_COUNTER) to each and every variable.
fn uniquify(mapping: &mut HashMap<String, String>, expr: SExpr) 
            -> SExpr {
    match expr {
        SExpr::Symbol(name) => 
            SExpr::Symbol(mapping.get(&name).unwrap().to_string()),
        SExpr::Number(_) => expr,
        SExpr::Bool(_) => expr,
        SExpr::Tuple(elts) => {
            let elts = elts.iter()
                .map(|e| uniquify(mapping, e.clone()))
                .collect();
            SExpr::Tuple(elts)
        },
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
        SExpr::List(mut elts) => {
            elts = elts.iter().map(|e| uniquify(mapping, e.clone())).collect();
            
            return SExpr::List(elts);
        }
        SExpr::Cmp(cc, left, right) => 
            return SExpr::Cmp(cc,
                              box uniquify(mapping, *left),
                              box uniquify(mapping, *right)),
        SExpr::Define(name, args, val) => {
            let uniq_fname = get_unique_varname(&name);
            mapping.insert(name, uniq_fname.clone());

            let mut new_args = vec![];
            for arg in args {
                let new_arg = get_unique_varname(&arg);
                new_args.push(new_arg.clone());
                mapping.insert(arg, new_arg);
            }

            return SExpr::Define(uniq_fname,
                                 new_args,
                                 Box::new(uniquify(mapping, *val)));
        },
        SExpr::If(cond, thn, els) => {
            return SExpr::If(Box::new(uniquify(mapping, *cond)),
                             Box::new(uniquify(mapping, *thn)),
                             Box::new(uniquify(mapping, *els)));
        },
        SExpr::App(f, mut args) => {
            args = args.iter().map(|a| uniquify(mapping, a.clone())).collect();
            return SExpr::App(box uniquify(mapping, *f),
                              args);
        },
        SExpr::Prog(mut defs, e) => {
            defs = defs.iter().map(|def| uniquify(mapping, def.clone())).collect();
            return SExpr::Prog(defs, Box::new(uniquify(mapping, *e)))
        },
        SExpr::EOF => {
            error!("Don't know what to do with EOF");
            process::exit(0);
        },
    }
}


fn flat_arg_type(v: &Flat) -> X86Arg {
    match v {
        &Flat::Symbol(ref name) => X86Arg::Var(name.clone()),
        &Flat::Number(n) => X86Arg::Imm((n << 1) as u64),
        &Flat::Bool(b) => {
            match b {
                true => X86Arg::Imm(CONST_TRUE),
                false => X86Arg::Imm(CONST_FALSE),
            }
        },
        &_ => {
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
                Flat::Symbol(name) => vec![X86::Mov(X86Arg::Var(dest), 
                                                    X86Arg::Var(name))],
                Flat::Number(n) => vec![X86::Mov(X86Arg::Var(dest),
                                                 X86Arg::Imm((n << 1) as u64))],
                Flat::Bool(b) => {
                    let bval = match b {
                        true => CONST_TRUE,
                        false => CONST_FALSE,
                    };
                    return vec![X86::Mov(X86Arg::Var(dest), 
                                         X86Arg::Imm(bval as u64))];
                },
                // https://github.com/rust-lang/rust/issues/16223
                x => match x {
                    Flat::Prim(f, args) => {
                        match &f[..] {
                            "+" => {
                                let (arg1, arg2) = match &args[..] {
                                    &[ref arg1, ref arg2] => (arg1, arg2),
                                    _ => {
                                        error!("`+` expects 2 arguments");
                                        process::exit(0);
                                    },
                                };
                                return vec![
                                    X86::Mov(X86Arg::Var(dest.clone()),
                                             flat_arg_type(arg1)),
                                    X86::Add(X86Arg::Var(dest),
                                             flat_arg_type(arg2))
                                ];
                            },
                            "-" => {
                                let arg = match &args[..] {
                                    &[ref arg] => arg,
                                    _ => {
                                        error!("`-` expects 1 argument");
                                        process::exit(0);
                                    },
                                };
                                return vec![
                                    X86::Mov(X86Arg::Var(dest.clone()),
                                             flat_arg_type(arg)),
                                    X86::Neg(X86Arg::Var(dest.clone()))
                                ];
                            },
                            "tuple-ref" => {
                                let (tuple, index) = match &args[..] {
                                    &[ref tuple, ref index] => (tuple, index),
                                    _ => {
                                        error!("`tuple-ref` expects 2 arguments");
                                        process::exit(0);
                                    },
                                };

                                let index = match index {
                                    &Flat::Number(n) => n,
                                    &_ => panic!("index to tuple-ref must be a literal number"),
                                };

                                return vec![
                                    X86::Mov(X86Arg::Reg(Reg::R11), flat_arg_type(tuple)),
                                    // subtract 1 from tuple tag
                                    X86::Sub(X86Arg::Reg(Reg::R11), X86Arg::Imm(1)),
                                    // NOTE: first word contains count
                                    X86::Mov(X86Arg::Var(dest), X86Arg::RegOffset(Reg::R11,
                                                                                  8*(index+1)))
                                ];
                            },
                            _ => panic!("primitive not defined"),
                        }
                    },
                    Flat::App(f, args) => {
                        let mut instrs = vec![];

                        // push caller-save-regs
                        for r in CALLER_SAVE_REGS.iter() {
                            instrs.push(X86::Push(r.clone()));
                        }

                        // TODO: if more than 6 args, spill args to stack
                        // push args
                        for (i, arg) in args.iter().map(|a| flat_arg_type(a)).enumerate() {
                            instrs.push(
                                X86::Mov(X86Arg::Reg(ARG_REG_ORDER[i].clone()),
                                         arg)
                            );
                        }

                        instrs.extend_from_slice(&[
                            X86::Call(f),
                        ]);

                        // pop caller-save regs
                        for r in CALLER_SAVE_REGS.iter().rev() {
                            instrs.push(X86::Pop(r.clone()));
                        }

                        instrs.extend_from_slice(&[
                            X86::Mov(X86Arg::Var(dest), X86Arg::Reg(Reg::RAX))
                        ]);

                        return instrs;
                    },
                    Flat::Cmp(cc, left, right) => {
                        vec![X86::Cmp(flat_arg_type(&*left), 
                                      flat_arg_type(&*right)),
                             X86::Set(X86Arg::Reg(Reg::AL), cc),
                             X86::MovZx(X86Arg::Var(dest), X86Arg::Reg(Reg::AL))]
                    },
                    Flat::Tuple(elts) => {
                        // with count in first word
                        let len = elts.len() + 1;
                        let total_len = 8*(len + (len % 2));
                        let mut instrs =
                            vec![X86::Mov(X86Arg::Var(dest.clone()),
                                          X86Arg::GlobalVal("free_ptr".to_string())),
                                 X86::Add(X86Arg::GlobalVal("free_ptr".to_string()),
                                          X86Arg::Imm(total_len as u64)),
                                 X86::Mov(X86Arg::Reg(Reg::R11),
                                          X86Arg::Var(dest.clone()))];
                        // store count in first word
                        instrs.extend_from_slice(&[
                            X86::Mov(X86Arg::RegOffset(Reg::R11, 0),
                                     X86Arg::Imm(elts.len() as u64))
                        ]);
                        
                        for (i, elt) in elts.iter().enumerate() {
                            instrs.push(
                                X86::Mov(X86Arg::RegOffset(Reg::R11,
                                                           8*(i+1) as i64),
                                         flat_arg_type(elt))
                            );
                        }

                        // add 1 to indicate it is a tuple
                        instrs.extend_from_slice(&[
                            X86::Add(X86Arg::Var(dest), X86Arg::Imm(1))
                        ]);

                        return instrs;
                    },
                    _ => {
                        println!("{:?}", x);
                        panic!("NYI")
                    },
                },
            }
        },
        Flat::Return(v) => {
            let val = flat_arg_type(&*v);
            return vec![X86::Mov(X86Arg::Reg(Reg::RAX), 
                                 val)]
        },
        Flat::If(cnd, thn, els) => {
            let (eq_left, eq_right) = match *cnd {
                x => match x {
                    // https://github.com/rust-lang/rust/issues/16223
                    Flat::Number(_) => (flat_arg_type(&x), X86Arg::Imm(1)),
                    Flat::Symbol(_) => (flat_arg_type(&x), X86Arg::Imm(1)),
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
            return vec![X86::If(Box::new(X86::EqP(eq_left, eq_right)),
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
        FlatResult::Define(name, args, assigns, mut vars) =>
        {
            // TODO: if more than 6 args, spill args to stack
            let mut move_args = vec![];
            for (i, arg) in args.iter().enumerate() {
                move_args.push(
                    X86::Mov(X86Arg::Var(arg.clone()), 
                             X86Arg::Reg(ARG_REG_ORDER[i].clone()))
                );
            }
            
            let mut x86_instrs = move_args;
            for i in assigns {
                let mut i_instrs = flat_to_px86(i);
                x86_instrs.append(&mut i_instrs);
            }

            vars.extend_from_slice(&args);
            return X86::Define(name,
                               vars,
                               x86_instrs);
        },
        
        FlatResult::Prog(defs, main_assigns, main_vars) => {
            let mut x86_defines = vec![];
            for def in defs {
                x86_defines.push(select_instructions(def));
            }
            
            let mut x86_instrs = vec![];
            for i in main_assigns {
                let mut i_instrs = flat_to_px86(i);
                x86_instrs.append(&mut i_instrs);
            }
            return X86::Prog(x86_defines, x86_instrs, main_vars);
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
        X86::Mov(X86Arg::Reg(_), X86Arg::Var(src)) => {
            return (vec![src.clone()],
                    vec![src],
                    vec![]);
        },
        X86::Mov(X86Arg::RegOffset(_, _), X86Arg::Var(src)) => {
            return (vec![src.clone()],
                    vec![src],
                    vec![]);
        },
        X86::Mov(_, _) => {
            return (vec![], vec![], vec![]);
        },
        X86::MovZx(_, _) => {
            return (vec![], vec![], vec![]);
        },
        X86::Set(_, _) =>  {
            return (vec![], vec![], vec![]);
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
        X86::Sub(X86Arg::Var(dest), X86Arg::Var(src)) |
        X86::Add(X86Arg::Var(dest), X86Arg::Var(src)) => {
            return (vec![dest.clone(), src.clone()],
                    vec![dest.clone(), src],
                    vec![dest]);
        },
        X86::Sub(X86Arg::Var(dest), _) |
        X86::Add(X86Arg::Var(dest), _) => {
            return (vec![dest.clone()],
                    vec![dest.clone()],
                    vec![dest]);
        },
        X86::Sub(X86Arg::Reg(_), X86Arg::Imm(_)) |
        X86::Add(X86Arg::GlobalVal(_), X86Arg::Imm(_)) |
        X86::Push(_) | X86::Pop(_) | X86::Call(_) =>
            return (vec![], vec![], vec![]),
        X86::Neg(X86Arg::Var(n)) => {
            return (vec![n.clone()],
                    vec![n.clone()],
                    vec![n.clone()]);
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

                let cond_vars : HashSet<_> = cond_vars.iter().cloned().collect();

                let mut live = lives.clone();
                live = live.union(&lives).cloned().collect();
                live = live.union(&cond_vars).cloned().collect();
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
                let (_, read_vars, written_vars) =
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
        X86::Define(name, vars, instrs) => {
            let (_, live_sets, new_instrs) = get_live_after_sets(instrs, HashSet::new());
            return X86::DefineWithLives(name, vars, live_sets, new_instrs);
        },
        
        X86::Prog(mut defs, instrs, vars) => {
            let (_, live_sets, new_instrs) = get_live_after_sets(instrs, HashSet::new());

            defs = defs.iter().map(|def| uncover_live(def.clone())).collect();
            return X86::ProgWithLives(defs, 
                                      new_instrs, 
                                      vars, 
                                      live_sets);
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
            (X86::IfWithLives(_, thns, thn_lives,
                              elss, els_lives), _) => {
                compute_live_intervals(thns.clone(), thn_lives, live_intervals, line_num);
                compute_live_intervals(elss.clone(), els_lives, live_intervals, line_num);
                line_num = line_num + thns.len() as i32 + elss.len() as i32;
            },
            (_, _) => {
                for v in live_set {
                    match live_intervals.get(&v) {
                        Some(&(start, _)) => {
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
    live_intervals_vec.sort_by_key(|interval| (interval.clone().1).0);

    let mut mapping : HashMap<String, i32> = HashMap::new();
    let mut free : Vec<i32> = (0..REGS.len()).map(|i| i as i32).collect();
    let mut active_intervals : HashSet<(String, (i32, i32))> = HashSet::new();
    for (v, (start, end)) in live_intervals_vec.clone() {
        // clear done intervals from active_intervals, and free
        // registers allocated to them
        for (a, (astart, aend)) in active_intervals.clone() {
            if aend < start {
                active_intervals.remove(&(a.clone(), (astart, aend)));
                match mapping.get(&a) {
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
        active_intervals.insert((v.clone(), (start, end)));
    }
    return mapping;
}

fn assign_homes_to_op2(locs: &HashMap<String, X86Arg>,
                       dest: X86Arg, src: X86Arg) -> (X86Arg, X86Arg) {
    match (dest.clone(), src.clone()) {
        (X86Arg::Var(d), X86Arg::Var(s)) =>
            (locs.get(&d).unwrap().clone(),
             locs.get(&s).unwrap().clone()),
        (X86Arg::Var(d), _) => {
            (locs.get(&d).unwrap().clone(),
             src)
        },
        (_, X86Arg::Var(s)) =>
            (dest, locs.get(&s).unwrap().clone()),
        
        (X86Arg::RegOffset(_, _), X86Arg::Imm(_)) |
        (X86Arg::GlobalVal(_), X86Arg::Imm(_)) |
        (X86Arg::Imm(_), X86Arg::Imm(_)) |
        (X86Arg::Reg(_), _) =>
            (dest, src),
        _ => panic!("unreachable: {:?}", (dest, src)),
    }
}

// Given a list of instructions and mapping from vars to
// "homes"(register/stack location), return a new list of instructions
// with vars replaced with their assigned homes.
fn assign_homes_to_instrs(instrs: Vec<X86>, locs: HashMap<String, X86Arg>) -> Vec<X86> {
    let mut new_instrs = vec![];
    for i in instrs {
        match i {
            X86::IfWithLives(cnd, thn, _, els, _) => {
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
                let (new_dest, new_src) = assign_homes_to_op2(&locs, dest, src);
                new_instrs.push(X86::Mov(new_dest, new_src))
            },
            X86::MovZx(dest, src) => {
                let (new_dest, new_src) = assign_homes_to_op2(&locs, dest, src);
                new_instrs.push(X86::MovZx(new_dest, new_src))
            },
            X86::Add(dest, src) => {
                let (new_dest, new_src) = assign_homes_to_op2(&locs, dest, src);
                new_instrs.push(X86::Add(new_dest, new_src))
            },
            X86::Cmp(left, right) => {
                let (new_left, new_right) =
                    assign_homes_to_op2(&locs, left, right);
                new_instrs.push(X86::Cmp(new_left, new_right))
            },
            X86::Neg(n) => {
                let new_n = match n {
                    X86Arg::Var(v) => locs.get(&v).unwrap().clone(),
                    _ => n,
                };

                new_instrs.push(X86::Neg(new_n))
            }
            X86::Set(X86Arg::Reg(_), _) |
            X86::Push(_) | X86::Pop(_) |
            X86::Sub(X86Arg::Reg(_), X86Arg::Imm(_)) |
            X86::Call(_) => {
                new_instrs.push(i);
            },
            _ => panic!("NYI: {:?}", i),
        }
    };
    
    return new_instrs;
}

fn decide_locs(vars: &Vec<String>, instrs: &Vec<X86>, 
               live_sets: Vec<HashSet<String>>) 
               -> (HashMap<String, X86Arg>, i64) {
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
                Some(reg) => X86Arg::Reg(REGS[reg.clone() as usize].clone()),
                None => {
                    stack_size += 1;
                    X86Arg::RegOffset(Reg::RBP, stack_size * -8)
                },
            }
        );
    };

    return (locs, stack_size);
}

fn assign_homes(prog: X86) -> X86 {
    match prog {
        X86::DefineWithLives(name, vars, live_sets, instrs) => {
            let (locs, stack_size) = decide_locs(&vars, &instrs, live_sets);
            return X86::DefineWithStackSize(name, stack_size, 
                                            assign_homes_to_instrs(instrs, locs));
        },
        
        X86::ProgWithLives(defs, instrs, vars, live_sets) => {
            let (locs, stack_size) = decide_locs(&vars, &instrs, live_sets);
            let mut new_defs = vec![];
            for def in defs {
                new_defs.push(assign_homes(def));
            }
            
            return X86::ProgWithStackSize(new_defs, assign_homes_to_instrs(instrs, locs), stack_size);
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
                X86::Cmp(eqp_left, eqp_right),
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
        X86::DefineWithStackSize(name, stack_size, mut instrs) => {
            instrs = instrs.iter().flat_map(|i| lower_if(i.clone())).collect();

            return X86::DefineWithStackSize(name, stack_size, instrs);
        },
        X86::ProgWithStackSize(mut defs, mut instrs, stack_size) => {
            instrs = instrs.iter().flat_map(|i| lower_if(i.clone())).collect();
            defs = defs.iter().map(|d| lower_conditionals(d.clone())).collect();
            
            return X86::ProgWithStackSize(defs, instrs, stack_size);
        }
        _ => panic!("lower_conditionals: not top-level Prog"),
    }
}

fn patch_single_instr(instr: X86) -> Vec<X86> {
    match instr {
        // both source and dest are indirect addresses
        X86::Mov(X86Arg::RegOffset(dest_reg, dest), 
                 X86Arg::RegOffset(src_reg, src)) => {
            vec![X86::Mov(X86Arg::Reg(Reg::RAX),
                          X86Arg::RegOffset(src_reg, src)),
                 X86::Mov(X86Arg::RegOffset(dest_reg, dest), 
                          X86Arg::Reg(Reg::RAX))]
        },
        X86::MovZx(X86Arg::RegOffset(dest_reg, offset),
                   src) => {
            vec![X86::MovZx(X86Arg::Reg(Reg::RAX), src),
                 X86::Mov(X86Arg::RegOffset(dest_reg, offset),
                          X86Arg::Reg(Reg::RAX))]
        },
        // both source and dest are indirect addresses
        X86::Add(X86Arg::RegOffset(dest_reg, dest), 
                 X86Arg::RegOffset(src_reg, src)) => {
            vec![X86::Mov(X86Arg::Reg(Reg::RAX),
                          X86Arg::RegOffset(dest_reg.clone(), dest)),
                 X86::Add(X86Arg::Reg(Reg::RAX),
                          X86Arg::RegOffset(src_reg, src)),
                 X86::Mov(X86Arg::RegOffset(dest_reg, dest),
                          X86Arg::Reg(Reg::RAX))
            ]
        },
        X86::Neg(X86Arg::RegOffset(reg, offset)) => {
            vec![X86::Mov(X86Arg::Reg(Reg::RAX),
                          X86Arg::RegOffset(reg.clone(), offset)),
                 X86::Neg(X86Arg::Reg(Reg::RAX)),
                 X86::Mov(X86Arg::RegOffset(reg, offset),
                          X86Arg::Reg(Reg::RAX))]
        },
        // cmp can't take imm64
        X86::Cmp(X86Arg::Imm(i), right) => {
            vec![X86::Mov(X86Arg::Reg(Reg::RAX), X86Arg::Imm(i)),
                 X86::Cmp(X86Arg::Reg(Reg::RAX), right)]
        },
        X86::Cmp(left, X86Arg::Imm(i)) => {
            vec![X86::Mov(X86Arg::Reg(Reg::RAX), X86Arg::Imm(i)),
                 X86::Cmp(left, X86Arg::Reg(Reg::RAX))]
        },
        _ => vec![instr],
    }
}

fn patch_instructions(prog: X86) -> X86 {
    match prog {
        X86::DefineWithStackSize(name, stack_size, instrs) => {
            let patched_instrs = 
                instrs.iter().flat_map(|i| patch_single_instr(i.clone())).collect();

            return X86::DefineWithStackSize(name, 
                                            stack_size, 
                                            patched_instrs);
        },
        X86::ProgWithStackSize(mut defs, instrs, stack_size) => {
            let patched_instrs = 
                instrs.iter().flat_map(|i| patch_single_instr(i.clone())).collect();

            defs = defs.iter().map(|d| patch_instructions(d.clone())).collect();

            return X86::ProgWithStackSize(defs, patched_instrs, stack_size);
        },
        _ => panic!("patch_instructions: not top-level Prog"),
    }
}


fn display_reg(reg: &Reg) -> String {
    match reg {
        &Reg::AL => "al",
        &Reg::RAX => "rax",
        &Reg::RBX => "rbx",
        &Reg::RBP => "rbp",
        &Reg::RDX => "rdx",
        &Reg::RCX => "rcx",
        &Reg::RDI => "rdi",
        &Reg::RSI => "rsi",
        &Reg::R8 => "r8",
        &Reg::R9 => "r9",
        &Reg::R10 => "r10",
        &Reg::R11 => "r11",
        &Reg::R12 => "r12",
        &Reg::R13 => "r13",
        &Reg::R14 => "r14",
        &Reg::R15 => "r15",
    }.to_string()
}

fn print_x86_arg(arg: X86Arg) -> String {
    match arg {
        X86Arg::Reg(r) => format!("{}", display_reg(&r)),
        X86Arg::Imm(n) => format!("{}", n),
        X86Arg::RegOffset(r, offset) => {
            if offset < 0 {
                format!("QWORD [{}{}]", 
                        display_reg(&r), 
                        offset)
            }
            else {
                format!("QWORD [{}+{}]", 
                        display_reg(&r), 
                        offset)
            }
        },
        X86Arg::GlobalVal(g) => format!("QWORD [rel {}]", g),
        _ => panic!("invalid arg type"),
    }
}

fn print_cc(cc: CC) -> String {
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
        X86::Sub(dest, src) => format!("sub {}, {}", 
                                       print_x86_arg(dest), 
                                       print_x86_arg(src)),
        X86::Cmp(left, right) => format!("cmp {}, {}",
                                        print_x86_arg(left), 
                                        print_x86_arg(right)),
        X86::JmpIf(cc, label) => format!("j{} {}",
                                         print_cc(cc),
                                         label),
        X86::Jmp(label) => format!("jmp {}", label),
        X86::Label(label) => format!("{}:", label),
        X86::Call(label) => format!("call {}", label),
        X86::Set(X86Arg::Reg(r), cc) =>
            format!("set{} {}", print_cc(cc), display_reg(&r)),
        X86::MovZx(dest, src) => format!("movzx {}, {}",
                                         print_x86_arg(dest),
                                         print_x86_arg(src)),
        X86::Neg(n) => format!("neg {}", print_x86_arg(n)),
        X86::Push(r) => format!("push {}", display_reg(&r)),
        X86::Pop(r) => format!("pop {}", display_reg(&r)),
        _ => panic!("invalid op: {:?}", instr),
    };

    match instr {
        X86::Label(_) => return format!("{}\n", instr_string),
        _ => return format!("    {}\n", instr_string),
    }
}

fn print_x86(prog: X86) -> String {
    let mut save_callee_save_regs = String::new();
    for r in CALLEE_SAVE_REGS.iter() {
        save_callee_save_regs.push_str(&format!("    push {}\n",
                                                display_reg(r)));
    }
    let mut restore_callee_save_regs = String::new();
    for r in CALLEE_SAVE_REGS.iter().rev() {
        restore_callee_save_regs.push_str(&format!("    pop {}\n",
                                                   display_reg(r)));
    }

    let instrs_str = match prog {
        X86::DefineWithStackSize(name, stack_size, instrs) => {
            let stack_size = 8 * stack_size;
            let prelude = format!("{}:
    push rbp
    mov rbp, rsp
{}
    sub rsp, {}\n", name, save_callee_save_regs, stack_size);
            let postlude = format!("    mov rdi, rax
    add rsp, {}
{}
    mov rsp, rbp
    pop rbp
    ret\n", stack_size,
                                   restore_callee_save_regs
            );

            let mut instrs_str = String::from(prelude);
            for i in instrs {
                instrs_str.push_str(&print_instr(i));
            }

            instrs_str.push_str(&postlude[..]);
            instrs_str
        },
        X86::ProgWithStackSize(defs, instrs, stack_size) => {
            let stack_size = 8 * stack_size;
            let mut defs_str = String::new();
            for def in defs {
                defs_str.push_str(&print_x86(def)[..]);
            }
            let prelude = format!("section .text
extern print
extern initialize
extern heap
extern rootstack
extern free_ptr
global main
main:
    push rbp
    mov rbp, rsp
{}
    sub rsp, {}
    call initialize
    mov r15, [rel heap]\n", save_callee_save_regs, stack_size);
            let postlude = format!("    mov rdi, rax
    call print
    add rsp, {}
{}
    mov rsp, rbp
    pop rbp
    ret\n", stack_size, restore_callee_save_regs);
            let mut instrs_str = String::from(prelude);
            for i in instrs {
                instrs_str.push_str(&print_instr(i));
            }
            instrs_str.push_str(&postlude[..]);
            instrs_str.push_str(&defs_str[..]);
            instrs_str
        },
        _ => panic!("print_x86: not top-level Prog"),
    };

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

    let mut uniquify_mapping : HashMap<String, String> = HashMap::new();
    for prim in ["+", "-", "tuple-ref", "tuple"].iter() {
        uniquify_mapping.insert(prim.to_string(), prim.to_string());
    }

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
    // println!("{:?}", flattened);
        
    let instrs = select_instructions(flattened);
    let instrs = uncover_live(instrs);
    
    let homes_assigned = assign_homes(instrs);

    let ifs_lowered = lower_conditionals(homes_assigned);
    let patched = patch_instructions(ifs_lowered);
    // println!("{:?}", patched);

    println!("{}", print_x86(patched));

    Ok(())
}

fn main() {
    let _ = read_input();
}
