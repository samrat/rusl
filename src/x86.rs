use std::rc::Rc;
use std::collections::HashSet;

use ast::CC;
use anf::{Flat, FlatResult};
use util::get_unique_varname;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
enum Reg {
    AL,

    RAX, RBX, RBP, RCX, RDX, RDI, RSI,
    R8, R9, R10, R11, R12, R13, R14, R15,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
enum X86Arg {
    Reg(Reg),
    Imm(u64),
    RegOffset(Reg, i64),
    GlobalVal(Rc<String>),
    FuncName(Rc<String>),
    Var(Rc<String>),     // pseudo-x86
}


// TODO: It might be a good idea to pull the constructors pertaining
// to pseudo-x86 into a separate datatype.
#[derive(Debug, Clone)]
pub enum X86 {
    Mov(X86Arg, X86Arg),
    Add(X86Arg, X86Arg),
    Sub(X86Arg, X86Arg),
    Neg(X86Arg),
    Cmp(X86Arg, X86Arg),
    And(X86Arg, X86Arg),
    Push(Reg),
    Pop(Reg),
    Set(X86Arg, CC),
    MovZx(X86Arg, X86Arg),
    EqP(X86Arg, X86Arg),          // pseudo-X86
    If(Box<X86>, Vec<X86>, Vec<X86>), // pseudo-X86

    // pseudo-X86
    IfWithLives(Box<X86>,                      // cond
                Vec<X86>,                      // then
                Vec<HashSet<Rc<String>>>,          // then-live-sets
                Vec<X86>,                      // else
                Vec<HashSet<Rc<String>>>           // else-live-sets
    ),
    Define(Rc<String>, Vec<Rc<String>>, Vec<X86>),
    DefineWithLives(Rc<String>,               //  name
                    Vec<Rc<String>>,          // vars
                    Vec<HashSet<Rc<String>>>, // live_sets
                    Vec<X86>,             // instrs
    ),
    DefineWithStackSize(Rc<String>, // name
                        i64,    // stack size
                        Vec<X86>, // instrs
                        ),

    Prog(Vec<X86>,              // defines
         Vec<X86>,              // main-instructions
         Vec<Rc<String>>            // main-vars
    ),

    ProgWithLives(Vec<X86>,     // defines
                  Vec<X86>,     // main-instructions
                  Vec<Rc<String>>,  // main-vars
                  Vec<HashSet<Rc<String>>> // live-sets
    ),

    ProgWithStackSize(Vec<X86>,     // defines
                      Vec<X86>,     // main-instructions
                      i64,          // stack size
    ),
    Call(X86Arg),
    JmpIf(CC, Rc<String>),
    Jmp(Rc<String>),
    Je(Rc<String>),
    Jne(Rc<String>),
    Label(Rc<String>),
}

const CONST_TRUE : u64  = 0xffffffffffffffff;
const CONST_FALSE : u64 = 0x7fffffffffffffff;

// R11 is used to point to heap
const CALLEE_SAVE_REGS : [Reg;5] =
    [Reg::RBX, Reg::R12, Reg::R13, Reg::R14, Reg::R15
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

// NOTE: registers to use during register allocation. Currently,
// registers that are used in passing arguments are excluded because
// the register allocation pass does not account for interferences
// between registers and variables.
const REGS : [Reg;6] = [
    // callee-save
    Reg::RBX, Reg::R12, Reg::R13, Reg::R14, Reg::R15,

    // caller-save
    Reg::R10,
    // Reg::RDX, Reg::RCX, Reg::RSI, Reg::RDI,
    // Reg::R8, Reg::R9, Reg::R11
];

fn flat_arg_type(v: &Flat) -> X86Arg {
    match v {
        &Flat::Symbol(ref name) => X86Arg::Var(name.clone()),
        &Flat::FuncName(ref name) => X86Arg::FuncName(name.clone()),
        &Flat::Number(n) => X86Arg::Imm((n << 1) as u64),
        &Flat::Bool(b) => {
            match b {
                true => X86Arg::Imm(CONST_TRUE),
                false => X86Arg::Imm(CONST_FALSE),
            }
        },
        &_ => {
            panic!("flat_arg_type: compound expression");
        },
    }
}

// Emits runtime checks to verify that a is an
// integer(ie. least-significant bit of `a` is zero)
fn ensure_number(a: X86Arg) -> Vec<X86> {
    vec![X86::Push(Reg::RCX),
         X86::Mov(X86Arg::Reg(Reg::RCX), a),
         X86::And(X86Arg::Reg(Reg::RCX), X86Arg::Imm(1)),
         X86::Cmp(X86Arg::Reg(Reg::RCX), X86Arg::Imm(0)),
         X86::Jne(Rc::new("internal_error_non_number".to_string())),
         X86::Pop(Reg::RCX)]
}

// Same as ensure_number but for booleans
fn ensure_bool(a: X86Arg) -> Vec<X86> {
    let error_label = Rc::new("internal_error_non_bool".to_string());
    vec![X86::Push(Reg::RCX),
         X86::Mov(X86Arg::Reg(Reg::RCX), a.clone()),
         X86::And(X86Arg::Reg(Reg::RCX), X86Arg::Imm(1)),
         X86::Cmp(X86Arg::Reg(Reg::RCX), X86Arg::Imm(0)),
         X86::Je(error_label.clone()),

         // second least significant bit
         X86::Mov(X86Arg::Reg(Reg::RCX), a),
         X86::And(X86Arg::Reg(Reg::RCX), X86Arg::Imm(2)),
         X86::Cmp(X86Arg::Reg(Reg::RCX), X86Arg::Imm(0)),
         X86::Je(error_label.clone()),
         X86::Pop(Reg::RCX)]
}

fn ensure_tuple(a: X86Arg) -> Vec<X86> {
    let error_label = Rc::new("internal_error_non_tuple".to_string());
    vec![X86::Push(Reg::RCX),
         X86::Mov(X86Arg::Reg(Reg::RCX), a.clone()),
         X86::And(X86Arg::Reg(Reg::RCX), X86Arg::Imm(1)),
         X86::Cmp(X86Arg::Reg(Reg::RCX), X86Arg::Imm(0)),
         X86::Je(error_label.clone()),

         // second least significant bit
         X86::Mov(X86Arg::Reg(Reg::RCX), a),
         X86::And(X86Arg::Reg(Reg::RCX), X86Arg::Imm(2)),
         X86::Cmp(X86Arg::Reg(Reg::RCX), X86Arg::Imm(0)),
         X86::Jne(error_label.clone()),

         X86::Pop(Reg::RCX)]
}


// convert one Flat instruction to pseudo-x86
fn flat_to_px86(instr: Flat) -> Vec<X86> {
    match instr {
        Flat::Assign(dest, e) => {
            match *e {
                Flat::FuncName(name) => vec![X86::Mov(X86Arg::Var(dest),
                                                      X86Arg::FuncName(name))],
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
                                        panic!("`+` expects 2 arguments");
                                    },
                                };
                                let mut ret = vec![X86::Mov(X86Arg::Var(dest.clone()),
                                                            flat_arg_type(arg1))];
                                ret.extend_from_slice(&ensure_number(flat_arg_type(arg1)));
                                ret.push(X86::Add(X86Arg::Var(dest),
                                                  flat_arg_type(arg2)));
                                ret.extend_from_slice(&ensure_number(flat_arg_type(arg2)));
                                return ret;
                            },
                            "-" => {
                                let arg = match &args[..] {
                                    &[ref arg] => arg,
                                    _ => {
                                        panic!("`-` expects 1 argument");
                                    },
                                };
                                let mut ret = vec![X86::Mov(X86Arg::Var(dest.clone()),
                                                            flat_arg_type(arg))];
                                ret.extend_from_slice(&ensure_number(flat_arg_type(arg)));
                                ret.push(X86::Neg(X86Arg::Var(dest.clone())));
                                return ret;
                            },
                            "tuple-ref" => {
                                let (tuple, index) = match &args[..] {
                                    &[ref tuple, ref index] => (tuple, index),
                                    _ => {
                                        panic!("`tuple-ref` expects 2 arguments");
                                    },
                                };

                                let index = match index {
                                    &Flat::Number(n) => n,
                                    &_ => panic!("index to tuple-ref must be a literal number"),
                                };

                                let mut ret = ensure_tuple(flat_arg_type(tuple));
                                ret.extend_from_slice(&[
                                    X86::Mov(X86Arg::Reg(Reg::R11), flat_arg_type(tuple)),
                                    // subtract 1 from tuple tag
                                    X86::Sub(X86Arg::Reg(Reg::R11), X86Arg::Imm(1)),
                                    // NOTE: first word contains count
                                    X86::Mov(X86Arg::Var(dest), X86Arg::RegOffset(Reg::R11,
                                                                                  8*(index+1)))
                                ]);

                                return ret;
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
                            X86::Call(X86Arg::FuncName(f)),
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
                        let false_label = Rc::new(get_unique_varname("false"));
                        let done_label = Rc::new(get_unique_varname("done"));

                        let mut ret = ensure_number(flat_arg_type(&*left));

                        ret.extend_from_slice(&ensure_number(flat_arg_type(&*right)));
                        ret.extend_from_slice(&[X86::Cmp(flat_arg_type(&*left),
                                                         flat_arg_type(&*right)),
                                                X86::Set(X86Arg::Reg(Reg::AL), cc),
                                                X86::MovZx(X86Arg::Reg(Reg::RAX), X86Arg::Reg(Reg::AL)),
                                                X86::Cmp(X86Arg::Reg(Reg::RAX), X86Arg::Imm(0)),
                                                X86::Je(false_label.clone()),
                                                X86::Mov(X86Arg::Var(dest.clone()), X86Arg::Imm(CONST_TRUE)),
                                                X86::Jmp(done_label.clone()),
                                                X86::Label(false_label),
                                                X86::Mov(X86Arg::Var(dest), X86Arg::Imm(CONST_FALSE)),
                                                X86::Label(done_label),]);
                        return ret;
                    },
                    Flat::Tuple(elts) => {
                        // with count in first word
                        let len = elts.len() + 1;
                        let total_len = 8*(len + (len % 2));
                        let free_ptr_str = Rc::new("free_ptr".to_string());
                        let mut instrs =
                            vec![X86::Mov(X86Arg::Var(dest.clone()),
                                          X86Arg::GlobalVal(free_ptr_str.clone())),
                                 X86::Add(X86Arg::GlobalVal(free_ptr_str.clone()),
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
                    Flat::Number(_) => (flat_arg_type(&x), X86Arg::Imm(CONST_TRUE)),
                    Flat::Symbol(_) => (flat_arg_type(&x), X86Arg::Imm(CONST_TRUE)),
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

/// convert a Flat expression into pseudo-x86 instructions. pseudo-x86
/// is like x86 but with if's and temporaries. It is also "unpatched"
/// (see `patch_instructions`)
pub fn select_instructions(flat_prog: FlatResult) -> X86 {

    match flat_prog {
        FlatResult::Define(name, args, assigns, mut vars) =>
        {
            // TODO: if more than 6 args, spill args to stack
            assert!(args.len() <= 6);
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
