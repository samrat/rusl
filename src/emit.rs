use x86::{Reg, X86Arg, X86,
          CALLEE_SAVE_REGS};
use ast::CC;

fn display_reg(reg: &Reg) -> String {
    match reg {
        Reg::AL => "al",
        Reg::RAX => "rax",
        Reg::RBX => "rbx",
        Reg::RBP => "rbp",
        Reg::RDX => "rdx",
        Reg::RCX => "rcx",
        Reg::RDI => "rdi",
        Reg::RSI => "rsi",
        Reg::R8 => "r8",
        Reg::R9 => "r9",
        Reg::R10 => "r10",
        Reg::R11 => "r11",
        Reg::R12 => "r12",
        Reg::R13 => "r13",
        Reg::R14 => "r14",
        Reg::R15 => "r15",
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
        X86Arg::FuncName(f) => format!("{}", f),
        X86Arg::GlobalVal(g) => format!("QWORD [rel {}]", g),
        _ => panic!("invalid arg type: {:?}", arg),
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
        X86::And(dest, src) => format!("and {}, {}",
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
        X86::Jne(label) => format!("jne {}", label),
        X86::Je(label) => format!("je {}", label),
        X86::Label(label) => format!("{}:", label),
        X86::Call(label) => format!("call {}", print_x86_arg(label)),
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

pub fn print_x86(prog: X86) -> String {
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

    match prog {
        X86::DefineWithStackSize(name, stack_size, instrs) => {
            let stack_size = 8 * stack_size;
            let prelude = format!("{}:
    push rbp
    mov rbp, rsp
    sub rsp, {}
{}\n", name, stack_size, save_callee_save_regs);
            let postlude = format!("    mov rdi, rax
{}
    add rsp, {}
    mov rsp, rbp
    pop rbp
    ret\n", restore_callee_save_regs, stack_size,);

            let mut instrs_str = prelude;
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
extern error_not_number
extern error_not_bool
extern error_not_tuple
global main
main:
    push rbp
    mov rbp, rsp
    sub rsp, {}
{}
    call initialize\n", stack_size, save_callee_save_regs);
            let postlude = format!("    mov rdi, rax
    call print
{}
    add rsp, {}
    mov rsp, rbp
    pop rbp
    ret
internal_error_non_number:
    call error_not_number
internal_error_non_bool:
    call error_not_bool
internal_error_non_tuple:
    call error_not_tuple
\n", restore_callee_save_regs, stack_size);
            let mut instrs_str = prelude;
            for i in instrs {
                instrs_str.push_str(&print_instr(i));
            }
            instrs_str.push_str(&postlude[..]);
            instrs_str.push_str(&defs_str[..]);
            instrs_str
        },
        _ => panic!("print_x86: not top-level Prog"),
    }
}
