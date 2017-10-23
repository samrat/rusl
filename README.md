# rusl

[![Build Status](https://travis-ci.org/samrat/rusl.svg?branch=master)](https://travis-ci.org/samrat/rusl)

A minimal Lisp that compiles to x86-64.

```scheme
(define (fibo x)
  (if (< x 2)
      1
      (+ (fibo (+ x (- 2)))
         (fibo (+ x (- 1))))))
         
(fibo 6)
```

## Trying out rusl

You need to have `Cargo`, `gcc` and `nasm` installed. Currently,
`rusl` requires the nightly version of Rust.

```shell
make test SRC=list_example && ./a.out
```

OR

```shell
cargo run foo.txt > test.s
nasm -f elf64 test.s
gcc -c -g -std=c99 runtime.c
gcc -g runtime.o test.o
./a.out
```

## Compiler organization

The compiler is organized as a series of passes. Each pass is a
function.

### Compiler pipeline for a single function:

A string is parsed into an SExpr(`parser::read`). The variables in
the SExpr is then uniquified(`uniquify`). Then, lambdas are
de-sugared into tuples(`convert_to_closures`). We then convert
SExpr into Flat-- the difference being that subexpressions are
lifted up into let-bindings. We then convert to a form we will
call pseudo-X86; this is a three-address code but uses variables
and supports if-conditionals so isn't quite X86. `uncover-live`
performs liveness analysis, and `assign_homes` does
register-allocation with spilling to stack where appropriate(we
use linear-scan register allocation).

Next, we lower if-conditionals to jumps(`lower_conditionals`). We are
very close to a representation of X86 now, but there will be some
instructions which are not valid X86 such as `mov [rbp-16],
[rbp-32]`. `patch_instructions` will fix those up (in the case of the
last example, by using a register as an intermediate buffer).

## Data representation in memory

- If LSB == 0 => ```integer```
- If LSB == 1 =>
   - If (bit 1) == 1 => ```boolean```
   - If (bit 1) == 0 => ```tuple```


- The first word in a tuple is the number of elements contained.
- Because the last two bits in a tuple are tag bits, a tuple must
  always be located in an address ending with 0b00. This means that if
  the no. of elements in the tuple would have resulted in an odd
  number of words, we add padding to the tuple storage space.
