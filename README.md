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
