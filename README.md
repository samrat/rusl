# rusl

```scheme
(define (fibo x)
  (if (< x 2)
      1
      (+ (fibo (+ x (- 2)))
         (fibo (+ x (- 1))))))
         
(fibo 6)
```

```shell
cargo run foo.txt > test.s
nasm -f elf64 test.s
gcc -c -g -std=c99 runtime.c
gcc -g runtime.o test.o
./a.out
```
