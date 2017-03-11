SRC=examples

runtime.o: runtime.c
	gcc -c -g -std=c99 runtime.c

test: runtime.o
	cargo run $(SRC) > test.s
	nasm -f elf64 test.s
	gcc -g runtime.o test.o

