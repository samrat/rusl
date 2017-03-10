runtime.o: runtime.c
	gcc -c -g -std=c99 runtime.c

test: runtime.o
	cargo run foo.txt > test.s
	nasm -f elf64 test.s
	gcc -g runtime.o test.o

