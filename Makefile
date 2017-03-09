test:
	cargo run foo.txt > test.s
	nasm -f elf64 test.s
	gcc -g runtime.o test.o

