SRC=examples

test: runtime.o
	cargo run $(SRC) > test.s
	nasm -f elf64 test.s
	gcc -o a.out runtime.o test.o

runtime.o: runtime.c
	gcc -c -g -std=c99 runtime.c

test_suite:
	python run_tests.py
