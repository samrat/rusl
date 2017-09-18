import glob
import subprocess
import re
import shutil
import os
import sys

"""
USAGE: python run_tests.py

Runs all tests in the `tests` directory.

The way to specify a test is to add a pair of files- a .input file with 
 the rusl program and a .output file with the expected output.
"""

def get_test_names():
    input_files = glob.glob("tests/*.input")
    test_names = list(map(lambda i: re.match(r"tests/(\w+)\.input", i).group(1), input_files))

    return test_names

def run_test(test_name):
    output_path = "tests/" + test_name + ".output"
    input_path  = "tests/" + test_name + ".input"
    try:
        output_file = open(output_path, "r")
    except FileNotFoundError:
        print("No corresponding .output file found for: " + test_name + ".input")
        return 1

    # TODO: Handle this for non-integer tests
    expected_output = int(output_file.read())

    # Compile and run the program
    asm_path = "tests/_bin/" + test_name + ".s"
    asm_file = open(asm_path, "w")
    object_path = "tests/_bin/" + test_name + ".o"
    bin_path = "tests/_bin/" + test_name + ".out"

    # TODO: check that each of these pass before running the next
    # command
    subprocess.run(["cargo", "run", input_path], stdout=asm_file)
    subprocess.run(["nasm", "-f", "elf64", asm_path])
    subprocess.run(["gcc", "-std=c99", "-o", bin_path, "runtime.c", object_path])

    bin_run = subprocess.run(bin_path, stdout=subprocess.PIPE)

    actual_output = int(bin_run.stdout)
    if expected_output != actual_output:
        print("Test %s failed. \t Expected: %d \t Actual: %d" %
              (test_name, expected_output, actual_output))
        return 1

    return 0


def run_all_tests(delete_generated_files=True):
    test_names = get_test_names()
    num_tests = len(test_names)
    failures = 0
    for test in test_names:
        print("Running %s" % test)
        failures += run_test(test)

    print("\n\n Ran %d tests. %d tests failed." % (num_tests, failures))
    
    # Delete generated files
    if delete_generated_files:
        shutil.rmtree("tests/_bin")

    if failures > 0:
        sys.exit(1)


if __name__ == '__main__':
    try:
        os.mkdir("tests/_bin")
    except FileExistsError:
        pass
    run_all_tests()
