# oktac 🐙

**oktac** is the compiler of the *okta* programming language! 

## How to use

```bash
# build the compiler

cargo build 

# this will read and compile the `test.k` file in 
# the repo's directory to llvm-ir 

./target/debug/oktac > test.ll

# compile the `test.ll` to an executable binary

clang -O3 test.ll -lc -o test
```
