# oktac 🐙

**oktac** the compiler of the *okta* programming language!

*okta* is a functional programming language made just for fun and the sake to learn about compilers and programming languages.
Although the project is in a very erly development phase, some features of *okta* include:
* Compiles to a single biary
* Usage of LLVM as backend (it's quite fast!)
* Functional programming language
* Statically typed
* KISS

For now, *okta* only supports x86_64 machines running Linux. However, support for other architecture and platforms is planned for the future.

**DISCLAIMER:** This project, as well as the *okta* language itself, it's in a very early development state, expect bugs and breaking changes.

## How to use

Dependencies:
* LLVM 12.0
* clang 12.0
* [Rust](https://www.rust-lang.org/tools/install) 
 
Once you have dependencies installed, the first step is to compile the compiler (:sweat_smile:):
```bash
# build the compiler
cargo build --release

# move the binary to the current directory
mv ./target/release/oktac .
```

**To compile okta source:**
*Oktac* compiles *okta* source files to LLVM-IR, so in order to convert *okta* source to binary we have to use `clang`:    

```bash
# this will read and compile the `test.ok` file
./oktac --input test.ok --emit llvm-ir > test.ll

# compile the `test.ll` to an executable binary
clang -O0 test.ll -o test

# run!
./test
```

**To display the AST of an okta source:**
```bash
# this will read and print the AST of `test.ok`
./oktac --input test.ok --emit ast
```

## Show me some code

<details><summary>Compute the factorial of a given number using recursion:</summary>
<p>

  ```cpp
fun factorial(i32 n): i32 {
    if n == 1 {
        ret n;
    } else {
        ret n*factorial(n-1);
    }
}

fun main() {
    ret factorial(5);
}
```
  
</p>
</details>
