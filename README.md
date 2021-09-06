# oktac 🐙

**oktac** the compiler of the *okta* programming language!

*okta* is an experimental programming language made just for fun and the sake to learn about compilers and programming languages.
Although the project is in a very erly development phase, some features of *okta* include:
* Compiles to a single biary
* Usage of LLVM as backend (it's quite fast!)
* Statically typed
* Use external functions written in C.
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
# this will read and compile the `test.ok` file to a binary named `test`
./oktac test.ok -o test

# run!
./test
```

**Useful debugging options**

Emit the AST of the source:
```bash
./oktac test.ok --emit-ast
```

Emit the LLVM-IR generated from the source:
```bash
./oktac test.ok --emit-llvm
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

## Special thanks

* [@stiviwonder](https://github.com/stiviwonder) for the alpha testing
