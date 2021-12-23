<h1>oktac  <img src='https://okta-lang.org/assets/img/okta-ottis.png' width='70' height='70'></h1>

The compiler of the *okta* programming language. For more information
please visit the official [website](https://okta-lang.org), and to quickly get 
okta running refer to the [getting started](https://okta-lang.org/#getting-started) 
section.

**DISCLAIMER:** This project, as well as the okta language itself, it's in a very 
early development state, expect bugs and frequent breaking changes.

## How to build

The compiler currently supports the following platform and architecture combinations:

* Linux x86_64 (tested on versions 5.\*)
* FreeBSD x86_64 (tested on 13.0-RELEASE)

The first step is to install the dependencies:

* git
* LLVM 12
* clang 12
* [Rust](https://www.rust-lang.org/tools/install) 1.57
* libffi
* libxml2

Once you have dependencies installed, you clone the repository:

```bash
git clone https://git.sr.ht/~mikelma/oktac
cd oktac
```

The final step is to build the compiler using rust's `cargo`. The `--release` flag isn't 
mandatory, however it is strongly recommended, as if this flag isn't provided
the debug version of oktac will be built instead.

```bash
cargo build --release
```

## How to use

This section covers the most common use cases of oktac, for more information 
run `oktac --help`.

### Compiling okta programs 

* Compiling a single okta file to a binary. 
    ```bash
    oktac test.ok -o test
    ```


* Compiling multiple okta files:
    ```bash
    oktac main.ok bar.ok foo.ok
    ```
    
    Note that if the `-o` (output) flag isn't provided the output binary is saved to `a.out`.

### Useful debugging options

* Emit the LLVM-IR generated from the source:
    ```bash
    ./oktac test.ok --emit=llvm-ir
    ```

* Emit the AST of the source code:
    ```bash
    oktac main.ok --emit=ast
    ```

* Emit the **full** AST of the source code (very verbose):
    ```bash
    oktac main.ok --emit=ast-dbg
    ```

## Contributing

If you want to contribute or found a bug, please take a look at the 
[contributing](https://okta-lang.org/#contributing) section of the okta website.

## License

Oktac is distributed under the terms of the GLPv3 license. 
See [LICENSE](https://git.sr.ht/~mikelma/oktac/tree/main/item/LICENSE) for more details.

## Special thanks

* [@stiviwonder](https://github.com/stiviwonder) for the alpha testing
* [@iiraaiitz](https://github.com/iiraaiitz) for helping with the grammar 
