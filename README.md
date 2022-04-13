<h1>oktac  <img src='https://git.sr.ht/~mikelma/oktac/blob/main/assets/okta-ottis.png' width='70'></h1>

The compiler of the *okta* programming language. For more information
please visit the official [website](https://okta-lang.org), and to quickly get 
okta running refer to the [getting started](https://okta-lang.org/docs/getting-started/) 
section.

Currently, *okta* only supports the following platform and architecture combinations:

* Linux:
    - x86_64 (tested on versions 5.\*)
    - riscv64 (tested on [debian](https://wiki.debian.org/RISC-V))
* FreeBSD x86_64 (tested on 13.0-RELEASE)

**DISCLAIMER:** This project, as well as the okta language itself, it's in a very 
early development state, expect bugs and frequent breaking changes.

## How to build

There are two ways to compile *oktac* from source: using docker, or using 
cargo (the traditional way), chose the most combinient method for you.

* Cargo:
    - Smaller binary (dynamically links agains non rust dependencies: LLVM, glibc, ...).
    - More dependencies have to be installed in the host machine.
    - More complex.

* Docker:
    - Fully automated build (simple).
    - Static binary (at the cost of greater size of the binary).
    - Very few dependencies. 

The instructions for both methods assume the repository is cloned and that 
the working directory is set to the cloned repository:

```bash
git clone https://git.sr.ht/~mikelma/oktac
cd oktac
```

### Using docker

First of all, check if the following dependencies are present in your host machine:

* git
* docker
* clang

Next, execute the following command to build the docker container with the build environment:

```bash
docker build -t oktac-build .
```

The last step is to run the docker container as the following:

```bash
docker run --rm -it -v "$(pwd)":/home/rust/src oktac-build:latest
```

After some minutes, the building process should finish, providing an `oktac` static binary 
in the current directory.

### Using cargo

The first step is to install the following dependencies (if not already present):

* git
* LLVM 12
* clang 12
* [Rust](https://www.rust-lang.org/tools/install) (latest stable)
* libffi-devel
* libxml2-devel
* libstdc++-devel

The next step is to build the compiler using rust's `cargo`. Note that 
the `--release` flag isn't mandatory, however it is strongly recommended, 
as if this flag isn't provided the debug version of oktac will be built instead.

```bash
cargo build --release
```

The output binary shoud be located in the `target/release/oktac` path.

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

* Including okta libraries:
    ```bash
    oktac -L path/to/lib some.ok code.ok
    ```

    `some.ok` and `code.ok` can import modules inside `path/to/lib` 
    as if they were in their same location (path).

### Useful debugging options

* Emit the LLVM-IR generated from the source:
    ```bash
    oktac test.ok --emit=llvm-ir
    ```

* Emit the AST of the source code:
    ```bash
    oktac main.ok --emit=ast
    ```

* Emit the **full** AST of the source code (very verbose):
    ```bash
    oktac main.ok --emit=ast-dbg
    ```

## Language reference

The language's reference is also provided with the compiler's source code.
The reference can be found in `docs/reference.md` inside this repo, or online
[here](https://git.sr.ht/~mikelma/oktac/tree/main/item/docs/reference.md).
You can also convert the reference to a pretty PDF using
[`pandoc`](https://pandoc.org/) and [wkhtmltopdf](https://wkhtmltopdf.org/):

```bash
pandoc --pdf-engine=wkhtmltopdf --css docs/style.css docs/reference.md -o reference.pdf
```

## Contributing

If you want to contribute or found a bug, please take a look at the 
[contributing](https://okta-lang.org/contribute) section of the okta website.

## License

Oktac is distributed under the terms of the GLPv3 license. 
See [LICENSE](https://git.sr.ht/~mikelma/oktac/tree/main/item/LICENSE) for more details.

## Special thanks

* [@stiviwonder](https://github.com/stiviwonder) for the alpha testing
* [@iiraaiitz](https://github.com/iiraaiitz) for helping with the grammar 
* [@hginigo](https://github.com/hginigo) for diving deep into the dark world of macros 
* [@ladecadence](https://github.com/ladecadence) for testing oktac in riscv64-linux
