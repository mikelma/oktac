use inkwell::context::Context;
use clap::Clap;

use std::process::{self, Command};
use std::io::prelude::*;
use std::fs::{File, self};
use std::path::Path;

use oktac::*;

fn main() {
    let opts: Opts = Opts::parse(); 

    // open and read the input file
    let mut f = match File::open(&opts.input) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("[ERR] Error opening input file {}: {}", opts.input, e);
            std::process::exit(1)
        },
    };

    let mut input = String::new();
    if let Err(e) = f.read_to_string(&mut input) {
        eprintln!("[ERR] Error reading file {}: {}", opts.input, e);
        std::process::exit(1);
    }

    // parse input source code and create the AST
    let ast = match ast::parse(&input) {
        Ok(ast) => ast,
        Err(e) => {
            ast::print_fancy_parse_err(e);
            process::exit(1);
        },
    };

    let mut stdout = std::io::stdout();

    if opts.emit_ast > 0 {
        writeln!(stdout, "{:#?}", ast).unwrap();
    }

    // compile the AST to LLVM-IR
    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    let mut errors = false;
    for subtree in ast {
        if let Err(e) = codegen.compile(&subtree) {
            eprintln!("[ERR] Compilation error: {}", e);
            errors = true;
        } 
    }

    if errors { // exit if the compilation was not successful
        process::exit(1);
    }

    if opts.emit_llvm > 0{
        writeln!(stdout, "{}", codegen.to_string()).unwrap();
    } 

    // write compiled bitcode to a temporary file
    if let Err(e) = fs::write("tmp.ll", codegen.to_string())
    // codegen.write_bc(&Path::new("tmp.bc")) {
    {
        eprintln!("{}", e);
        process::exit(1);
    }

    // compile the bitcode into a binary using clang
    let mut cmd = Command::new("clang");
    cmd
        // .arg("clang")
        .arg("-O0")
        .arg("tmp.ll")
        .arg(format!("-o{}", opts.output));
    match cmd.status() {
            Ok(stat) => if !stat.success() {
                eprintln!("Failed to run clang command:");
                match stat.code() {
                    Some(code) => eprintln!("\t* Clang exited with status code: {}", code),
                    None => eprintln!("\t* Process terminated by signal")
                }
                eprintln!("\t* Command:{:?}", cmd);
                process::exit(1);
            } 
            Err(e) => {
                eprintln!("Failed to run clang command:");
                eprintln!("\t* Error: {}", e);
                eprintln!("\t* Command: {:?}", cmd);
                process::exit(1);
            },
    } 
}

