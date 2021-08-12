use inkwell::context::Context;
use clap::Clap;
use console::style;

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

    // check for number of semantic errors
    let n_errs  = GLOBAL_STAT.lock().unwrap().errors;
    let n_warns = GLOBAL_STAT.lock().unwrap().warnings;

    if n_warns > 0 {
        eprintln!("{}", style(format!("[W] {} warnings emitted\n", n_warns)).bold().yellow());
    }
    if n_errs > 0 {
        eprintln!("[!] Compilation failed due to {} errors", n_errs);
        process::exit(1);
    }

    if opts.emit_ast > 0 {
        eprintln!("{:#?}", ast);
    }

    // compile the AST to LLVM-IR
    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    let mut comp_errs = false;
    for subtree in ast {
        if let Err(e) = codegen.compile(&subtree) {
            eprintln!("[ERR] Compilation error: {}", e);
            comp_errs = true;
        } 
    }

    if comp_errs { // exit if the compilation was not successful
        eprintln!("Compilation failed due to inner compiler errors! =(");
        process::exit(1);
    }

    if opts.emit_llvm > 0{
        println!("{}", codegen.to_string());
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
    cmd.arg("-O0");

    let mut compile_files = vec!["tmp.ll".to_string()]; // files to compile
    
    // get the header and C files to compile (if some) 
    if let Some(files) = opts.c_include {
        for file in files {
            if let Some(extension) = Path::new(&file).extension() {
                if extension == "h" {
                    cmd.args(&["-I", &file]);
                } else {
                    compile_files.push(file);
                }
            }
        }
    } 

    cmd.args(&compile_files);

    cmd.arg(format!("-o{}", opts.output));
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

