use inkwell::context::Context;
use clap::Clap;

use std::process;
use std::io::prelude::*;
use std::fs::File;

use oktac::*;

fn main() {
    let opts: Opts = Opts::parse(); 

    // read the input file
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

    match opts.emit {
        EmitOpts::Ast => {
            println!("{:#?}", ast);
        },
        EmitOpts::LlvmIr => {
            // let context = Context::create();
            // let mut codegen = CodeGen::new(&context);
            // if let Err(e) = codegen.compile(&ast) {
            //     eprintln!("[ERR] Compilation error: {}", e);
            // } else {
            //     println!("{}", codegen.print());
            // }
        }
    }
}

