use inkwell::context::Context;

use std::process;
use std::io::prelude::*;
use std::fs::File;

use oktac::*;

fn main() {
    // read the input file
    let mut f = File::open("test.k").expect("Cannot read `test.k` file");
    let mut input = String::new();
    f.read_to_string(&mut input).expect("Failed to read `test.k`");

    // parse input source code and create the AST
    let ast = match ast::parse(&input) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("[INFO] Parsing error: {}", e);
            process::exit(1);
        },
    };

    // println!("ast: {:#?}", ast);
    // println!("[INFO] Parsing done!");

    let context = Context::create();

    let mut codegen = CodeGen::new(&context);
    for expr in ast.iter() {
        if let Err(e) = codegen.compile(expr) {
            eprintln!("[ERR] Compilation error: {}", e);
        }
    }

    println!("{}", codegen.print());
}
