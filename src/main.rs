use clap::Clap;
use console::style;
// use inkwell::context::Context;

// use std::path::Path;
// use std::process::{self, Command};
use std::process;

use oktac::*;

fn main() {
    let opts: Opts = Opts::parse();

    actions::source_to_ast(opts.input);

    if actions::show_astgen_msgs().is_err() {
        eprintln!("\n{}", style("Compilation failed").red());
        process::exit(1);
    }

    /*
    // check for number of semantic errors
    let n_errs: usize = GLOBAL_STAT.units.values().map(|u| u.lock().unwrap().errors).sum();
    let n_warns: usize = GLOBAL_STAT.units.values().map(|u| u.lock().unwrap().warnings).sum();

    if n_warns > 0 {
        eprintln!(
            "{}",
            style(format!("[W] {} warnings emitted\n", n_warns))
                .bold()
                .yellow()
        );
    }
    if n_errs > 0 {
        eprintln!("[!] Compilation failed due to {} errors", n_errs);
        process::exit(1);
    }

    if opts.emit_ast > 0 {
        eprintln!("{:#?}", protos);
        eprintln!("{:#?}", ast);
    }

    // compile the AST to LLVM-IR
    let context = Context::create();
    let mut codegen = CodeGen::new(&context);
    let mut comp_errs = false;

    if let Err(e) = codegen.compile(&protos, &ast) {
        eprintln!("[ERR] Compilation error: {}", e);
        comp_errs = true;
    }

    if comp_errs {
        // exit if the compilation was not successful
        eprintln!("Compilation failed due to inner compiler errors! =(");
        process::exit(1);
    }

    if opts.emit_llvm > 0 {
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
        Ok(stat) => {
            if !stat.success() {
                eprintln!("Failed to run clang command:");
                match stat.code() {
                    Some(code) => eprintln!("\t* Clang exited with status code: {}", code),
                    None => eprintln!("\t* Process terminated by signal"),
                }
                eprintln!("\t* Command:{:?}", cmd);
                process::exit(1);
            }
        }
        Err(e) => {
            eprintln!("Failed to run clang command:");
            eprintln!("\t* Error: {}", e);
            eprintln!("\t* Command: {:?}", cmd);
            process::exit(1);
        }
    }
    */
}
