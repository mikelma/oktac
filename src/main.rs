use clap::Parser;
use console::style;

use std::process;
use std::path::PathBuf;

use oktac::*;

fn main() {
    let opts: Opts = Opts::parse();

    actions::source_to_ast(opts.input);

    if actions::show_astgen_msgs().is_err() {
        eprintln!("\n{}", style("Compilation failed").red());
        process::exit(1);
    }

    if opts.emit_ast > 0 {
        actions::print_ast();
    }

    let tmp_dir = PathBuf::from(&opts.tmp_dir);

    actions::codegen(tmp_dir.clone());

    actions::llvm_to_bin(tmp_dir, 
                         &opts.output, 
                         opts.c_include.as_ref())
}
