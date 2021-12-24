use clap::Parser;
use console::style;

use std::path::PathBuf;
use std::process;
use std::time::Instant;

use oktac::*;

fn main() {
    let opts: Opts = Opts::parse();

    let root_path = match opts.root_path {
        Some(rp) => PathBuf::from(rp),
        // TODO: Replace this with the maximum comman path of `opts.input`
        None => PathBuf::from(""),
    };

    let start = Instant::now();
    actions::source_to_ast(opts.input, root_path);

    if actions::show_astgen_msgs().is_err() {
        eprintln!("\n{}", style("Compilation failed").red().bold());
        process::exit(1);
    }

    if let Some(opt) = opts.emit {
        if opt.ast() {
            actions::print_ast(opt == EmitOpts::AstDebug);
            process::exit(0);
        }
    }

    let tmp_dir = PathBuf::from(&opts.tmp_dir);

    actions::codegen(tmp_dir.clone());

    actions::llvm_to_bin(tmp_dir, 
                         &opts.output, 
                         &opts.opt_level, 
                         opts.c_include.as_ref());

    println!(
        "{}: {:.2?}",
        style("Compilation successful").bold().green(),
        start.elapsed()
    );
}
