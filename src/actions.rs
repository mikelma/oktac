use console::style;

use std::thread;
use std::sync::{Mutex, Arc, Barrier};
use std::fs::File;
use std::io::prelude::*;
use std::process;

use crate::{GLOBAL_STAT, CompUnitStatus, current_unit_status, ast};

/// Reads all input files and generates the AST of compilation unit (that are stored in each 
/// CompUnitStatus inside the GlobalStatus). All this process occurs in parallel, as each 
/// compilation unit is send to a thread.
pub fn source_to_ast(paths: Vec<String>) {
    let mut thread_handles = vec![];

    let barrier_protos = Arc::new(Barrier::new(paths.len()));

    for input_path in paths {
        let protos_ready = Arc::clone(&barrier_protos);

        thread_handles.push(thread::spawn(move || {
            // open and read the input file
            let mut f = match File::open(&input_path) {
                Ok(v) => v,
                Err(e) => {
                    eprintln!("[ERR] Error opening input file {}: {}", input_path, e);
                    std::process::exit(1)
                }
            };

            let mut input = String::new();
            if let Err(e) = f.read_to_string(&mut input) {
                eprintln!("[ERR] Error reading file {}: {}", input_path, e);
                std::process::exit(1);
            }

            GLOBAL_STAT.lock()
                .unwrap()
                .units
                .insert(thread::current().id(), 
                        Mutex::new(CompUnitStatus::new(&input_path)));

            // parse input source code and create the AST
            let pairs = match ast::parse_input(&input) {
                Ok(p) => p,
                Err(e) => {
                    ast::print_fancy_parse_err(e);
                    process::exit(1);
                }
            };

            let protos = ast::generate_protos(pairs.clone());
            
            current_unit_status!().lock().unwrap().protos = protos;

            protos_ready.wait();

            let ast = ast::generate_ast(pairs);

            current_unit_status!().lock().unwrap().ast = ast;
        }));
    }

    for handle in thread_handles {
        handle.join().unwrap();
    }
}

/// Reads all errors that have been generated in the AST generation and prints them to the stdout.
/// This function returns `Ok(())` if no errors were reported, otherwise `Err(())` is returned. 
pub fn show_astgen_msgs() -> Result<(), ()> {
    let mut num_wars = 0;
    let mut num_errs = 0;

    // first, print all warnings
    for unit in GLOBAL_STAT.lock().unwrap().units.values() {
        for w in &unit.lock().unwrap().warnings {
            eprintln!("{}\n", w); 
            num_wars += 1;
        }
    }

    // then, print all erros
    for unit in GLOBAL_STAT.lock().unwrap().units.values() {
        for e in &unit.lock().unwrap().errors {
            eprintln!("{}\n", e); 
            num_errs += 1;
        }
    }

    if num_wars > 0 || num_errs > 0 {
        eprintln!();
    }

    // finally print a summary
    if num_wars > 0 {
        eprintln!(
            "{}",
            style(format!("[W] total number of warnings: {}", num_wars))
                .bold()
                .yellow()
        );
    }

    if num_errs > 0 {
        eprintln!(
            "{}",
            style(format!("[E] total number of errors: {}", num_errs))
                .bold()
                .red()
        );

        Err(())

    } else {
        Ok(())
    }

}
