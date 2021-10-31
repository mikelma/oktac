use console::style;
use inkwell::context::Context;

use std::thread;
use std::sync::{Mutex, Arc, Barrier};
use std::fs::{File, self};
use std::io::prelude::*;
use std::process;
use std::path::{PathBuf, Path};
use std::hash::*;
use std::collections::hash_map::DefaultHasher;
use std::process::Command;

use crate::{
    GLOBAL_STAT, CompUnitStatus, 
    current_unit_status, ast,
    CodeGen,
};

/// Reads all input files and generates the AST of compilation unit (that are stored in each 
/// CompUnitStatus inside the GlobalStatus). All this process occurs in parallel, as each 
/// compilation unit is send to a thread.
pub fn source_to_ast(paths: Vec<String>, root_path: Option<String>) {
    let mut thread_handles = vec![];

    // extract project's root path
    let root_path_arc = Arc::new(match root_path {
        Some(rp) => PathBuf::from(rp),
        None => todo!(),
    });

    let barrier_protos = Arc::new(Barrier::new(paths.len()));

    for input_path in paths {
        let protos_ready = Arc::clone(&barrier_protos);
        let root_path = Arc::clone(&root_path_arc);

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

            // get the realtive path of the unit relative to the project's root
            let pb = PathBuf::from(&input_path);
            let units_path = match pb.strip_prefix(root_path.to_path_buf()) {
                Ok(p) => {
                    // remove the extension
                    PathBuf::from(p.to_str().unwrap().split(".").next().unwrap())
                },
                Err(_) => {
                    eprintln!("[ERR] Unit's path `{}` it's not included in \
                            the project's root directory `{}`", 
                            input_path, root_path.display());
                    process::exit(1);
                },
            };

            GLOBAL_STAT.lock()
                .unwrap()
                .units
                .insert(thread::current().id(), 
                        Mutex::new(CompUnitStatus::new(&input_path, units_path.to_path_buf())));

            // parse input source code and create the AST
            let pairs = match ast::parse_input(&input) {
                Ok(p) => p,
                Err(e) => {
                    ast::print_fancy_parse_err(e);
                    process::exit(1);
                }
            };

            let mut hasher = DefaultHasher::new();

            let (imports, protos) = ast::generate_protos(pairs.clone());
            
            protos.hash(&mut hasher);
            imports.hash(&mut hasher);

            current_unit_status!().lock().unwrap().protos = Arc::new(protos);
            current_unit_status!().lock().unwrap().imports = imports;

            protos_ready.wait();

            let ast = ast::generate_ast(pairs);
            
            ast.hash(&mut hasher);

            current_unit_status!().lock().unwrap().ast = Arc::new(ast);
            // generate the hash of the compilation unit, based on it's AST (including protos)
            current_unit_status!().lock().unwrap().hash = hasher.finish();
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

pub fn print_ast() {
    let multiple_units = GLOBAL_STAT.lock().unwrap().units.len() > 1;
    for unit in GLOBAL_STAT.lock().unwrap().units.values() {

        if multiple_units {
            println!("\n> Compilation unit: {}", unit.lock().unwrap().filename);
        }

        println!("{:#?}", unit.lock().unwrap().protos);
        println!("{:#?}", unit.lock().unwrap().ast);

    }
}

/// Generates the LLVM-IR of the compilation units (in parallel) and saves them into a file 
/// (one for each unit) in the temporal directory.
/// If the temporal directory does not exist, the function will try to create it.
pub fn codegen(tmp_dir: PathBuf) {
    // ensure that the directory for temporal files exists before 
    // the compilation step
    if !tmp_dir.exists() {
        if let Err(e) = fs::create_dir_all(&tmp_dir) {
            eprintln!("[ERR] Cannot create temporal directory {}: {}", 
                      tmp_dir.to_str().unwrap_or(""), e);
            process::exit(1);
        }
    }

    // old thread-ids of the compilation units (these will be replaced by codegen thread-ids)
    let old_keys: Vec<thread::ThreadId> = GLOBAL_STAT.lock()
        .unwrap()
        .units
        .keys()
        .cloned()
        .collect();

    let mut thread_handles = vec![];
    let compilation_err_mutex = Arc::new(Mutex::new(false));
    let shared_tmp_dir = Arc::new(tmp_dir);

    for old_key in old_keys {
        let unit = GLOBAL_STAT.lock().unwrap().units.remove(&old_key).unwrap();
        let compile_errors = Arc::clone(&compilation_err_mutex);
        let filename = unit.lock().unwrap().filename.clone();
        let tmp_dir_ref = Arc::clone(&shared_tmp_dir);
        
        thread_handles.push(thread::spawn(move || {

            // insert the unit in the global status with the new codegen thread-id as it's key
            GLOBAL_STAT.lock().unwrap().units.insert(thread::current().id(), unit);

            // compile the AST to LLVM-IR
            let context = Context::create();
            let mut codegen = CodeGen::new(&context);

            let protos = Arc::clone(&current_unit_status!().lock().unwrap().protos);
            let ast = Arc::clone(&current_unit_status!().lock().unwrap().ast);

            if let Err(e) = codegen.compile(&protos, &ast) {
                // note that this scope will run as a critical section (thus, we can print the
                // error safely here)
                let mut has_err = compile_errors.lock().unwrap();
                eprintln!("Inner error occurred in the codegen step for file {}: {}", &filename, e);
                *has_err = true;
            }

            // save the generated IR to the temporal directory
            let fname = tmp_dir_ref.join(format!("{:x}.ll", current_unit_status!().lock().unwrap().hash));
            if let Err(e) = fs::write(&fname, codegen.to_string()) {
                let mut has_err = compile_errors.lock().unwrap();
                eprintln!("Inner error occurred while writing temporal file {}: {}", fname.to_str().unwrap(), e);
                *has_err = true;
            }
        }));
    }

    for handle in thread_handles {
        handle.join().unwrap();
    }
}

/// Compiles the LLVM-IR of each unit, creating an executable binary
/// The function will exit after an error message if the compilation fails. 
pub fn llvm_to_bin(tmp_dir: PathBuf, output: &str, c_include: Option<&Vec<String>>) {
    let mut to_compile = GLOBAL_STAT.lock()
        .unwrap()
        .units
        .values()
        .map(|u| tmp_dir.join(format!("{:x}.ll", u.lock().unwrap().hash)).to_str().unwrap().into())
        .collect::<Vec<String>>();

    let mut cmd = Command::new("clang");

    cmd.arg("-O0");

    // get the header and C files to compile (if some)
    if let Some(files) = c_include {
        for file in files {
            if let Some(extension) = Path::new(&file).extension() {
                if extension == "h" {
                    cmd.args(&["-I", &file]);

                } else {
                    to_compile.push(file.into());
                }
            }
        }
    }

    cmd.args(&to_compile);

    cmd.arg(format!("-o{}", output));

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
}
