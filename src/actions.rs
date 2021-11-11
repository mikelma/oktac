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

use crate::{AstNode, CodeGen, CompUnitStatus, GLOBAL_STAT, ast, current_unit_st, current_unit_status};

/// Reads all input files and generates the AST of compilation unit (that are stored in each 
/// CompUnitStatus inside the GlobalStatus). All this process occurs in parallel, as each 
/// compilation unit is sent to a thread.
pub fn source_to_ast(paths: Vec<String>, root_path: PathBuf) {
    let mut thread_handles = vec![];

    let root_path_arc = Arc::new(root_path);

    let n_units = paths.len();
    let barrier_syntax_arc = Arc::new(Barrier::new(n_units));
    let barrier_protos_arc = Arc::new(Barrier::new(n_units));
    let barrier_imports_arc = Arc::new(Barrier::new(n_units));

    for input_path in paths {
        let barrier_syntax = Arc::clone(&barrier_syntax_arc);
        let barrier_protos = Arc::clone(&barrier_protos_arc);
        let barrier_imports = Arc::clone(&barrier_imports_arc);

        // let units_ready = Arc::clone(&barrier_unit_create);
        let root_path = Arc::clone(&root_path_arc);

        // spawn a new thread to process the unit
        thread_handles.push(thread::Builder::new()
                            .name(input_path.to_string())
                            .spawn(move || {
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

            // get the relative path of the unit with respect the root path
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

            // create the compilation unit
            let unit = CompUnitStatus::new(&input_path, units_path.to_path_buf());
            GLOBAL_STAT.lock().unwrap().inser_unit(thread::current().id(), unit);

            // wait until all the compilation units are generated
            // units_ready.wait();

            // parse input source code to untyped parse tree (pest)
            let syntax_tree = match ast::parse_syntax_tree(&input) {
                Ok(p) => p,
                Err(e) => {
                    ast::print_fancy_parse_err(e);
                    process::exit(1);
                }
            };

            // create a table for the module's scope
            current_unit_st!().push_table();

            // create the hasher, as we need to compute the hash of the unit's AST 
            let mut hasher = DefaultHasher::new();

            let imports = ast::rec_types_and_parse_imports(syntax_tree.clone());
            imports.hash(&mut hasher);

            barrier_syntax.wait(); // sync threads

            ast::validate_imports(&imports, &units_path);

            current_unit_status!().lock().unwrap().imports = ast::imported_units_map(&imports);

            // import opaque types from imported modules
            ast::import_extern_symbols();

            // wait until all units have their imported symbols ready
            barrier_imports.wait(); 

            // create the AST of the import statements and prototypes of the unit
            let protos = ast::generate_protos(syntax_tree.clone());
            protos.hash(&mut hasher);

            current_unit_status!().lock().unwrap().protos = Arc::new(
                protos.into_iter()
                    .map(|v| Arc::new(v))
                    .collect::<Vec<Arc<AstNode>>>()
            );

            // wait until all units have their prototypes parsed 
            barrier_protos.wait();

            // import types from imported modules
            ast::import_protos();

            ast::validate_protos();

            /*{ // DEBUG
                let _ = GLOBAL_STAT.lock();
                println!("\n{} after imports protos: \n{:#?}", input_path, &current_unit_status!().lock().unwrap().protos);
                println!("{:#?}", &current_unit_status!().lock().unwrap().imported_protos);
            }*/

            let ast = ast::generate_ast(syntax_tree);
            ast.hash(&mut hasher);

            current_unit_status!().lock().unwrap().ast = Arc::new(ast);
            current_unit_status!().lock().unwrap().hash = hasher.finish();

            /* 
            * ~~~~~~~~~ AST Generation process: ~~~~~~~~~ 
            *
            *                    (TODO)
            *
            *  1. Parse import statements, protypes and functions
            *  2. Record the types of each unit in it's symbol table
            *  3. Wait until all units have parsed their types (sync all threads)
            *
            *  4. Validate imports and record imported module types to the unit's ST (in parallel)
            *  5. For each unit, generate the AST of the protos and then functions.
            *  6. Sync threads
            *
            *  7. Push the (publics only) prototypes of the imported modules to the unit's protos list
            *  8. Check for cyclic dependency type definitions (recursive types). 
            *
            */


            /*
            // create the AST of the import statements and prototypes of the unit
            let (imports, protos) = ast::generate_protos(pairs.clone());
            
            protos.hash(&mut hasher);
            imports.hash(&mut hasher);

            current_unit_status!().lock().unwrap().protos = Arc::new(protos);

            protos_ready.wait();

            // now that all compilation unit instances are created,
            // validate the imports of the module
            ast::validate_imports(&imports, units_path);

            current_unit_status!().lock().unwrap().imports = imports;

            // current_unit_status!().lock().unwrap().imported_protos = ast::resolve_imports(&imports);

            let ast = ast::generate_ast(pairs);
            
            ast.hash(&mut hasher);

            current_unit_status!().lock().unwrap().ast = Arc::new(ast);
            // generate the hash of the compilation unit, based on it's AST (including protos)
            current_unit_status!().lock().unwrap().hash = hasher.finish();
            */
        }).expect("Cannot spawn thread"));
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

            // merge all prototypes
            let mut all_protos = vec![];
            for p in &*current_unit_status!().lock().unwrap().imported_protos {
                all_protos.push(Arc::clone(p));
            }
            for p in &*current_unit_status!().lock().unwrap().protos {
                all_protos.push(Arc::clone(p));
            }


            if let Err(e) = codegen.compile_protos(&all_protos) {
                // note that this scope will run as a critical section (thus, we can print the
                // error safely here)
                let mut has_err = compile_errors.lock().unwrap();
                eprintln!("Inner error occurred in the codegen step for file {}: {}", &filename, e);
                *has_err = true;
            }

            let ast = Arc::clone(&current_unit_status!().lock().unwrap().ast);

            if let Err(e) = codegen.compile_node(&ast) {
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
