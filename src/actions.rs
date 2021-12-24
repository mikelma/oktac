use console::{style, Term};
use inkwell::context::Context;
use ptree::print_tree;

use std::collections::hash_map::DefaultHasher;
use std::fs::{self, File};
use std::hash::*;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::process;
use std::process::Command;
use std::sync::{Arc, Barrier, Mutex};
use std::thread;

use crate::*;

/// Reads all input files and generates the AST of each compilation unit (that are stored in
/// their respective `CompUnitStatus` inside `GlobalStatus`). All this process occurs in parallel,
/// as each compilation unit is sent to a thread to be processed.
///
/// # Algorithm
///
/// 1. Create a `CompUnitStatus` for each compilation unit.
/// 2. Parse the input source of each unit into an untyped syntax tree.
/// 3. For each compilation unit, all the module-local symbols are recorded as opaque symbols in
///    the unit's symbol table's main scope. All the import statements are also parsed in the same
///    step.
/// 4. Sync all threads.
/// 5. Validate imports (check for possible errors).
/// 6. Push public symbols of the imported units into the current unit's symbol table (as opaque
///    symbols).
/// 7. Sync all threads.
/// 8. Parse the prototypes of the unit, in this step, prototypes get pushed into the symbol table,
///    replacing the opaque symbols introduced in step 3.
/// 7. Sync all threads.
/// 9. Push public prototypes of the imported units into the current unit's symbol table, replacing
///    the opaque symbols introduced in step 6.
/// 10. Prototype validation. In this step, cyclic type definitions are detected (infinite size
///     types).
/// 11. An unique hash is computed for the unit, including: imported paths, prototypes and the AST.
pub fn source_to_ast(paths: Vec<String>, root_path: PathBuf) {
    let mut thread_handles = vec![];

    let root_path_arc = Arc::new(root_path);

    let n_units = paths.len();
    let barrier_syntax_arc = Arc::new(Barrier::new(n_units));
    let barrier_protos_arc = Arc::new(Barrier::new(n_units));
    let barrier_imports_arc = Arc::new(Barrier::new(n_units));

    let info_mutex_arc = Arc::new(Mutex::new(0));

    for input_path in paths {
        let barrier_syntax = Arc::clone(&barrier_syntax_arc);
        let barrier_protos = Arc::clone(&barrier_protos_arc);
        let barrier_imports = Arc::clone(&barrier_imports_arc);
        let root_path = Arc::clone(&root_path_arc);
        let info_mutex = Arc::clone(&info_mutex_arc);

        // spawn a new thread to process the compilation unit
        thread_handles.push(
            thread::Builder::new()
                .name(input_path.to_string())
                .spawn(move || {
                    info_print_line(
                        Some("Reading input files and parsing syntax tree"),
                        &info_mutex,
                        0,
                    );

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
                        }
                        Err(_) => {
                            eprintln!(
                                "[ERR] Unit's path `{}` it's not included in \
                            the project's root directory `{}`",
                                input_path,
                                root_path.display()
                            );
                            process::exit(1);
                        }
                    };

                    // create the compilation unit and insert it in `GlobalStatus`
                    let unit = CompUnitStatus::new(&input_path, units_path.to_path_buf());
                    GLOBAL_STAT
                        .lock()
                        .unwrap()
                        .inser_unit(thread::current().id(), unit);

                    // parse input source to untyped syntax tree (done via pest)
                    let syntax_tree = match ast::parse_syntax_tree(&input) {
                        Ok(p) => p,
                        Err(e) => {
                            info_print_line(None, &info_mutex, 0);
                            ast::print_fancy_parse_err(e, &input_path);
                            process::exit(1);
                        }
                    };

                    // create a table for the unit's main scope. In this scope, all module-local
                    // symbols will be recorded (e.g. struct/enums definitions, local functions...).
                    current_unit_st!().push_table();

                    // create the hasher, as we need to compute the hash of the unit's AST
                    let mut hasher = DefaultHasher::new();

                    let imports = ast::rec_types_and_parse_imports(syntax_tree.clone());
                    imports.hash(&mut hasher);

                    barrier_syntax.wait(); // sync threads

                    info_print_line(Some("Resolving modules"), &info_mutex, 1);

                    ast::validate_imports(&imports, &units_path);

                    current_unit_status!().lock().unwrap().imports =
                        ast::imported_units_map(&imports);

                    // import opaque types from imported modules
                    ast::import_extern_symbols();

                    // wait until all units have their imported symbols ready
                    barrier_imports.wait();

                    info_print_line(Some("Generating AST"), &info_mutex, 2);

                    // create the AST of the import statements and prototypes of the unit
                    let protos = ast::generate_protos(syntax_tree.clone());
                    protos.hash(&mut hasher);

                    current_unit_status!().lock().unwrap().protos = Arc::new(
                        protos
                            .into_iter()
                            .map(|v| Arc::new(v))
                            .collect::<Vec<Arc<AstNode>>>(),
                    );

                    // wait until all units have their prototypes parsed
                    barrier_protos.wait();

                    // import types from imported modules
                    ast::import_protos();

                    ast::validate_protos();

                    let ast = ast::generate_ast(syntax_tree);
                    ast.hash(&mut hasher);

                    current_unit_status!().lock().unwrap().ast = Arc::new(ast);
                    current_unit_status!().lock().unwrap().hash = hasher.finish();

                    info_print_line(None, &info_mutex, 3);
                })
                .expect("Cannot spawn thread"),
        );
    }

    for handle in thread_handles {
        handle.join().unwrap();
    }
}

/// If the expected value and the value of the mutex is the same (or lower) print the `msg` (if
/// some) and clear the line above, else just clear the line above.
fn info_print_line(msg: Option<&str>, mutex: &Arc<Mutex<usize>>, expected: usize) {
    let (term, msg) = match msg {
        Some(m) => (Term::stdout(), m),
        None => {
            let term = Term::stdout();
            term.move_cursor_up(1).unwrap();
            term.clear_line().unwrap();
            return;
        },
    };

    if *mutex.lock().unwrap() <= expected {
        if expected > 0 {
            term.move_cursor_up(1).unwrap();
            term.clear_line().unwrap();
        }

        term.write_line(msg.to_string().as_str()).unwrap();

        *mutex.lock().unwrap() += 1;
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

pub fn print_ast(debug: bool) {
    let multiple_units = GLOBAL_STAT.lock().unwrap().units.len() > 1;
    for unit in GLOBAL_STAT.lock().unwrap().units.values() {
        if multiple_units {
            println!(
                "\n\n{} {}: {}\n",
                style(">").bold().color256(5),
                style("Compilation unit").bold().underlined(),
                unit.lock().unwrap().filename
            );
        }

        unit.lock().unwrap().protos.iter().for_each(|p| {
            if debug {
                println!("{:#?}", p);
            } else {
                print_tree(&**p).unwrap()
            }
        });

        match &*unit.lock().unwrap().ast {
            AstNode::Stmts(stmts) => {
                if !stmts.is_empty() {
                    println!();
                }
                stmts.iter().for_each(|p| {
                    if debug {
                        println!("{:#?}", p);
                    } else {
                        print_tree(p).unwrap()
                    }
                })
            }
            _ => unreachable!(),
        }
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
            eprintln!(
                "[ERR] Cannot create temporal directory {}: {}",
                tmp_dir.to_str().unwrap_or(""),
                e
            );
            process::exit(1);
        }
    }

    let term = Term::stdout();
    term.write_line("Translating AST to LLVM-IR").unwrap();

    // old thread-ids of the compilation units (these will be replaced by codegen thread-ids)
    let old_keys: Vec<thread::ThreadId> =
        GLOBAL_STAT.lock().unwrap().units.keys().cloned().collect();

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
            GLOBAL_STAT
                .lock()
                .unwrap()
                .units
                .insert(thread::current().id(), unit);

            // compile the AST to LLVM-IR
            let context = Context::create();
            let name = current_unit_status!()
                .lock()
                .unwrap()
                .path
                .display()
                .to_string();
            let mut codegen = CodeGen::new(&context, name);

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
                eprintln!(
                    "Inner error occurred in the codegen step for file {}: {}",
                    &filename, e
                );
                *has_err = true;
            }

            let ast = Arc::clone(&current_unit_status!().lock().unwrap().ast);

            if let Err(e) = codegen.compile_node(&ast) {
                // note that this scope will run as a critical section (thus, we can print the
                // error safely here)
                let mut has_err = compile_errors.lock().unwrap();
                eprintln!(
                    "Inner error occurred in the codegen step for file {}: {}",
                    &filename, e
                );
                *has_err = true;
            }

            // save the generated IR to the temporal directory
            let fname = tmp_dir_ref.join(format!(
                "{:x}.ll",
                current_unit_status!().lock().unwrap().hash
            ));
            if let Err(e) = fs::write(&fname, codegen.to_string()) {
                let mut has_err = compile_errors.lock().unwrap();
                eprintln!(
                    "Inner error occurred while writing temporal file {}: {}",
                    fname.to_str().unwrap(),
                    e
                );
                *has_err = true;
            }
        }));
    }

    for handle in thread_handles {
        handle.join().unwrap();
    }

    term.move_cursor_up(1).unwrap();
    term.clear_line().unwrap();
}

/// Compiles the LLVM-IR of each unit, creating an executable binary
/// The function will exit after an error message if the compilation fails.
pub fn llvm_to_bin(tmp_dir: PathBuf, 
                   output: &str, 
                   opt_level: &OptLevel, 
                   c_include: Option<&Vec<String>>) {
    let mut to_compile = GLOBAL_STAT
        .lock()
        .unwrap()
        .units
        .values()
        .map(|u| {
            tmp_dir
                .join(format!("{:x}.ll", u.lock().unwrap().hash))
                .to_str()
                .unwrap()
                .into()
        })
        .collect::<Vec<String>>();

    let mut cmd = Command::new("clang");

    cmd.arg(format!("-{}", opt_level));

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

    let term = Term::stdout();
    term.write_line("Linking and generating binary file")
        .unwrap();

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

    term.move_cursor_up(1).unwrap();
    term.clear_line().unwrap();
}
