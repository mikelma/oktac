use console::style;
use inkwell::context::Context;
use ptree::print_tree;
use target_lexicon::Triple;

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
///    the unit's symbol table's main scope. All the import statements and macros are parsed in
///    this same step.
/// 4. Sync all threads.
/// 5. Generate a map of imported units (mapping paths to compilation units).
/// 6. Push public symbols of the imported units into the current unit's symbol table (as opaque
///    symbols).
/// 7. Sync all threads.
/// 8. Parse the prototypes of the unit, in this step, prototypes get pushed into the symbol table,
///    replacing the opaque symbols introduced in step 3.
/// 7. Sync all threads.
/// 9. Push public prototypes of the imported units into the current unit's symbol table, replacing
///    the opaque symbols introduced in step 6. Imported prototypes are also appended to the
///    `imported_protos` list of the compilation unit.
/// 10. Prototype validation. In this step, cyclic type definitions are detected (infinite size
///     types).
/// 11. Generation of the full AST.
/// 12. Merge all prototypes in the `imported_prototypes` list of the compilation unit to the `protos`
///     list. Then, topologically sort all nodes related to constant value declarations in the `protos`
///     list of the compilation unit.
/// 13. Sync all threads.
/// 14. An unique hash is computed for the unit, including: imported paths, macros prototypes and the AST.
pub fn source_to_ast(paths: Vec<String>) {
    // first of all, generate the AST and compilation unit of the intrinsics unit
    if let Err(e) = units::intrinsics::intrinsics_unit() {
        eprintln!("{}", e);
        process::exit(1);
    }

    let mut thread_handles = vec![];

    let n_units = paths.len();
    let barrier_syntax_arc = Arc::new(Barrier::new(n_units));
    let barrier_protos_arc = Arc::new(Barrier::new(n_units));
    let barrier_imports_arc = Arc::new(Barrier::new(n_units));

    let info_mutex_arc = Arc::new(Mutex::new(0));

    for input_path in paths {
        let barrier_syntax = Arc::clone(&barrier_syntax_arc);
        let barrier_protos = Arc::clone(&barrier_protos_arc);
        let barrier_imports = Arc::clone(&barrier_imports_arc);
        let info_mutex = Arc::clone(&info_mutex_arc);

        // spawn a new thread to process the compilation unit
        thread_handles.push(
            thread::Builder::new()
                .name(input_path.to_string())
                .spawn(move || {
                    log::info_mt(
                        "Reading input files and parsing syntax tree",
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

                    // create the compilation unit and insert it in `GlobalStatus`
                    let unit = CompUnitStatus::new(pb);
                    GLOBAL_STAT
                        .lock()
                        .unwrap()
                        .inser_unit(thread::current().id(), unit);

                    // parse input source to untyped syntax tree (done via pest)
                    let syntax_tree = match ast::parse_syntax_tree(&input) {
                        Ok(p) => p,
                        Err(e) => {
                            ast::print_fancy_parse_err(e, &input_path);
                            process::exit(1);
                        }
                    };

                    // create a table for the unit's main scope. In this scope, all module-local
                    // symbols will be recorded (e.g. struct/enums definitions, local functions...).
                    current_unit_st!().push_table();

                    // create the hasher, as we need to compute the hash of the unit's AST
                    let mut hasher = DefaultHasher::new();

                    let (imports, macros) =
                        ast::rec_types_and_parse_imports_and_macros(syntax_tree.clone());
                    imports.hash(&mut hasher);
                    macros.hash(&mut hasher);

                    current_unit_status!().lock().unwrap().macros = Arc::new(macros);

                    barrier_syntax.wait(); // sync threads

                    log::info_mt("Resolving modules", &info_mutex, 1);

                    current_unit_status!().lock().unwrap().imports =
                        ast::imported_units_map(&imports);

                    // import symbols from imported modules as opaque types (types with no body)
                    ast::import_extern_symbols();

                    // wait until all units have their imported symbols ready
                    barrier_imports.wait();

                    log::info_mt("Generating AST", &info_mutex, 2);

                    // create the AST of the import statements and prototypes of the unit
                    let protos = ast::generate_protos(syntax_tree.clone());
                    protos.hash(&mut hasher);

                    current_unit_protos!().lock().unwrap().append(
                        &mut protos
                            .into_iter()
                            .map(|v| Arc::new(v))
                            .collect::<Vec<Arc<AstNode>>>(),
                    );

                    // wait until all units have their prototypes parsed
                    barrier_protos.wait();

                    // import types from imported modules
                    ast::import_protos();

                    ast::validate_protos();

                    let mut ast = ast::generate_ast(syntax_tree);

                    // merge all imported prototypes to the list of all prototyes
                    let imported =
                        Arc::clone(&current_unit_status!().lock().unwrap().imported_protos);

                    for p in &*imported {
                        current_unit_status!()
                            .lock()
                            .unwrap()
                            .protos
                            .lock()
                            .unwrap()
                            .push(Arc::clone(p));
                    }

                    ast::consts::toposort_const_vars();

                    ast.hash(&mut hasher);
                    current_unit_status!()
                        .lock()
                        .unwrap()
                        .ast
                        .lock()
                        .unwrap()
                        .append(&mut ast);

                    current_unit_status!().lock().unwrap().hash = hasher.finish();
                })
                .expect("Cannot spawn thread"),
        );
    }

    for handle in thread_handles {
        handle.join().unwrap();
    }
}

/// Reads all errors that have been generated in the whole compilation and prints them to the stdout.
/// This function returns `Ok(())` if no errors were reported, otherwise `Err(())` is returned.
pub fn dump_msgs() -> Result<(), ()> {
    let mut num_wars = 0;
    let mut num_errs = 0;

    // first, print all warnings
    //      from the global compilation unit
    GLOBAL_STAT.lock().unwrap().warnings.iter().for_each(|w| {
        num_wars += 1;
        eprintln!("{}\n", w);
    });
    //      from the rest of compilation units
    for unit in GLOBAL_STAT.lock().unwrap().units.values() {
        for w in &unit.lock().unwrap().warnings {
            eprintln!("{}\n", w);
            num_wars += 1;
        }
    }

    // then, print all erros
    //      from the global compilation unit
    GLOBAL_STAT.lock().unwrap().errors.iter().for_each(|e| {
        num_errs += 1;
        eprintln!("{}\n", e);
    });
    //      from the rest of compilation units
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
        let unit_path = unit.lock().unwrap().path.clone();

        if unit_path.to_str().unwrap() == units::intrinsics::INTRINSICS_UNIT_NAME {
            continue;
        }

        if multiple_units {
            println!(
                "\n\n{} {}: {}\n",
                style(">").bold().color256(5),
                style("Compilation unit").bold().underlined(),
                unit_path.display(),
            );
        }

        unit.lock()
            .unwrap()
            .protos
            .lock()
            .unwrap()
            .iter()
            .for_each(|p| {
                if debug {
                    println!("{:#?}", p);
                } else {
                    print_tree(&**p).unwrap()
                }
            });

        unit.lock().unwrap().macros.iter().for_each(|p| {
            if debug {
                println!("{:#?}", p);
            } else {
                print_tree(&**p).unwrap()
            }
        });

        let ast = Arc::clone(&unit.lock().unwrap().ast);
        for p in &*ast.lock().unwrap() {
            if debug {
                println!("{:#?}", p);
            } else {
                print_tree(p).unwrap()
            }
        }
    }
}

/// Generates the LLVM-IR of the compilation units (in parallel) and saves them into a file
/// (one for each unit) in the temporal directory.
/// If the temporal directory does not exist, the function will try to create it.
pub fn codegen(tmp_dir: PathBuf, target: &Triple) {
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

    log::info("Translating AST to LLVM-IR");

    // old thread-ids of the compilation units (these will be replaced by codegen thread-ids)
    let old_keys: Vec<thread::ThreadId> =
        GLOBAL_STAT.lock().unwrap().units.keys().cloned().collect();

    let intrinsics_unit_protos_arc = Arc::new(
        GLOBAL_STAT
            .lock()
            .unwrap()
            .units_by_path
            .get(&PathBuf::from(units::intrinsics::INTRINSICS_UNIT_NAME))
            .unwrap()
            .lock()
            .unwrap()
            .protos
            .lock()
            .unwrap()
            .iter()
            .map(|node| Arc::clone(node))
            .collect::<Vec<Arc<AstNode>>>(),
    );

    let mut thread_handles = vec![];
    let compilation_err_mutex = Arc::new(Mutex::new(false));
    let shared_tmp_dir = Arc::new(tmp_dir);

    for old_key in old_keys {
        let unit = GLOBAL_STAT.lock().unwrap().units.remove(&old_key).unwrap();
        let compile_errors = Arc::clone(&compilation_err_mutex);
        let path = unit.lock().unwrap().path.clone();
        let tmp_dir_ref = Arc::clone(&shared_tmp_dir);
        let intrinsics_unit_protos = Arc::clone(&intrinsics_unit_protos_arc);
        let target = target.clone();

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
            let mut codegen = CodeGen::new(&context, name, target);

            // if the unit isn't the intrinsics unit, include intrinsics function protos
            if path.to_str().unwrap() != units::intrinsics::INTRINSICS_UNIT_NAME {
                for p in &*intrinsics_unit_protos {
                    //all_protos.push(Arc::clone(p));
                    current_unit_status!()
                        .lock()
                        .unwrap()
                        .protos
                        .lock()
                        .unwrap()
                        .push(Arc::clone(p));
                }
            }

            if let Err(e) = codegen.compile_protos(&current_unit_protos!().lock().unwrap()) {
                // note that this scope will run as a critical section (thus, we can print the
                // error safely here)
                let mut has_err = compile_errors.lock().unwrap();
                eprintln!(
                    "Inner error occurred in the codegen step for file {}: {}",
                    path.display(),
                    e
                );
                *has_err = true;
            }

            let ast = Arc::clone(&current_unit_status!().lock().unwrap().ast);

            if let Err(e) = codegen.compile_nodes(&ast.lock().unwrap()) {
                // note that this scope will run as a critical section (thus, we can print the
                // error safely here)
                let mut has_err = compile_errors.lock().unwrap();
                eprintln!(
                    "Inner error occurred in the codegen step for file {}: {}",
                    path.display(),
                    e
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
}

/// Compiles the LLVM-IR of each unit, creating an executable binary
/// The function will exit after an error message if the compilation fails.
pub fn llvm_to_bin(
    tmp_dir: PathBuf,
    output: &str,
    opt_level: &OptLevel,
    c_include: Option<&Vec<String>>,
    target: &Triple,
    clang_args: Option<&Vec<String>>,
) {
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

    // set optimization level
    cmd.arg(format!("-{}", opt_level));

    // set compilation target
    cmd.arg("-target");
    cmd.arg(target.to_string());

    // set user defined flags
    if let Some(args) = clang_args {
        cmd.args(args);
    }

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

    log::info("Linking and generating binary file");

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
