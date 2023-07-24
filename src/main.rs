use crap_lang::*;
use std::{env, path::PathBuf, process::Command};

fn main() {
    let args: Vec<String> = env::args().collect();

    let src = match args.get(1) {
        Some(path) => path,
        None => {
            eprintln!("Please provide an input file as the first argument");
            eprintln!("Exitting...");
            return;
        }
    };

    let target_dir = "generated";

    let module_name = match transpile(src, target_dir) {
        Ok(name) => {
            println!("Successfully transpiled `{src}` to `{target_dir}`!");
            name
        },
        Err(e) => {
            eprintln!("An error occured during compilation: {e:#?}");
            return;
        }
    };

    match compilation::compile(PathBuf::from(target_dir)) {
        Ok(()) => println!("Compiled generated C code to `{target_dir}/build/{module_name}`"),
        Err(compilation::CompilationError::IoError(err)) => panic!("IO Error while compiling generated C code: {err}"),
        Err(compilation::CompilationError::UnsupportedPlatform(plat)) => {
            eprintln!("Cannot compile generated C code on {plat}, skipping compilation...");
            return;
        },
        Err(compilation::CompilationError::CompilerError(msg)) => {
            eprintln!("Compiler failed to compile generared C code: \n{msg}");
            return;
        }
    }

    println!("Running `{target_dir}/build/{module_name}`...");
    println!();
    Command::new(format!("{target_dir}/build/{module_name}")).spawn().unwrap();

}
