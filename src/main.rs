use crap_lang::*;
use std::env;

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

    match transpile(src, target_dir) {
        Ok(_) => println!("Successfully compiled `{src}` to `{target_dir}`!"),
        Err(e) => eprintln!("An error occured during compilation: {e:#?}")
    };
}
