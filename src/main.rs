use crap_lang::*;

fn main() {

    let src = "src/refs.chi";

    // println!("Source: `{src}`");
    
    transpile(src).unwrap();
}
