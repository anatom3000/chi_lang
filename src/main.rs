use crap_lang::*;

fn main() {

    let src = include_str!("refs.chi"); // extension is temporary until i find a name for the language

    // println!("Source: `{src}`");
    
    transpile(src);
}
