use std::fs::File;
use std::io::Write;

mod analysis;
mod ast;
mod lexer;
mod parser;
mod transpiler;

pub fn transpile(source: &str) {
    let stmts = match parser::Parser::from_source(source).parse() {
        Ok(s) => s,
        Err(e) => {
            for err in e {
                eprintln!("{err}")
            }
            panic!();
        }
    };

    let mut scope = analysis::ModuleScope::from_source(stmts);

    match scope.analyse() {
        Ok(s) => s,
        Err(e) => {
            dbg!(e);
            panic!();
        }
    };

    let t = transpiler::Transpiler::transpile("main".to_string(), scope);

    let mut c = File::create("generated/main.c").unwrap();
    let mut h = File::create("generated/main.h").unwrap();

    write!(c, "{}", t.lines()).unwrap();
    write!(h, "{}", t.header()).unwrap();
}
