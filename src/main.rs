#![allow(dead_code)]

mod ast_equality;
mod ast_printer;
mod compiler;
mod lexer;
mod parser;

use std::env;
use std::fs;

fn main() {
    // Skip name of compiler executable
    let mut args = env::args().skip(1);
    let file_path = args
        .next()
        .expect("An argument must be provided with the source program URL.");
    let source = fs::read_to_string(file_path).expect("Failed to read the source file.");

    compiler::compile(&source);
}
