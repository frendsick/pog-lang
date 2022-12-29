mod assembly;
mod ast;
mod defs;
mod lexer;
mod utils;

use assembly::generate_assembly;
use ast::generate_ast;
use defs::Program;
use std::fs;

use lexer::Parser;
fn main() {
    // TODO: Parse command line arguments

    // Read code files
    // TODO: Enable including code from multiple files
    let code: String = fs::read_to_string("test.pog").expect("Failed to read the file");

    // Parse code to Tokens
    let mut parser: Parser = Parser::init(code.as_str());
    let tokens: Vec<&str> = parser.parse();

    // Generate abstract syntax tree (AST)
    let program: Program = generate_ast(&tokens);
    dbg!(&program);

    // TODO: Type check Program
    // TODO: Generate assembly code
    let assembly: String = generate_assembly(&program);
    print!("{}", assembly);
    // TODO: Compile executable
}
