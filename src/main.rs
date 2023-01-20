mod ast;
mod defs;
mod lexer;
mod utils;

use ast::generate_ast;
use defs::{Token, Program};

use lexer::Parser;
fn main() {
    // TODO: Parse command line arguments

    // Read code files
    // TODO: Enable including code from multiple files
    const CODE_FILE: &str = "test.pog";
    // Parse code to Tokens
    let mut parser: Parser = Parser::init(CODE_FILE);
    let tokens: Vec<Token> = parser.parse();

    dbg!(&tokens);
    // Generate abstract syntax tree (AST)
    // let program: Program = generate_ast(&tokens);
    // dbg!(&program);

    // TODO: Type check Program
    // TODO: Generate assembly code
    // TODO: Compile executable
}
