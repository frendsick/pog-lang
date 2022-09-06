mod defs;
mod lexer;

use std::fs;

use lexer::Parser;
fn main() {
  let code: String = fs::read_to_string("test.pog").expect("Failed to read the file");
  let mut parser: Parser = Parser::init(code.as_str());
  parser.parse();
}
