mod defs;
mod lexer;

use lexer::Parser;
fn main() {
  let code: &str = "1337+/-*<=42 \"This is string\"<>()[]{},;: // Comment \n/* Also \n a \n comment */";
  let mut parser: Parser = Parser::init(code);
  parser.parse();
}
