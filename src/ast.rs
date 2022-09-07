use crate::defs::{Program,Token};

pub(crate) fn generate_ast<'a>(tokens: &'a Vec<Token>) -> Program<'a> {
  let mut program: Program = Program {
    statements: vec![],
  };
  return program;
}
