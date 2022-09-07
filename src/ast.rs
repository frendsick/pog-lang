use crate::defs::{DataType,Program,Token,TokenType,Statement,StatementType};

pub(crate) fn generate_ast(tokens: &Vec<Token>) -> Program {
  let mut statements: Vec<Statement> = vec![];

  // Parse all Statements from Tokens
  let mut index: usize = 0;
  while index < tokens.len() {
    statements.push(get_next_statement(&tokens, &mut index));
    index += 1;
  }
  return Program {
    statements: statements,
  };

  fn get_next_statement(tokens: &Vec<Token>, index: &mut usize) -> Statement {
    let mut token: &Token = tokens.first().unwrap();

    // Compound or NoOperation
    if token.typ == &TokenType::Delimiter {
      if token.value == ";" { return get_no_operation_statement() }
    }
    Statement { typ: StatementType::Compound, expression: None, statement: None }
  }

  fn get_no_operation_statement() -> Statement {
    return Statement { typ: StatementType::NoOperation, expression: None, statement: None }
  }
}

#[cfg(test)]
mod tests {
  use crate::defs::{StatementType};
  use super::*;

  #[test]
  fn test_no_operation_ast() {
    let tokens = vec![
      Token::new(&TokenType::Delimiter, ";"),
      Token::new(&TokenType::Delimiter, ";"),
    ];
    let program: Program = generate_ast(&tokens);
    assert_eq!(program, Program {
      statements: vec![
        Statement::new(StatementType::NoOperation, None, None),
        Statement::new(StatementType::NoOperation, None, None),
      ]
    })
  }
}
