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
    let mut token: &Token = tokens.get(*index).unwrap();

    // Compound or NoOperation
    if token.typ == &TokenType::Delimiter {
      if token.value == ";" { return get_no_operation_statement() }
      if token.value == "{" { return get_compound_statement(tokens, index) }
      panic!("Unexpected Delimiter at the beginning of Statement: '{}'", token.value)
    }
    Statement { typ: StatementType::Compound, expression: None, statement: None }
  }

  fn get_no_operation_statement() -> Statement {
    return Statement { typ: StatementType::NoOperation, expression: None, statement: None }
  }

  fn get_compound_statement(tokens: &Vec<Token>, index: &mut usize) -> Statement {
    *index += 1; // Go past the open curly brace '{'
    dbg!(*index, tokens.len());
    if index >= &mut tokens.len() {
      panic!("Program cannot end to open curly brace")
    }
    let statement: Statement = get_next_statement(tokens, index);

    // TODO: Verify if the current Token's value is '}'
    *index += 1; // Go past the closing curly brace '}'
    return Statement{
      typ: StatementType::Compound,
      expression: None,
      statement: Some(Box::new(statement)),
    }
  }
}

#[cfg(test)]
mod tests {
  use crate::defs::{StatementType};
  use super::*;

  #[test]
  fn test_no_operation_statement_ast() {
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

  #[test]
  fn test_compound_statement_ast() {
    let tokens = vec![
      Token::new(&TokenType::Delimiter, "{"),
      Token::new(&TokenType::Delimiter, ";"),
      Token::new(&TokenType::Delimiter, "}"),
    ];
    let program: Program = generate_ast(&tokens);
    assert_eq!(program, Program {
      statements: vec![
        Statement::new(
          StatementType::Compound, None, Some(Box::new(Statement::new(
            StatementType::NoOperation, None, None,
          )))
        ),
      ]
    })
  }
}
