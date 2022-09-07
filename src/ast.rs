use crate::defs::{DataType,Expression,ExpressionType,Program};
use crate::defs::{Statement,StatementType,Token,TokenType};

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

    // Conditional, Function, Loop or Return
    if token.typ == &TokenType::Keyword {
      if token.value == "fun"     { return get_function_statement(tokens, index) }
      if token.value == "if"      { return get_if_statement(tokens, index) }
      if token.value == "return"  { return get_return_statement(tokens, index) }
      if token.value == "while"   { return get_while_statement(tokens, index) }
      panic!("Unexpected Keyword at the beginning of Statement: '{}'", token.value)
    }

    // Expression
    return get_expression_statement(tokens, index);
  }

  fn get_no_operation_statement() -> Statement {
    Statement { typ: StatementType::NoOperation, expression: None, statement: None }
  }

  fn get_compound_statement(tokens: &Vec<Token>, index: &mut usize) -> Statement {
    *index += 1; // Go past the open curly brace '{'
    if index >= &mut tokens.len() {
      panic!("Program cannot end to open curly brace")
    }
    let statement: Statement = get_next_statement(tokens, index);

    // TODO: Verify if the current Token's value is '}'
    *index += 1; // Go past the closing curly brace '}'
    return Statement {
      typ: StatementType::Compound,
      expression: None,
      statement: Some(Box::new(statement)),
    }
  }

  fn get_function_statement(tokens: &Vec<Token>, index: &mut usize) -> Statement {
    todo!("get_function_statement is not implemented yet")
  }

  fn get_if_statement(tokens: &Vec<Token>, index: &mut usize) -> Statement {
    todo!("get_if_statement is not implemented yet")
  }

  fn get_return_statement(tokens: &Vec<Token>, index: &mut usize) -> Statement {
    *index += 1; // Go past 'return' Token
    let expression: Expression = get_next_expression(tokens, index);

    // TODO: Verify if the current Token's value is ';'
    *index += 1; // Go past ';' Token
    return Statement {
      typ: StatementType::Return,
      expression: Some(expression),
      statement: None,
    }
  }

  fn get_while_statement(tokens: &Vec<Token>, index: &mut usize) -> Statement {
    todo!("get_if_statement is not implemented yet")
  }

  fn get_expression_statement(tokens: &Vec<Token>, index: &mut usize) -> Statement {
    Statement {
      typ: StatementType::Expression,
      expression: Some(get_next_expression(tokens, index)),
      statement: None,
    }
  }

  fn get_next_expression(tokens: &Vec<Token>, index: &mut usize) -> Expression {
    let token: &Token = tokens.get(*index)
      .expect("There are no Tokens left for Expression");

    // Unary Expressions
    // TODO: Parse post-increment and post-decrement expressions
    if token.typ == &TokenType::UnaryOperator {
      return get_unary_expression(token.value.to_string(), tokens, index)
    }
    let lookahead: &Token = tokens.get(*index+1)
      .expect("Lookahead failed: Program cannot end to an Expression");

    // Literal Expressions
    if lookahead.value == ";" {
      *index += 1;
      if token.typ == &TokenType::Literal(DataType::Character) {
        return get_character_literal_expression(token.value.to_string())
      }
      if token.typ == &TokenType::Literal(DataType::Integer) {
        return get_integer_literal_expression(token.value.to_string())
      }
      if token.typ == &TokenType::Literal(DataType::String) {
        return get_string_literal_expression(token.value.to_string())
      }
      if token.typ == &TokenType::Identifier {
        return get_identifier_literal_expression(token.value.to_string())
      }
      panic!("Unknown Token for Literal Expression: {:?}", token);
    }
    todo!("Parsing this kind of Expression is not implemented yet")
  }

  fn get_unary_expression(operator: String, tokens: &Vec<Token>, index: &mut usize) -> Expression {
    *index += 1; // Go past unary operator
    Expression {
      value: Some(operator),
      typ: ExpressionType::Unary,
      expressions: Some(
        vec![get_next_expression(tokens, index)]
      ),
    }
  }

  fn get_character_literal_expression(token_value: String) -> Expression {
    Expression {
      value: Some(token_value),
      typ: ExpressionType::Literal(DataType::Character),
      expressions: None
    }
  }

  fn get_integer_literal_expression(token_value: String) -> Expression {
    Expression {
      value: Some(token_value),
      typ: ExpressionType::Literal(DataType::Integer),
      expressions: None
    }
  }

  fn get_string_literal_expression(token_value: String) -> Expression {
    Expression {
      value: Some(token_value),
      typ: ExpressionType::Literal(DataType::String),
      expressions: None
    }
  }

  fn get_identifier_literal_expression(token_value: String) -> Expression {
    Expression {
      value: Some(token_value),
      typ: ExpressionType::Identifier,
      expressions: None
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_no_operation_statement_ast() {
    let tokens: Vec<Token> = vec![
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
  fn test_return_statement_ast() {
    let variable_name: &str = "var_name";
    let tokens: Vec<Token> = vec![
      Token::new(&TokenType::Keyword, "return"),
      Token::new(&TokenType::Identifier, variable_name),
      Token::new(&TokenType::Delimiter, ";"),
    ];
    let program: Program = generate_ast(&tokens);
    assert_eq!(program, Program {
      statements: vec![
        Statement::new(StatementType::Return, Some(Expression::new(
          Some(variable_name.to_string()), ExpressionType::Identifier, None
        )), None),
      ]
    })
  }

  #[test]
  fn test_nested_compound_statement_ast() {
    let tokens: Vec<Token> = vec![
      Token::new(&TokenType::Delimiter, "{"),
      Token::new(&TokenType::Delimiter, "{"),
      Token::new(&TokenType::Delimiter, ";"),
      Token::new(&TokenType::Delimiter, "}"),
      Token::new(&TokenType::Delimiter, "}"),
    ];
    let program: Program = generate_ast(&tokens);
    assert_eq!(program, Program {
      statements: vec![
        Statement::new(
          StatementType::Compound, None, Some(Box::new(Statement::new(
            StatementType::Compound, None, Some(Box::new(Statement::new(
              StatementType::NoOperation, None, None,
            ))),
          ))),
        ),
      ]
    })
  }
}
