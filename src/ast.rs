use crate::defs::{EXPRESSION_DELIMITERS,DataType,Expression,ExpressionType,Program};
use crate::defs::{Statement,StatementType,Token,TokenType};

pub(crate) fn generate_ast(tokens: &Vec<Token>) -> Program {
  let mut statements: Vec<Statement> = vec![];

  // Parse all Statements from Tokens
  let mut index: usize = 0;
  while index < tokens.len() {
    statements.push(get_next_statement(&tokens, &mut index));
  }
  return Program {
    statements: statements,
  };

  fn get_next_statement(tokens: &Vec<Token>, index: &mut usize) -> Statement {
    let mut token: &Token = tokens.get(*index).unwrap();

    // Compound or NoOperation
    if token.typ == &TokenType::Delimiter {
      if token.value == ";" {
        *index += 1; // Go past the semicolon
        return get_no_operation_statement()
      }
      if token.value == "{" { return get_compound_statement(tokens, index) }
      panic!("Unexpected Delimiter at the beginning of Statement: '{}'", token.value)
    }

    // Conditional, Function, Loop or Return
    if token.typ == &TokenType::Keyword {
      if token.value == "fun"     { return get_function_statement(tokens, index) }
      if token.value == "if"      { return get_conditional_statement(tokens, index, "if") }
      if token.value == "return"  { return get_return_statement(tokens, index) }
      if token.value == "while"   { return get_while_statement(tokens, index) }
      panic!("Unexpected Keyword at the beginning of Statement: '{}'", token.value)
    }

    // Expression
    return get_expression_statement(tokens, index);
  }

  fn get_no_operation_statement() -> Statement {
    Statement::new(StatementType::NoOperation, None, None, None)
  }

  fn get_compound_statement(tokens: &Vec<Token>, index: &mut usize) -> Statement {
    *index += 1; // Go past the open curly brace '{'
    if index >= &mut tokens.len() {
      panic!("Program cannot end to open curly brace")
    }

    // Get all Statements inside the Compound Statement
    let mut statements: Vec<Statement> = vec![];
    let mut token: &Token = tokens.get(*index)
      .expect("Unclosed Compound Statement"); // TODO: Enhance error reporting
    while token.value != "}" {
      statements.push(get_next_statement(tokens, index));
      token = tokens.get(*index)
        .expect("Unclosed Compound Statement"); // TODO: Enhance error reporting
    }
    *index += 1; // Go past the closing curly brace '}'
    return Statement::new(StatementType::Compound, None, None, Some(statements));
  }

  fn get_function_statement(tokens: &Vec<Token>, index: &mut usize) -> Statement {
    todo!("get_function_statement is not implemented yet")
  }

  fn get_conditional_statement(tokens: &Vec<Token>, index: &mut usize, condition_type: &str) -> Statement {
    *index += 1; // Go past 'if' Token

    // Test if the next Token's value is an opening round bracket '('
    let open_bracket: &Token = tokens.get(*index)
      .expect("Expected '(' after 'if' but found nothing"); // TODO: Enhance error reporting
    assert_eq!("(", open_bracket.value, "Expected '(' after 'if' but found '{}'", open_bracket.value);
    *index += 1;

    // Get Expression between round brackets
    let expression: Expression = get_next_expression(tokens, index);

    // Test if the next Token's value is a closing round bracket ')'
    let open_bracket: &Token = tokens.get(*index)
      .expect("Expected ')' after if's Expression but found nothing"); // TODO: Enhance error reporting
    assert_eq!(")", open_bracket.value, "Expected ')' after if's Expression but found '{}'", open_bracket.value);
    *index += 1;

    let statement: Statement = get_next_statement(tokens, index);
    Statement {
      typ: StatementType::Conditional,
      value: Some(condition_type.to_string()),
      expression: Some(expression),
      statements: Some(vec![statement]),
    }
  }

  fn get_return_statement(tokens: &Vec<Token>, index: &mut usize) -> Statement {
    *index += 1; // Go past 'return' Token
    let expression: Expression = get_next_expression(tokens, index);
    return Statement::new(StatementType::Return, None, Some(expression), None)
  }

  fn get_while_statement(tokens: &Vec<Token>, index: &mut usize) -> Statement {
    todo!("get_if_statement is not implemented yet")
  }

  fn get_expression_statement(tokens: &Vec<Token>, index: &mut usize) -> Statement {
    let expression: Expression = get_next_expression(tokens, index);
    Statement::new(StatementType::Expression, None, Some(expression), None)
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
      .expect("Lookahead failed in get_next_expression: Program cannot end to an Expression");

    // Literal Expressions
    if EXPRESSION_DELIMITERS.contains(&lookahead.value) {
      if lookahead.value == ";" { *index += 1; }
      if token.typ == &TokenType::Literal(DataType::Character) {
        *index += 1;
        return get_character_literal_expression(token.value.to_string())
      }
      if token.typ == &TokenType::Literal(DataType::Integer) {
        *index += 1;
        return get_integer_literal_expression(token.value.to_string())
      }
      if token.typ == &TokenType::Literal(DataType::String) {
        *index += 1;
        return get_string_literal_expression(token.value.to_string())
      }
      if token.typ == &TokenType::Identifier {
        *index += 1;
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
        Statement::new(StatementType::NoOperation, None, None, None),
        Statement::new(StatementType::NoOperation, None, None, None),
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
        Statement::new(StatementType::Return, None, Some(Expression::new(
          ExpressionType::Identifier, Some(variable_name.to_string()), None
        )), None),
      ]
    })
  }

  #[test]
  fn test_nested_compound_statement_ast() {
    let tokens: Vec<Token> = vec![
      Token::new(&TokenType::Delimiter, "{"),
      Token::new(&TokenType::Delimiter, "{"),
      Token::new(&TokenType::Delimiter, "}"),
      Token::new(&TokenType::Delimiter, ";"),
      Token::new(&TokenType::Delimiter, "}"),
    ];
    let program: Program = generate_ast(&tokens);
    assert_eq!(program, Program {
      statements: vec![
        Statement::new(
          StatementType::Compound, None, None, Some(vec![
            Statement::new(StatementType::Compound, None, None, Some(vec![])),
            Statement::new(StatementType::NoOperation, None, None, None),
          ])
        ),
      ] // statements
    })
  }

  #[test]
  fn test_unary_expression_statement_ast() {
    let unary_operator: &str  = "++";
    let variable_name: &str   = "var_name";
    let tokens: Vec<Token>    = vec![
      Token::new(&TokenType::UnaryOperator, unary_operator),
      Token::new(&TokenType::Identifier, variable_name),
      Token::new(&TokenType::Delimiter, ";"),
    ];
    let program: Program = generate_ast(&tokens);
    assert_eq!(program, Program {
      statements: vec![
        Statement::new(StatementType::Expression, None, Some(Expression::new(
          ExpressionType::Unary, Some(unary_operator.to_string()), Some(vec![
            Expression::new(
              ExpressionType::Identifier, Some(variable_name.to_string()), None
            ),
          ])
        )), None),
      ] // statements
    })
  }

  #[test]
  fn test_if_statement_ast() {
    let integer_value: &str = "1";
    let variable_name: &str = "var_name";
    let tokens: Vec<Token>  = vec![
      Token::new(&TokenType::Keyword, "if"),
      Token::new(&TokenType::Delimiter, "("),
      Token::new(&TokenType::Literal(DataType::Integer), integer_value),
      Token::new(&TokenType::Delimiter, ")"),
      Token::new(&TokenType::Delimiter, "{"),
      Token::new(&TokenType::Keyword, "return"),
      Token::new(&TokenType::Identifier, variable_name),
      Token::new(&TokenType::Delimiter, ";"),
      Token::new(&TokenType::Delimiter, "}"),
    ];
    let program: Program = generate_ast(&tokens);
    assert_eq!(program, Program {
      statements: vec![
        Statement {
          typ: StatementType::Conditional,
          value: Some("if".to_string()),
          expression: Some(Expression::new(
            ExpressionType::Literal(DataType::Integer), Some(integer_value.to_string()), None
          )),
          statements: Some(vec![Statement {
            typ: StatementType::Compound,
            value: None,
            expression: None,
            statements: Some(vec![
              Statement {
                typ: StatementType::Return,
                value: None,
                expression: Some(Expression {
                  typ: ExpressionType::Identifier,
                  value: Some(variable_name.to_string()),
                  expressions: None,
                }),
                statements: None,
              } // Statement (inner)
            ]) // Statement.statements (inner)
          }]), // Statement.statements (outer)
        } // Statement (outer)
      ] // Program.statements
    })
  }
}
