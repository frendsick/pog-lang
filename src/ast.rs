use crate::defs::{BINARY_OPERATORS,DATATYPES,EXPRESSION_DELIMITERS};
use crate::defs::{DataType,Expression,ExpressionType,Program};
use crate::defs::{Statement,StatementType,Token,TokenType};
use crate::utils::get_datatype_from_str;

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
      if token.value == "if"      { return get_block_statement(tokens, index, "if") }
      if token.value == "return"  { return get_return_statement(tokens, index) }
      if token.value == "while"   { return get_block_statement(tokens, index, "while") }
      panic!("Unexpected Keyword at the beginning of Statement: '{}'", token.value)
    }

    // Variable Definition
    if DATATYPES.contains(&token.value) {
      let variable_datatype: DataType = get_datatype_from_str(token.value);
      return get_variable_definition_statement(tokens, index, variable_datatype);
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

  /// Block statements consist of a Keyword, a condition in round brackets and a Compound Statement
  fn get_block_statement(tokens: &Vec<Token>, index: &mut usize, block_type: &str) -> Statement {
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

    let statement: Statement = get_compound_statement(tokens, index);
    Statement {
      typ: StatementType::Conditional,
      value: Some(block_type.to_string()),
      expression: Some(expression),
      statements: Some(vec![statement]),
    }
  }

  fn get_return_statement(tokens: &Vec<Token>, index: &mut usize) -> Statement {
    *index += 1; // Go past 'return' Token
    let expression: Expression = get_next_expression(tokens, index);
    return Statement::new(StatementType::Return, None, Some(expression), None)
  }

  fn get_variable_definition_statement(tokens: &Vec<Token>, index: &mut usize, datatype: DataType) -> Statement {
    // Get variable name from after the DataType Token ('char', 'int', 'str', ...)
    let variable_name: String = tokens.get(*index+1)
      .expect("Could not get Identifier for variable definition").value.to_string();

    // Verify if the next Token is '='
    // Note: Other assignment operators like '+=' and '*=' do not make sense when declaring variable
    let assignment_token_value: &str = tokens.get(*index+2)
      .expect("Expected '=' in variable definition but got nothing").value;
    if assignment_token_value != "=" {
      panic!("Expected '=' in variable definition but got '{}'", assignment_token_value)
    }

    // Go past the DataType, variable name Identifier and assignment Tokens
    *index += 3;
    Statement {
      typ: StatementType::Variable(datatype),
      value: Some(variable_name),
      expression: Some(get_next_expression(tokens, index)),
      statements: None
    }
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
      *index += 1;
      if lookahead.value == ";" { *index += 1; }
      return get_literal_expression(token);
    }

    // TODO: Parse other binary Expressions than Literals as LHS
    if BINARY_OPERATORS.contains(&lookahead.value) {
      return get_binary_expression(lookahead.value.to_string(), tokens, index)
    }
    todo!("Parsing this kind of Expression is not implemented yet: {:?}", token);
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

  fn get_binary_expression(operator: String, tokens: &Vec<Token>, index: &mut usize) -> Expression {
    // TODO: Parse other LHS Expressions than Literals
    let left: Expression  = get_literal_expression(tokens.get(*index).unwrap());
    *index += 2; // Go past binary operator
    let right: Expression = get_next_expression(tokens, index);
    Expression {
      typ: ExpressionType::Binary,
      value: Some(operator),
      expressions: Some(vec![
        left,
        right,
      ])
    }
  }

  fn get_literal_expression(token: &Token) -> Expression {
    if token.typ == &TokenType::Literal(DataType::Character) {
      return get_character_literal_expression(token.value.to_string())
    }
    if token.typ == &TokenType::Literal(DataType::Integer) {
      return get_integer_literal_expression(token.value.to_string())
    }
    if token.typ == &TokenType::Literal(DataType::String) {
      return get_string_literal_expression(token.value.to_string())
    }
    // Identifier
    return get_identifier_literal_expression(token.value.to_string())
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
  fn test_binary_expression_statement_ast() {
    // TODO: Test correct operator precedence
    // a + b * c
    let tokens: Vec<Token>    = vec![
      Token::new(&TokenType::Identifier, "a"),
      Token::new(&TokenType::BinaryOperator, "+"),
      Token::new(&TokenType::Identifier, "b"),
      Token::new(&TokenType::BinaryOperator, "*"),
      Token::new(&TokenType::Identifier, "c"),
      Token::new(&TokenType::Delimiter, ";"),
    ];
    let program: Program = generate_ast(&tokens);
    assert_eq!(program, Program {
      statements: vec![
        Statement::new(StatementType::Expression, None, Some(Expression::new(
          ExpressionType::Binary, Some("+".to_string()), Some(vec![
            Expression::new(
              ExpressionType::Identifier, Some("a".to_string()), None
            ),
            Expression::new(
              ExpressionType::Binary, Some("*".to_string()), Some(vec![
                Expression::new(
                  ExpressionType::Identifier, Some("b".to_string()), None
                ),
                Expression::new(
                  ExpressionType::Identifier, Some("c".to_string()), None
                ),
              ])
            ),
          ])
        )), None),
      ] // statements
    })
  }

  #[test]
  fn test_conditional_statement_ast() {
    // if(var_name) { return var_name; }
    let keyword: &str       = "if";
    let variable_name: &str = "var_name";
    let tokens: Vec<Token>  = vec![
      Token::new(&TokenType::Keyword, keyword),
      Token::new(&TokenType::Delimiter, "("),
      Token::new(&TokenType::Identifier, variable_name),
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
          value: Some(keyword.to_string()),
          expression: Some(Expression::new(
            ExpressionType::Identifier, Some(variable_name.to_string()), None
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

  #[test]
  fn test_loop_statement_ast() {
    // while(var_name) { --var_name; }
    let keyword: &str         = "while";
    let unary_operator: &str  = "--";
    let variable_name: &str   = "var_name";
    let tokens: Vec<Token>    = vec![
      Token::new(&TokenType::Keyword, keyword),
      Token::new(&TokenType::Delimiter, "("),
      Token::new(&TokenType::Identifier, variable_name),
      Token::new(&TokenType::Delimiter, ")"),
      Token::new(&TokenType::Delimiter, "{"),
      Token::new(&TokenType::UnaryOperator, unary_operator),
      Token::new(&TokenType::Identifier, variable_name),
      Token::new(&TokenType::Delimiter, ";"),
      Token::new(&TokenType::Delimiter, "}"),
    ];
    let program: Program = generate_ast(&tokens);
    assert_eq!(program, Program {
      statements: vec![
        Statement {
          typ: StatementType::Conditional,
          value: Some(keyword.to_string()),
          expression: Some(Expression::new(
            ExpressionType::Identifier, Some(variable_name.to_string()), None
          )),
          statements: Some(vec![Statement {
            typ: StatementType::Compound,
            value: None,
            expression: None,
            statements: Some(vec![
              Statement {
                typ: StatementType::Expression,
                value: None,
                expression: Some(Expression {
                  typ: ExpressionType::Unary,
                  value: Some(unary_operator.to_string()),
                  expressions: Some(vec![Expression{
                    typ: ExpressionType::Identifier,
                    value: Some(variable_name.to_string()),
                    expressions: None,
                  }]),
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
