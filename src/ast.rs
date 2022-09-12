use crate::defs::{BINARY_OPERATORS,DATATYPES,EXPRESSION_DELIMITERS};
use crate::defs::{DataType,Expression,ExpressionType,Program};
use crate::defs::{Statement,StatementOptions,Parameter,StatementType,TokenType};
use crate::utils::{get_datatype_from_str,get_token_type};

pub(crate) fn generate_ast(tokens: &Vec<&str>) -> Program {
  let mut statements: Vec<Statement> = vec![];

  // Parse all Statements from Tokens
  let mut index: usize = 0;
  while index < tokens.len() {
    statements.push(get_next_statement(&tokens, &mut index));
  }
  return Program {
    statements: statements,
  };

  fn get_next_statement(tokens: &Vec<&str>, index: &mut usize) -> Statement {
    let token: &str = tokens.get(*index).unwrap();

    // Statements with known first Token
    if token == ";"       { return get_no_operation_statement(index) }
    if token == "{"       { return get_compound_statement(tokens, index) }
    if token == "fun"     { return get_function_statement(tokens, index) }
    if token == "if"      { return get_block_statement(tokens, index, "if") }
    if token == "elif"    { return get_block_statement(tokens, index, "elif") }
    if token == "else"    { todo!("Parsing 'else' statements are not implemented") }
    if token == "return"  { return get_return_statement(tokens, index) }
    if token == "while"   { return get_block_statement(tokens, index, "while") }

    // Variable Definition
    if DATATYPES.contains(&token) {
      let variable_datatype: DataType = get_datatype_from_str(token);
      return get_variable_definition_statement(tokens, index, variable_datatype);
    }

    // Expression
    return get_expression_statement(tokens, index);
  }

  fn get_no_operation_statement(index: &mut usize) -> Statement {
    *index += 1; // Go past the semicolon
    Statement::new(StatementType::NoOperation, None, None, None, None)
  }

  fn get_compound_statement(tokens: &Vec<&str>, index: &mut usize) -> Statement {
    *index += 1; // Go past the open curly brace '{'
    if index >= &mut tokens.len() {
      panic!("Program cannot end to open curly brace")
    }

    // Get all Statements inside the Compound Statement
    let mut statements: Vec<Statement> = vec![];
    let mut token: &str = tokens.get(*index)
      .expect("Unclosed Compound Statement"); // TODO: Enhance error reporting

    // Closing curly bracket ends the compound statement
    while token != "}" {
      statements.push(get_next_statement(tokens, index));
      token = tokens.get(*index)
        .expect("Unclosed Compound Statement"); // TODO: Enhance error reporting
    }
    *index += 1; // Go past the closing curly brace '}'
    return Statement::new(StatementType::Compound, None, None, None, Some(statements));
  }

  fn get_expression_in_round_brackets(tokens: &Vec<&str>, index: &mut usize) -> Expression {
    // Test if the current Token's value is an opening round bracket '('
    let open_bracket: &str = tokens.get(*index)
      .expect("Expected '(' after function name but found nothing"); // TODO: Enhance error reporting
    assert_eq!("(", open_bracket, "Expected '(' after 'if' but found '{}'", open_bracket);
    *index += 1;

    // Get Expression between round brackets
    let expression: Expression = get_next_expression(tokens, index);

    // Test if the next Token's value is a closing round bracket ')'
    let open_bracket: &str = tokens.get(*index)
      .expect("Expected ')' after if's Expression but found nothing"); // TODO: Enhance error reporting
    assert_eq!(open_bracket, ")", "Expected ')' after if's Expression but found '{}'", open_bracket);
    *index += 1;
    return expression;
  }

  fn get_function_statement(tokens: &Vec<&str>, index: &mut usize) -> Statement {
    // Get function name from after 'fun' Token
    let function_name: String = tokens.get(*index+1)
      .expect("Expected function name but got nothing").to_string();

    // Test if the next Token's value is an opening round bracket '('
    let open_bracket: &str = tokens.get(*index+2)
      .expect("Expected '(' after function name but found nothing"); // TODO: Enhance error reporting
    assert_eq!(open_bracket, "(", "Expected '(' after 'if' but found '{}'", open_bracket);
    *index += 3; // Go past the round bracket '('

    // Gather function parameters
    let parameters: Vec<Parameter>  = get_function_parameters(&function_name, tokens, index);
    let return_type: DataType       = get_function_return_type(tokens, index);
    return Statement {
      typ: StatementType::Function,
      value: Some(function_name),
      options: Some(StatementOptions{
        parameters: parameters,
        return_type: return_type,
      }),
      expression: None,
      statements: Some(vec![get_compound_statement(tokens, index)]),
    }
  }

  fn get_function_parameters(function_name: &String, tokens: &Vec<&str>, index: &mut usize) -> Vec<Parameter> {
    let mut parameters: Vec<Parameter> = vec![];
    while *index < tokens.len() {
      // Example definition:
      // fun sum(int a, int b) { return a+b; }

      // Get Parameter's DataType
      let datatype: &str = tokens.get(*index)
        .expect("Expected parameter datatype but got nothing");
      if datatype == ")" {
        *index += 1;
        return parameters;
      }
      if !DATATYPES.contains(&datatype) {
        panic!("Expected parameter datatype, got '{}'", datatype);
      }

      // Get Parameter's name
      let parameter_name: String = tokens.get(*index+1)
        .expect("Expected parameter name but got nothing").to_string();

      // Append the parsed parameter to the Parameter list
      parameters.push(Parameter {
        name: parameter_name,
        typ: get_datatype_from_str(datatype),
      });
      *index += 2; // Go past parameter name

      // After Parameter declaration there should be either ',' or ')'
      // ',' => There will be more Parameters
      // ')' => Parameter declarations are over
      let delimiter: &str = tokens.get(*index)
        .expect("Expected ',' or ')' but found nothing");
      if delimiter == "," { *index += 1; }  // Go past comma to the next parameter
    }
    panic!("Could not parse parameters for function '{}'", function_name);
  }

  fn get_function_return_type(tokens: &Vec<&str>, index: &mut usize) -> DataType {
    let first_token_value: &str = tokens.get(*index)
      .expect("Deficient Tokens for function declaration");
    if first_token_value != "->" {
      if first_token_value != "{" {
        panic!("Expected '->' or '{}' but got '{}'", "{", first_token_value)
      }
      return DataType::None;
    }

    *index += 1;
    let datatype_str: &str = tokens.get(*index)
      .expect("Expected return type but got nothing");
    let datatype = get_datatype_from_str(datatype_str);

    let open_curly: &str = tokens.get(*index+1)
      .expect("Expected open curly bracket for function but got nothing");
    if open_curly != "{" {
      panic!("Expected function's opening curly bracket '{}' but got '{}'", "{", open_curly);
    }
    *index += 1;
    return datatype;
  }

  /// Block statements consist of a Keyword, a condition in round brackets and a Compound Statement
  fn get_block_statement(tokens: &Vec<&str>, index: &mut usize, block_type: &str) -> Statement {
    *index += 1; // Go past 'if' Token
    let expression: Expression  = get_expression_in_round_brackets(tokens, index);
    let statement: Statement    = get_compound_statement(tokens, index);

    // If, elif, else
    let mut typ: StatementType  = StatementType::Conditional;

    // While
    if block_type == "while" { typ = StatementType::Loop; }

    Statement {
      typ: typ,
      value: Some(block_type.to_string()),
      options: None,
      expression: Some(expression),
      statements: Some(vec![statement]),
    }
  }

  fn get_return_statement(tokens: &Vec<&str>, index: &mut usize) -> Statement {
    *index += 1; // Go past 'return' Token
    let expression: Expression = get_next_expression(tokens, index);
    return Statement::new(StatementType::Return, None, None, Some(expression), None)
  }

  fn get_variable_definition_statement(tokens: &Vec<&str>, index: &mut usize, datatype: DataType) -> Statement {
    // Get variable name from after the DataType Token ('char', 'int', 'str', ...)
    let variable_name: String = tokens.get(*index+1)
      .expect("Could not get Identifier for variable definition").to_string();

    // Verify if the next Token is '='
    // Note: Other assignment operators like '+=' and '*=' do not make sense when declaring variable
    let assignment_token_value: &str = tokens.get(*index+2)
      .expect("Expected '=' in variable definition but got nothing");
    if assignment_token_value != "=" {
      panic!("Expected '=' in variable definition but got '{}'", assignment_token_value)
    }

    // Go past the DataType, variable name Identifier and assignment Tokens
    *index += 3;
    Statement {
      typ: StatementType::Variable(datatype),
      value: Some(variable_name),
      options: None,
      expression: Some(get_next_expression(tokens, index)),
      statements: None
    }
  }

  fn get_expression_statement(tokens: &Vec<&str>, index: &mut usize) -> Statement {
    let expression: Expression = get_next_expression(tokens, index);
    Statement::new(StatementType::Expression, None, None, Some(expression), None)
  }

  fn get_next_expression(tokens: &Vec<&str>, index: &mut usize) -> Expression {
    let first_token: &str = tokens.get(*index)
      .expect("There are no Tokens left for Expression");

    // Unary Expressions
    // TODO: Parse post-increment and post-decrement expressions
    if get_token_type(first_token) == &TokenType::UnaryOperator {
      return get_unary_expression(first_token.to_string(), tokens, index)
    }
    let lookahead_value: &str = tokens.get(*index+1)
      .expect("Lookahead failed in get_next_expression: Program cannot end to an Expression");

    // Literal Expressions
    if EXPRESSION_DELIMITERS.contains(&lookahead_value) {
      *index += 1;
      if lookahead_value == "(" {
        return get_function_call_expression(first_token.to_string(), tokens, index);
      }
      if lookahead_value == ";" { *index += 1; }
      return get_literal_expression(&first_token);
    }

    // Define the type of Expression from the next Expression delimiter
    let mut temp_index: usize = *index;
    while temp_index < tokens.len() {
      temp_index += 1;
      let token_value: &str = tokens.get(temp_index)
        .expect("Could not parse current Expression's type");

      // TODO: Parse other binary Expressions than Literals as LHS
      if BINARY_OPERATORS.contains(&token_value) {
        return get_binary_expression(token_value.to_string(), tokens, index)
      }

      todo!("Parsing this kind of Expression is not implemented yet: {:?}", token_value);
    }
    todo!("Parsing this kind of Expression is not implemented yet: {:?}", first_token);
  }

  fn get_unary_expression(operator: String, tokens: &Vec<&str>, index: &mut usize) -> Expression {
    *index += 1; // Go past unary operator
    Expression {
      value: Some(operator),
      typ: ExpressionType::Unary,
      expressions: Some(
        vec![get_next_expression(tokens, index)]
      ),
    }
  }

  fn get_binary_expression(operator: String, tokens: &Vec<&str>, index: &mut usize) -> Expression {
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

  fn get_literal_expression(token: &str) -> Expression {
    if get_token_type(token) == &TokenType::Literal(DataType::Character) {
      return get_character_literal_expression(token.to_string())
    }
    if get_token_type(token) == &TokenType::Literal(DataType::Integer) {
      return get_integer_literal_expression(token.to_string())
    }
    if get_token_type(token) == &TokenType::Literal(DataType::String) {
      return get_string_literal_expression(token.to_string())
    }
    // Identifier
    return get_identifier_literal_expression(token.to_string())
  }

  fn get_function_call_expression(function_name: String, tokens: &Vec<&str>, index: &mut usize) -> Expression {
    *index += 1; // Go past opening round bracket of function call
    let mut token_value: &str = tokens.get(*index)
      .expect("Expected function parameter but got nothing");

    // Parse all parameter Expressions for the function
    let mut parameter_expressions: Vec<Expression> = vec![];
    while *index < tokens.len() && token_value != ")" {
      parameter_expressions.push(get_next_expression(tokens, index));

      // Verify if the current Token is ',' or ')'
      token_value = tokens.get(*index)
      .expect("Expected function parameter but got nothing");
      *index += 1;
    }

    // Verify that the last token of the expression is ';'
    token_value = tokens.get(*index)
      .expect("Expected ';' but got nothing");
    if token_value != ";" { panic!("Expected ';' but got '{}'", token_value) }
    *index += 1;

    Expression {
      typ: ExpressionType::FunctionCall,
      value: Some(function_name),
      expressions: Some(parameter_expressions),
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
    let tokens: Vec<&str> = vec![
      ";", ";",
    ];
    let program: Program = generate_ast(&tokens);
    assert_eq!(program, Program {
      statements: vec![
        Statement::new(StatementType::NoOperation, None, None, None, None),
        Statement::new(StatementType::NoOperation, None, None, None, None),
      ]
    })
  }

  #[test]
  fn test_return_statement_ast() {
    let variable_name: &str = "var_name";
    let tokens: Vec<&str> = vec![
      "return", variable_name, ";",
    ];
    let program: Program = generate_ast(&tokens);
    assert_eq!(program, Program {
      statements: vec![
        Statement::new(StatementType::Return, None, None, Some(Expression::new(
          ExpressionType::Identifier, Some(variable_name.to_string()), None
        )), None),
      ]
    })
  }

  #[test]
  fn test_nested_compound_statement_ast() {
    let tokens: Vec<&str> = vec![
      "{", "{", "}", ";", "}",
    ];
    let program: Program = generate_ast(&tokens);
    assert_eq!(program, Program {
      statements: vec![
        Statement::new(
          StatementType::Compound, None, None, None, Some(vec![
            Statement::new(StatementType::Compound, None, None, None, Some(vec![])),
            Statement::new(StatementType::NoOperation, None, None, None, None),
          ])
        ),
      ] // statements
    })
  }

  #[test]
  fn test_unary_expression_statement_ast() {
    let unary_operator: &str  = "++";
    let variable_name: &str   = "var_name";
    let tokens: Vec<&str>    = vec![
      unary_operator, variable_name, ";"
    ];
    let program: Program = generate_ast(&tokens);
    assert_eq!(program, Program {
      statements: vec![
        Statement::new(StatementType::Expression, None, None, Some(Expression::new(
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
    let tokens: Vec<&str>    = vec![
      "a", "+", "b", "*", "c", ";",
    ];
    let program: Program = generate_ast(&tokens);
    assert_eq!(program, Program {
      statements: vec![
        Statement::new(StatementType::Expression, None, None, Some(Expression::new(
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
  fn test_variable_assignment_statement_ast() {
    let variable_name: &str = "var_name";
    let tokens: Vec<&str> = vec![
      "int", variable_name, "=", "a", "*", "b", ";",
    ];
    let program: Program = generate_ast(&tokens);
    assert_eq!(program, Program {
      statements: vec![
        Statement {
          typ: StatementType::Variable(DataType::Integer),
          value: Some(variable_name.to_string()),
          options: None,
          expression: Some(Expression::new(
            ExpressionType::Binary, Some("*".to_string()), Some(vec![
              Expression::new(
                ExpressionType::Identifier, Some("a".to_string()), None
              ),
              Expression::new(
                ExpressionType::Identifier, Some("b".to_string()), None
              ),
            ]
          ))), // expression
          statements: None
        },
      ] // statements
    })
  }

  #[test]
  fn test_conditional_statement_ast() {
    // if(var_name) { return var_name; }
    let keyword: &str       = "if";
    let variable_name: &str = "var_name";
    let tokens: Vec<&str>  = vec![
      keyword, "(", variable_name, ")",
      "{",
        "return", variable_name, ";",
      "}",
    ];
    let program: Program = generate_ast(&tokens);
    assert_eq!(program, Program {
      statements: vec![
        Statement {
          typ: StatementType::Conditional,
          value: Some(keyword.to_string()),
          options: None,
          expression: Some(Expression::new(
            ExpressionType::Identifier, Some(variable_name.to_string()), None
          )),
          statements: Some(vec![Statement {
            typ: StatementType::Compound,
            value: None,
            options: None,
            expression: None,
            statements: Some(vec![
              Statement {
                typ: StatementType::Return,
                value: None,
                options: None,
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
    let tokens: Vec<&str>    = vec![
      keyword, "(", variable_name, ")",
      "{",
        unary_operator, variable_name, ";",
      "}",
    ];
    let program: Program = generate_ast(&tokens);
    assert_eq!(program, Program {
      statements: vec![
        Statement {
          typ: StatementType::Loop,
          value: Some(keyword.to_string()),
          options: None,
          expression: Some(Expression::new(
            ExpressionType::Identifier, Some(variable_name.to_string()), None
          )),
          statements: Some(vec![Statement {
            typ: StatementType::Compound,
            value: None,
            options: None,
            expression: None,
            statements: Some(vec![
              Statement {
                typ: StatementType::Expression,
                value: None,
                options: None,
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
