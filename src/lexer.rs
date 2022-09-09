use regex::{Captures,Match,Regex};
use crate::defs::{TOKEN_REGEXES,DataType,Token,TokenType};

#[derive(Debug)]
pub(crate) struct Parser<'a> {
  tokenizer: Tokenizer<'a>,
}

impl<'a> Parser<'a> {
  pub(crate) fn init(code: &'a str) -> Self {
    Self {
      tokenizer: Tokenizer::init(code),
    }
  }

  pub(crate) fn parse(&mut self) -> Vec<Token> {
    let mut tokens: Vec<Token> = vec![];
    loop {
      let token: Option<Token> = self.tokenizer.get_next_token();
      if token.is_none() { break }
      tokens.push(token.unwrap());
    }
    return tokens;
  }
}

#[derive(Debug)]
struct Tokenizer<'a> {
  code: &'a str,
  cursor: usize,
}

impl<'a> Tokenizer<'a> {
  fn init(code: &'a str) -> Self {
    Self { code: code, cursor: 0 }
  }

  fn has_more_tokens(&self) -> bool {
    self.cursor < self.code.len()
  }

  fn get_next_token(&mut self) -> Option<Token<'a>> {
    if !self.has_more_tokens() { return None }
    
    // Test if the remaining code matches with any Token regex
    let unparsed_code: &str = self.code.split_at(self.cursor).1;
    for (regex, token_type) in TOKEN_REGEXES.entries() {
      let captures: Option<Captures> = Regex::new(regex).unwrap().captures(unparsed_code);
      if !captures.is_none() {

        // Take match from capture group if it is explicitly specified
        let whole_match: Option<Match> = captures.as_ref().unwrap().get(0);
        let mut token_match: Option<Match> = captures.unwrap().get(1);
        if token_match.is_none() {
          token_match = whole_match;
        }

        // Move cursor to the end of the parsed Token
        self.cursor += whole_match.unwrap().end();

        // Token should be skipped, e.g. whitespace or comment
        if token_type == &TokenType::None { return self.get_next_token() }

        return Some(Token::new(token_type, token_match.unwrap().as_str()))
      }
    }

    // TODO: Enhance error reporting
    panic!("Unknown Token at the start of the following code:\n{}", unparsed_code)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_lexing_comments() {
    let mut parser: Parser = Parser::init("/* multi\nline */ 42 // single-line");
    let tokens: Vec<Token> = parser.parse();
    assert_eq!( tokens, vec![
      Token::new(&TokenType::Literal(DataType::Integer), "42"),
    ]);
  }

  #[test]
  fn test_lexing_character() {
    let mut parser: Parser = Parser::init("'c'");
    let tokens: Vec<Token> = parser.parse();
    assert_eq!( tokens, vec![
      Token::new(&TokenType::Literal(DataType::Character), "c"),
    ]);
  }

  #[test]
  fn test_lexing_integer() {
    let mut parser: Parser = Parser::init("42");
    let tokens: Vec<Token> = parser.parse();
    assert_eq!( tokens, vec![
      Token::new(&TokenType::Literal(DataType::Integer), "42"),
    ]);
  }

  #[test]
  fn test_lexing_string() {
    let mut parser: Parser = Parser::init("\"This is String\"");
    let tokens: Vec<Token> = parser.parse();
    assert_eq!( tokens, vec![
      Token::new(&TokenType::Literal(DataType::String), "This is String"),
    ]);
  }

  fn count_token_types(token_type: TokenType) -> usize {
    let mut keyword_count: usize = 0;
    for typ in TOKEN_REGEXES.values() {
      if typ == &token_type { keyword_count+=1 }
    }
    return keyword_count;
  }

  #[test]
  fn test_lexing_datatypes() {
    let datatype_count: usize = count_token_types(TokenType::DataType);
    assert_eq!(datatype_count, 3, "Exhaustive testing of DataTypes");

    let datatypes: &str = "char int str";
    let mut parser: Parser = Parser::init(datatypes);
    let tokens: Vec<Token> = parser.parse();
    assert_eq!( tokens, vec![
      Token::new(&TokenType::DataType, "char"),
      Token::new(&TokenType::DataType, "int"),
      Token::new(&TokenType::DataType, "str"),
    ]);
  }

  #[test]
  fn test_lexing_keywords() {
    let keyword_count: usize = count_token_types(TokenType::Keyword);
    assert_eq!(keyword_count, 8, "Exhaustive testing of Keywords");

    let keywords: &str = "break continue elif else fun if return while";
    let mut parser: Parser = Parser::init(keywords);
    let tokens: Vec<Token> = parser.parse();
    assert_eq!( tokens, vec![
      Token::new(&TokenType::Keyword, "break"),
      Token::new(&TokenType::Keyword, "continue"),
      Token::new(&TokenType::Keyword, "elif"),
      Token::new(&TokenType::Keyword, "else"),
      Token::new(&TokenType::Keyword, "fun"),
      Token::new(&TokenType::Keyword, "if"),
      Token::new(&TokenType::Keyword, "return"),
      Token::new(&TokenType::Keyword, "while"),
    ]);
  }

  #[test]
  fn test_lexing_unary_operators() {
    let operator_count: usize = count_token_types(TokenType::UnaryOperator);
    assert_eq!(operator_count, 4, "Exhaustive testing of UnaryOperators");

    let operators: &str = "++ -- ! &";
    let mut parser: Parser = Parser::init(operators);
    let tokens: Vec<Token> = parser.parse();
    assert_eq!( tokens, vec![
      Token::new(&TokenType::UnaryOperator, "++"),
      Token::new(&TokenType::UnaryOperator, "--"),
      Token::new(&TokenType::UnaryOperator, "!"),
      Token::new(&TokenType::UnaryOperator, "&"),
    ]);
  }

  #[test]
  fn test_lexing_binary_operators() {
    let operator_count: usize = count_token_types(TokenType::BinaryOperator);
    assert_eq!(operator_count, 15, "Exhaustive testing of BinaryOperators");

    let operators: &str = "+ / == >= > <= < * != - = += -= *= /=";
    let mut parser: Parser = Parser::init(operators);
    let tokens: Vec<Token> = parser.parse();
    assert_eq!( tokens, vec![
      Token::new(&TokenType::BinaryOperator, "+"),
      Token::new(&TokenType::BinaryOperator, "/"),
      Token::new(&TokenType::BinaryOperator, "=="),
      Token::new(&TokenType::BinaryOperator, ">="),
      Token::new(&TokenType::BinaryOperator, ">"),
      Token::new(&TokenType::BinaryOperator, "<="),
      Token::new(&TokenType::BinaryOperator, "<"),
      Token::new(&TokenType::BinaryOperator, "*"),
      Token::new(&TokenType::BinaryOperator, "!="),
      Token::new(&TokenType::BinaryOperator, "-"),
      Token::new(&TokenType::BinaryOperator, "="),
      Token::new(&TokenType::BinaryOperator, "+="),
      Token::new(&TokenType::BinaryOperator, "-="),
      Token::new(&TokenType::BinaryOperator, "*="),
      Token::new(&TokenType::BinaryOperator, "/="),
    ]);
  }

  #[test]
  fn test_lexing_delimiters() {
    let delimiter_count: usize = count_token_types(TokenType::Delimiter);
    assert_eq!(delimiter_count, 10, "Exhaustive testing of Delimiters");

    let delimiters: &str = "()[]{}->:,;";
    let mut parser: Parser = Parser::init(delimiters);
    let tokens: Vec<Token> = parser.parse();
    assert_eq!( tokens, vec![
      Token::new(&TokenType::Delimiter, "("),
      Token::new(&TokenType::Delimiter, ")"),
      Token::new(&TokenType::Delimiter, "["),
      Token::new(&TokenType::Delimiter, "]"),
      Token::new(&TokenType::Delimiter, "{"),
      Token::new(&TokenType::Delimiter, "}"),
      Token::new(&TokenType::Delimiter, "->"),
      Token::new(&TokenType::Delimiter, ":"),
      Token::new(&TokenType::Delimiter, ","),
      Token::new(&TokenType::Delimiter, ";"),
    ]);
  }

  #[test]
  fn test_lexing_assignment_statement() {
    let mut parser: Parser = Parser::init("a += 42;");
    let tokens: Vec<Token> = parser.parse();
    assert_eq!( tokens, vec![
      Token::new(&TokenType::Identifier, "a"),
      Token::new(&TokenType::BinaryOperator, "+="),
      Token::new(&TokenType::Literal(DataType::Integer), "42"),
      Token::new(&TokenType::Delimiter, ";"),
    ]);
  }

  #[test]
  fn test_lexing_if_else() {
    let mut parser: Parser = Parser::init("if a==b { a++; } else { --a; }");
    let tokens: Vec<Token> = parser.parse();
    assert_eq!( tokens, vec![
      Token::new(&TokenType::Keyword, "if"),
      Token::new(&TokenType::Identifier, "a"),
      Token::new(&TokenType::BinaryOperator, "=="),
      Token::new(&TokenType::Identifier, "b"),
      Token::new(&TokenType::Delimiter, "{"),
      Token::new(&TokenType::Identifier, "a"),
      Token::new(&TokenType::UnaryOperator, "++"),
      Token::new(&TokenType::Delimiter, ";"),
      Token::new(&TokenType::Delimiter, "}"),
      Token::new(&TokenType::Keyword, "else"),
      Token::new(&TokenType::Delimiter, "{"),
      Token::new(&TokenType::UnaryOperator, "--"),
      Token::new(&TokenType::Identifier, "a"),
      Token::new(&TokenType::Delimiter, ";"),
      Token::new(&TokenType::Delimiter, "}"),
    ]);
  }
}
