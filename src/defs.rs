#[warn(dead_code)]
use phf::phf_map;

pub(crate) const TOKEN_REGEXES: phf::Map<&str, TokenType> = phf_map!(
  // Whitespace
  r"^\s+"           => TokenType::None,

  // Comments
  r"^//.*"          => TokenType::None, // Single-line comment
  r"^/\*[\s\S]*\*/" => TokenType::None, // Multi-line comment

  // Numbers
  r"^\d+"           => TokenType::Literal(DataType::Integer),

  // Strings
  r#"^"([^"]*)""#   => TokenType::Literal(DataType::String),

  // Binary Operators
  r"^\+"            => TokenType::BinaryOperator,   // Addition
  r"^(/)[^/\*]"     => TokenType::BinaryOperator,   // Division
  r"^=="            => TokenType::BinaryOperator,   // Equals
  r"^\^"            => TokenType::BinaryOperator,   // Exponentiation
  r"^>="            => TokenType::BinaryOperator,   // GreaterOrEqual
  r"^(>)[^=]"       => TokenType::BinaryOperator,   // GreaterThan
  r"^<="            => TokenType::BinaryOperator,   // LessOrEqual
  r"^(<)[^=]"       => TokenType::BinaryOperator,   // LessThan
  r"^\*"            => TokenType::BinaryOperator,   // Multiplication
  r"^!="            => TokenType::BinaryOperator,   // NotEquals
  r"^-"             => TokenType::BinaryOperator,   // Substraction

  // Delimiters
  r"^\}"            => TokenType::Delimiter,
  r"^\)"            => TokenType::Delimiter,
  r"^\]"            => TokenType::Delimiter,
  r"^:"             => TokenType::Delimiter,
  r"^,"             => TokenType::Delimiter,
  r"^\{"            => TokenType::Delimiter,
  r"^\("            => TokenType::Delimiter,
  r"^\["            => TokenType::Delimiter,
  r"^;"             => TokenType::Delimiter,
);

#[derive(Debug)]
pub(crate) struct Token<'a> {
  pub(crate) typ: &'a TokenType,
  pub(crate) value: &'a str,
}

#[derive(Debug, PartialEq)]
pub(crate) enum TokenType {
  Delimiter,
  Literal(DataType),
  BinaryOperator,
  None,
}

#[derive(Debug, PartialEq)]
pub(crate) enum DataType {
  Integer,
  String,
}
