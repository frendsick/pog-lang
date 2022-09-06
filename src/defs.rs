#[warn(dead_code)]
use phf::phf_ordered_map;

pub(crate) const TOKEN_REGEXES: phf::OrderedMap<&str, TokenType> = phf_ordered_map!(
  // Whitespace
  r"^\s+"           => TokenType::None,

  // Comments
  r"^//.*"          => TokenType::None, // Single-line comment
  r"^/\*[\s\S]*\*/" => TokenType::None, // Multi-line comment

  // Literals
  r"^'([^'])'"      => TokenType::Literal(DataType::Character),
  r"^\d+"           => TokenType::Literal(DataType::Integer),
  r#"^"([^"]*)""#   => TokenType::Literal(DataType::String),

  // Datatypes
  r"^char"          => TokenType::DataType,
  r"^int"           => TokenType::DataType,
  r"^str"           => TokenType::DataType,

  // Keywords
  r"^break"         => TokenType::Keyword,
  r"^continue"      => TokenType::Keyword,
  r"^elif"          => TokenType::Keyword,
  r"^else"          => TokenType::Keyword,
  r"^fun"           => TokenType::Keyword,
  r"^if"            => TokenType::Keyword,
  r"^return"        => TokenType::Keyword,
  r"^while"         => TokenType::Keyword,

  // Assignment Operators
  r"^(=)[^=]"       => TokenType::AssignmentOperator,   // Simple assignment
  r"^\+="           => TokenType::AssignmentOperator,   // Addition assignment
  r"^-="            => TokenType::AssignmentOperator,   // Substraction assignment
  r"^\*="           => TokenType::AssignmentOperator,   // Multiplication assignment
  r"^/="            => TokenType::AssignmentOperator,   // Division assignment

  // Binary Operators
  r"^\+"            => TokenType::BinaryOperator,   // Addition
  r"^/"             => TokenType::BinaryOperator,   // Division
  r"^=="            => TokenType::BinaryOperator,   // Equals
  r"^>="            => TokenType::BinaryOperator,   // GreaterOrEqual
  r"^>"             => TokenType::BinaryOperator,   // GreaterThan
  r"^<="            => TokenType::BinaryOperator,   // LessOrEqual
  r"^<"             => TokenType::BinaryOperator,   // LessThan
  r"^\*"            => TokenType::BinaryOperator,   // Multiplication
  r"^!="            => TokenType::BinaryOperator,   // NotEquals
  r"^-"             => TokenType::BinaryOperator,   // Substraction

  // Unary Operators
  r"^\+\+"          => TokenType::UnaryOperator,    // Increment
  r"^--"            => TokenType::UnaryOperator,    // Decrement
  r"^&"             => TokenType::UnaryOperator,    // Address
  r"^!"             => TokenType::UnaryOperator,    // Logical negation

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
  r"^->"            => TokenType::Delimiter,

  // Labels - Named value representing some value or other entity
  r"^[a-zA-Z_$][a-zA-Z_$0-9]*"  => TokenType::Label,
);

#[derive(Debug, PartialEq)]
pub(crate) struct Token<'a> {
  pub(crate) typ: &'a TokenType,
  pub(crate) value: &'a str,
}

impl<'a> Token<'a> {
  pub(crate) fn new(typ: &'a TokenType, value: &'a str) -> Self {
    Self { typ: typ, value: value }
  }
}

#[derive(Debug, PartialEq)]
pub(crate) enum TokenType {
  AssignmentOperator,
  BinaryOperator,
  DataType,
  Delimiter,
  Label,
  Literal(DataType),
  Keyword,
  UnaryOperator,
  None,
}

#[derive(Debug, PartialEq)]
pub(crate) enum DataType {
  Character,
  Integer,
  String,
}
