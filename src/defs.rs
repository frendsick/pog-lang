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
);

#[derive(Debug)]
pub(crate) struct Token<'a> {
  pub(crate) typ: &'a TokenType,
  pub(crate) value: &'a str,
}

#[derive(Debug, PartialEq)]
pub(crate) enum TokenType {
  Literal(DataType),
  None,
}

#[derive(Debug, PartialEq)]
pub(crate) enum DataType {
  Integer,
  String,
}
