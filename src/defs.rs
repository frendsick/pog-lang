#[allow(dead_code)]
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

  // Unary Operators
  r"^\+\+"          => TokenType::UnaryOperator,    // Increment
  r"^--"            => TokenType::UnaryOperator,    // Decrement
  r"^&"             => TokenType::UnaryOperator,    // Address
  r"^(!)[^=]"       => TokenType::UnaryOperator,    // Logical negation

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

  // Binary Operators
  r"^=="            => TokenType::BinaryOperator,   // Equals
  r"^="             => TokenType::BinaryOperator,   // Simple assignment
  r"^\+="           => TokenType::BinaryOperator,   // Addition assignment
  r"^-="            => TokenType::BinaryOperator,   // Substraction assignment
  r"^\*="           => TokenType::BinaryOperator,   // Multiplication assignment
  r"^/="            => TokenType::BinaryOperator,   // Division assignment
  r"^\+"            => TokenType::BinaryOperator,   // Addition
  r"^/"             => TokenType::BinaryOperator,   // Division
  r"^>="            => TokenType::BinaryOperator,   // GreaterOrEqual
  r"^>"             => TokenType::BinaryOperator,   // GreaterThan
  r"^<="            => TokenType::BinaryOperator,   // LessOrEqual
  r"^<"             => TokenType::BinaryOperator,   // LessThan
  r"^\*"            => TokenType::BinaryOperator,   // Multiplication
  r"^!="            => TokenType::BinaryOperator,   // NotEquals
  r"^-"             => TokenType::BinaryOperator,   // Substraction

  // Identifier - Named value representing some value or other entity
  r"^[a-zA-Z_$][a-zA-Z_$0-9]*"  => TokenType::Identifier,
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
  Identifier,
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

#[derive(Debug, PartialEq)]
pub(crate) struct Program {
  pub(crate) statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Statement {
  pub(crate) typ: StatementType,
  pub(crate) expression: Option<Expression>,
  pub(crate) statements: Option<Vec<Statement>>,
}

impl Statement {
  pub(crate) fn new(
    typ: StatementType, expression: Option<Expression>, statements: Option<Vec<Statement>>
  ) -> Self {
    Self { typ: typ, expression: expression, statements: statements }
  }
}

#[derive(Debug, PartialEq)]
pub(crate) enum StatementType {
  Compound,
  Conditional,
  Expression,
  Function,
  Loop,
  NoOperation,
  Return,
  Variable(DataType),
}

#[derive(Debug, PartialEq)]
pub(crate) struct Expression {
  pub(crate) value: Option<String>,
  pub(crate) typ: ExpressionType,
  pub(crate) expressions: Option<Vec<Expression>>,
}

impl Expression {
  pub(crate) fn new(
    value: Option<String>, typ: ExpressionType, expressions: Option<Vec<Expression>>
  ) -> Self {
    Self { value: value, typ: typ, expressions: expressions }
  }
}

#[derive(Debug, PartialEq)]
pub(crate) enum ExpressionType {
  Binary,
  Enclosure,
  FunctionCall,
  Identifier,
  Indexing,
  Literal(DataType),
  Unary,
}
