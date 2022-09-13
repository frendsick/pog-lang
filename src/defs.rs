#[allow(dead_code)]
use phf::phf_ordered_map;
use std::fmt;

pub(crate) const DATATYPES: [&str; 3] = [
  "char",
  "int",
  "str",
];

pub(crate) const EXPRESSION_DELIMITERS: [&str; 7] = [
  ";",
  ")",
  "[",
  "]",
  "(",
  ")",
  ",",
];

pub(crate) const BINARY_OPERATORS: [&str; 15] = [
  "+",
  "-",
  "*",
  "/",
  "=",
  "+=",
  "-=",
  "*=",
  "/=",
  "==",
  "!=",
  ">",
  ">=",
  "<",
  "<=",
];

pub(crate) const TOKEN_REGEXES: phf::OrderedMap<&str, TokenType> = phf_ordered_map!(
  // Whitespace
  r"^\s+"           => TokenType::None,

  // Comments
  r"^//.*"          => TokenType::None, // Single-line comment
  r"^/\*[\s\S]*?\*/"=> TokenType::None, // Multi-line comment

  // Literals
  r"^'[^']'"        => TokenType::Literal(DataType::Character),
  r"^\d+"           => TokenType::Literal(DataType::Integer),
  r#"^"[^"]*""#     => TokenType::Literal(DataType::String),

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
  None,
  String,
}

impl fmt::Display for DataType {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      DataType::Character   => write!(f, "char"),
      DataType::Integer     => write!(f, "int"),
      DataType::None        => write!(f, "None"),
      DataType::String      => write!(f, "str"),
    }
  }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Parameter {
  pub(crate) name: String,
  pub(crate) typ: DataType,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Program {
  pub(crate) statements: Vec<Statement>,
}

impl fmt::Display for Program {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    for statement in &self.statements {
      writeln!(f, "{}", statement);
    }
    Ok(())
  }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Statement {
  pub(crate) typ: StatementType,
  pub(crate) value: Option<String>,
  pub(crate) options: Option<StatementOptions>,
  pub(crate) expression: Option<Expression>,
  pub(crate) statements: Option<Vec<Statement>>,
}

impl Statement {
  pub(crate) fn new(
    typ: StatementType,
    value: Option<String>,
    options: Option<StatementOptions>,
    expression: Option<Expression>,
    statements: Option<Vec<Statement>>
  ) -> Self {
    Self { typ: typ, value: value, options: options, expression: expression, statements: statements }
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

impl fmt::Display for Statement {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match &self.typ {
      StatementType::Compound => {
        write!(f, "{}\n", "{").unwrap();
        for statement in self.statements.as_ref().unwrap() {
          write!(f, "  {}\n", statement).unwrap();
        }
        write!(f, "{}", "}")
      },
      StatementType::Conditional => {
        let typ: &String = self.value.as_ref().unwrap();
        let expression: &Expression = self.expression.as_ref().unwrap();
        write!(f, "{} ({}) {}", typ, expression, self.statements.as_ref().unwrap()[0])
      },
      StatementType::Expression => {
        write!(f, "{};", self.expression.as_ref().unwrap())
      },
      StatementType::Function => {
        let name: &String = self.value.as_ref().unwrap();
        write!(f, "fun {} {}", name, self.statements.as_ref().unwrap()[0])
      },
      StatementType::Loop => {
        let typ: &String = self.value.as_ref().unwrap();
        let expression: &Expression = self.expression.as_ref().unwrap();
        write!(f, "{} ({}) {}", typ, expression, self.statements.as_ref().unwrap()[0])
      },
      StatementType::NoOperation => write!(f, "NoOp"),
      StatementType::Return => {
        write!(f, "return {};", self.expression.as_ref().unwrap())
      },
      StatementType::Variable(typ) => {
        let name: &String = self.value.as_ref().unwrap();
        write!(f, "{} {} = {};", typ, name, self.expression.as_ref().unwrap())
      }
    }
  }
}

#[derive(Debug, PartialEq)]
pub(crate) struct StatementOptions {
  pub(crate) parameters: Vec<Parameter>,
  pub(crate) return_type: DataType,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Expression {
  pub(crate) typ: ExpressionType,
  pub(crate) value: Option<String>,
  pub(crate) expressions: Option<Vec<Expression>>,
}

impl Expression {
  pub(crate) fn new(
    typ: ExpressionType, value: Option<String>, expressions: Option<Vec<Expression>>
  ) -> Self {
    Self { typ: typ, value: value, expressions: expressions }
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

impl fmt::Display for Expression {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match &self.typ {
      ExpressionType::Binary => {
        let operator: &String = self.value.as_ref().unwrap();
        let lhs: &String = self.expressions.as_ref().unwrap()[0]
          .value.as_ref().unwrap();
        if self.expressions.as_ref().unwrap()[1].expressions.is_none() {
          write!(f, "{} {} {}", lhs, operator, self.expressions.as_ref().unwrap()[1].value.as_ref().unwrap()).unwrap();
        } else {
          write!(f, "{} {} {}", lhs, operator, self.expressions.as_ref().unwrap()[1]).unwrap();
        }
        Ok(())
      },
      ExpressionType::Enclosure           => todo!(),
      ExpressionType::FunctionCall => {
        let name: &String = self.value.as_ref().unwrap();
        write!(f, "{}(", name).unwrap();
        for (index, expression) in self.expressions.as_ref().unwrap().iter().enumerate() {
          if index > 0 { write!(f, ", ").unwrap() }
          write!(f, "{}", expression).unwrap()
        }
        write!(f, ")")
      },
      ExpressionType::Identifier => write!(f, "{}", self.value.as_ref().unwrap()),
      ExpressionType::Indexing            => todo!(),
      ExpressionType::Literal(typ) => {
        let value: &String = self.value.as_ref().unwrap();
        write!(f, "{}", value)
      },
      ExpressionType::Unary => {
        let operator: &String = self.value.as_ref().unwrap();
        // TODO: Parsing Expression value
        let value: &String = self.expressions.as_ref().unwrap()[0]
          .value.as_ref().unwrap();
        write!(f, "{}{}", operator, value)
      }
    }
  }
}
