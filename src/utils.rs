use regex::Regex;
use crate::defs::{DATATYPES,TOKEN_REGEXES,DataType,TokenType};

pub(crate) fn get_datatype_from_str(datatype_str: &str) -> DataType {
  assert_eq!(DATATYPES.len(), 3);
  match datatype_str {
    "char"    => DataType::Character,
    "int"     => DataType::Integer,
    "str"     => DataType::String,
    &_        => panic!("'{}' is not a valid DataType", datatype_str),
  }
}

pub(crate) fn get_token_type(token: &str) -> &TokenType {
  for (regex, token_type) in TOKEN_REGEXES.entries() {
    // Take match from capture group if it is explicitly specified
    let is_match: bool = Regex::new(regex).unwrap().is_match(token);
    if is_match { return token_type; }
  }
  panic!("Did not get TokenType for '{}'", token);
}
