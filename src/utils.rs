use crate::defs::{DataType, DATATYPES};

pub(crate) fn get_datatype_from_str(datatype_str: &str) -> DataType {
  assert_eq!(DATATYPES.len(), 3);
  match datatype_str {
    "char"    => DataType::Character,
    "int"     => DataType::Integer,
    "str"     => DataType::String,
    &_        => panic!("'{}' is not a valid DataType", datatype_str),
  }
}
