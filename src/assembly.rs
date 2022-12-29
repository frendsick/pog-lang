use crate::defs::{Program, Statement, StatementType};

pub(crate) fn generate_assembly(program: &Program) -> String {
  let mut assembly: String = "".to_string();

  for statement in &program.statements {
    let statement_assembly: String = get_statement_assembly(&statement);
    assembly.push_str(statement_assembly.as_str())
  }
  return assembly;

}

fn get_statement_assembly(statement: &Statement) -> String {
  match &statement.typ {
    StatementType::Compound => todo!(),
    StatementType::Conditional => todo!(),
    StatementType::Expression => todo!(),
    StatementType::Function => todo!(),
    StatementType::Loop => todo!(),
    StatementType::NoOperation => todo!(),
    StatementType::Return => todo!(),
    StatementType::Variable(typ) => todo!(),
  }
}
