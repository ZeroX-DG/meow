use std::collections::HashSet;

use crate::parser::ast::{ItemKind, Program, StatementKind};

use super::CompileError;

pub struct ProgramChecker {
    scope: Scope,
}

pub struct Scope {
    assignments: HashSet<String>,
}

impl ProgramChecker {
    pub fn new() -> Self {
        Self {
            scope: Scope {
                assignments: HashSet::new(),
            },
        }
    }

    pub fn check(&mut self, program: &Program) -> Result<(), CompileError> {
        for item in &program.items {
            match &item.kind {
                ItemKind::Statement(statement) => match &statement.kind {
                    StatementKind::Assignment(assignment) => {
                        if self.scope.assignments.contains(&assignment.identifier.name) {
                            return Err(CompileError::reassignment(statement));
                        }
                        self.scope
                            .assignments
                            .insert(assignment.identifier.name.clone());
                    }
                    _ => {}
                },
            }
        }
        Ok(())
    }
}
