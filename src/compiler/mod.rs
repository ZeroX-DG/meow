mod compiler;
mod javascript;
mod program_checker;

use std::fmt::Debug;

use codespan::Span;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
};
pub use compiler::*;
pub use javascript::*;
pub use program_checker::*;

use crate::parser::ast::Statement;

#[derive(PartialEq, Clone)]
pub enum CompileErrorType {
    Reassignment(Span),
}

pub struct CompileError {
    files: SimpleFiles<String, String>,
    diagnostic: Diagnostic<usize>,
    error_type: CompileErrorType,
}

impl PartialEq for CompileError {
    fn eq(&self, other: &Self) -> bool {
        self.error_type == other.error_type
    }
}

impl Debug for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.diagnostic.fmt(f)
    }
}

impl CompileError {
    pub fn diagnostic(&self) -> &Diagnostic<usize> {
        &self.diagnostic
    }

    pub fn error_type(&self) -> &CompileErrorType {
        &self.error_type
    }

    pub fn files(&self) -> &SimpleFiles<String, String> {
        &self.files
    }

    pub fn reassignment(statement: &Statement) -> Self {
        let diagnostic = Diagnostic::error()
            .with_code("E01")
            .with_message("Reassignment is not allowed");

        Self {
            error_type: CompileErrorType::Reassignment(statement.span),
            files: SimpleFiles::new(),
            diagnostic,
        }
    }

    pub fn add_file(mut self, name: String, source: String, range: Span) -> Self {
        let file_id = self.files.add(name, source);
        self.diagnostic = self
            .diagnostic
            .with_labels(vec![Label::primary(file_id, range)]);
        self
    }
}
