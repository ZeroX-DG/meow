use std::marker::PhantomData;

use crate::parser::ast::{Function, ItemKind, Program, StatementKind, VariableDeclaration};

pub trait TargetCompiler {
    fn compile_variable_declaration(var_declaration: VariableDeclaration) -> String;
}

pub struct Compiler<T: TargetCompiler> {
    root_scope: Scope,
    __target: PhantomData<T>,
}

#[derive(Debug, Default)]
pub struct Scope {
    functions: Vec<Function>,
}

impl<Target: TargetCompiler> Compiler<Target> {
    fn new() -> Self {
        Self {
            root_scope: Scope::default(),
            __target: PhantomData::default(),
        }
    }

    pub fn compile(program: Program) -> String {
        let mut this = Self::new();
        this.compile_program(program)
    }

    fn compile_program(&mut self, program: Program) -> String {
        let mut compiled_program = String::new();

        for item in program.items {
            let compiled_item = match item.kind {
                ItemKind::Statement(statement) => match statement.kind {
                    StatementKind::Let(variable_declaration) => {
                        self.compile_variable_declaration(variable_declaration)
                    }
                    _ => String::new(),
                },
                _ => String::new(),
            };

            compiled_program.push_str(&compiled_item);
        }
        compiled_program
    }

    fn compile_variable_declaration(
        &mut self,
        variable_declaration: VariableDeclaration,
    ) -> String {
        Target::compile_variable_declaration(variable_declaration)
    }
}
