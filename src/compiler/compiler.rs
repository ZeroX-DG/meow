use std::marker::PhantomData;

use crate::parser::ast::{
    BinaryOp, Call, Expression, Function, ItemKind, Literal, Path, Program, Statement,
    StatementKind, VariableDeclaration,
};

pub trait TargetCompiler {
    fn compile_variable_declaration(var_declaration: VariableDeclaration) -> String;
    fn compile_expression(expr: Expression) -> String;
    fn compile_literal(literal: Literal) -> String;
    fn compile_function(function: Function) -> String;
    fn compile_statement(statement: Statement) -> String;
    fn compile_call(call: Call) -> String;
    fn compile_path(path: Path) -> String;
    fn compile_binary_op(op: BinaryOp) -> String;
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
                    StatementKind::Expr(expr) => self.compile_expression(expr),
                },
            };

            compiled_program.push_str(&compiled_item);
            compiled_program.push(';');
        }
        compiled_program
    }

    fn compile_variable_declaration(
        &mut self,
        variable_declaration: VariableDeclaration,
    ) -> String {
        Target::compile_variable_declaration(variable_declaration)
    }

    fn compile_expression(&mut self, expression: Expression) -> String {
        Target::compile_expression(expression)
    }
}
