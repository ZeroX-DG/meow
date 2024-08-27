use std::marker::PhantomData;

use crate::parser::ast::{
    BinaryOp, Call, Expression, Function, Item, Literal, Path, Program, Statement,
    VariableDeclaration,
};

pub trait TargetCompiler {
    fn compile_item(item: Item) -> String;
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
    __target: PhantomData<T>,
}

impl<Target: TargetCompiler> Compiler<Target> {
    fn new() -> Self {
        Self {
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
            let compiled_item = Target::compile_item(item);
            compiled_program.push_str(&compiled_item);
        }
        compiled_program
    }
}
