use crate::parser::ast::{
    Assignment, BinaryOp, Call, Expression, Function, Item, Literal, Path, Program, Statement,
    VariableDeclaration,
};

pub trait TargetCompiler {
    fn new() -> Self;
    fn compile_item(&mut self, item: Item) -> String;
    fn compile_variable_declaration(&mut self, var_declaration: VariableDeclaration) -> String;
    fn compile_expression(&mut self, expr: Expression) -> String;
    fn compile_literal(&mut self, literal: Literal) -> String;
    fn compile_function(&mut self, function: Function) -> String;
    fn compile_statement(&mut self, statement: Statement) -> String;
    fn compile_call(&mut self, call: Call) -> String;
    fn compile_path(&mut self, path: Path) -> String;
    fn compile_binary_op(&mut self, op: BinaryOp) -> String;
    fn compile_assignment(&mut self, assignment: Assignment) -> String;
}

pub struct Compiler<T: TargetCompiler> {
    target_compiler: T,
}

impl<T: TargetCompiler> Compiler<T> {
    fn new() -> Self {
        Self {
            target_compiler: T::new(),
        }
    }

    pub fn compile(program: Program) -> String {
        let mut this = Self::new();
        this.compile_program(program)
    }

    fn compile_program(&mut self, program: Program) -> String {
        let mut compiled_program = String::new();

        for item in program.items {
            let compiled_item = self.target_compiler.compile_item(item);
            compiled_program.push_str(&compiled_item);
        }
        compiled_program
    }
}
