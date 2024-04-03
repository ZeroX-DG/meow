use crate::parser::ast::{Expression, ExpressionKind, ItemKind, LiteralKind, Program, Statement, StatementKind, VariableDeclaration, VariableDeclarationKind};

#[derive(Debug)]
pub struct CompileError(String);

pub struct Compiler;

impl Compiler {
    pub fn compile(program: Program) -> Result<String, CompileError> {
        let mut result = String::new();

        for item in program.items {
            let compiled = match item.kind {
                ItemKind::Statement(statement) => Compiler::compile_statement(statement)?
            };
            result.push_str(&compiled);
        }

        Ok(result)
    }

    fn compile_statement(statement: Statement) -> Result<String, CompileError> {
        match statement.kind {
            StatementKind::Let(declaration) => Compiler::compile_variable_declaration(declaration),
        }
    }

    fn compile_variable_declaration(declaration: VariableDeclaration) -> Result<String, CompileError> {
        match declaration.kind {
            VariableDeclarationKind::Declaration => Ok(format!("const {};", declaration.identifier.name)),
            VariableDeclarationKind::Init(expression) => Ok(format!("const {} = {};", declaration.identifier.name, Compiler::compile_expression(expression)?))
        }
    }

    fn compile_expression(expression: Expression) -> Result<String, CompileError> {
        match expression.kind {
            ExpressionKind::Literal(literal) => match literal.kind {
                LiteralKind::String(value) => Ok(format!("'{}'", value)),
                LiteralKind::Boolean(value) => Ok(value.to_string()),
                LiteralKind::Float(value) => Ok(value.to_string()),
                LiteralKind::Int(value) => Ok(value.to_string())
            }
        }
    }
}