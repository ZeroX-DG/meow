use crate::parser::ast::{Block, Expression, ExpressionKind, ItemKind, LiteralKind, Program, Statement, StatementKind, VariableDeclaration, VariableDeclarationKind};

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
        let result = match statement.kind {
            StatementKind::Let(declaration) => Compiler::compile_variable_declaration(declaration)?,
            StatementKind::Expr(expression) => Compiler::compile_expression(expression)?,
        };
        Ok(format!("{}\n", result))
    }

    fn compile_variable_declaration(declaration: VariableDeclaration) -> Result<String, CompileError> {
        let declaration_keyword = if declaration.identifier.mutable { "let" } else { "const" };
        match declaration.kind {
            VariableDeclarationKind::Declaration => Ok(format!("{} {};", declaration_keyword, declaration.identifier.name)),
            VariableDeclarationKind::Init(expression) => Ok(format!("{} {} = {};", declaration_keyword, declaration.identifier.name, Compiler::compile_expression(expression)?))
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
            ExpressionKind::Function(function) => {
                let args: Vec<String> = function.args.iter().map(|arg| arg.identifier.name.clone()).collect();
                Ok(format!("({}) => {{\n{}}}", args.join(", "), Compiler::compile_block(function.body)?))
            }
            ExpressionKind::PropertyAccess(path) => {
                Ok(path.iter().map(|ident| ident.name.to_owned()).collect::<Vec<String>>().join("."))
            }
        }
    }

    fn compile_block(block: Block) -> Result<String, CompileError> {
        let mut result = String::new();
        for statement in block.statements {
            result.push_str(&Compiler::compile_statement(statement)?);
        }
        Ok(result)
    }
}