use crate::parser::ast::{
    ExpressionKind, ItemKind, LiteralKind, Operator, StatementKind, VariableDeclarationKind,
};

use super::TargetCompiler;
use std::fmt::Write;

pub struct JavaScriptCompiler;

impl TargetCompiler for JavaScriptCompiler {
    fn new() -> Self {
        Self {}
    }

    fn compile_item(&mut self, item: crate::parser::ast::Item) -> String {
        let mut compiled_item = match item.kind {
            ItemKind::Statement(statement) => self.compile_statement(statement),
        };
        compiled_item.push(';');
        compiled_item
    }

    fn compile_variable_declaration(
        &mut self,
        var_declaration: crate::parser::ast::VariableDeclaration,
    ) -> String {
        let mut output = String::new();

        if var_declaration.is_mutable {
            write!(&mut output, "let").unwrap();
        } else {
            write!(&mut output, "const").unwrap();
        }

        write!(&mut output, " ").unwrap();
        write!(&mut output, "{}", var_declaration.identifier.name).unwrap();

        match var_declaration.kind {
            VariableDeclarationKind::Declaration => {}
            VariableDeclarationKind::Init(expr) => {
                write!(&mut output, "={}", self.compile_expression(expr)).unwrap();
            }
        }

        output
    }

    fn compile_expression(&mut self, expr: crate::parser::ast::Expression) -> String {
        match expr.kind {
            ExpressionKind::Literal(literal) => self.compile_literal(literal),
            ExpressionKind::Function(function) => self.compile_function(function),
            ExpressionKind::Call(call) => self.compile_call(call),
            ExpressionKind::Path(path) => self.compile_path(path),
            ExpressionKind::BinaryOp(binary_op) => self.compile_binary_op(binary_op),
            _ => String::new(),
        }
    }

    fn compile_binary_op(&mut self, op: crate::parser::ast::BinaryOp) -> String {
        let op_char = match op.op {
            Operator::Add => '+',
            Operator::Subtract => '-',
            Operator::Multiply => '*',
            Operator::Divide => '/',
            _ => unimplemented!(),
        };
        format!(
            "{}{}{}",
            self.compile_expression(*op.left),
            op_char,
            self.compile_expression(*op.right)
        )
    }

    fn compile_literal(&mut self, literal: crate::parser::ast::Literal) -> String {
        match literal.kind {
            LiteralKind::String(value) => format!("\"{}\"", value),
            LiteralKind::Boolean(value) => format!("{}", if value { "true" } else { "false" }),
            LiteralKind::Float(value) => format!("{}", value),
            LiteralKind::Int(value) => format!("{}", value),
        }
    }

    fn compile_function(&mut self, function: crate::parser::ast::Function) -> String {
        let args = function
            .args
            .into_iter()
            .map(|arg| arg.identifier.name)
            .collect::<Vec<String>>()
            .join(",");

        let body = function
            .body
            .statements
            .into_iter()
            .map(|statement| self.compile_statement(statement))
            .collect::<Vec<String>>()
            .join(";");

        format!("({})=>{{{};}}", args, body)
    }

    fn compile_statement(&mut self, statement: crate::parser::ast::Statement) -> String {
        match statement.kind {
            StatementKind::Let(var_dclr) => self.compile_variable_declaration(var_dclr),
            StatementKind::Expr(expr) => self.compile_expression(expr),
            StatementKind::Assignment(assignment) => self.compile_assignment(assignment),
            StatementKind::Return(expr) => format!("return {}", self.compile_expression(expr)),
        }
    }

    fn compile_assignment(&mut self, assingment: crate::parser::ast::Assignment) -> String {
        format!(
            "{}={}",
            assingment.identifier.name,
            self.compile_expression(assingment.expression)
        )
    }

    fn compile_call(&mut self, call: crate::parser::ast::Call) -> String {
        let args = call
            .args
            .into_iter()
            .map(|arg| self.compile_expression(arg))
            .collect::<Vec<String>>()
            .join(",");
        if let ExpressionKind::MemberAccess(member_access) = call.function.kind {
            let expr = self.compile_expression(*member_access.object);
            return format!("({}).{}({})", expr, member_access.member.name, args);
        }
        if let ExpressionKind::Path(path) = call.function.kind {
            return format!("{}({})", self.compile_path(path), args);
        }
        String::new()
    }

    fn compile_path(&mut self, path: crate::parser::ast::Path) -> String {
        let segments = path.segments.into_iter().map(|segment| segment.ident.name);
        segments.collect::<Vec<String>>().join(".")
    }
}
