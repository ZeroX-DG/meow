use crate::parser::ast::{
    ExpressionKind, LiteralKind, Operator, StatementKind, VariableDeclarationKind,
};

use super::TargetCompiler;
use std::fmt::Write;

pub struct JavaScriptCompiler;

impl TargetCompiler for JavaScriptCompiler {
    fn compile_variable_declaration(
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
                write!(&mut output, "={}", Self::compile_expression(expr)).unwrap();
            }
        }

        output
    }

    fn compile_expression(expr: crate::parser::ast::Expression) -> String {
        match expr.kind {
            ExpressionKind::Literal(literal) => Self::compile_literal(literal),
            ExpressionKind::Function(function) => Self::compile_function(function),
            ExpressionKind::Call(call) => Self::compile_call(call),
            ExpressionKind::Path(path) => Self::compile_path(path),
            ExpressionKind::BinaryOp(binary_op) => Self::compile_binary_op(binary_op),
            _ => String::new(),
        }
    }

    fn compile_binary_op(op: crate::parser::ast::BinaryOp) -> String {
        let op_char = match op.op {
            Operator::Add => '+',
            Operator::Subtract => '-',
            Operator::Multiply => '*',
            Operator::Divide => '/',
            _ => unimplemented!(),
        };
        format!(
            "{}{}{}",
            Self::compile_expression(*op.left),
            op_char,
            Self::compile_expression(*op.right)
        )
    }

    fn compile_literal(literal: crate::parser::ast::Literal) -> String {
        match literal.kind {
            LiteralKind::String(value) => format!("\"{}\"", value),
            LiteralKind::Boolean(value) => format!("{}", if value { "true" } else { "false" }),
            LiteralKind::Float(value) => format!("{}", value),
            LiteralKind::Int(value) => format!("{}", value),
        }
    }

    fn compile_function(function: crate::parser::ast::Function) -> String {
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
            .map(|statement| Self::compile_statement(statement))
            .collect::<Vec<String>>()
            .join(";");

        format!("({})=>{{ {}; }}", args, body)
    }

    fn compile_statement(statement: crate::parser::ast::Statement) -> String {
        match statement.kind {
            StatementKind::Let(var_dclr) => Self::compile_variable_declaration(var_dclr),
            StatementKind::Expr(expr) => Self::compile_expression(expr),
        }
    }

    fn compile_call(call: crate::parser::ast::Call) -> String {
        let args = call
            .args
            .into_iter()
            .map(|arg| Self::compile_expression(arg))
            .collect::<Vec<String>>()
            .join(",");
        if let ExpressionKind::MemberAccess(member_access) = call.function.kind {
            let expr = Self::compile_expression(*member_access.object);
            return format!("({}).{}({})", expr, member_access.member.name, args);
        }
        if let ExpressionKind::Path(path) = call.function.kind {
            return format!("{}({})", Self::compile_path(path), args);
        }
        String::new()
    }

    fn compile_path(path: crate::parser::ast::Path) -> String {
        let segments = path.segments.into_iter().map(|segment| segment.ident.name);
        segments.collect::<Vec<String>>().join(".")
    }
}
