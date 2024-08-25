use crate::parser::ast::VariableDeclarationKind;

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
            VariableDeclarationKind::Init(_) => {}
        }

        write!(&mut output, ";").unwrap();

        output
    }
}
