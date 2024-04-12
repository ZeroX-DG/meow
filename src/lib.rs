mod lexer;
mod parser;
mod compiler;
mod stream;
mod span;

use compiler::Compiler;
use lexer::Lexer;
use parser::Parser;

pub fn compile(input: &str) -> String {
    let tokens = Lexer::tokenize(input).expect("Error while tokenizing");
    let program = Parser::parse(tokens).expect("Error while parsing");
    Compiler::compile(program).expect("Error while compiling")
}