mod lexer;
mod parser;
mod span;
mod stream;

use lexer::Lexer;
use parser::{ast::Program, Parser};

pub fn compile(input: &str) -> Program {
    let tokens = Lexer::tokenize(input).expect("Error while tokenizing");
    let program = Parser::parse(tokens).expect("Error while parsing");
    program
}
