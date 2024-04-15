mod lexer;
mod parser;
mod span;
mod stream;

use lexer::Lexer;
use parser::Parser;

pub fn compile(input: &str) -> String {
    let tokens = Lexer::tokenize(input).expect("Error while tokenizing");
    let program = Parser::parse(tokens).expect("Error while parsing");
    format!("{:#?}", program)
}
