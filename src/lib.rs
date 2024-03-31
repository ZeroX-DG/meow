mod lexer;
mod stream;

use lexer::Lexer;

pub fn compile(input: &str) -> String {
    let tokens = Lexer::tokenize(input);
    println!("{:?}", tokens);
    String::new()
}