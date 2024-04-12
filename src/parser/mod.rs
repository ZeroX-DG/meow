pub mod ast;
mod path;
mod expression;
mod statement;

use std::fmt::Debug;

use crate::stream::ParsingStream;

use self::ast::{Identifier, Item, ItemKind, Program};

use super::lexer::Token;

#[derive(PartialEq)]
pub enum ParsingError {
    UnexpectedToken(Token),
}

impl Debug for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsingError::UnexpectedToken(token) => {
                write!(f, "Unexpected token encountered: {:?}", token)
            }
        }
    }
}

macro_rules! expect_token {
    ($x:expr, $y:pat) => {
        match $x {
            $y => {}
            token => return Err(ParsingError::UnexpectedToken(token.clone()))
        }
    };
}

pub(crate) use expect_token;

pub struct Parser {
    pub program: Program
}

impl Parser {
    pub fn new() -> Self {
        Self {
            program: Program {
                items: Vec::new()
            }
        }
    }

    /// Parse at the top level of the program
    pub fn parse(tokens: Vec<Token>) -> Result<Program, ParsingError> {
        let mut parser = Parser::new();
        let mut tokens_iter = tokens.iter();
        let mut stream = ParsingStream::new(&mut tokens_iter, &Token::EOF);
        
        loop {
            let token = stream.peek();

            if let Token::EOF = token {
                break;
            }

            let item = Item {
                kind: ItemKind::Statement(statement::parse_statement(&mut stream)?)
            };
            parser.program.items.push(item);
        }

        return Ok(parser.program)
    }

    /// Parse an identiifier with syntax:
    /// Identifier = <Identifier>
    fn parse_identifier(stream: &mut ParsingStream<&Token>) -> Result<Identifier, ParsingError> {
        match stream.next() {
            Token::Identifier(ident) => Ok(Identifier { name: ident.clone() }),
            token => Err(ParsingError::UnexpectedToken(token.clone()))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_empty_program() {
        let tokens = vec![Token::EOF];
        let ast = Parser::parse(tokens).expect("Failed to parse tokens");
        let expected = Program {
            items: Vec::new()
        };
        assert_eq!(ast, expected)
    }
}