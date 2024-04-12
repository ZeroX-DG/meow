pub mod ast;
mod expression;
mod path;
mod statement;

use std::fmt::Debug;

use crate::{span::Span, stream::ParsingStream};

use self::ast::{Identifier, Item, ItemKind, Program};

use super::lexer::{Token, TokenType};

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
        match &$x.token_type {
            $y => {}
            _ => return Err(ParsingError::UnexpectedToken($x.clone())),
        }
    };
}

pub(crate) use expect_token;

pub struct Parser {
    pub program: Program,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            program: Program { items: Vec::new() },
        }
    }

    /// Parse at the top level of the program
    pub fn parse(tokens: Vec<Token>) -> Result<Program, ParsingError> {
        let mut parser = Parser::new();
        let mut tokens_iter = tokens.into_iter();
        let mut stream = ParsingStream::new(
            &mut tokens_iter,
            Token {
                token_type: TokenType::EOF,
                span: Span::from(((0, 0), (0, 0))), // random span cus we don't care
            },
        );

        loop {
            let token = stream.peek();

            if let TokenType::EOF = token.token_type {
                break;
            }

            let item = Item {
                kind: ItemKind::Statement(statement::parse_statement(&mut stream)?),
            };
            parser.program.items.push(item);
        }

        return Ok(parser.program);
    }

    /// Parse an identiifier with syntax:
    /// Identifier = <Identifier>
    fn parse_identifier(stream: &mut ParsingStream<Token>) -> Result<Identifier, ParsingError> {
        let token = stream.next();
        match token.token_type {
            TokenType::Identifier(ident) => Ok(Identifier {
                name: ident.clone(),
            }),
            _ => Err(ParsingError::UnexpectedToken(token.clone())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    pub fn assert_parsing_result<T, F>(
        token_types: Vec<TokenType>,
        parsing_fn: F,
        expected: Result<T, ParsingError>,
    ) where
        F: Fn(&mut ParsingStream<Token>) -> Result<T, ParsingError>,
        T: PartialEq + Debug,
    {
        let tokens = token_types
            .into_iter()
            .map(|token_type| Token {
                token_type,
                span: Span::from(((0, 0), (0, 0))),
            })
            .collect::<Vec<Token>>();
        let mut iter = tokens.into_iter();
        let mut stream = ParsingStream::new(
            &mut iter,
            Token {
                token_type: TokenType::EOF,
                span: Span::from(((0, 0), (0, 0))), // random span cus we don't care
            },
        );
        assert_eq!(parsing_fn(&mut stream), expected)
    }

    #[test]
    fn test_parse_empty_program() {
        let tokens = vec![Token {
            token_type: TokenType::EOF,
            span: Span::from(((0, 0), (0, 0))), // random span cus we don't care
        }];
        let ast = Parser::parse(tokens).expect("Failed to parse tokens");
        let expected = Program { items: Vec::new() };
        assert_eq!(ast, expected)
    }
}
