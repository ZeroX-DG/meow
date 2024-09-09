pub mod ast;
mod basics;
mod expression;
mod path;
mod statement;

use std::fmt::Debug;

use crate::{
    lexer::TokenData,
    span::Span,
    stream::{peek, ParsingStream},
};

use self::ast::{Item, ItemKind, Program};

use super::lexer::{Token, TokenType};

#[derive(PartialEq)]
pub enum ParsingError {
    UnexpectedToken {
        expected_token_types: Vec<TokenType>,
        found_token: Token,
    },
}

impl Debug for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsingError::UnexpectedToken {
                expected_token_types,
                found_token,
            } => {
                write!(
                    f,
                    "Expected token types {}, but found: {:?}",
                    expected_token_types
                        .iter()
                        .map(|token_type| format!("{:?}", token_type))
                        .collect::<Vec<String>>()
                        .join(" or "),
                    found_token
                )
            }
        }
    }
}

macro_rules! expect_token {
    ($token:expr, [$($token_type:ident),*]) => {
        match $token.token_type {
            $(
                TokenType::$token_type => {}
            ),*,
            _ => return Err(ParsingError::UnexpectedToken {
                found_token: $token.clone(),
                expected_token_types: vec![$(TokenType::$token_type),*],
            })
        }
    };
}

macro_rules! match_token {
    ($token:expr, {$($($token_type:ident)|* => $block:block),*}) => {
        match $token.token_type {
            $(
                $(TokenType::$token_type)|* => $block
            ),*,
            _ => return Err(ParsingError::UnexpectedToken {
                found_token: $token.clone(),
                expected_token_types: vec![$($(TokenType::$token_type),*),*],
            })
        }
    };
    ($token:expr, {$($($token_type:ident)|* => $expr:expr),*}) => {
        match $token.token_type {
            $(
                $(TokenType::$token_type)|* => {
                    $expr
                }
            ),*,
            _ => return Err(ParsingError::UnexpectedToken {
                found_token: $token.clone(),
                expected_token_types: vec![$($(TokenType::$token_type),*),*],
            })
        }
    }
}

pub(crate) use expect_token;
pub(crate) use match_token;

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
        let last_span = tokens
            .last()
            .cloned()
            .map(|token| token.span)
            .unwrap_or(Span::from(((1, 0), (1, 1))));

        let mut tokens_iter = tokens.into_iter();

        let mut stream = ParsingStream::new(
            &mut tokens_iter,
            Token {
                token_type: TokenType::EOF,
                token_data: TokenData::None,
                span: last_span,
            },
        );

        loop {
            let token = peek!(stream);

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
}

#[cfg(test)]
mod tests {
    use super::*;

    pub fn make_token(token_type: TokenType, data: TokenData) -> Token {
        Token {
            token_type,
            token_data: data,
            span: Span::from(((0, 0), (0, 0))),
        }
    }

    pub fn assert_parsing_result<T, F>(
        tokens: Vec<Token>,
        parsing_fn: F,
        expected: Result<T, ParsingError>,
    ) where
        F: Fn(&mut ParsingStream<Token>) -> Result<T, ParsingError>,
        T: PartialEq + Debug,
    {
        let mut iter = tokens.clone().into_iter();
        let mut stream = ParsingStream::new(
            &mut iter,
            Token {
                token_type: TokenType::EOF,
                token_data: TokenData::None,
                span: Span::from(((0, 0), (0, 0))),
            },
        );
        assert_eq!(parsing_fn(&mut stream), expected)
    }

    #[test]
    fn test_parse_empty_program() {
        let tokens = vec![Token {
            token_type: TokenType::EOF,
            token_data: TokenData::None,
            span: Span::from(((0, 0), (0, 0))), // random span cus we don't care
        }];
        let ast = Parser::parse(tokens).expect("Failed to parse tokens");
        let expected = Program { items: Vec::new() };
        assert_eq!(ast, expected)
    }
}
