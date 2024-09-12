pub mod ast;
mod basics;
mod expression;
mod path;
mod statement;

use std::fmt::Debug;

use crate::{
    lexer::TokenData,
    stream::{peek, ParsingStream},
};

use self::ast::{Item, ItemKind, Program};

use super::lexer::{Token, TokenType};
use codespan::Span;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
};

#[derive(PartialEq, Clone)]
pub enum ParsingErrorType {
    UnexpectedToken(Token),
    MissingSemicolon(Span),
}

pub struct ParsingError {
    files: SimpleFiles<String, String>,
    diagnostic: Diagnostic<usize>,
    error_type: ParsingErrorType,
}

impl PartialEq for ParsingError {
    fn eq(&self, other: &Self) -> bool {
        self.error_type == other.error_type
    }
}

impl Debug for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.diagnostic.fmt(f)
    }
}

impl ParsingError {
    pub fn diagnostic(&self) -> &Diagnostic<usize> {
        &self.diagnostic
    }

    pub fn error_type(&self) -> &ParsingErrorType {
        &self.error_type
    }

    pub fn files(&self) -> &SimpleFiles<String, String> {
        &self.files
    }

    pub fn unexpected_token(found_token: &Token, expected_token_types: Vec<TokenType>) -> Self {
        let mut notes = String::from("Expected ");

        notes.push_str(
            &expected_token_types
                .iter()
                .map(|token| format!("{:?}", token))
                .collect::<Vec<String>>()
                .join(" or "),
        );

        let diagnostic = Diagnostic::error()
            .with_code("E01")
            .with_message(format!("Unexpected token {:?}", found_token.token_type))
            .with_notes(vec![notes]);

        Self {
            error_type: ParsingErrorType::UnexpectedToken(found_token.clone()),
            files: SimpleFiles::new(),
            diagnostic,
        }
    }

    pub fn missing_semicolon(span: Span) -> Self {
        let diagnostic = Diagnostic::error()
            .with_code("E02")
            .with_message("Missing semicolon");

        Self {
            error_type: ParsingErrorType::MissingSemicolon(span),
            files: SimpleFiles::new(),
            diagnostic,
        }
    }

    pub fn add_file(mut self, name: String, source: String, range: Span) -> Self {
        let file_id = self.files.add(name, source);
        self.diagnostic = self
            .diagnostic
            .with_labels(vec![Label::primary(file_id, range)]);
        self
    }
}

macro_rules! expect_token {
    ($token:expr, [SemiConlon]) => {
        match $token.token_type {
            TokenType::SemiConlon => {},
            _ => return Err(ParsingError::missing_semicolon(codespan::Span::new($token.span.start().0, $token.span.start().0)))
        }
    };
    ($token:expr, [$($token_type:ident),*]) => {
        match $token.token_type {
            $(
                TokenType::$token_type => {}
            ),*,
            _ => return Err(ParsingError::unexpected_token(&$token, vec![$(TokenType::$token_type),*]))
        }
    };
}

macro_rules! match_token {
    ($token:expr, {$($($token_type:ident)|* => $block:block),*}) => {
        match $token.token_type {
            $(
                $(TokenType::$token_type)|* => $block
            ),*,
            _ => return Err(ParsingError::unexpected_token(&$token, vec![$($(TokenType::$token_type),*),*]))
        }
    };
    ($token:expr, {$($($token_type:ident)|* => $expr:expr),*}) => {
        match $token.token_type {
            $(
                $(TokenType::$token_type)|* => {
                    $expr
                }
            ),*,
            _ => return Err(ParsingError::unexpected_token(&$token, vec![$($(TokenType::$token_type),*),*]))
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
            .unwrap_or(Span::new(0, 1));

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
            span: Span::default(),
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
                span: Span::default(),
            },
        );
        assert_eq!(parsing_fn(&mut stream), expected)
    }

    #[test]
    fn test_parse_empty_program() {
        let tokens = vec![Token {
            token_type: TokenType::EOF,
            token_data: TokenData::None,
            span: Span::default(),
        }];
        let ast = Parser::parse(tokens).expect("Failed to parse tokens");
        let expected = Program { items: Vec::new() };
        assert_eq!(ast, expected)
    }
}
