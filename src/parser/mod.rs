pub mod ast;
use std::fmt::Debug;

use crate::stream::ParsingStream;

use self::ast::{Expression, ExpressionKind, Identifier, Item, ItemKind, Literal, LiteralKind, Path, PathSegment, Program, Statement, StatementKind, Type, TypeKind, VariableDeclaration, VariableDeclarationKind};

use super::lexer::Token;

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
                kind: ItemKind::Statement(Parser::parse_statement(&mut stream)?)
            };
            parser.program.items.push(item);
        }

        return Ok(parser.program)
    }

    /// Parse a statement with syntax:
    /// Statement = <LetStatement> | <ExpressionStatement> + ;
    fn parse_statement(stream: &mut ParsingStream<&Token>) -> Result<Statement, ParsingError> {
        let kind = match stream.peek() {
            Token::Let => StatementKind::Let(Parser::parse_variable_declaration(stream)?),
            _ => StatementKind::Expr(Parser::parse_expression(stream)?),
        };

        expect_token!(stream.next(), Token::SemiConlon);
        Ok(Statement { kind })
    }

    /// Parse a variable declaration with syntax:
    /// VariableDeclaration = <Let> + <Identifier> + (<Mut>)? + (: <Type>)? + = + <Expression> + ; 
    fn parse_variable_declaration(stream: &mut ParsingStream<&Token>) -> Result<VariableDeclaration, ParsingError> {
        // Variable declaration start with keyword let
        expect_token!(stream.next(), Token::Let);

        let variable_name;
        let mut is_mutable = false;

        loop {
            match stream.next() {
                Token::Mut => { is_mutable = true; }
                Token::Identifier(identifier) => {
                    variable_name = identifier.clone();
                    break;
                },
                token => return Err(ParsingError::UnexpectedToken(token.clone()))
            }
        }

        let variable_type = match stream.peek() {
            Token::Colon => {
                stream.next(); // Consume Colon
                let path = Parser::parse_path(stream)?;
                Type { kind: TypeKind::TypePath(path) }
            }
            Token::Eq | Token::SemiConlon => Type { kind: TypeKind::Infer },
            token => return Err(ParsingError::UnexpectedToken(token.clone()))
        };

        let variable_declaration_kind = match stream.peek() {
            Token::Eq => {
                stream.next(); // Consume Eq
                VariableDeclarationKind::Init(Parser::parse_expression(stream)?)
            },
            _ => VariableDeclarationKind::Declaration
        };

        let variable_declaration = VariableDeclaration {
            kind: variable_declaration_kind,
            identifier: Identifier { name: variable_name },
            variable_type,
            is_mutable
        };
        
        Ok(variable_declaration)
    }

    /// Parse a path with syntax:
    /// Path = <PathSegment> + (:: + <PathSegment>)*
    fn parse_path(stream: &mut ParsingStream<&Token>) -> Result<Path, ParsingError> {
        let mut path = Path {
            segments: Vec::new()
        };

        loop {
            let segment = Parser::parse_path_segment(stream)?;
            path.segments.push(segment);

            match stream.peek() {
                Token::ColonColon => { stream.next(); }
                _ => break
            }
        }

        Ok(path)
    }

    /// Parse a path with syntax:
    /// PathSegment = <Identifier>
    fn parse_path_segment(stream: &mut ParsingStream<&Token>) -> Result<PathSegment, ParsingError> {
        let ident = Parser::parse_identifier(stream)?;
        Ok(PathSegment{ ident })
    }

    /// Parse an identiifier with syntax:
    /// Identifier = <Identifier>
    fn parse_identifier(stream: &mut ParsingStream<&Token>) -> Result<Identifier, ParsingError> {
        match stream.next() {
            Token::Identifier(ident) => Ok(Identifier { name: ident.clone() }),
            token => Err(ParsingError::UnexpectedToken(token.clone()))
        }
    }

    /// Parse an expression with syntax:
    /// Expression = <LiteralExpression>
    fn parse_expression(stream: &mut ParsingStream<&Token>) -> Result<Expression, ParsingError> {
        match stream.peek() {
            Token::Boolean(_) | Token::Int(_) | Token::Float(_) | Token::String(_) => Parser::parse_literal_expression(stream),
            token => return Err(ParsingError::UnexpectedToken(token.clone()))
        }
    }

    /// Parse a literal expression with syntax:
    /// LiteralExpression = <Boolean> | <Int> | <Float> | <String>
    fn parse_literal_expression(stream: &mut ParsingStream<&Token>) -> Result<Expression, ParsingError> {
        let kind = match stream.next() {
            Token::Boolean(value) => {
                ExpressionKind::Literal(Literal { kind: LiteralKind::Boolean(*value) })
            },
            Token::Int(value) => {
                ExpressionKind::Literal(Literal { kind: LiteralKind::Int(*value) })
            },
            Token::Float(value) => {
                ExpressionKind::Literal(Literal { kind: LiteralKind::Float(*value) })
            },
            Token::String(value) => {
                ExpressionKind::Literal(Literal { kind: LiteralKind::String(value.to_owned()) })
            },
            token => return Err(ParsingError::UnexpectedToken(token.clone()))
        };

        Ok(Expression { kind })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_empty_program() {
        let tokens = vec![];
        let ast = Parser::parse(tokens).expect("Failed to parse tokens");
        let expected = Program {
            items: Vec::new()
        };
        assert_eq!(ast, expected)
    }

    #[test]
    fn test_parse_variable_declaration() {
        // let mut hello: string;
        let tokens = vec![
            Token::Let,
            Token::Mut,
            Token::Identifier(String::from("hello")),
            Token::Colon,
            Token::Identifier(String::from("string")),
            Token::SemiConlon,
            Token::EOF
        ];
        let ast = Parser::parse(tokens).expect("Failed to parse tokens");
        let expected = Program {
            items: vec![
                Item{
                    kind: ItemKind::Statement(Statement { kind: StatementKind::Let(VariableDeclaration {
                        variable_type: Type {
                            kind: TypeKind::TypePath(Path { segments: vec![PathSegment { ident: Identifier { name: String::from("string") } }] })
                        },
                        identifier: Identifier { name: String::from("hello") },
                        kind: VariableDeclarationKind::Declaration,
                        is_mutable: true
                    })}) 
                }
            ]
        };
        assert_eq!(ast, expected)
    }
}