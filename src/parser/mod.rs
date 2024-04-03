mod ast;
use crate::stream::ParsingStream;

use self::ast::{Expression, ExpressionKind, Identifier, Item, ItemKind, Literal, LiteralKind, Program, Statement, StatementKind, Type, TypeKind, VariableDeclaration, VariableDeclarationKind};

use super::lexer::Token;

#[derive(Debug)]
pub enum ParsingError {
    UnexpectedToken(Token),
    ExpectToken(String),
    UnexpectedEOF
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

    pub fn parse(tokens: Vec<Token>) -> Result<Program, ParsingError> {
        let mut parser = Parser::new();
        let mut tokens_iter = tokens.iter();
        let mut stream = ParsingStream::new(&mut tokens_iter);
        
        while let Some(token) = stream.next() {
            match token {
                Token::Let => {
                    let item = Item {
                        kind: ItemKind::Statement(Parser::parse_variable_declaration(&mut stream)?)
                    };
                    parser.program.items.push(item);
                }
                _ => {
                    return Err(ParsingError::UnexpectedToken(token.clone()))
                }
            }
        }

        return Ok(parser.program)
    }

    fn parse_variable_declaration(stream: &mut ParsingStream<&Token>) -> Result<Statement, ParsingError> {
        let Some(Token::Identifier(ident)) = stream.next() else {
            return Err(ParsingError::ExpectToken("Expected Identifier token".to_string()))
        };

        let variable_type = if let Some(Token::Colon) = stream.peek(1).get(0) {
            stream.next();
            if let Some(Token::Identifier(name)) = stream.next() {
                Type { kind: TypeKind::Ident(Identifier { name: name.to_owned(), mutable: false }) }
            } else {
                return Err(ParsingError::ExpectToken("Expected Identifier token".to_string()));
            }
        } else {
            Type { kind: TypeKind::Infer }
        };

        let variable_declaration_kind = if let Some(Token::Eq) = stream.next() {
            VariableDeclarationKind::Init(Parser::parse_expression(stream)?)
        } else {
            VariableDeclarationKind::Declaration
        };

        let variable_declaration = VariableDeclaration {
            kind: variable_declaration_kind,
            identifier: Identifier { name: ident.to_owned(), mutable: false },
            variable_type: variable_type
        };
        let statement = Statement {
            kind: StatementKind::Let(variable_declaration)
        };

        stream.consume_until(|tk| matches!(tk, Token::SemiConlon));
        
        Ok(statement)
    }

    fn parse_expression(stream: &mut ParsingStream<&Token>) -> Result<Expression, ParsingError> {
        let kind = match stream.next() {
            Some(tk) => match tk {
                Token::Boolean(value) => ExpressionKind::Literal(Literal { kind: LiteralKind::Boolean(*value) }),
                Token::Int(value) => ExpressionKind::Literal(Literal { kind: LiteralKind::Int(*value) }),
                Token::Float(value) => ExpressionKind::Literal(Literal { kind: LiteralKind::Float(*value) }),
                Token::String(value) => ExpressionKind::Literal(Literal { kind: LiteralKind::String(value.to_owned()) }),
                _ => return Err(ParsingError::UnexpectedToken(tk.clone()))
            }
            _ => return Err(ParsingError::UnexpectedEOF)
        };
        let expression = Expression { kind };
        Ok(expression)
    }
}