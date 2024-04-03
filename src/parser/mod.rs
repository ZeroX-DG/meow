pub mod ast;
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
        let (variable_name, is_mutable) = match stream.next() {
            Some(Token::Identifier(ident)) => (ident.to_owned(), false),
            Some(Token::Mut) => {
                let name_token = stream.next();
                let Some(Token::Identifier(ident)) = name_token else {
                    return Err(ParsingError::ExpectToken(format!("Expected Identifier but got: {:?}", name_token)))
                };
                (ident.to_owned(), true)
            }
            _ => {
                return Err(ParsingError::UnexpectedEOF);
            }
        };

        let variable_type = if let Some(Token::Colon) = stream.peek(1).get(0) {
            stream.next();
            let maybe_type_token = stream.next();
            if let Some(Token::Identifier(name)) = maybe_type_token {
                Type { kind: TypeKind::Ident(Identifier { name: name.to_owned(), mutable: false }) }
            } else {
                return Err(ParsingError::ExpectToken(format!("Expected Identifier but got: {:?}", maybe_type_token)));
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
            identifier: Identifier { name: variable_name, mutable: is_mutable },
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