pub mod ast;
use std::fmt::Debug;

use crate::stream::ParsingStream;

use self::ast::{Block, Expression, ExpressionKind, Function, FunctionArg, Identifier, Item, ItemKind, Literal, LiteralKind, Program, Statement, StatementKind, Type, TypeKind, VariableDeclaration, VariableDeclarationKind};

use super::lexer::Token;

pub enum ParsingError {
    UnexpectedToken(Token),
    ExpectToken(String),
    UnexpectedEOF
}

impl Debug for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsingError::ExpectToken(error) => {
                write!(f, "{}", error)
            }
            ParsingError::UnexpectedToken(token) => {
                write!(f, "Unexpected token encountered: {:?}", token)
            }
            ParsingError::UnexpectedEOF => {
                write!(f, "Unexpected End of File")
            }
        }
    }
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
        let mut stream = ParsingStream::new(&mut tokens_iter, &Token::EOF);
        
        while let Some(token) = stream.peek(1).get(0).cloned() {
            match token {
                Token::Let | Token::Identifier(_) => {
                    let item = Item {
                        kind: ItemKind::Statement(Parser::parse_statement(&mut stream)?)
                    };
                    parser.program.items.push(item);
                }
                Token::EOF => break,
                _ => {
                    return Err(ParsingError::UnexpectedToken(token.clone()))
                }
            }
        }

        return Ok(parser.program)
    }

    fn parse_statement(stream: &mut ParsingStream<&Token>) -> Result<Statement, ParsingError> {
        let kind = match stream.peek(1).get(0).cloned().unwrap_or(&Token::EOF) {
            Token::Let => StatementKind::Let(Parser::parse_variable_declaration(stream)?),
            Token::EOF => return Err(ParsingError::UnexpectedEOF),
            _ => StatementKind::Expr(Parser::parse_expression(stream)?),
        };

        Parser::expect_token(stream.next(), Token::SemiConlon)?;
        Ok(Statement { kind })
    }

    fn parse_variable_declaration(stream: &mut ParsingStream<&Token>) -> Result<VariableDeclaration, ParsingError> {
        Parser::expect_token(stream.next(), Token::Let)?;
        let (variable_name, is_mutable) = match stream.next() {
            Token::Identifier(ident) => (ident.to_owned(), false),
            Token::Mut => {
                let name_token = stream.next();
                let Token::Identifier(ident) = name_token else {
                    return Err(ParsingError::ExpectToken(format!("Expected Identifier but got: {:?}", name_token)))
                };
                (ident.to_owned(), true)
            }
            Token::EOF => return Err(ParsingError::UnexpectedEOF),
            token => return Err(ParsingError::UnexpectedToken(token.clone()))
        };

        let variable_type = if let Some(Token::Colon) = stream.peek(1).get(0) {
            stream.next();
            let maybe_type_token = stream.next();
            if let Token::Identifier(name) = maybe_type_token {
                Type { kind: TypeKind::Ident(Identifier { name: name.to_owned(), mutable: false }) }
            } else {
                return Err(ParsingError::ExpectToken(format!("Expected Identifier but got: {:?}", maybe_type_token)));
            }
        } else {
            Type { kind: TypeKind::Infer }
        };

        let variable_declaration_kind = if let Token::Eq = stream.next() {
            VariableDeclarationKind::Init(Parser::parse_expression(stream)?)
        } else {
            VariableDeclarationKind::Declaration
        };

        let variable_declaration = VariableDeclaration {
            kind: variable_declaration_kind,
            identifier: Identifier { name: variable_name, mutable: is_mutable },
            variable_type: variable_type
        };
        
        Ok(variable_declaration)
    }

    fn parse_expression(stream: &mut ParsingStream<&Token>) -> Result<Expression, ParsingError> {
        let kind = match stream.peek(1).get(0).cloned() {
            Some(token) => match token {
                Token::Boolean(value) => {
                    stream.next();
                    ExpressionKind::Literal(Literal { kind: LiteralKind::Boolean(*value) })
                },
                Token::Int(value) => {
                    stream.next();
                    ExpressionKind::Literal(Literal { kind: LiteralKind::Int(*value) })
                },
                Token::Float(value) => {
                    stream.next();
                    ExpressionKind::Literal(Literal { kind: LiteralKind::Float(*value) })
                },
                Token::String(value) => {
                    stream.next();
                    ExpressionKind::Literal(Literal { kind: LiteralKind::String(value.to_owned()) })
                },
                Token::Function => ExpressionKind::Function(Parser::parse_function_expression(stream)?),
                Token::Identifier(_) => {
                    let property_path = Parser::parse_property_access_expression(stream)?;
                    match stream.peek(1).get(0).cloned().unwrap_or(&Token::EOF) {
                        Token::ParenOpen => {
                            stream.next();
                            let property_access = Expression { kind: ExpressionKind::PropertyAccess(property_path) };
                            let mut args = Vec::new();
                            loop {
                                match stream.peek(1).get(0).cloned().unwrap_or(&Token::EOF) {
                                    Token::ParenClose => {
                                        stream.next();
                                        break;
                                    },
                                    _ => {
                                        args.push(Parser::parse_expression(stream)?);

                                        match stream.next() {
                                            Token::Comma => continue,
                                            Token::ParenClose => break,
                                            Token::EOF => return Err(ParsingError::UnexpectedEOF),
                                            token => return Err(ParsingError::UnexpectedToken(token.clone()))
                                        }
                                    }
                                }
                            }
                            ExpressionKind::Call(Box::new(property_access), args)
                        }
                        Token::EOF => return Err(ParsingError::UnexpectedEOF),
                        _ => ExpressionKind::PropertyAccess(property_path),
                    }
                },
                _ => return Err(ParsingError::UnexpectedToken(token.clone()))
            }
            _ => return Err(ParsingError::UnexpectedEOF)
        };
        let expression = Expression { kind };
        Ok(expression)
    }

    fn parse_property_access_expression(stream: &mut ParsingStream<&Token>) -> Result<Vec<Identifier>, ParsingError> {
        let mut path = Vec::new();
        loop {
            match stream.peek(1).get(0).cloned().unwrap_or(&Token::EOF) {
                Token::Identifier(ident) => {
                    path.push(Identifier { mutable: false, name: ident.clone() });
                    stream.next();
                }
                Token::Period => {
                    stream.next();
                },
                _ => break,
            }
        }
        Ok(path)
    }

    fn parse_function_expression(stream: &mut ParsingStream<&Token>) -> Result<Function, ParsingError> {
        Parser::expect_token(stream.next(), Token::Function)?;

        let mut args: Vec<FunctionArg> = Vec::new();
        Parser::expect_token(stream.next(), Token::ParenOpen)?;

        loop {
            let arg_name = match stream.next() {
                Token::Identifier(name) => name.to_owned(),
                Token::EOF => return Err(ParsingError::UnexpectedEOF),
                token => return Err(ParsingError::ExpectToken(format!("Expecting Identifier but got {:?}", token))),
            };
            Parser::expect_token(stream.next(), Token::Colon)?;
            let arg_type = match stream.next() {
                Token::Identifier(type_name) => type_name.to_owned(),
                Token::EOF => return Err(ParsingError::UnexpectedEOF),
                token => return Err(ParsingError::ExpectToken(format!("Expecting Identifier but got {:?}", token))),
            };

            let arg = FunctionArg {
                identifier: Identifier { name: arg_name, mutable: false },
                arg_type: Type { kind: TypeKind::Ident(Identifier { name: arg_type, mutable: false }) }
            };
            args.push(arg);

            match stream.next() {
                Token::Comma => continue,
                Token::ParenClose => break,
                Token::EOF => return Err(ParsingError::UnexpectedEOF),
                token => return Err(ParsingError::UnexpectedToken(token.clone())),
            }
        }
        
        let body = Parser::parse_block(stream)?;
        let function = Function {
            args,
            body
        };

        Ok(function)
    }

    fn parse_block(stream: &mut ParsingStream<&Token>) -> Result<Block, ParsingError> {
        Parser::expect_token(stream.next(), Token::CurlyBracketOpen)?;
        let mut statements = Vec::new();

        loop {
            if let Some(Token::CurlyBracketClose) = stream.peek(1).get(0) {
                stream.next();
                break;
            }
            let statement = Parser::parse_statement(stream)?;
            statements.push(statement);
        }

        let block = Block {
            statements
        };
        Ok(block)
    }

    fn expect_token(input: &Token, expected: Token) -> Result<(), ParsingError> {
        match input {
            token if *token == expected => Ok(()),
            Token::EOF => Err(ParsingError::UnexpectedEOF),
            token => Err(ParsingError::ExpectToken(format!("Expecting token {:?} but got {:?}", expected, token))),
        }
    }
}