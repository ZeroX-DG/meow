use serde::Serialize;

use crate::{
    lexer::{Token, TokenData, TokenType},
    parser::{ast::TypeKind, basics, expect_token, expression, path},
    stream::{peek, ParsingStream},
};

use super::{
    ast::{Identifier, Type},
    expression::Expression,
    ParsingError,
};

#[derive(Debug, PartialEq, Serialize)]
pub struct Statement {
    pub kind: StatementKind,
}

#[derive(Debug, PartialEq, Serialize)]
pub enum StatementKind {
    Let(VariableDeclaration),
    Assignment(Assignment),
    Expr(Expression),
    Return(Expression),
}

#[derive(Debug, PartialEq, Serialize)]
pub struct Assignment {
    pub identifier: Identifier,
    pub expression: Expression,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct VariableDeclaration {
    pub identifier: Identifier,
    pub variable_type: Type,
    pub kind: VariableDeclarationKind,
    pub is_mutable: bool,
}

#[derive(Debug, PartialEq, Serialize)]
pub enum VariableDeclarationKind {
    Declaration,
    Init(Expression),
}

/// Parse a statement with syntax:
/// Statement = <LetStatement> | <ExpressionStatement>
pub fn parse_statement(stream: &mut ParsingStream<Token>) -> Result<Statement, ParsingError> {
    match peek!(stream).token_type {
        TokenType::Let => parse_let_statement(stream),
        TokenType::Return => parse_return_statement(stream),
        TokenType::Identifier if peek!(stream, 2).token_type == TokenType::Eq => {
            parse_assignment_statement(stream)
        }
        _ => parse_expression_statement(stream),
    }
}

/// Parse an AssignmentStatement with syntax:
/// AssignmentStatement = <Identifier> = <Expression> + ;
fn parse_assignment_statement(
    stream: &mut ParsingStream<Token>,
) -> Result<Statement, ParsingError> {
    let identifier = basics::parse_identifier(stream)?;
    expect_token!(&stream.next(), [Eq]);
    let expression = expression::parse_expression(stream)?;
    expect_token!(&stream.next(), [SemiConlon]);
    Ok(Statement {
        kind: StatementKind::Assignment(Assignment {
            identifier,
            expression,
        }),
    })
}

/// Parse an ExpressionStatement with syntax:
/// ExpressionStatement = <Expression> + ;
fn parse_expression_statement(
    stream: &mut ParsingStream<Token>,
) -> Result<Statement, ParsingError> {
    let expression = expression::parse_expression(stream)?;
    expect_token!(&stream.next(), [SemiConlon]);
    Ok(Statement {
        kind: StatementKind::Expr(expression),
    })
}

/// Parse an ReturnStatement with syntax:
/// ReturnStatement = <Return> + <Expression> + ;
fn parse_return_statement(stream: &mut ParsingStream<Token>) -> Result<Statement, ParsingError> {
    expect_token!(&stream.next(), [Return]);
    let expression = expression::parse_expression(stream)?;
    expect_token!(&stream.next(), [SemiConlon]);
    Ok(Statement {
        kind: StatementKind::Return(expression),
    })
}

/// Parse a LetStatement with syntax:
/// LetStatement = <VariableDeclaration> + ;
fn parse_let_statement(stream: &mut ParsingStream<Token>) -> Result<Statement, ParsingError> {
    let variable_declaration = parse_variable_declaration(stream)?;
    expect_token!(&stream.next(), [SemiConlon]);
    Ok(Statement {
        kind: StatementKind::Let(variable_declaration),
    })
}

/// Parse a variable declaration with syntax:
/// VariableDeclaration = <Let> + (<Mut>)? + <Identifier> + (: <Type>)? + = + <Expression> + ;
fn parse_variable_declaration(
    stream: &mut ParsingStream<Token>,
) -> Result<VariableDeclaration, ParsingError> {
    // Variable declaration start with keyword let
    expect_token!(&stream.next(), [Let]);

    let variable_name;
    let variable_name_token_span;
    let mut is_mutable = false;

    loop {
        let token = stream.next();

        expect_token!(&token, [Mut, Identifier]);

        match &token.token_data {
            TokenData::Identifier(identifier) => {
                variable_name = identifier.clone();
                variable_name_token_span = token.span;
                break;
            }
            // This can only be TokenType::Mut (due to verification before)
            _ => {
                is_mutable = true;
            }
        }
    }

    let variable_type = parse_type(stream)?;

    let variable_declaration_kind = match peek!(stream).token_type {
        TokenType::Eq => {
            stream.next(); // Consume Eq
            VariableDeclarationKind::Init(expression::parse_expression(stream)?)
        }
        _ => VariableDeclarationKind::Declaration,
    };

    let variable_declaration = VariableDeclaration {
        kind: variable_declaration_kind,
        identifier: Identifier {
            name: variable_name,
            span: variable_name_token_span,
        },
        variable_type,
        is_mutable,
    };

    Ok(variable_declaration)
}

/// Parsing a type
pub(crate) fn parse_type(stream: &mut ParsingStream<Token>) -> Result<Type, ParsingError> {
    let token = peek!(stream);

    expect_token!(&token, [Colon, Eq, SemiConlon, Comma]);

    let variable_type = match token.token_type {
        TokenType::Colon => {
            stream.next(); // Consume Colon
            let path = path::parse_path(stream)?;
            Type {
                kind: TypeKind::TypePath(path),
            }
        }
        TokenType::Eq | TokenType::SemiConlon | TokenType::Comma => Type {
            kind: TypeKind::Infer,
        },
        _ => unreachable!(),
    };

    Ok(variable_type)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        parser::{
            ast::{Expression, ExpressionKind, Literal, LiteralKind, Path, PathSegment},
            tests::{assert_parsing_result, make_token},
        },
        span::Span,
    };

    #[test]
    fn test_parse_statement_missing_semicolon() {
        // let mut hello: string
        let tokens = vec![
            make_token(TokenType::Let, TokenData::None),
            make_token(TokenType::Mut, TokenData::None),
            make_token(
                TokenType::Identifier,
                TokenData::Identifier(String::from("hello")),
            ),
            make_token(TokenType::Colon, TokenData::None),
            make_token(
                TokenType::Identifier,
                TokenData::Identifier(String::from("string")),
            ),
            make_token(TokenType::EOF, TokenData::None),
        ];
        assert_parsing_result(
            tokens,
            parse_statement,
            Err(ParsingError::UnexpectedToken {
                expected_token_types: vec![TokenType::SemiConlon],
                found_token: Token {
                    token_type: TokenType::EOF,
                    token_data: TokenData::None,
                    span: Span::from(((0, 0), (0, 0))),
                },
            }),
        );
    }

    #[test]
    fn test_parse_variable_declaration_immutable() {
        // let hello: string;
        let tokens = vec![
            make_token(TokenType::Let, TokenData::None),
            make_token(
                TokenType::Identifier,
                TokenData::Identifier(String::from("hello")),
            ),
            make_token(TokenType::Colon, TokenData::None),
            make_token(
                TokenType::Identifier,
                TokenData::Identifier(String::from("string")),
            ),
            make_token(TokenType::SemiConlon, TokenData::None),
            make_token(TokenType::EOF, TokenData::None),
        ];
        let expected = Statement {
            kind: StatementKind::Let(VariableDeclaration {
                variable_type: Type {
                    kind: TypeKind::TypePath(Path {
                        segments: vec![PathSegment {
                            ident: Identifier {
                                name: String::from("string"),
                                span: Span::default(),
                            },
                            span: Span::default(),
                        }],
                        span: Span::default(),
                    }),
                },
                identifier: Identifier {
                    name: String::from("hello"),
                    span: Span::default(),
                },
                kind: VariableDeclarationKind::Declaration,
                is_mutable: false,
            }),
        };
        assert_parsing_result(tokens, parse_statement, Ok(expected));
    }

    #[test]
    fn test_parse_variable_declaration() {
        // let mut hello: string;
        let tokens = vec![
            make_token(TokenType::Let, TokenData::None),
            make_token(TokenType::Mut, TokenData::None),
            make_token(
                TokenType::Identifier,
                TokenData::Identifier(String::from("hello")),
            ),
            make_token(TokenType::Colon, TokenData::None),
            make_token(
                TokenType::Identifier,
                TokenData::Identifier(String::from("string")),
            ),
            make_token(TokenType::SemiConlon, TokenData::None),
            make_token(TokenType::EOF, TokenData::None),
        ];
        let expected = Statement {
            kind: StatementKind::Let(VariableDeclaration {
                variable_type: Type {
                    kind: TypeKind::TypePath(Path {
                        segments: vec![PathSegment {
                            ident: Identifier {
                                name: String::from("string"),
                                span: Span::default(),
                            },
                            span: Span::default(),
                        }],
                        span: Span::default(),
                    }),
                },
                identifier: Identifier {
                    name: String::from("hello"),
                    span: Span::default(),
                },
                kind: VariableDeclarationKind::Declaration,
                is_mutable: true,
            }),
        };
        assert_parsing_result(tokens, parse_statement, Ok(expected));
    }

    #[test]
    fn test_parse_variable_declaration_infer_type() {
        // let mut hello: string;
        let tokens = vec![
            make_token(TokenType::Let, TokenData::None),
            make_token(TokenType::Mut, TokenData::None),
            make_token(
                TokenType::Identifier,
                TokenData::Identifier(String::from("hello")),
            ),
            make_token(TokenType::SemiConlon, TokenData::None),
            make_token(TokenType::EOF, TokenData::None),
        ];
        let expected = Statement {
            kind: StatementKind::Let(VariableDeclaration {
                variable_type: Type {
                    kind: TypeKind::Infer,
                },
                identifier: Identifier {
                    name: String::from("hello"),
                    span: Span::default(),
                },
                kind: VariableDeclarationKind::Declaration,
                is_mutable: true,
            }),
        };
        assert_parsing_result(tokens, parse_statement, Ok(expected));
    }

    #[test]
    fn test_parse_variable_initialization() {
        // let mut hello: string = "hello";
        let tokens = vec![
            make_token(TokenType::Let, TokenData::None),
            make_token(TokenType::Mut, TokenData::None),
            make_token(
                TokenType::Identifier,
                TokenData::Identifier(String::from("hello")),
            ),
            make_token(TokenType::Colon, TokenData::None),
            make_token(
                TokenType::Identifier,
                TokenData::Identifier(String::from("string")),
            ),
            make_token(TokenType::Eq, TokenData::None),
            make_token(TokenType::String, TokenData::String(String::from("hello"))),
            make_token(TokenType::SemiConlon, TokenData::None),
            make_token(TokenType::EOF, TokenData::None),
        ];
        let expected = Statement {
            kind: StatementKind::Let(VariableDeclaration {
                variable_type: Type {
                    kind: TypeKind::TypePath(Path {
                        segments: vec![PathSegment {
                            ident: Identifier {
                                name: String::from("string"),
                                span: Span::default(),
                            },
                            span: Span::default(),
                        }],
                        span: Span::default(),
                    }),
                },
                identifier: Identifier {
                    name: String::from("hello"),
                    span: Span::default(),
                },
                kind: VariableDeclarationKind::Init(Expression {
                    kind: ExpressionKind::Literal(Literal {
                        kind: LiteralKind::String(String::from("hello")),
                        span: Span::default(),
                    }),
                    span: Span::default(),
                }),
                is_mutable: true,
            }),
        };
        assert_parsing_result(tokens, parse_statement, Ok(expected));
    }

    #[test]
    fn test_parse_variable_initialization_infer_type() {
        // let mut hello = "hello";
        let tokens = vec![
            make_token(TokenType::Let, TokenData::None),
            make_token(TokenType::Mut, TokenData::None),
            make_token(
                TokenType::Identifier,
                TokenData::Identifier(String::from("hello")),
            ),
            make_token(TokenType::Eq, TokenData::None),
            make_token(TokenType::String, TokenData::String(String::from("hello"))),
            make_token(TokenType::SemiConlon, TokenData::None),
            make_token(TokenType::EOF, TokenData::None),
        ];

        let expected = Statement {
            kind: StatementKind::Let(VariableDeclaration {
                variable_type: Type {
                    kind: TypeKind::Infer,
                },
                identifier: Identifier {
                    name: String::from("hello"),
                    span: Span::default(),
                },
                kind: VariableDeclarationKind::Init(Expression {
                    kind: ExpressionKind::Literal(Literal {
                        kind: LiteralKind::String(String::from("hello")),
                        span: Span::default(),
                    }),
                    span: Span::default(),
                }),
                is_mutable: true,
            }),
        };
        assert_parsing_result(tokens, parse_statement, Ok(expected));
    }
}
