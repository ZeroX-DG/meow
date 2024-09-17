use codespan::Span;
use serde::Serialize;

use crate::{
    lexer::{Token, TokenType},
    parser::{
        ast::TypeKind, basics, expect_semicolon, expect_token, expression, match_token, path,
    },
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
    pub span: Span,
}

#[derive(Debug, PartialEq, Serialize)]
pub enum StatementKind {
    Assignment(Assignment),
    Expr(Expression),
    Return(Expression),
}

#[derive(Debug, PartialEq, Serialize)]
pub struct Assignment {
    pub identifier: Identifier,
    pub variable_type: Type,
    pub expression: Expression,
}

/// Parse a statement with syntax:
/// Statement = <AssignmentStatement> | <ExpressionStatement>
pub fn parse_statement(stream: &mut ParsingStream<Token>) -> Result<Statement, ParsingError> {
    match peek!(stream).token_type {
        TokenType::Return => parse_return_statement(stream),
        TokenType::Identifier if peek!(stream, 2).token_type == TokenType::ColonColon => {
            parse_assignment_statement(stream)
        }
        TokenType::Identifier if peek!(stream, 2).token_type == TokenType::Eq => {
            parse_assignment_statement(stream)
        }
        _ => parse_expression_statement(stream),
    }
}

/// Parse an AssignmentStatement with syntax:
/// AssignmentStatement = <Identifier> (:: <Type>)? = <Expression> + ;
fn parse_assignment_statement(
    stream: &mut ParsingStream<Token>,
) -> Result<Statement, ParsingError> {
    let identifier = basics::parse_identifier(stream)?;

    let next_token = stream.next();

    let variable_type = match_token!(next_token, {
        ColonColon => {
            let path = path::parse_path(stream)?;
            expect_token!(stream.next(), [Eq]);
            Type {
                kind: TypeKind::TypePath(path),
            }
        },
        Eq => {
            Type {
                kind: TypeKind::Infer,
            }
        }
    });

    let expression = expression::parse_expression(stream)?;
    expect_semicolon!(stream.current().clone().unwrap(), stream.peek(1));
    stream.next();

    let span = Span::new(identifier.span.start(), expression.span.end());

    Ok(Statement {
        kind: StatementKind::Assignment(Assignment {
            identifier,
            variable_type,
            expression,
        }),
        span,
    })
}

/// Parse an ExpressionStatement with syntax:
/// ExpressionStatement = <Expression> + ;
fn parse_expression_statement(
    stream: &mut ParsingStream<Token>,
) -> Result<Statement, ParsingError> {
    let expression = expression::parse_expression(stream)?;
    expect_semicolon!(stream.current().clone().unwrap(), stream.peek(1));
    stream.next();
    let span = Span::new(expression.span.start(), expression.span.end().0 + 1);

    Ok(Statement {
        kind: StatementKind::Expr(expression),
        span,
    })
}

/// Parse an ReturnStatement with syntax:
/// ReturnStatement = <Return> + <Expression> + ;
fn parse_return_statement(stream: &mut ParsingStream<Token>) -> Result<Statement, ParsingError> {
    let return_token = stream.next();
    expect_token!(return_token, [Return]);
    let expression = expression::parse_expression(stream)?;
    expect_semicolon!(stream.current().clone().unwrap(), stream.peek(1));
    stream.next();
    let span = Span::new(return_token.span.start(), expression.span.end().0 + 1);
    Ok(Statement {
        kind: StatementKind::Return(expression),
        span,
    })
}

#[cfg(test)]
mod tests {
    use codespan::Span;

    use super::*;
    use crate::{
        lexer::TokenData,
        parser::{
            ast::{Expression, ExpressionKind, Literal, LiteralKind, Path, PathSegment},
            tests::{assert_parsing_result, make_token},
        },
    };

    #[test]
    fn test_parse_statement_missing_semicolon() {
        // hello :: string
        let tokens = vec![
            make_token(
                TokenType::Identifier,
                TokenData::Identifier(String::from("hello")),
            ),
            make_token(TokenType::ColonColon, TokenData::None),
            make_token(
                TokenType::Identifier,
                TokenData::Identifier(String::from("string")),
            ),
            make_token(TokenType::EOF, TokenData::None),
        ];
        assert_parsing_result(
            tokens,
            parse_statement,
            Err(ParsingError::unexpected_token(
                &Token {
                    token_type: TokenType::EOF,
                    token_data: TokenData::None,
                    span: Span::default(),
                },
                vec![TokenType::SemiConlon],
            )),
        );
    }

    #[test]
    fn test_parse_assignment() {
        // hello :: string = "hello";
        let tokens = vec![
            make_token(
                TokenType::Identifier,
                TokenData::Identifier(String::from("hello")),
            ),
            make_token(TokenType::ColonColon, TokenData::None),
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
            kind: StatementKind::Assignment(Assignment {
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
                expression: Expression {
                    kind: ExpressionKind::Literal(Literal {
                        kind: LiteralKind::String(String::from("hello")),
                        span: Span::default(),
                    }),
                    span: Span::default(),
                },
            }),
            span: Span::default(),
        };
        assert_parsing_result(tokens, parse_statement, Ok(expected));
    }

    #[test]
    fn test_parse_assignment_infer_type() {
        // hello = "hello";
        let tokens = vec![
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
            kind: StatementKind::Assignment(Assignment {
                variable_type: Type {
                    kind: TypeKind::Infer,
                },
                identifier: Identifier {
                    name: String::from("hello"),
                    span: Span::default(),
                },
                expression: Expression {
                    kind: ExpressionKind::Literal(Literal {
                        kind: LiteralKind::String(String::from("hello")),
                        span: Span::default(),
                    }),
                    span: Span::default(),
                },
            }),
            span: Span::default(),
        };
        assert_parsing_result(tokens, parse_statement, Ok(expected));
    }
}
