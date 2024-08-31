use serde::Serialize;

use crate::{
    lexer::{Token, TokenType},
    parser::{ast::TypeKind, statement::parse_statement},
    span::Span,
    stream::{peek, ParsingStream},
};

use super::{
    ast::{Identifier, Type},
    basics::parse_identifier,
    expect_token,
    path::{parse_path, Path},
    statement::{parse_type, Statement},
    unexpected_token, ParsingError,
};

#[derive(Debug, PartialEq, Serialize)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Serialize)]
pub enum ExpressionKind {
    Literal(Literal),
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    Function(Function),
    Path(Path),
    Call(Call),
    MemberAccess(MemberAccess),
}

#[derive(Debug, PartialEq, Serialize)]
pub struct MemberAccess {
    pub object: Box<Expression>,
    pub member: Identifier,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct Call {
    pub function: Box<Expression>,
    pub args: Vec<Expression>,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct UnaryOp {
    pub op: Operator,
    pub expression: Box<Expression>,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct BinaryOp {
    pub op: Operator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq, Serialize)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    MemberAccess,
    FunctionInvocation,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct Literal {
    pub kind: LiteralKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Serialize)]
pub enum LiteralKind {
    Int(i64),
    Float(f64),
    String(String),
    Boolean(bool),
}

#[derive(Debug, PartialEq, Serialize)]
pub struct Function {
    pub args: Vec<FunctionArg>,
    pub body: Block,
    pub return_type: Type,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct FunctionArg {
    pub identifier: Identifier,
    pub arg_type: Type,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct Block {
    pub statements: Vec<Statement>,
}

/// Parse an expression with syntax:
/// Expression = <LiteralExpression> | <BinaryOperationExpression> | <FunctionDeclaration>
pub fn parse_expression(stream: &mut ParsingStream<Token>) -> Result<Expression, ParsingError> {
    match peek!(stream).token_type {
        TokenType::Function => parse_function_declaration(stream),
        _ => parse_expression_binding_power(stream, 0),
    }
}

/// Parse function declaration with syntax:
/// FunctionDeclaration = fn + ( + <Arg> + (, + <Arg>)* + ) + (: <Type>)* + <Block>
fn parse_function_declaration(
    stream: &mut ParsingStream<Token>,
) -> Result<Expression, ParsingError> {
    let start_token = stream.next();
    expect_token!(start_token, TokenType::Function);

    let start_location = start_token.span.start;

    expect_token!(stream.next(), TokenType::ParenOpen);

    let mut args = Vec::new();

    loop {
        let token = peek!(stream);
        match token.token_type {
            TokenType::Comma => {
                stream.next();
            }
            TokenType::ParenClose => {
                stream.next();
                break;
            }
            TokenType::Identifier(_) => {
                args.push(parse_argument(stream)?);
            }
            _ => {
                unexpected_token!(token);
            }
        }
    }

    let mut return_type = Type {
        kind: crate::parser::ast::TypeKind::Nothing,
    };

    if let TokenType::ThinArrow = peek!(stream).token_type {
        stream.next();
        return_type = Type {
            kind: TypeKind::TypePath(parse_path(stream)?),
        };
    }

    let body = parse_block(stream)?;

    let end_location = stream.current().clone().unwrap().span.end;

    Ok(Expression {
        kind: ExpressionKind::Function(Function {
            args,
            body,
            return_type,
        }),
        span: Span {
            start: start_location,
            end: end_location,
        },
    })
}

/// Parse an argument with syntax:
/// Arg = <Identifer> + : + <Type>
fn parse_argument(stream: &mut ParsingStream<Token>) -> Result<FunctionArg, ParsingError> {
    let token = stream.next();
    let identifier = match token.token_type {
        TokenType::Identifier(name) => Identifier {
            name,
            span: token.span,
        },
        _ => {
            unexpected_token!(token);
        }
    };

    let arg_type = parse_type(stream)?;

    Ok(FunctionArg {
        identifier,
        arg_type,
    })
}

/// Parse a block with syntax:
/// Block = { + <Statements> + }
fn parse_block(stream: &mut ParsingStream<Token>) -> Result<Block, ParsingError> {
    expect_token!(stream.next(), TokenType::CurlyBracketOpen);

    let mut statements = Vec::new();

    loop {
        if peek!(stream).token_type == TokenType::CurlyBracketClose {
            stream.next();
            break;
        }
        statements.push(parse_statement(stream)?);
    }

    Ok(Block { statements })
}

/// Parse an expression with a specified min binding power for Pratt parsing.
fn parse_expression_binding_power(
    stream: &mut ParsingStream<Token>,
    min_binding_power: u8,
) -> Result<Expression, ParsingError> {
    let token = peek!(stream);
    let mut left = match token.token_type {
        TokenType::Boolean(_) | TokenType::Int(_) | TokenType::Float(_) | TokenType::String(_) => {
            parse_literal_expression(stream)?
        }
        TokenType::Identifier(_) => parse_path_expression(stream)?,
        TokenType::ParenOpen => parse_parenthesised_expression(stream)?,
        TokenType::Plus | TokenType::Minus => parse_unary_operation(stream)?,
        _ => {
            unexpected_token!(token);
        }
    };

    loop {
        let token = peek!(stream);
        let Some(op) = token_type_to_operator(token.token_type) else {
            break;
        };

        if let Some((left_binding_power, ())) = postfix_binding_power(&op) {
            if left_binding_power < min_binding_power {
                break;
            }

            stream.next();

            left = match op {
                Operator::FunctionInvocation => {
                    let mut args = Vec::new();

                    if peek!(stream).token_type != TokenType::ParenClose {
                        loop {
                            let expr = parse_expression(stream)?;
                            args.push(expr);

                            if peek!(stream).token_type == TokenType::ParenClose {
                                break;
                            }

                            expect_token!(stream.next(), TokenType::Comma);
                        }
                    }

                    let end_token = stream.next();
                    expect_token!(end_token, TokenType::ParenClose);

                    let start_location = left.span.start.clone();
                    let end_location = end_token.span.end;

                    Expression {
                        kind: ExpressionKind::Call(Call {
                            function: Box::new(left),
                            args,
                        }),
                        span: Span {
                            start: start_location,
                            end: end_location,
                        },
                    }
                }
                _ => unreachable!("Invalid operator"),
            };

            continue;
        }

        let (left_binding_power, right_binding_power) = infix_binding_power(&op);

        if left_binding_power < min_binding_power {
            break;
        }

        stream.next();

        left = match op {
            Operator::MemberAccess => {
                let member = parse_identifier(stream)?;
                let start_location = left.span.start.clone();
                let end_location = member.span.end.clone();

                Expression {
                    kind: ExpressionKind::MemberAccess(MemberAccess {
                        object: Box::new(left),
                        member,
                    }),
                    span: Span {
                        start: start_location,
                        end: end_location,
                    },
                }
            }
            _ => {
                let right = parse_expression_binding_power(stream, right_binding_power)?;
                let start_location = left.span.start.clone();
                let end_location = right.span.end.clone();
                Expression {
                    kind: ExpressionKind::BinaryOp(BinaryOp {
                        left: Box::new(left),
                        right: Box::new(right),
                        op,
                    }),
                    span: Span {
                        start: start_location,
                        end: end_location,
                    },
                }
            }
        }
    }

    Ok(left)
}

fn token_type_to_operator(token_type: TokenType) -> Option<Operator> {
    match token_type {
        TokenType::Plus => Some(Operator::Add),
        TokenType::Minus => Some(Operator::Subtract),
        TokenType::Multiply => Some(Operator::Multiply),
        TokenType::Divide => Some(Operator::Divide),
        TokenType::Period => Some(Operator::MemberAccess),
        TokenType::ParenOpen => Some(Operator::FunctionInvocation),
        _ => None,
    }
}

/// Binding power for infix ooperators like add, subtract, etc.
fn infix_binding_power(op: &Operator) -> (u8, u8) {
    match op {
        Operator::Add | Operator::Subtract => (1, 2),
        Operator::Multiply | Operator::Divide => (3, 4),
        Operator::MemberAccess => (6, 5),
        _ => unreachable!("Invalid operator"),
    }
}

/// Binding power for prefix ooperators like add, subtract
fn prefix_binding_power(op: &Operator) -> ((), u8) {
    match op {
        Operator::Add | Operator::Subtract => ((), 5),
        _ => unreachable!("Invalid operator"),
    }
}

/// Binding power for postfix ooperators like function call
fn postfix_binding_power(op: &Operator) -> Option<(u8, ())> {
    match op {
        Operator::FunctionInvocation => Some((7, ())),
        _ => None,
    }
}

/// Parse path expression
/// PathExpression = <Path>
fn parse_path_expression(stream: &mut ParsingStream<Token>) -> Result<Expression, ParsingError> {
    let path = parse_path(stream)?;
    let span = path.span.clone();
    Ok(Expression {
        kind: ExpressionKind::Path(path),
        span,
    })
}

/// Parse a parenthesised expression
/// ParenthesisedExpression = ( + <Expression> + )
fn parse_parenthesised_expression(
    stream: &mut ParsingStream<Token>,
) -> Result<Expression, ParsingError> {
    expect_token!(stream.next(), TokenType::ParenOpen);
    let expression = parse_expression_binding_power(stream, 0)?;
    expect_token!(stream.next(), TokenType::ParenClose);
    Ok(expression)
}

/// Parse a unary operation
/// UnaryOperation = <Plus> | <Minus> + <Expression>
fn parse_unary_operation(stream: &mut ParsingStream<Token>) -> Result<Expression, ParsingError> {
    let token = stream.next();
    let op = token_type_to_operator(token.token_type).unwrap();
    let ((), right_binding_power) = prefix_binding_power(&op);
    let right = parse_expression_binding_power(stream, right_binding_power)?;

    let start_location = token.span.start;
    let end_location = right.span.end.clone();

    Ok(Expression {
        kind: ExpressionKind::UnaryOp(UnaryOp {
            op,
            expression: Box::new(right),
        }),
        span: Span {
            start: start_location,
            end: end_location,
        },
    })
}

/// Parse a literal expression with syntax:
/// LiteralExpression = <Boolean> | <Int> | <Float> | <String>
fn parse_literal_expression(stream: &mut ParsingStream<Token>) -> Result<Expression, ParsingError> {
    let token = stream.next();
    let span = token.span.clone();
    let kind = match &token.token_type {
        TokenType::Boolean(value) => ExpressionKind::Literal(Literal {
            kind: LiteralKind::Boolean(*value),
            span: token.span,
        }),
        TokenType::Int(value) => ExpressionKind::Literal(Literal {
            kind: LiteralKind::Int(*value),
            span: token.span,
        }),
        TokenType::Float(value) => ExpressionKind::Literal(Literal {
            kind: LiteralKind::Float(*value),
            span: token.span,
        }),
        TokenType::String(value) => ExpressionKind::Literal(Literal {
            kind: LiteralKind::String(value.to_owned()),
            span: token.span,
        }),
        _ => {
            unexpected_token!(token);
        }
    };

    Ok(Expression { kind, span })
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{
            ast::TypeKind,
            path::{Path, PathSegment},
            statement::{StatementKind, VariableDeclaration, VariableDeclarationKind},
            tests::assert_parsing_result,
        },
        span::Span,
    };

    use super::*;

    #[test]
    fn test_parse_literal_expression() {
        //  "hello"
        assert_parsing_result(
            vec![TokenType::String(String::from("hello"))],
            parse_expression,
            Ok(Expression {
                kind: ExpressionKind::Literal(Literal {
                    kind: LiteralKind::String(String::from("hello")),
                    span: Span::default(),
                }),
                span: Span::default(),
            }),
        );

        // 12
        assert_parsing_result(
            vec![TokenType::Int(12)],
            parse_expression,
            Ok(Expression {
                kind: ExpressionKind::Literal(Literal {
                    kind: LiteralKind::Int(12),
                    span: Span::default(),
                }),
                span: Span::default(),
            }),
        );

        // 47.2821
        assert_parsing_result(
            vec![TokenType::Float(47.2821)],
            parse_expression,
            Ok(Expression {
                kind: ExpressionKind::Literal(Literal {
                    kind: LiteralKind::Float(47.2821),
                    span: Span::default(),
                }),
                span: Span::default(),
            }),
        );

        // true
        assert_parsing_result(
            vec![TokenType::Boolean(true)],
            parse_expression,
            Ok(Expression {
                kind: ExpressionKind::Literal(Literal {
                    kind: LiteralKind::Boolean(true),
                    span: Span::default(),
                }),
                span: Span::default(),
            }),
        );

        // false
        assert_parsing_result(
            vec![TokenType::Boolean(false)],
            parse_expression,
            Ok(Expression {
                kind: ExpressionKind::Literal(Literal {
                    kind: LiteralKind::Boolean(false),
                    span: Span::default(),
                }),
                span: Span::default(),
            }),
        );
    }

    #[test]
    fn test_parse_simple_binary_op() {
        assert_parsing_result(
            vec![
                TokenType::Int(1),
                TokenType::Plus,
                TokenType::Int(2),
                TokenType::Multiply,
                TokenType::Int(3),
                TokenType::EOF,
            ],
            parse_expression,
            Ok(Expression {
                kind: ExpressionKind::BinaryOp(BinaryOp {
                    op: Operator::Add,
                    left: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(1),
                            span: Span::default(),
                        }),
                        span: Span::default(),
                    }),
                    right: Box::new(Expression {
                        kind: ExpressionKind::BinaryOp(BinaryOp {
                            op: Operator::Multiply,
                            left: Box::new(Expression {
                                kind: ExpressionKind::Literal(Literal {
                                    kind: LiteralKind::Int(2),
                                    span: Span::default(),
                                }),
                                span: Span::default(),
                            }),
                            right: Box::new(Expression {
                                kind: ExpressionKind::Literal(Literal {
                                    kind: LiteralKind::Int(3),
                                    span: Span::default(),
                                }),
                                span: Span::default(),
                            }),
                        }),
                        span: Span::default(),
                    }),
                }),
                span: Span::default(),
            }),
        );
    }

    #[test]
    fn test_parse_simple_function() {
        assert_parsing_result(
            vec![
                TokenType::Function,
                TokenType::ParenOpen,
                TokenType::Identifier("hello".to_string()),
                TokenType::Comma,
                TokenType::Identifier("world".to_string()),
                TokenType::Colon,
                TokenType::Identifier("string".to_string()),
                TokenType::ParenClose,
                TokenType::CurlyBracketOpen,
                TokenType::Let,
                TokenType::Identifier("a".to_string()),
                TokenType::Eq,
                TokenType::Int(10),
                TokenType::SemiConlon,
                TokenType::CurlyBracketClose,
                TokenType::EOF,
            ],
            parse_expression,
            Ok(Expression {
                kind: ExpressionKind::Function(Function {
                    args: vec![
                        FunctionArg {
                            identifier: Identifier {
                                name: "hello".to_string(),
                                span: Span::default(),
                            },
                            arg_type: Type {
                                kind: TypeKind::Infer,
                            },
                        },
                        FunctionArg {
                            identifier: Identifier {
                                name: "world".to_string(),
                                span: Span::default(),
                            },
                            arg_type: Type {
                                kind: TypeKind::TypePath(Path {
                                    segments: vec![PathSegment {
                                        ident: Identifier {
                                            name: "string".to_string(),
                                            span: Span::default(),
                                        },
                                        span: Span::default(),
                                    }],
                                    span: Span::default(),
                                }),
                            },
                        },
                    ],
                    body: Block {
                        statements: vec![Statement {
                            kind: StatementKind::Let(VariableDeclaration {
                                identifier: Identifier {
                                    name: "a".to_string(),
                                    span: Span::default(),
                                },
                                variable_type: Type {
                                    kind: TypeKind::Infer,
                                },
                                kind: VariableDeclarationKind::Init(Expression {
                                    kind: ExpressionKind::Literal(Literal {
                                        kind: LiteralKind::Int(10),
                                        span: Span::default(),
                                    }),
                                    span: Span::default(),
                                }),
                                is_mutable: false,
                            }),
                        }],
                    },
                    return_type: Type {
                        kind: TypeKind::Nothing,
                    },
                }),
                span: Span::default(),
            }),
        );
    }

    #[test]
    fn test_parse_empty_body_function() {
        assert_parsing_result(
            vec![
                TokenType::Function,
                TokenType::ParenOpen,
                TokenType::Identifier("hello".to_string()),
                TokenType::Comma,
                TokenType::Identifier("world".to_string()),
                TokenType::Colon,
                TokenType::Identifier("string".to_string()),
                TokenType::ParenClose,
                TokenType::CurlyBracketOpen,
                TokenType::CurlyBracketClose,
                TokenType::EOF,
            ],
            parse_expression,
            Ok(Expression {
                kind: ExpressionKind::Function(Function {
                    args: vec![
                        FunctionArg {
                            identifier: Identifier {
                                name: "hello".to_string(),
                                span: Span::default(),
                            },
                            arg_type: Type {
                                kind: TypeKind::Infer,
                            },
                        },
                        FunctionArg {
                            identifier: Identifier {
                                name: "world".to_string(),
                                span: Span::default(),
                            },
                            arg_type: Type {
                                kind: TypeKind::TypePath(Path {
                                    segments: vec![PathSegment {
                                        ident: Identifier {
                                            name: "string".to_string(),
                                            span: Span::default(),
                                        },
                                        span: Span::default(),
                                    }],
                                    span: Span::default(),
                                }),
                            },
                        },
                    ],
                    body: Block { statements: vec![] },
                    return_type: Type {
                        kind: TypeKind::Nothing,
                    },
                }),
                span: Span::default(),
            }),
        );
    }

    #[test]
    fn test_parse_empty_arg_function() {
        assert_parsing_result(
            vec![
                TokenType::Function,
                TokenType::ParenOpen,
                TokenType::ParenClose,
                TokenType::CurlyBracketOpen,
                TokenType::CurlyBracketClose,
                TokenType::EOF,
            ],
            parse_expression,
            Ok(Expression {
                kind: ExpressionKind::Function(Function {
                    args: vec![],
                    body: Block { statements: vec![] },
                    return_type: Type {
                        kind: TypeKind::Nothing,
                    },
                }),
                span: Span::default(),
            }),
        );
    }

    #[test]
    fn test_parse_return_type_function() {
        assert_parsing_result(
            vec![
                TokenType::Function,
                TokenType::ParenOpen,
                TokenType::ParenClose,
                TokenType::ThinArrow,
                TokenType::Identifier("number".to_string()),
                TokenType::CurlyBracketOpen,
                TokenType::CurlyBracketClose,
                TokenType::EOF,
            ],
            parse_expression,
            Ok(Expression {
                kind: ExpressionKind::Function(Function {
                    args: vec![],
                    body: Block { statements: vec![] },
                    return_type: Type {
                        kind: TypeKind::TypePath(Path {
                            segments: vec![PathSegment {
                                ident: Identifier {
                                    name: "number".to_string(),
                                    span: Span::default(),
                                },
                                span: Span::default(),
                            }],
                            span: Span::default(),
                        }),
                    },
                }),
                span: Span::default(),
            }),
        );
    }

    #[test]
    fn test_path_in_binary_op() {
        assert_parsing_result(
            vec![
                TokenType::Identifier("a".to_string()),
                TokenType::Plus,
                TokenType::Identifier("module".to_string()),
                TokenType::ColonColon,
                TokenType::Identifier("b".to_string()),
                TokenType::Multiply,
                TokenType::Int(3),
                TokenType::EOF,
            ],
            parse_expression,
            Ok(Expression {
                kind: ExpressionKind::BinaryOp(BinaryOp {
                    op: Operator::Add,
                    left: Box::new(Expression {
                        kind: ExpressionKind::Path(Path {
                            segments: vec![PathSegment {
                                ident: Identifier {
                                    name: "a".to_string(),
                                    span: Span::default(),
                                },
                                span: Span::default(),
                            }],
                            span: Span::default(),
                        }),
                        span: Span::default(),
                    }),
                    right: Box::new(Expression {
                        kind: ExpressionKind::BinaryOp(BinaryOp {
                            op: Operator::Multiply,
                            left: Box::new(Expression {
                                kind: ExpressionKind::Path(Path {
                                    segments: vec![
                                        PathSegment {
                                            ident: Identifier {
                                                name: "module".to_string(),
                                                span: Span::default(),
                                            },
                                            span: Span::default(),
                                        },
                                        PathSegment {
                                            ident: Identifier {
                                                name: "b".to_string(),
                                                span: Span::default(),
                                            },
                                            span: Span::default(),
                                        },
                                    ],
                                    span: Span::default(),
                                }),
                                span: Span::default(),
                            }),
                            right: Box::new(Expression {
                                kind: ExpressionKind::Literal(Literal {
                                    kind: LiteralKind::Int(3),
                                    span: Span::default(),
                                }),
                                span: Span::default(),
                            }),
                        }),
                        span: Span::default(),
                    }),
                }),
                span: Span::default(),
            }),
        );
    }

    #[test]
    fn test_unary_op() {
        assert_parsing_result(
            vec![
                TokenType::Int(5),
                TokenType::Plus,
                TokenType::Minus,
                TokenType::Int(5),
                TokenType::EOF,
            ],
            parse_expression,
            Ok(Expression {
                kind: ExpressionKind::BinaryOp(BinaryOp {
                    op: Operator::Add,
                    left: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(5),
                            span: Span::default(),
                        }),
                        span: Span::default(),
                    }),
                    right: Box::new(Expression {
                        kind: ExpressionKind::UnaryOp(UnaryOp {
                            op: Operator::Subtract,
                            expression: Box::new(Expression {
                                kind: ExpressionKind::Literal(Literal {
                                    kind: LiteralKind::Int(5),
                                    span: Span::default(),
                                }),
                                span: Span::default(),
                            }),
                        }),
                        span: Span::default(),
                    }),
                }),
                span: Span::default(),
            }),
        );
    }

    #[test]
    fn test_parenthesised_expression() {
        assert_parsing_result(
            vec![
                TokenType::ParenOpen,
                TokenType::Int(5),
                TokenType::Minus,
                TokenType::Int(1),
                TokenType::ParenClose,
                TokenType::Multiply,
                TokenType::Int(4),
                TokenType::EOF,
            ],
            parse_expression,
            Ok(Expression {
                kind: ExpressionKind::BinaryOp(BinaryOp {
                    op: Operator::Multiply,
                    left: Box::new(Expression {
                        kind: ExpressionKind::BinaryOp(BinaryOp {
                            op: Operator::Subtract,
                            left: Box::new(Expression {
                                kind: ExpressionKind::Literal(Literal {
                                    kind: LiteralKind::Int(5),
                                    span: Span::default(),
                                }),
                                span: Span::default(),
                            }),
                            right: Box::new(Expression {
                                kind: ExpressionKind::Literal(Literal {
                                    kind: LiteralKind::Int(1),
                                    span: Span::default(),
                                }),
                                span: Span::default(),
                            }),
                        }),
                        span: Span::default(),
                    }),
                    right: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(4),
                            span: Span::default(),
                        }),
                        span: Span::default(),
                    }),
                }),
                span: Span::default(),
            }),
        );
    }

    #[test]
    fn test_multiple_parenthesised_expression() {
        assert_parsing_result(
            vec![
                TokenType::ParenOpen,
                TokenType::ParenOpen,
                TokenType::ParenOpen,
                TokenType::Int(5),
                TokenType::ParenClose,
                TokenType::ParenClose,
                TokenType::ParenClose,
                TokenType::EOF,
            ],
            parse_expression,
            Ok(Expression {
                kind: ExpressionKind::Literal(Literal {
                    kind: LiteralKind::Int(5),
                    span: Span::default(),
                }),
                span: Span::default(),
            }),
        );
    }

    #[test]
    fn test_function_call_expression() {
        assert_parsing_result(
            vec![
                TokenType::Identifier(String::from("a")),
                TokenType::Period,
                TokenType::Identifier(String::from("b")),
                TokenType::Period,
                TokenType::Identifier(String::from("c")),
                TokenType::ParenOpen,
                TokenType::Identifier(String::from("d")),
                TokenType::Comma,
                TokenType::Identifier(String::from("f")),
                TokenType::ParenClose,
                TokenType::EOF,
            ],
            parse_expression,
            Ok(Expression {
                kind: ExpressionKind::Call(Call {
                    function: Box::new(Expression {
                        kind: ExpressionKind::MemberAccess(MemberAccess {
                            object: Box::new(Expression {
                                kind: ExpressionKind::MemberAccess(MemberAccess {
                                    object: Box::new(Expression {
                                        kind: ExpressionKind::Path(Path {
                                            segments: vec![PathSegment {
                                                ident: Identifier {
                                                    name: String::from("a"),
                                                    span: Span::default(),
                                                },
                                                span: Span::default(),
                                            }],
                                            span: Span::default(),
                                        }),
                                        span: Span::default(),
                                    }),
                                    member: Identifier {
                                        name: String::from("b"),
                                        span: Span::default(),
                                    },
                                }),
                                span: Span::default(),
                            }),
                            member: Identifier {
                                name: String::from("c"),
                                span: Span::default(),
                            },
                        }),
                        span: Span::default(),
                    }),
                    args: vec![
                        Expression {
                            kind: ExpressionKind::Path(Path {
                                segments: vec![PathSegment {
                                    ident: Identifier {
                                        name: String::from("d"),
                                        span: Span::default(),
                                    },
                                    span: Span::default(),
                                }],
                                span: Span::default(),
                            }),
                            span: Span::default(),
                        },
                        Expression {
                            kind: ExpressionKind::Path(Path {
                                segments: vec![PathSegment {
                                    ident: Identifier {
                                        name: String::from("f"),
                                        span: Span::default(),
                                    },
                                    span: Span::default(),
                                }],
                                span: Span::default(),
                            }),
                            span: Span::default(),
                        },
                    ],
                }),
                span: Span::default(),
            }),
        );
    }
}
