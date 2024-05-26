use crate::{
    lexer::{Token, TokenType},
    parser::statement::parse_statement,
    span::Span,
    stream::{peek, ParsingStream},
};

use super::{
    ast::{Identifier, Type}, basics::parse_identifier, expect_token, path::{parse_path, Path}, statement::{parse_type, Statement}, ParsingError
};

#[derive(Debug, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind,
}

#[derive(Debug, PartialEq)]
pub enum ExpressionKind {
    Literal(Literal),
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),
    Function(Function),
    Path(Path),
    Call(Call),
    MemberAccess(MemberAccess)
}

#[derive(Debug, PartialEq)]
pub struct MemberAccess {
    pub object: Box<Expression>,
    pub member: Identifier
}

#[derive(Debug, PartialEq)]
pub struct Call {
    pub function: Box<Expression>,
    pub args: Vec<Expression>
}

#[derive(Debug, PartialEq)]
pub struct UnaryOp {
    pub op: Operator,
    pub expression: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct BinaryOp {
    pub op: Operator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    MemberAccess,
    FunctionInvocation,
}

#[derive(Debug, PartialEq)]
pub struct Literal {
    pub kind: LiteralKind,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum LiteralKind {
    Int(i64),
    Float(f64),
    String(String),
    Boolean(bool),
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub args: Vec<FunctionArg>,
    pub body: Block,
}

#[derive(Debug, PartialEq)]
pub struct FunctionArg {
    pub identifier: Identifier,
    pub arg_type: Type,
}

#[derive(Debug, PartialEq)]
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
/// FunctionDeclaration = fn + ( + <Arg> + (, + <Arg>)* + ) + <Block>
fn parse_function_declaration(
    stream: &mut ParsingStream<Token>,
) -> Result<Expression, ParsingError> {
    expect_token!(stream.next(), TokenType::Function);
    expect_token!(stream.next(), TokenType::ParenOpen);

    let mut args = Vec::new();

    loop {
        match peek!(stream).token_type {
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
            _ => return Err(ParsingError::UnexpectedToken(stream.next())),
        }
    }

    let body = parse_block(stream)?;

    Ok(Expression {
        kind: ExpressionKind::Function(Function { args, body }),
    })
}

/// Parse an argument with syntax:
/// Arg = <Identifer> + : + <Type>
fn parse_argument(stream: &mut ParsingStream<Token>) -> Result<FunctionArg, ParsingError> {
    let token = stream.next();
    let identifier = match token.token_type {
        TokenType::Identifier(name) => Identifier { name },
        _ => return Err(ParsingError::UnexpectedToken(token)),
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
        TokenType::Identifier(_) => {
            parse_path_expression(stream)?
        },
        TokenType::ParenOpen => parse_parenthesised_expression(stream)?,
        TokenType::Plus | TokenType::Minus => parse_unary_operation(stream)?,
        _ => return Err(ParsingError::UnexpectedToken(token.clone())),
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

                    expect_token!(stream.next(), TokenType::ParenClose);

                    Expression {
                        kind: ExpressionKind::Call(Call {
                            function: Box::new(left),
                            args
                        })
                    }
                },
                _ => unreachable!("Invalid operator")
            };

            continue;
        }

        let (left_binding_power, right_binding_power) = infix_binding_power(&op);

        if left_binding_power < min_binding_power {
            break;
        }

        stream.next();

        left = match op {
            Operator::MemberAccess => Expression {
                kind: ExpressionKind::MemberAccess(MemberAccess {
                    object: Box::new(left),
                    member: parse_identifier(stream)?
                }),
            },
            _ => {
                let right = parse_expression_binding_power(stream, right_binding_power)?;
                Expression {
                    kind: ExpressionKind::BinaryOp(BinaryOp {
                        left: Box::new(left),
                        right: Box::new(right),
                        op,
                    }),
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
    Ok(Expression {
        kind: ExpressionKind::Path(path),
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
    Ok(Expression {
        kind: ExpressionKind::UnaryOp(UnaryOp {
            op,
            expression: Box::new(right),
        }),
    })
}

/// Parse a literal expression with syntax:
/// LiteralExpression = <Boolean> | <Int> | <Float> | <String>
fn parse_literal_expression(stream: &mut ParsingStream<Token>) -> Result<Expression, ParsingError> {
    let token = stream.next();
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
        _ => return Err(ParsingError::UnexpectedToken(token.clone())),
    };

    Ok(Expression { kind })
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
        let span = Span::from(((0, 0), (0, 0)));
        //  "hello"
        assert_parsing_result(
            vec![TokenType::String(String::from("hello"))],
            parse_expression,
            Ok(Expression {
                kind: ExpressionKind::Literal(Literal {
                    kind: LiteralKind::String(String::from("hello")),
                    span: span.clone(),
                }),
            }),
        );

        // 12
        assert_parsing_result(
            vec![TokenType::Int(12)],
            parse_expression,
            Ok(Expression {
                kind: ExpressionKind::Literal(Literal {
                    kind: LiteralKind::Int(12),
                    span: span.clone(),
                }),
            }),
        );

        // 47.2821
        assert_parsing_result(
            vec![TokenType::Float(47.2821)],
            parse_expression,
            Ok(Expression {
                kind: ExpressionKind::Literal(Literal {
                    kind: LiteralKind::Float(47.2821),
                    span: span.clone(),
                }),
            }),
        );

        // true
        assert_parsing_result(
            vec![TokenType::Boolean(true)],
            parse_expression,
            Ok(Expression {
                kind: ExpressionKind::Literal(Literal {
                    kind: LiteralKind::Boolean(true),
                    span: span.clone(),
                }),
            }),
        );

        // false
        assert_parsing_result(
            vec![TokenType::Boolean(false)],
            parse_expression,
            Ok(Expression {
                kind: ExpressionKind::Literal(Literal {
                    kind: LiteralKind::Boolean(false),
                    span: span.clone(),
                }),
            }),
        );
    }

    #[test]
    fn test_parse_simple_binary_op() {
        let span = Span::from(((0, 0), (0, 0)));
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
                            span: span.clone(),
                        }),
                    }),
                    right: Box::new(Expression {
                        kind: ExpressionKind::BinaryOp(BinaryOp {
                            op: Operator::Multiply,
                            left: Box::new(Expression {
                                kind: ExpressionKind::Literal(Literal {
                                    kind: LiteralKind::Int(2),
                                    span: span.clone(),
                                }),
                            }),
                            right: Box::new(Expression {
                                kind: ExpressionKind::Literal(Literal {
                                    kind: LiteralKind::Int(3),
                                    span: span.clone(),
                                }),
                            }),
                        }),
                    }),
                }),
            }),
        );
    }

    #[test]
    fn test_parse_simple_function() {
        let span = Span::from(((0, 0), (0, 0)));
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
                            },
                            arg_type: Type {
                                kind: TypeKind::Infer,
                            },
                        },
                        FunctionArg {
                            identifier: Identifier {
                                name: "world".to_string(),
                            },
                            arg_type: Type {
                                kind: TypeKind::TypePath(Path {
                                    segments: vec![PathSegment {
                                        ident: Identifier {
                                            name: "string".to_string(),
                                        },
                                    }],
                                }),
                            },
                        },
                    ],
                    body: Block {
                        statements: vec![Statement {
                            kind: StatementKind::Let(VariableDeclaration {
                                identifier: Identifier {
                                    name: "a".to_string(),
                                },
                                variable_type: Type {
                                    kind: TypeKind::Infer,
                                },
                                kind: VariableDeclarationKind::Init(Expression {
                                    kind: ExpressionKind::Literal(Literal {
                                        kind: LiteralKind::Int(10),
                                        span,
                                    }),
                                }),
                                is_mutable: false,
                            }),
                        }],
                    },
                }),
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
                            },
                            arg_type: Type {
                                kind: TypeKind::Infer,
                            },
                        },
                        FunctionArg {
                            identifier: Identifier {
                                name: "world".to_string(),
                            },
                            arg_type: Type {
                                kind: TypeKind::TypePath(Path {
                                    segments: vec![PathSegment {
                                        ident: Identifier {
                                            name: "string".to_string(),
                                        },
                                    }],
                                }),
                            },
                        },
                    ],
                    body: Block { statements: vec![] },
                }),
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
                }),
            }),
        );
    }

    #[test]
    fn test_path_in_binary_op() {
        let span = Span::from(((0, 0), (0, 0)));
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
                                },
                            }],
                        }),
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
                                            },
                                        },
                                        PathSegment {
                                            ident: Identifier {
                                                name: "b".to_string(),
                                            },
                                        },
                                    ],
                                }),
                            }),
                            right: Box::new(Expression {
                                kind: ExpressionKind::Literal(Literal {
                                    kind: LiteralKind::Int(3),
                                    span: span.clone(),
                                }),
                            }),
                        }),
                    }),
                }),
            }),
        );
    }

    #[test]
    fn test_unary_op() {
        let span = Span::from(((0, 0), (0, 0)));
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
                            span: span.clone(),
                        }),
                    }),
                    right: Box::new(Expression {
                        kind: ExpressionKind::UnaryOp(UnaryOp {
                            op: Operator::Subtract,
                            expression: Box::new(Expression {
                                kind: ExpressionKind::Literal(Literal {
                                    kind: LiteralKind::Int(5),
                                    span: span.clone(),
                                }),
                            }),
                        }),
                    }),
                }),
            }),
        );
    }

    #[test]
    fn test_parenthesised_expression() {
        let span = Span::from(((0, 0), (0, 0)));
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
                                    span: span.clone(),
                                }),
                            }),
                            right: Box::new(Expression {
                                kind: ExpressionKind::Literal(Literal {
                                    kind: LiteralKind::Int(1),
                                    span: span.clone(),
                                }),
                            }),
                        }),
                    }),
                    right: Box::new(Expression {
                        kind: ExpressionKind::Literal(Literal {
                            kind: LiteralKind::Int(4),
                            span: span.clone(),
                        }),
                    }),
                }),
            }),
        );
    }

    #[test]
    fn test_multiple_parenthesised_expression() {
        let span = Span::from(((0, 0), (0, 0)));
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
                    span: span.clone(),
                }),
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
                                                    name: String::from("a")
                                                }
                                            }]
                                        })
                                    }),
                                    member: Identifier {
                                        name: String::from("b")
                                    }
                                })
                            }),
                            member: Identifier {
                                name: String::from("c")
                            }
                        }),
                    }),
                    args: vec![
                        Expression {
                            kind: ExpressionKind::Path(Path {
                                segments: vec![PathSegment {
                                    ident: Identifier { name: String::from("d") }
                                }]
                            }),
                        },
                        Expression {
                            kind: ExpressionKind::Path(Path {
                                segments: vec![PathSegment {
                                    ident: Identifier { name: String::from("f") }
                                }]
                            }),
                        },
                    ]
                })
            }),
        );
    }
}
