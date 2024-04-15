use crate::{
    lexer::{Token, TokenType}, parser::statement::parse_statement, span::Span, stream::ParsingStream
};

use super::{ast::{Identifier, Type}, expect_token, statement::{parse_type, Statement}, ParsingError};

#[derive(Debug, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind,
}

#[derive(Debug, PartialEq)]
pub enum ExpressionKind {
    Literal(Literal),
    BinaryOp(BinaryOp),
    Function(Function)
}

#[derive(Debug, PartialEq)]
pub struct BinaryOp {
    pub op: Operator,
    pub left: Box<Expression>,
    pub right: Box<Expression>
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide
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
/// Expression = <LiteralExpression> | <BinaryOperationExpression> | <FunctionExpression>
pub fn parse_expression(stream: &mut ParsingStream<Token>) -> Result<Expression, ParsingError> {
    match stream.peek().token_type {
        TokenType::Function => parse_function_expression(stream),
        _ => parse_expression_binding_power(stream, 0)
    }
}

fn parse_function_expression(stream: &mut ParsingStream<Token>) -> Result<Expression, ParsingError> {
    expect_token!(stream.next(), TokenType::Function);
    expect_token!(stream.next(), TokenType::ParenOpen);

    let mut args = Vec::new();

    loop {
        match stream.peek().token_type {
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
            _ => return Err(ParsingError::UnexpectedToken(stream.next()))
        }
    }

    let body = parse_block(stream)?;

    Ok(Expression { kind: ExpressionKind::Function(Function { args, body }) })
}

/// Parse an argument with syntax:
/// Arg = <Identifer> + : + <Type>
fn parse_argument(stream: &mut ParsingStream<Token>) -> Result<FunctionArg, ParsingError> {
    let token = stream.next();
    let identifier = match token.token_type {
        TokenType::Identifier(name) => Identifier { name },
        _ => return Err(ParsingError::UnexpectedToken(token))
    };

    let arg_type = parse_type(stream)?;

    Ok(FunctionArg { identifier, arg_type })
}

/// Parse a block with syntax:
/// Block = { + <Statements> + }
fn parse_block(stream: &mut ParsingStream<Token>) -> Result<Block, ParsingError> {
    expect_token!(stream.next(), TokenType::CurlyBracketOpen);

    let mut statements = Vec::new();

    loop {
        if stream.peek().token_type == TokenType::CurlyBracketClose {
            stream.next();
            break;
        }
        statements.push(parse_statement(stream)?);
    }

    Ok(Block { statements })
}

/// Parse an expression with a specified min binding power for Pratt parsing.
fn parse_expression_binding_power(stream: &mut ParsingStream<Token>, min_binding_power: u8) -> Result<Expression, ParsingError> {
    let token = stream.peek();
    let mut left = match token.token_type {
        TokenType::Boolean(_) | TokenType::Int(_) | TokenType::Float(_) | TokenType::String(_) => {
            parse_literal_expression(stream)?
        }
        _ => return Err(ParsingError::UnexpectedToken(token.clone())),
    };

    loop {
        let token = stream.peek();
        let op = match token.token_type {
            TokenType::Plus => Operator::Add,
            TokenType::Minus => Operator::Subtract,
            TokenType::Multiply => Operator::Multiply,
            TokenType::Divide => Operator::Divide,
            _ => break
        };

        let (left_binding_power, right_binding_power) = infix_binding_power(&op);

        if left_binding_power < min_binding_power {
            break;
        }

        stream.next();
        let right = parse_expression_binding_power(stream, right_binding_power)?;

        left = Expression {
            kind: ExpressionKind::BinaryOp(BinaryOp {
                left: Box::new(left),
                right: Box::new(right),
                op
            })
        }
    }

    Ok(left)
}

/// Binding power for infix ooperators like add, subtract, etc.
fn infix_binding_power(op: &Operator) -> (u8, u8) {
    match op {
        Operator::Add | Operator::Subtract => (1, 2),
        Operator::Multiply | Operator::Divide => (3, 4),
    }
}

/// Parse a literal expression with syntax:
/// LiteralExpression = <Boolean> | <Int> | <Float> | <String>
fn parse_literal_expression(
    stream: &mut ParsingStream<Token>,
) -> Result<Expression, ParsingError> {
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
    use crate::{parser::{ast::TypeKind, path::{Path, PathSegment}, statement::{StatementKind, VariableDeclaration, VariableDeclarationKind}, tests::assert_parsing_result}, span::Span};

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
                TokenType::EOF
            ],
            parse_expression,
            Ok(Expression { kind: ExpressionKind::BinaryOp(BinaryOp {
                op: Operator::Add,
                left: Box::new(Expression { kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(1), span: span.clone() }) }),
                right: Box::new(Expression { kind: ExpressionKind::BinaryOp(BinaryOp {
                    op: Operator::Multiply,
                    left: Box::new(Expression { kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(2), span: span.clone() }) }),
                    right: Box::new(Expression { kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(3), span: span.clone() }) })
                })})
            })})
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
                TokenType::EOF
            ], parse_expression,
            Ok(Expression { kind: ExpressionKind::Function(Function {
                args: vec![
                    FunctionArg {
                        identifier: Identifier { name: "hello".to_string() },
                        arg_type: Type { kind: TypeKind::Infer }
                    },
                    FunctionArg {
                        identifier: Identifier { name: "world".to_string() },
                        arg_type: Type { kind: TypeKind::TypePath(Path { segments: vec![PathSegment { ident: Identifier { name: "string".to_string() } }] }) }
                    },
                ],
                body: Block {
                    statements: vec![
                        Statement { kind: StatementKind::Let(VariableDeclaration {
                            identifier: Identifier { name: "a".to_string() },
                            variable_type: Type { kind: TypeKind::Infer },
                            kind: VariableDeclarationKind::Init(Expression {
                                kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(10), span })
                            }),
                            is_mutable: false
                        })}
                    ]
                }
            })})
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
                TokenType::EOF
            ], parse_expression,
            Ok(Expression { kind: ExpressionKind::Function(Function {
                args: vec![
                    FunctionArg {
                        identifier: Identifier { name: "hello".to_string() },
                        arg_type: Type { kind: TypeKind::Infer }
                    },
                    FunctionArg {
                        identifier: Identifier { name: "world".to_string() },
                        arg_type: Type { kind: TypeKind::TypePath(Path { segments: vec![PathSegment { ident: Identifier { name: "string".to_string() } }] }) }
                    },
                ],
                body: Block { statements: vec![] }
            })})
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
                TokenType::EOF
            ], parse_expression,
            Ok(Expression { kind: ExpressionKind::Function(Function {
                args: vec![],
                body: Block { statements: vec![] }
            })})
        );
    }
}
