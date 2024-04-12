use crate::{
    lexer::{Token, TokenType},
    span::Span,
    stream::ParsingStream,
};

use super::ParsingError;

#[derive(Debug, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind,
}

#[derive(Debug, PartialEq)]
pub enum ExpressionKind {
    Literal(Literal),
    BinaryOp(BinaryOp),
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

/// Parse an expression with syntax:
/// Expression = <LiteralExpression> | <BinaryOperationExpression>
pub fn parse_expression(stream: &mut ParsingStream<Token>) -> Result<Expression, ParsingError> {
    parse_expression_binding_power(stream, 0)
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
            TokenType::EOF => break,
            TokenType::Plus => Operator::Add,
            TokenType::Minus => Operator::Subtract,
            TokenType::Multiply => Operator::Multiply,
            TokenType::Divide => Operator::Divide,
            _ => return Err(ParsingError::UnexpectedToken(token.clone()))
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
    use crate::{parser::tests::assert_parsing_result, span::Span};

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
}
