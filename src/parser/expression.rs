use crate::{lexer::{Token, TokenType}, span::Span, stream::ParsingStream};

use super::ParsingError;

#[derive(Debug, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind
}

#[derive(Debug, PartialEq)]
pub enum ExpressionKind {
    Literal(Literal),
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
/// Expression = <LiteralExpression>
pub fn parse_expression(stream: &mut ParsingStream<Token>) -> Result<Expression, ParsingError> {
    let token = stream.peek();
    match token.token_type {
        TokenType::Boolean(_) | TokenType::Int(_) | TokenType::Float(_) | TokenType::String(_) => parse_literal_expression(stream),
        _ => return Err(ParsingError::UnexpectedToken(token.clone()))
    }
}

/// Parse a literal expression with syntax:
/// LiteralExpression = <Boolean> | <Int> | <Float> | <String>
pub fn parse_literal_expression(stream: &mut ParsingStream<Token>) -> Result<Expression, ParsingError> {
    let token = stream.next();
    let kind = match &token.token_type {
        TokenType::Boolean(value) => {
            ExpressionKind::Literal(Literal { kind: LiteralKind::Boolean(*value), span: token.span })
        },
        TokenType::Int(value) => {
            ExpressionKind::Literal(Literal { kind: LiteralKind::Int(*value), span: token.span })
        },
        TokenType::Float(value) => {
            ExpressionKind::Literal(Literal { kind: LiteralKind::Float(*value), span: token.span })
        },
        TokenType::String(value) => {
            ExpressionKind::Literal(Literal { kind: LiteralKind::String(value.to_owned()), span: token.span })
        },
        _ => return Err(ParsingError::UnexpectedToken(token.clone()))
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
            Ok(Expression { kind: ExpressionKind::Literal(Literal {
                kind: LiteralKind::String(String::from("hello")),
                span: span.clone(),
            })})
        );

        // 12
        assert_parsing_result(
            vec![TokenType::Int(12)],
            parse_expression,
            Ok(Expression { kind: ExpressionKind::Literal(Literal {
                kind: LiteralKind::Int(12),
                span: span.clone(),
            })})
        );

        // 47.2821
        assert_parsing_result(
            vec![TokenType::Float(47.2821)],
            parse_expression,
            Ok(Expression { kind: ExpressionKind::Literal(Literal {
                kind: LiteralKind::Float(47.2821),
                span: span.clone(),
            })})
        );

        // true
        assert_parsing_result(
            vec![TokenType::Boolean(true)],
            parse_expression,
            Ok(Expression { kind: ExpressionKind::Literal(Literal {
                kind: LiteralKind::Boolean(true),
                span: span.clone(),
            })})
        );

        // false
        assert_parsing_result(
            vec![TokenType::Boolean(false)],
            parse_expression,
            Ok(Expression { kind: ExpressionKind::Literal(Literal {
                kind: LiteralKind::Boolean(false),
                span: span.clone(),
            })})
        );
    }
}