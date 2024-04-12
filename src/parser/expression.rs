use crate::{lexer::Token, stream::ParsingStream};

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
    pub kind: LiteralKind
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
pub fn parse_expression(stream: &mut ParsingStream<&Token>) -> Result<Expression, ParsingError> {
    match stream.peek() {
        Token::Boolean(_) | Token::Int(_) | Token::Float(_) | Token::String(_) => parse_literal_expression(stream),
        token => return Err(ParsingError::UnexpectedToken(token.clone()))
    }
}

/// Parse a literal expression with syntax:
/// LiteralExpression = <Boolean> | <Int> | <Float> | <String>
pub fn parse_literal_expression(stream: &mut ParsingStream<&Token>) -> Result<Expression, ParsingError> {
    let kind = match stream.next() {
        Token::Boolean(value) => {
            ExpressionKind::Literal(Literal { kind: LiteralKind::Boolean(*value) })
        },
        Token::Int(value) => {
            ExpressionKind::Literal(Literal { kind: LiteralKind::Int(*value) })
        },
        Token::Float(value) => {
            ExpressionKind::Literal(Literal { kind: LiteralKind::Float(*value) })
        },
        Token::String(value) => {
            ExpressionKind::Literal(Literal { kind: LiteralKind::String(value.to_owned()) })
        },
        token => return Err(ParsingError::UnexpectedToken(token.clone()))
    };

    Ok(Expression { kind })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_literal_expression() {
        //  "hello"
        assert_literal_expression(
            vec![Token::String(String::from("hello"))],
            Expression { kind: ExpressionKind::Literal(Literal { kind: LiteralKind::String(String::from("hello")) }) }
        );

        // 12
        assert_literal_expression(
            vec![Token::Int(12)],
            Expression { kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Int(12) }) }
        );

        // 47.2821
        assert_literal_expression(
            vec![Token::Float(47.2821)],
            Expression { kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Float(47.2821) }) }
        );

        // true
        assert_literal_expression(
            vec![Token::Boolean(true)],
            Expression { kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Boolean(true) }) }
        );

        // false
        assert_literal_expression(
            vec![Token::Boolean(false)],
            Expression { kind: ExpressionKind::Literal(Literal { kind: LiteralKind::Boolean(false) }) }
        );
    }

    fn assert_literal_expression(tokens: Vec<Token>, expected: Expression) {
        let mut iter = tokens.iter();
        let mut stream = ParsingStream::new(&mut iter, &Token::EOF);
        let expression = parse_literal_expression(&mut stream);
        assert_eq!(expression, Ok(expected))
    }
}