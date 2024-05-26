use crate::{lexer::{Token, TokenType}, stream::ParsingStream};

use super::{ast::Identifier, expect_token, ParsingError};

/// Parse an identiifier with syntax:
/// Identifier = <Identifier>
pub fn parse_identifier(stream: &mut ParsingStream<Token>) -> Result<Identifier, ParsingError> {
    let token = stream.next();

    expect_token!(token, TokenType::Identifier(_));

    match token.token_type {
        TokenType::Identifier(ident) => Ok(Identifier {
            name: ident.clone(),
        }),
        _ => unreachable!(),
    }
}