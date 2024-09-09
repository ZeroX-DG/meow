use crate::{
    lexer::{Token, TokenData, TokenType},
    stream::ParsingStream,
};

use super::{ast::Identifier, expect_token, ParsingError};

/// Parse an identiifier with syntax:
/// Identifier = <Identifier>
pub fn parse_identifier(stream: &mut ParsingStream<Token>) -> Result<Identifier, ParsingError> {
    let token = stream.next();

    expect_token!(&token, [Identifier]);

    match token.token_data {
        TokenData::Identifier(ident) => Ok(Identifier {
            name: ident.clone(),
            span: token.span,
        }),
        _ => unreachable!(),
    }
}
