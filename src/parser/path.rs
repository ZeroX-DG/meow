use serde::Serialize;

use crate::{
    lexer::{Token, TokenType},
    span::Span,
    stream::{peek, ParsingStream},
};

use super::{ast::Identifier, basics::parse_identifier, ParsingError};

#[derive(Debug, PartialEq, Serialize)]
pub struct Path {
    pub segments: Vec<PathSegment>,
    pub span: Span,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct PathSegment {
    pub ident: Identifier,
    pub span: Span,
}

/// Parse a path with syntax:
/// Path = <PathSegment> + (:: + <PathSegment>)*
pub fn parse_path(stream: &mut ParsingStream<Token>) -> Result<Path, ParsingError> {
    let mut segments = Vec::new();

    loop {
        let segment = parse_path_segment(stream)?;
        segments.push(segment);

        match peek!(stream).token_type {
            TokenType::ColonColon => {
                stream.next();
            }
            _ => break,
        }
    }

    let start_location = segments.first().unwrap().span.start.clone();
    let end_location = segments.last().unwrap().span.end.clone();

    Ok(Path {
        segments,
        span: Span {
            start: start_location,
            end: end_location,
        },
    })
}

/// Parse a path with syntax:
/// PathSegment = <Identifier>
pub fn parse_path_segment(stream: &mut ParsingStream<Token>) -> Result<PathSegment, ParsingError> {
    let ident = parse_identifier(stream)?;
    let span = ident.span.clone();
    Ok(PathSegment { ident, span })
}

#[cfg(test)]
mod tests {
    use crate::parser::tests::assert_parsing_result;

    use super::*;

    #[test]
    fn test_parse_path() {
        // std::string
        assert_parsing_result(
            vec![
                TokenType::Identifier(String::from("std")),
                TokenType::ColonColon,
                TokenType::Identifier(String::from("string")),
            ],
            parse_path,
            Ok(Path {
                segments: vec![
                    PathSegment {
                        ident: Identifier {
                            name: String::from("std"),
                            span: Span::default(),
                        },
                        span: Span::default(),
                    },
                    PathSegment {
                        ident: Identifier {
                            name: String::from("string"),
                            span: Span::default(),
                        },
                        span: Span::default(),
                    },
                ],
                span: Span::default(),
            }),
        );
    }
}
