use crate::{
    lexer::{Token, TokenType},
    stream::{peek, ParsingStream},
};

use super::{ast::Identifier, basics::parse_identifier, ParsingError};

#[derive(Debug, PartialEq)]
pub struct Path {
    pub segments: Vec<PathSegment>,
}

#[derive(Debug, PartialEq)]
pub struct PathSegment {
    pub ident: Identifier,
}

/// Parse a path with syntax:
/// Path = <PathSegment> + (:: + <PathSegment>)*
pub fn parse_path(stream: &mut ParsingStream<Token>) -> Result<Path, ParsingError> {
    let mut path = Path {
        segments: Vec::new(),
    };

    loop {
        let segment = parse_path_segment(stream)?;
        path.segments.push(segment);

        match peek!(stream).token_type {
            TokenType::ColonColon => {
                stream.next();
            }
            _ => break,
        }
    }

    Ok(path)
}

/// Parse a path with syntax:
/// PathSegment = <Identifier>
pub fn parse_path_segment(stream: &mut ParsingStream<Token>) -> Result<PathSegment, ParsingError> {
    let ident = parse_identifier(stream)?;
    Ok(PathSegment { ident })
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
                        },
                    },
                    PathSegment {
                        ident: Identifier {
                            name: String::from("string"),
                        },
                    },
                ],
            }),
        );
    }
}
