use crate::{lexer::Token, stream::ParsingStream};

use super::{ast::Identifier, Parser, ParsingError};

#[derive(Debug, PartialEq)]
pub struct Path {
    pub segments: Vec<PathSegment>
}

#[derive(Debug, PartialEq)]
pub struct PathSegment {
    pub ident: Identifier,
}

/// Parse a path with syntax:
/// Path = <PathSegment> + (:: + <PathSegment>)*
pub fn parse_path(stream: &mut ParsingStream<&Token>) -> Result<Path, ParsingError> {
    let mut path = Path {
        segments: Vec::new()
    };

    loop {
        let segment = parse_path_segment(stream)?;
        path.segments.push(segment);

        match stream.peek() {
            Token::ColonColon => { stream.next(); }
            _ => break
        }
    }

    Ok(path)
}

/// Parse a path with syntax:
/// PathSegment = <Identifier>
pub fn parse_path_segment(stream: &mut ParsingStream<&Token>) -> Result<PathSegment, ParsingError> {
    let ident = Parser::parse_identifier(stream)?;
    Ok(PathSegment{ ident })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_path() {
        // std::string
        let tokens = vec![
            Token::Identifier(String::from("std")),
            Token::ColonColon,
            Token::Identifier(String::from("string")),
        ];

        let mut iter = tokens.iter();
        let mut stream = ParsingStream::new(&mut iter, &Token::EOF);

        let path = parse_path(&mut stream);
        assert_eq!(path, Ok(Path { segments: vec![
            PathSegment { ident: Identifier { name: String::from("std") } },
            PathSegment { ident: Identifier { name: String::from("string") } },
        ]}))
    }
}