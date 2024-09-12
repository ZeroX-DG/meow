use codespan::Span;

use super::token::{Token, TokenData, TokenType};
use crate::stream::{peek, ParsingStream};

macro_rules! peek_next {
    ($stream:ident, $x:expr) => {
        matches!(peek!($stream), $x)
    };
}

#[derive(Debug)]
pub enum LexingError {
    UnexpectedCharacter(char),
    InvalidFloatingNumberFormat,
    InvalidIntFormat,
}

pub struct Lexer {
    tokens: Vec<Token>,
    byte_index: usize,
}

impl Lexer {
    fn new() -> Self {
        Self {
            tokens: Vec::new(),
            byte_index: 0,
        }
    }

    pub fn tokenize(input: &str) -> Result<Vec<Token>, LexingError> {
        let mut lexer = Self::new();
        let mut chars = input.chars();
        let mut stream = ParsingStream::new(&mut chars, '\0');

        stream.on_next_item(move |ch| {
            lexer.byte_index += ch.len_utf8();
        });

        loop {
            let ch = stream.next();
            match ch {
                ch if ch.is_whitespace() => {
                    continue;
                }
                '(' => lexer.push_token(lexer.char_span(), TokenType::ParenOpen, TokenData::None),
                ')' => lexer.push_token(lexer.char_span(), TokenType::ParenClose, TokenData::None),
                '[' => lexer.push_token(
                    lexer.char_span(),
                    TokenType::SquareBracketOpen,
                    TokenData::None,
                ),
                ']' => lexer.push_token(
                    lexer.char_span(),
                    TokenType::SquareBracketClose,
                    TokenData::None,
                ),
                '{' => lexer.push_token(
                    lexer.char_span(),
                    TokenType::CurlyBracketOpen,
                    TokenData::None,
                ),
                '}' => lexer.push_token(
                    lexer.char_span(),
                    TokenType::CurlyBracketClose,
                    TokenData::None,
                ),
                '/' if peek_next!(stream, '/') => {
                    // Comments
                    // Ignore for now.
                    stream.consume_until(|c| *c == '\n');
                }
                '/' => lexer.push_token(lexer.char_span(), TokenType::Divide, TokenData::None),
                '*' => lexer.push_token(lexer.char_span(), TokenType::Multiply, TokenData::None),
                '+' => lexer.push_token(lexer.char_span(), TokenType::Plus, TokenData::None),
                '-' if peek_next!(stream, '>') => {
                    lexer.push_token(lexer.new_span(2), TokenType::ThinArrow, TokenData::None);
                    stream.next();
                }
                '-' => lexer.push_token(lexer.char_span(), TokenType::Minus, TokenData::None),
                '%' => lexer.push_token(lexer.char_span(), TokenType::Mod, TokenData::None),
                '|' if peek_next!(stream, '|') => {
                    lexer.push_token(lexer.new_span(2), TokenType::OrOr, TokenData::None);
                    stream.next();
                }
                '|' => lexer.push_token(lexer.char_span(), TokenType::Or, TokenData::None),
                '&' if peek_next!(stream, '&') => {
                    lexer.push_token(lexer.new_span(2), TokenType::AndAnd, TokenData::None);
                    stream.next();
                }
                '&' => lexer.push_token(lexer.char_span(), TokenType::And, TokenData::None),
                '!' if peek_next!(stream, '=') => {
                    lexer.push_token(lexer.new_span(2), TokenType::NotEq, TokenData::None);
                    stream.next();
                }
                '!' => lexer.push_token(lexer.char_span(), TokenType::Not, TokenData::None),
                '=' if peek_next!(stream, '=') => {
                    lexer.push_token(lexer.new_span(2), TokenType::EqEq, TokenData::None);
                    stream.next();
                }
                '=' if peek_next!(stream, '>') => {
                    lexer.push_token(lexer.new_span(2), TokenType::FatArrow, TokenData::None);
                    stream.next();
                }
                '=' => lexer.push_token(lexer.char_span(), TokenType::Eq, TokenData::None),
                '>' if peek_next!(stream, '=') => {
                    lexer.push_token(lexer.new_span(2), TokenType::GreaterEq, TokenData::None);
                    stream.next();
                }
                '>' => lexer.push_token(lexer.char_span(), TokenType::GreaterThan, TokenData::None),
                '<' if peek_next!(stream, '=') => {
                    lexer.push_token(lexer.new_span(2), TokenType::LessEq, TokenData::None);
                    stream.next();
                }
                '<' => lexer.push_token(lexer.char_span(), TokenType::LessThan, TokenData::None),
                ',' => lexer.push_token(lexer.char_span(), TokenType::Comma, TokenData::None),
                '.' => lexer.push_token(lexer.char_span(), TokenType::Period, TokenData::None),
                ':' if peek_next!(stream, ':') => {
                    lexer.push_token(lexer.new_span(2), TokenType::ColonColon, TokenData::None);
                    stream.next();
                }
                ':' => lexer.push_token(lexer.char_span(), TokenType::Colon, TokenData::None),
                ';' => lexer.push_token(lexer.char_span(), TokenType::SemiConlon, TokenData::None),
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut content = String::from(ch);

                    loop {
                        let c = peek!(stream);
                        if !c.is_alphanumeric() && c != '_' {
                            break;
                        }
                        content.push(c);
                        stream.next();
                    }

                    if content == "fn" {
                        lexer.push_token(lexer.new_span(2), TokenType::Function, TokenData::None);
                        continue;
                    }

                    if content == "class" {
                        lexer.push_token(lexer.new_span(5), TokenType::Class, TokenData::None);
                        continue;
                    }

                    if content == "return" {
                        lexer.push_token(lexer.new_span(6), TokenType::Return, TokenData::None);
                        continue;
                    }

                    if content == "true" {
                        lexer.push_token(
                            lexer.new_span(4),
                            TokenType::Boolean,
                            TokenData::Boolean(true),
                        );
                        continue;
                    }

                    if content == "false" {
                        lexer.push_token(
                            lexer.new_span(5),
                            TokenType::Boolean,
                            TokenData::Boolean(false),
                        );
                        continue;
                    }

                    lexer.push_token(
                        lexer.new_span(content.len()),
                        TokenType::Identifier,
                        TokenData::Identifier(content),
                    );
                }
                '\'' => {
                    let mut content = String::new();
                    loop {
                        let c = peek!(stream);
                        if c == '\\' {
                            stream.next();
                            let c2 = stream.next();
                            if c2 == 'n' {
                                content.push('\n');
                            } else if c2 == 'r' {
                                content.push('\r');
                            } else if c2 == 't' {
                                content.push('\t');
                            } else if c2 == '0' {
                                content.push('\0');
                            } else {
                                content.push(c2);
                            }
                            continue;
                        }

                        if c == '\'' {
                            stream.next();
                            break;
                        }
                        stream.next();
                        content.push(c);
                    }
                    lexer.push_token(
                        lexer.new_span(content.len()),
                        TokenType::String,
                        TokenData::String(content),
                    );
                }
                '0'..='9' => {
                    let mut content = String::from(ch);
                    let mut is_float = false;
                    loop {
                        let c = peek!(stream);
                        if !c.is_numeric() && c != '.' {
                            break;
                        }
                        if c == '.' {
                            is_float = true;
                        }
                        content.push(c);
                        stream.next();
                    }
                    if is_float {
                        lexer.push_token(
                            lexer.new_span(content.len()),
                            TokenType::Float,
                            TokenData::Float(
                                content
                                    .parse()
                                    .map_err(|_| LexingError::InvalidFloatingNumberFormat)?,
                            ),
                        );
                    } else {
                        lexer.push_token(
                            lexer.new_span(content.len()),
                            TokenType::Int,
                            TokenData::Int(
                                content.parse().map_err(|_| LexingError::InvalidIntFormat)?,
                            ),
                        );
                    };
                }
                '\0' => {
                    lexer.push_token(lexer.char_span(), TokenType::EOF, TokenData::None);
                    break;
                }
                _ => {
                    return Err(LexingError::UnexpectedCharacter(ch));
                }
            }
        }

        Ok(lexer.tokens)
    }

    fn new_span(&self, length: usize) -> Span {
        Span::new(self.byte_index as u32, (self.byte_index + length) as u32)
    }

    fn char_span(&self) -> Span {
        self.new_span(1)
    }

    fn push_token(&mut self, span: Span, token_type: TokenType, token_data: TokenData) {
        self.byte_index = (span.end().0 + 1) as usize;

        let token = Token {
            span,
            token_type,
            token_data,
        };

        self.tokens.push(token);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_delimeters() {
        let input = "(){}[].,:;::";

        assert_token_types(
            input,
            vec![
                TokenType::ParenOpen,
                TokenType::ParenClose,
                TokenType::CurlyBracketOpen,
                TokenType::CurlyBracketClose,
                TokenType::SquareBracketOpen,
                TokenType::SquareBracketClose,
                TokenType::Period,
                TokenType::Comma,
                TokenType::Colon,
                TokenType::SemiConlon,
                TokenType::ColonColon,
                TokenType::EOF,
            ],
        );
    }

    #[test]
    fn tokenize_operators() {
        let input = "+-*/%|=&& == != &!>< >= <= =>";

        assert_token_types(
            input,
            vec![
                TokenType::Plus,
                TokenType::Minus,
                TokenType::Multiply,
                TokenType::Divide,
                TokenType::Mod,
                TokenType::Or,
                TokenType::Eq,
                TokenType::AndAnd,
                TokenType::EqEq,
                TokenType::NotEq,
                TokenType::And,
                TokenType::Not,
                TokenType::GreaterThan,
                TokenType::LessThan,
                TokenType::GreaterEq,
                TokenType::LessEq,
                TokenType::FatArrow,
                TokenType::EOF,
            ],
        );
    }

    #[test]
    fn tokenize_keywords() {
        let input = "fn class _something_else20 return";

        assert_token_types(
            input,
            vec![
                TokenType::Function,
                TokenType::Class,
                TokenType::Identifier,
                TokenType::Return,
                TokenType::EOF,
            ],
        );
    }

    #[test]
    fn tokenize_string_literals() {
        let input = "hello = 'Hi! I\\'m Hung'";

        assert_tokens(
            input,
            vec![
                Token {
                    span: Span::new(0, 5),
                    token_type: TokenType::Identifier,
                    token_data: TokenData::Identifier("hello".to_string()),
                },
                Token {
                    span: Span::new(6, 7),
                    token_type: TokenType::Eq,
                    token_data: TokenData::None,
                },
                Token {
                    span: Span::new(8, 20),
                    token_type: TokenType::String,
                    token_data: TokenData::String("Hi! I'm Hung".to_string()),
                },
                Token {
                    span: Span::new(20, 21),
                    token_type: TokenType::EOF,
                    token_data: TokenData::None,
                },
            ],
        );
    }

    #[test]
    fn tokenize_boolean() {
        let input = "isAwesome = true";

        assert_tokens(
            input,
            vec![
                Token {
                    span: Span::new(0, 9),
                    token_type: TokenType::Identifier,
                    token_data: TokenData::Identifier("isAwesome".to_string()),
                },
                Token {
                    span: Span::new(10, 11),
                    token_type: TokenType::Eq,
                    token_data: TokenData::None,
                },
                Token {
                    span: Span::new(12, 16),
                    token_type: TokenType::Boolean,
                    token_data: TokenData::Boolean(true),
                },
                Token {
                    span: Span::new(16, 17),
                    token_type: TokenType::EOF,
                    token_data: TokenData::None,
                },
            ],
        );
    }

    #[test]
    fn tokenize_float_literals() {
        let input = "age = 10.5235";

        assert_tokens(
            input,
            vec![
                Token {
                    span: Span::new(0, 3),
                    token_type: TokenType::Identifier,
                    token_data: TokenData::Identifier("age".to_string()),
                },
                Token {
                    span: Span::new(4, 5),
                    token_type: TokenType::Eq,
                    token_data: TokenData::None,
                },
                Token {
                    span: Span::new(6, 13),
                    token_type: TokenType::Float,
                    token_data: TokenData::Float(10.5235),
                },
                Token {
                    span: Span::new(13, 14),
                    token_type: TokenType::EOF,
                    token_data: TokenData::None,
                },
            ],
        );
    }

    #[test]
    fn tokenize_int_literals() {
        let input = "age = 22";

        assert_tokens(
            input,
            vec![
                Token {
                    span: Span::new(0, 3),
                    token_type: TokenType::Identifier,
                    token_data: TokenData::Identifier("age".to_string()),
                },
                Token {
                    span: Span::new(4, 5),
                    token_type: TokenType::Eq,
                    token_data: TokenData::None,
                },
                Token {
                    span: Span::new(6, 8),
                    token_type: TokenType::Int,
                    token_data: TokenData::Int(22),
                },
                Token {
                    span: Span::new(8, 9),
                    token_type: TokenType::EOF,
                    token_data: TokenData::None,
                },
            ],
        );
    }

    fn assert_tokens(input: &str, expect: Vec<Token>) {
        let tokens = Lexer::tokenize(input).expect("Lexer tokenization error");

        assert_eq!(tokens, expect);
    }

    fn assert_token_types(input: &str, expect: Vec<TokenType>) {
        let tokens = Lexer::tokenize(input)
            .expect("Lexer tokenization error")
            .into_iter()
            .map(|token| token.token_type)
            .collect::<Vec<TokenType>>();

        assert_eq!(tokens, expect);
    }
}
