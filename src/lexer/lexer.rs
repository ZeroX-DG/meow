use crate::{span::{LineColumn, Span}, stream::ParsingStream};

use super::token::{Token, TokenType};

macro_rules! peek_next {
    ($stream:ident, $x:expr) => {
        matches!($stream.peek(), $x)
    };
}

#[derive(Debug)]
pub enum LexingError {
    UnexpectedCharacter(char),
    InvalidFloatingNumberFormat,
    InvalidIntFormat
}

pub struct Lexer {
    tokens: Vec<Token>,
    line: usize,
    column: usize,
}

impl Lexer {
    fn new() -> Self {
        Self {
            tokens: Vec::new(),
            line: 1,
            column: 0
        }
    }

    pub fn tokenize(input: &str) -> Result<Vec<Token>, LexingError> {
        let mut lexer = Self::new();
        let mut chars = input.chars();
        let mut stream = ParsingStream::new(&mut chars, '\0');

        loop {
            let ch = stream.next();
            match ch {
                ch if ch.is_whitespace() => {
                    if ch == '\n' {
                        lexer.line += 1;
                    } else {
                        lexer.column += 1;
                    }
                    continue
                },
                '(' => lexer.push_token(lexer.char_span(), TokenType::ParenOpen),
                ')' => lexer.push_token(lexer.char_span(), TokenType::ParenClose),
                '[' => lexer.push_token(lexer.char_span(), TokenType::SquareBracketOpen),
                ']' => lexer.push_token(lexer.char_span(), TokenType::SquareBracketClose),
                '{' => lexer.push_token(lexer.char_span(), TokenType::CurlyBracketOpen),
                '}' => lexer.push_token(lexer.char_span(), TokenType::CurlyBracketClose),
                '/' if peek_next!(stream, '/') => {
                    // Comments
                    // Ignore for now.
                    stream.consume_until(|c| {
                        lexer.column += 1;
                        *c == '\n'
                    });
                    lexer.line += 1;
                },
                '/' => lexer.push_token(lexer.char_span(), TokenType::Divide),
                '*' => lexer.push_token(lexer.char_span(), TokenType::Multiply),
                '+' => lexer.push_token(lexer.char_span(), TokenType::Plus),
                '-' => lexer.push_token(lexer.char_span(), TokenType::Minus),
                '%' => lexer.push_token(lexer.char_span(), TokenType::Mod),
                '|' if peek_next!(stream, '|') => {
                    lexer.push_token(lexer.line_span(2), TokenType::OrOr);
                    stream.next();
                },
                '|' => lexer.push_token(lexer.char_span(), TokenType::Or),
                '&' if peek_next!(stream, '&') => {
                    lexer.push_token(lexer.line_span(2), TokenType::AndAnd);
                    stream.next();
                },
                '&' => lexer.push_token(lexer.char_span(), TokenType::And),
                '!' if peek_next!(stream, '=') => {
                    lexer.push_token(lexer.line_span(2), TokenType::NotEq);
                    stream.next();
                },
                '!' => lexer.push_token(lexer.char_span(), TokenType::Not),
                '=' if peek_next!(stream, '=') => {
                    lexer.push_token(lexer.line_span(2), TokenType::EqEq);
                    stream.next();
                },
                '=' if peek_next!(stream, '>') => {
                    lexer.push_token(lexer.line_span(2), TokenType::FatArrow);
                    stream.next();
                },
                '=' => lexer.push_token(lexer.char_span(), TokenType::Eq),
                '>' if peek_next!(stream, '=') => {
                    lexer.push_token(lexer.line_span(2), TokenType::GreaterEq);
                    stream.next();
                },
                '>' => lexer.push_token(lexer.char_span(), TokenType::GreaterThan),
                '<' if peek_next!(stream, '=') => {
                    lexer.push_token(lexer.line_span(2), TokenType::LessEq);
                    stream.next();
                },
                '<' => lexer.push_token(lexer.char_span(), TokenType::LessThan),
                ',' => lexer.push_token(lexer.char_span(), TokenType::Comma),
                '.' => lexer.push_token(lexer.char_span(), TokenType::Period),
                ':' if peek_next!(stream, ':') => {
                    lexer.push_token(lexer.line_span(2), TokenType::ColonColon);
                    stream.next();
                }
                ':' => lexer.push_token(lexer.char_span(), TokenType::Colon),
                ';' => lexer.push_token(lexer.char_span(), TokenType::SemiConlon),
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut content = String::from(ch);

                    loop {
                        let c = stream.peek();
                        if !c.is_alphanumeric() && c != '_' {
                            break;
                        }
                        content.push(c);
                        stream.next();
                    }

                    if content == "fn" {
                        lexer.push_token(lexer.line_span(2), TokenType::Function);
                        continue;
                    }

                    if content == "class" {
                        lexer.push_token(lexer.line_span(5), TokenType::Class);
                        continue;
                    }

                    if content == "let" {
                        lexer.push_token(lexer.line_span(3), TokenType::Let);
                        continue;
                    }

                    if content == "mut" {
                        lexer.push_token(lexer.line_span(3), TokenType::Mut);
                        continue;
                    }

                    if content == "return" {
                        lexer.push_token(lexer.line_span(6), TokenType::Return);
                        continue;
                    }

                    if content == "true" {
                        lexer.push_token(lexer.line_span(4), TokenType::Boolean(true));
                        continue;
                    }

                    if content == "false" {
                        lexer.push_token(lexer.line_span(5), TokenType::Boolean(false));
                        continue;
                    }

                    lexer.push_token(lexer.line_span(content.len()), TokenType::Identifier(content));
                }
                '\'' => {
                    let mut content = String::new();
                    loop {
                        let c = stream.peek();
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
                    lexer.push_token(lexer.line_span(content.len()), TokenType::String(content));
                }
                '0'..='9' => {
                    let mut content = String::from(ch);
                    let mut is_float = false;
                    loop {
                        let c = stream.peek();
                        if !c.is_numeric() && c != '.' {
                            break;
                        }
                        if c == '.' {
                            is_float = true;
                        }
                        content.push(c);
                        stream.next();
                    }
                    let token = if is_float {
                        TokenType::Float(content.parse().map_err(|_| LexingError::InvalidFloatingNumberFormat)?)
                    } else {
                        TokenType::Int(content.parse().map_err(|_| LexingError::InvalidIntFormat)?)
                    };
                    lexer.push_token(lexer.line_span(content.len()), token);
                }
                '\0' => {
                    lexer.push_token(lexer.char_span(), TokenType::EOF);
                    break;
                }
                _ => {
                    return Err(LexingError::UnexpectedCharacter(ch));
                }
            }
        }

        Ok(lexer.tokens)
    }

    fn char_span(&self) -> Span {
        Span {
            start: LineColumn {
                line: self.line,
                column: self.column
            },
            end: LineColumn {
                line: self.line,
                column: self.column + 1
            }
        }
    }

    fn line_span(&self, length: usize) -> Span {
        Span {
            start: LineColumn {
                line: self.line,
                column: self.column
            },
            end: LineColumn {
                line: self.line,
                column: self.column + length
            }
        }
    }

    fn push_token(&mut self, span: Span, token_type: TokenType) {
        self.line = span.end.line;
        self.column = span.end.column;

        let token = Token {
            span,
            token_type
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

        assert_token_types(input, vec![
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
            TokenType::EOF
        ]);
    }

    #[test]
    fn tokenize_operators() {
        let input = "+-*/%|=&& == != &!>< >= <= =>";

        assert_token_types(input, vec![
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
            TokenType::EOF
        ]);
    }

    #[test]
    fn tokenize_keywords() {
        let input = "fn class mut let _something_else20 return";

        assert_token_types(input, vec![
            TokenType::Function,
            TokenType::Class,
            TokenType::Mut,
            TokenType::Let,
            TokenType::Identifier("_something_else20".to_string()),
            TokenType::Return,
            TokenType::EOF
        ]);
    }

    #[test]
    fn tokenize_string_literals() {
        let input = "let hello = 'Hi! I\\'m Hung'";

        assert_tokens(input, vec![
            Token { span: Span::from(((1, 0), (1, 3))), token_type: TokenType::Let },
            Token { span: Span::from(((1, 4), (1, 9))), token_type: TokenType::Identifier("hello".to_string()) },
            Token { span: Span::from(((1, 10), (1, 11))), token_type: TokenType::Eq },
            Token { span: Span::from(((1, 12), (1, 24))), token_type: TokenType::String("Hi! I'm Hung".to_string()) },
            Token { span: Span::from(((1, 24), (1, 25))), token_type: TokenType::EOF }
        ]);
    }

    #[test]
    fn tokenize_boolean() {
        let input = "let isAwesome = true";

        assert_tokens(input, vec![
            Token { span: Span::from(((1, 0), (1, 3))), token_type: TokenType::Let },
            Token { span: Span::from(((1, 4), (1, 13))), token_type: TokenType::Identifier("isAwesome".to_string()) },
            Token { span: Span::from(((1, 14), (1, 15))), token_type: TokenType::Eq },
            Token { span: Span::from(((1, 16), (1, 20))), token_type: TokenType::Boolean(true) },
            Token { span: Span::from(((1, 20), (1, 21))), token_type: TokenType::EOF }
        ]);
    }

    #[test]
    fn tokenize_float_literals() {
        let input = "let age = 10.5235";

        assert_tokens(input, vec![
            Token { span: Span::from(((1, 0), (1, 3))), token_type: TokenType::Let },
            Token { span: Span::from(((1, 4), (1, 7))), token_type: TokenType::Identifier("age".to_string()) },
            Token { span: Span::from(((1, 8), (1, 9))), token_type: TokenType::Eq },
            Token { span: Span::from(((1, 10), (1, 17))), token_type: TokenType::Float(10.5235) },
            Token { span: Span::from(((1, 17), (1, 18))), token_type: TokenType::EOF }
        ]);
    }

    #[test]
    fn tokenize_int_literals() {
        let input = "let age = 22";

        assert_tokens(input, vec![
            Token { span: Span::from(((1, 0), (1, 3))), token_type: TokenType::Let },
            Token { span: Span::from(((1, 4), (1, 7))), token_type: TokenType::Identifier("age".to_string()) },
            Token { span: Span::from(((1, 8), (1, 9))), token_type: TokenType::Eq },
            Token { span: Span::from(((1, 10), (1, 12))), token_type: TokenType::Int(22) },
            Token { span: Span::from(((1, 12), (1, 13))), token_type: TokenType::EOF }
        ]);
    }

    fn assert_tokens(input: &str, expect: Vec<Token>) {
        let tokens = Lexer::tokenize(input)
            .expect("Lexer tokenization error");

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