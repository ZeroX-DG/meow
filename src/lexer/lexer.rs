use crate::stream::ParsingStream;

use super::token::Token;

macro_rules! peek_next {
    ($stream:ident, $x:expr) => {
        matches!($stream.peek(1).get(0), Some($x))
    };
}

#[derive(Debug)]
pub enum LexingError {
    UnexpectedCharacter(char)
}

pub struct Lexer {
    tokens: Vec<Token>,
}

impl Lexer {
    fn new() -> Self {
        Self {
            tokens: Vec::new(),
        }
    }

    pub fn tokenize(input: &str) -> Result<Vec<Token>, LexingError> {
        let mut lexer = Self::new();
        let mut chars = input.chars();
        let mut stream = ParsingStream::new(&mut chars);

        while let Some(ch) = stream.next() {
            match ch {
                ch if ch.is_whitespace() => continue,
                '(' => lexer.push_token(Token::ParenOpen),
                ')' => lexer.push_token(Token::ParenClose),
                '[' => lexer.push_token(Token::SquareBracketOpen),
                ']' => lexer.push_token(Token::SquareBracketClose),
                '{' => lexer.push_token(Token::CurlyBracketOpen),
                '}' => lexer.push_token(Token::CurlyBracketClose),
                '/' if peek_next!(stream, '/') => {
                    // Comments
                    // Ignore for now.
                    stream.consume_until(|c| *c == '\n');
                },
                '/' => lexer.push_token(Token::Divide),
                '*' => lexer.push_token(Token::Multiply),
                '+' => lexer.push_token(Token::Plus),
                '-' => lexer.push_token(Token::Minus),
                '%' => lexer.push_token(Token::Mod),
                '|' if peek_next!(stream, '|') => {
                    lexer.push_token(Token::OrOr);
                    stream.next();
                },
                '|' => lexer.push_token(Token::Or),
                '&' if peek_next!(stream, '&') => {
                    lexer.push_token(Token::AndAnd);
                    stream.next();
                },
                '&' => lexer.push_token(Token::And),
                '!' if peek_next!(stream, '=') => {
                    lexer.push_token(Token::NotEq);
                    stream.next();
                },
                '!' => lexer.push_token(Token::Not),
                '=' if peek_next!(stream, '=') => {
                    lexer.push_token(Token::EqEq);
                    stream.next();
                },
                '=' if peek_next!(stream, '>') => {
                    lexer.push_token(Token::FatArrow);
                    stream.next();
                },
                '=' => lexer.push_token(Token::Eq),
                '>' if peek_next!(stream, '=') => {
                    lexer.push_token(Token::GreaterEq);
                    stream.next();
                },
                '>' => lexer.push_token(Token::GreaterThan),
                '<' if peek_next!(stream, '=') => {
                    lexer.push_token(Token::LessEq);
                    stream.next();
                },
                '<' => lexer.push_token(Token::LessThan),
                ',' => lexer.push_token(Token::Comma),
                '.' => lexer.push_token(Token::Period),
                ':' => lexer.push_token(Token::Colon),
                ';' => lexer.push_token(Token::SemiConlon),
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                    let mut content = String::from(ch);
                    while let Some(c) = stream.peek(1).get(0) {
                        if !c.is_alphanumeric() && *c != '_' {
                            break;
                        }
                        content.push(*c);
                        stream.next();
                    }

                    if content == "fn" {
                        lexer.push_token(Token::Function);
                        continue;
                    }

                    if content == "class" {
                        lexer.push_token(Token::Class);
                        continue;
                    }

                    if content == "let" {
                        lexer.push_token(Token::Let);
                        continue;
                    }

                    if content == "mut" {
                        lexer.push_token(Token::Mut);
                        continue;
                    }

                    if content == "return" {
                        lexer.push_token(Token::Return);
                        continue;
                    }

                    lexer.push_token(Token::Identifier(content));
                }
                '\'' => {
                    let mut content = String::new();
                    while let Some(c) = stream.next() {
                        if c == '\'' {
                            break;
                        }
                        content.push(c);
                    }
                    lexer.push_token(Token::String(content));
                }
                _ => {
                    return Err(LexingError::UnexpectedCharacter(ch));
                }
            }
        }

        Ok(lexer.tokens)
    }

    fn push_token(&mut self, token: Token) {
        self.tokens.push(token);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_delimeters() {
        let input = "(){}[].,:;";
        let tokens = Lexer::tokenize(input).expect("Lexer tokenization error");
        assert_eq!(tokens, vec![
            Token::ParenOpen,
            Token::ParenClose,
            Token::CurlyBracketOpen,
            Token::CurlyBracketClose,
            Token::SquareBracketOpen,
            Token::SquareBracketClose,
            Token::Period,
            Token::Comma,
            Token::Colon,
            Token::SemiConlon,
        ]);
    }

    #[test]
    fn tokenize_operators() {
        let input = "+-*/%|=&& == != &!>< >= <= =>";
        let tokens = Lexer::tokenize(input).expect("Lexer tokenization error");
        assert_eq!(tokens, vec![
            Token::Plus,
            Token::Minus,
            Token::Multiply,
            Token::Divide,
            Token::Mod,
            Token::Or,
            Token::Eq,
            Token::AndAnd,
            Token::EqEq,
            Token::NotEq,
            Token::And,
            Token::Not,
            Token::GreaterThan,
            Token::LessThan,
            Token::GreaterEq,
            Token::LessEq,
            Token::FatArrow
        ]);
    }

    #[test]
    fn tokenize_keywords() {
        let input = "fn class mut let _something_else20 return";
        let tokens = Lexer::tokenize(input).expect("Lexer tokenization error");
        assert_eq!(tokens, vec![
            Token::Function,
            Token::Class,
            Token::Mut,
            Token::Let,
            Token::Identifier("_something_else20".to_string()),
            Token::Return
        ]);
    }

    #[test]
    fn tokenize_string_literals() {
        let input = "let hello = 'yay!'";
        let tokens = Lexer::tokenize(input).expect("Lexer tokenization error");
        assert_eq!(tokens, vec![
            Token::Let,
            Token::Identifier("hello".to_string()),
            Token::Eq,
            Token::String("yay!".to_string()),
        ]);
    }
}