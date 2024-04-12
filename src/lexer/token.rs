use crate::span::Span;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub span: Span,
    pub token_type: TokenType
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Identifier(String),

    // Keywords
    Function,
    Class,
    Let,
    Mut,
    Return,

    // Operators
    Plus,
    Minus,
    Multiply,
    Divide,
    Mod,
    Or,
    OrOr,
    And,
    AndAnd,
    Not,
    Eq,
    EqEq,
    NotEq,
    GreaterThan,
    LessThan,
    GreaterEq,
    LessEq,
    FatArrow,

    // Delimiters
    Comma,
    Period,
    Colon,
    SemiConlon,
    ParenOpen,
    ParenClose,
    SquareBracketOpen,
    SquareBracketClose,
    CurlyBracketOpen,
    CurlyBracketClose,
    ColonColon,

    // Literals
    Int(i64),
    Float(f64),
    String(String),
    Boolean(bool),

    EOF
}
