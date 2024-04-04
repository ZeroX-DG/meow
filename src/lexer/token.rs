#[derive(Debug, PartialEq, Clone)]
pub enum Token {
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

    // Literals
    Int(i64),
    Float(f64),
    String(String),
    Boolean(bool),

    EOF
}
