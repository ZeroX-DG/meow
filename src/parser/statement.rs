use crate::{lexer::Token, parser::{ast::TypeKind, expect_token, expression, path}, stream::ParsingStream};

use super::{ast::{Identifier, Type}, expression::Expression, ParsingError};

#[derive(Debug, PartialEq)]
pub struct Statement {
    pub kind: StatementKind
}

#[derive(Debug, PartialEq)]
pub enum StatementKind {
    Let(VariableDeclaration),
    Expr(Expression)
}

#[derive(Debug, PartialEq)]
pub struct VariableDeclaration {
    pub identifier: Identifier,
    pub variable_type: Type,
    pub kind: VariableDeclarationKind,
    pub is_mutable: bool
}

#[derive(Debug, PartialEq)]
pub enum VariableDeclarationKind {
    Declaration,
    Init(Expression),
}

/// Parse a statement with syntax:
/// Statement = <LetStatement> | <ExpressionStatement>
pub fn parse_statement(stream: &mut ParsingStream<&Token>) -> Result<Statement, ParsingError> {
    match stream.peek() {
        Token::Let => parse_let_statement(stream),
        _ =>  parse_expression_statement(stream)
    }
}

/// Parse an ExpressionStatement with syntax:
/// ExpressionStatement = <Expression> + ;
fn parse_expression_statement(stream: &mut ParsingStream<&Token>) -> Result<Statement, ParsingError> {
    let expression = expression::parse_expression(stream)?;
    expect_token!(stream.next(), Token::SemiConlon);
    Ok(Statement { kind: StatementKind::Expr(expression) })
}

/// Parse a LetStatement with syntax:
/// LetStatement = <VariableDeclaration> + ;
fn parse_let_statement(stream: &mut ParsingStream<&Token>) -> Result<Statement, ParsingError> {
    let variable_declaration = parse_variable_declaration(stream)?;
    expect_token!(stream.next(), Token::SemiConlon);
    Ok(Statement { kind: StatementKind::Let(variable_declaration) })
}

/// Parse a variable declaration with syntax:
/// VariableDeclaration = <Let> + <Identifier> + (<Mut>)? + (: <Type>)? + = + <Expression> + ; 
fn parse_variable_declaration(stream: &mut ParsingStream<&Token>) -> Result<VariableDeclaration, ParsingError> {
    // Variable declaration start with keyword let
    expect_token!(stream.next(), Token::Let);

    let variable_name;
    let mut is_mutable = false;

    loop {
        match stream.next() {
            Token::Mut => { is_mutable = true; }
            Token::Identifier(identifier) => {
                variable_name = identifier.clone();
                break;
            },
            token => return Err(ParsingError::UnexpectedToken(token.clone()))
        }
    }

    let variable_type = match stream.peek() {
        Token::Colon => {
            stream.next(); // Consume Colon
            let path = path::parse_path(stream)?;
            Type { kind: TypeKind::TypePath(path) }
        }
        Token::Eq | Token::SemiConlon => Type { kind: TypeKind::Infer },
        token => return Err(ParsingError::UnexpectedToken(token.clone()))
    };

    let variable_declaration_kind = match stream.peek() {
        Token::Eq => {
            stream.next(); // Consume Eq
            VariableDeclarationKind::Init(expression::parse_expression(stream)?)
        },
        _ => VariableDeclarationKind::Declaration
    };

    let variable_declaration = VariableDeclaration {
        kind: variable_declaration_kind,
        identifier: Identifier { name: variable_name },
        variable_type,
        is_mutable
    };
    
    Ok(variable_declaration)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::{Path, PathSegment, Expression, ExpressionKind, Literal, LiteralKind};

    #[test]
    fn test_parse_statement_missing_semicolon() {
        // let mut hello: string
        let tokens = vec![
            Token::Let,
            Token::Mut,
            Token::Identifier(String::from("hello")),
            Token::Colon,
            Token::Identifier(String::from("string")),
            Token::EOF
        ]; 
        let mut iter = tokens.iter();
        let mut stream = ParsingStream::new(&mut iter, &Token::EOF);
        let ast = parse_statement(&mut stream);
        assert_eq!(ast, Err(ParsingError::UnexpectedToken(Token::EOF)));
    }

    #[test]
    fn test_parse_variable_declaration_immutable() {
        // let hello: string;
        let tokens = vec![
            Token::Let,
            Token::Identifier(String::from("hello")),
            Token::Colon,
            Token::Identifier(String::from("string")),
            Token::SemiConlon,
            Token::EOF
        ];
        let mut iter = tokens.iter();
        let mut stream = ParsingStream::new(&mut iter, &Token::EOF);
        let ast = parse_statement(&mut stream);
        let expected = Statement { kind: StatementKind::Let(VariableDeclaration {
            variable_type: Type {
                kind: TypeKind::TypePath(Path { segments: vec![PathSegment { ident: Identifier { name: String::from("string") } }] })
            },
            identifier: Identifier { name: String::from("hello") },
            kind: VariableDeclarationKind::Declaration,
            is_mutable: false
        })};
        assert_eq!(ast, Ok(expected))
    }

    #[test]
    fn test_parse_variable_declaration() {
        // let mut hello: string;
        let tokens = vec![
            Token::Let,
            Token::Mut,
            Token::Identifier(String::from("hello")),
            Token::Colon,
            Token::Identifier(String::from("string")),
            Token::SemiConlon,
            Token::EOF
        ];
        let mut iter = tokens.iter();
        let mut stream = ParsingStream::new(&mut iter, &Token::EOF);
        let ast = parse_statement(&mut stream);
        let expected = Statement { kind: StatementKind::Let(VariableDeclaration {
            variable_type: Type {
                kind: TypeKind::TypePath(Path { segments: vec![PathSegment { ident: Identifier { name: String::from("string") } }] })
            },
            identifier: Identifier { name: String::from("hello") },
            kind: VariableDeclarationKind::Declaration,
            is_mutable: true
        })};
        assert_eq!(ast, Ok(expected))
    }

    #[test]
    fn test_parse_variable_declaration_infer_type() {
        // let mut hello: string;
        let tokens = vec![
            Token::Let,
            Token::Mut,
            Token::Identifier(String::from("hello")),
            Token::SemiConlon,
            Token::EOF
        ];
        let mut iter = tokens.iter();
        let mut stream = ParsingStream::new(&mut iter, &Token::EOF);
        let ast = parse_statement(&mut stream);
        let expected = Statement { kind: StatementKind::Let(VariableDeclaration {
            variable_type: Type {
                kind: TypeKind::Infer
            },
            identifier: Identifier { name: String::from("hello") },
            kind: VariableDeclarationKind::Declaration,
            is_mutable: true
        })};
        assert_eq!(ast, Ok(expected))
    }

    #[test]
    fn test_parse_variable_initialization() {
        // let mut hello: string = "hello";
        let tokens = vec![
            Token::Let,
            Token::Mut,
            Token::Identifier(String::from("hello")),
            Token::Colon,
            Token::Identifier(String::from("string")),
            Token::Eq,
            Token::String(String::from("hello")),
            Token::SemiConlon,
            Token::EOF
        ];
        let mut iter = tokens.iter();
        let mut stream = ParsingStream::new(&mut iter, &Token::EOF);
        let ast = parse_statement(&mut stream);
        let expected = Statement { kind: StatementKind::Let(VariableDeclaration {
            variable_type: Type {
                kind: TypeKind::TypePath(Path { segments: vec![PathSegment { ident: Identifier { name: String::from("string") } }] })
            },
            identifier: Identifier { name: String::from("hello") },
            kind: VariableDeclarationKind::Init(Expression { kind: ExpressionKind::Literal(Literal { kind: LiteralKind::String(String::from("hello")) }) }),
            is_mutable: true
        })};
        assert_eq!(ast, Ok(expected))
    }

    #[test]
    fn test_parse_variable_initialization_infer_type() {
        // let mut hello = "hello";
        let tokens = vec![
            Token::Let,
            Token::Mut,
            Token::Identifier(String::from("hello")),
            Token::Eq,
            Token::String(String::from("hello")),
            Token::SemiConlon,
            Token::EOF
        ];

        let mut iter = tokens.iter();
        let mut stream = ParsingStream::new(&mut iter, &Token::EOF);
        let ast = parse_statement(&mut stream);
        let expected = Statement { kind: StatementKind::Let(VariableDeclaration {
            variable_type: Type { kind: TypeKind::Infer },
            identifier: Identifier { name: String::from("hello") },
            kind: VariableDeclarationKind::Init(Expression { kind: ExpressionKind::Literal(Literal { kind: LiteralKind::String(String::from("hello")) }) }),
            is_mutable: true
        })};
        assert_eq!(ast, Ok(expected))
    }
}