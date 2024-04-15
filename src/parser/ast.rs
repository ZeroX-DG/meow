pub use super::expression::{Expression, ExpressionKind, Literal, LiteralKind};
pub use super::path::{Path, PathSegment};
pub use super::statement::{
    Statement, StatementKind, VariableDeclaration, VariableDeclarationKind,
};

#[derive(Debug, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, PartialEq)]
pub struct Item {
    pub kind: ItemKind,
}

#[derive(Debug, PartialEq)]
pub enum ItemKind {
    Statement(Statement),
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub struct Type {
    pub kind: TypeKind,
}

#[derive(Debug, PartialEq)]
pub enum TypeKind {
    Infer,
    TypePath(Path),
}
