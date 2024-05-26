use serde::Serialize;

pub use super::expression::{Expression, ExpressionKind, Literal, LiteralKind};
pub use super::path::{Path, PathSegment};
pub use super::statement::{
    Statement, StatementKind, VariableDeclaration, VariableDeclarationKind,
};

#[derive(Debug, PartialEq, Serialize)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct Item {
    pub kind: ItemKind,
}

#[derive(Debug, PartialEq, Serialize)]
pub enum ItemKind {
    Statement(Statement),
}

#[derive(Debug, PartialEq, Serialize)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, PartialEq, Serialize)]
pub struct Type {
    pub kind: TypeKind,
}

#[derive(Debug, PartialEq, Serialize)]
pub enum TypeKind {
    Infer,
    TypePath(Path),
}
