use serde::Serialize;

use crate::span::Span;

pub use super::expression::*;
pub use super::path::*;
pub use super::statement::*;

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
    pub span: Span,
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
