#[derive(Debug, PartialEq)]
pub struct Program {
    pub items: Vec<Item>
}

#[derive(Debug, PartialEq)]
pub struct Item {
    pub kind: ItemKind
}

#[derive(Debug, PartialEq)]
pub enum ItemKind {
    Statement(Statement),
}

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

#[derive(Debug, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind
}

#[derive(Debug, PartialEq)]
pub enum ExpressionKind {
    Literal(Literal),
    Function(Function),
    PropertyAccess(Vec<Identifier>),
    Call(Box<Expression>, Vec<Expression>)
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub args: Vec<FunctionArg>,
    pub body: Block
}

#[derive(Debug, PartialEq)]
pub struct FunctionArg {
    pub identifier: Identifier,
    pub arg_type: Type,
}

#[derive(Debug, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>
}

#[derive(Debug, PartialEq)]
pub struct Literal {
    pub kind: LiteralKind
}

#[derive(Debug, PartialEq)]
pub enum LiteralKind {
    Int(i64),
    Float(f64),
    String(String),
    Boolean(bool),
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub struct Type {
    pub kind: TypeKind
}

#[derive(Debug, PartialEq)]
pub enum TypeKind {
    Infer,
    TypePath(Path)
}

#[derive(Debug, PartialEq)]
pub struct Path {
    pub segments: Vec<PathSegment>
}

#[derive(Debug, PartialEq)]
pub struct PathSegment {
    pub ident: Identifier,
}