#[derive(Debug)]
pub struct Program {
    pub items: Vec<Item>
}

#[derive(Debug)]
pub struct Item {
    pub kind: ItemKind
}

#[derive(Debug)]
pub enum ItemKind {
    Statement(Statement),
}

#[derive(Debug)]
pub struct Statement {
    pub kind: StatementKind
}

#[derive(Debug)]
pub enum StatementKind {
    Let(VariableDeclaration),
    Expr(Expression)
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub identifier: Identifier,
    pub variable_type: Type,
    pub kind: VariableDeclarationKind,
    pub is_mutable: bool
}

#[derive(Debug)]
pub enum VariableDeclarationKind {
    Declaration,
    Init(Expression),
}

#[derive(Debug)]
pub struct Expression {
    pub kind: ExpressionKind
}

#[derive(Debug)]
pub enum ExpressionKind {
    Literal(Literal),
    Function(Function),
    PropertyAccess(Vec<Identifier>),
    Call(Box<Expression>, Vec<Expression>)
}

#[derive(Debug)]
pub struct Function {
    pub args: Vec<FunctionArg>,
    pub body: Block
}

#[derive(Debug)]
pub struct FunctionArg {
    pub identifier: Identifier,
    pub arg_type: Type,
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<Statement>
}

#[derive(Debug)]
pub struct Literal {
    pub kind: LiteralKind
}

#[derive(Debug)]
pub enum LiteralKind {
    Int(i64),
    Float(f64),
    String(String),
    Boolean(bool),
}

#[derive(Debug)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug)]
pub struct Type {
    pub kind: TypeKind
}

#[derive(Debug)]
pub enum TypeKind {
    Infer,
    TypePath(Path)
}

#[derive(Debug)]
pub struct Path {
    pub segments: Vec<PathSegment>
}

#[derive(Debug)]
pub struct PathSegment {
    pub ident: Identifier,
}