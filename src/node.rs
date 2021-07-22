use crate::{Expr, Statement};

/// An AST node.
pub enum Node {
    /// A single line comment
    SinglelineComment(Box<str>),
    /// A multi-line comment
    MultilineComment(Box<str>),
    /// A statement
    Statement(Statement),
}

impl From<Statement> for Node {
    fn from(value: Statement) -> Self {
        Self::Statement(value)
    }
}

impl From<Expr> for Node {
    fn from(value: Expr) -> Self {
        Self::Statement(Statement::Expression(value))
    }
}
