use std::collections::btree_set::Union;

use crate::{
    DoWhileStatement, Expr, ForStatement, FunctionCall, FunctionDeclaration, IfStatement,
    Statement, StructDeclaration, SwitchStatement, TypedefStatement, UnionDeclaration,
    VariableStatement, WhileStatement,
};

impl From<IfStatement> for Statement {
    fn from(i: IfStatement) -> Self {
        Self::If(i)
    }
}

impl From<SwitchStatement> for Statement {
    fn from(s: SwitchStatement) -> Self {
        Self::Switch(s)
    }
}

impl From<Vec<Statement>> for Statement {
    fn from(s: Vec<Statement>) -> Self {
        Self::Block(s)
    }
}

impl From<VariableStatement> for Statement {
    fn from(s: VariableStatement) -> Self {
        Self::VariableDeclaration(s)
    }
}

impl From<FunctionDeclaration> for Statement {
    fn from(f: FunctionDeclaration) -> Self {
        Self::FunctionDeclaration(f)
    }
}

impl From<Expr> for Statement {
    fn from(e: Expr) -> Self {
        Self::Expression(e)
    }
}

impl From<FunctionCall> for Statement {
    fn from(f: FunctionCall) -> Self {
        Expr::Call(f).into()
    }
}

impl From<ForStatement> for Statement {
    fn from(f: ForStatement) -> Self {
        Self::For(f)
    }
}

impl From<WhileStatement> for Statement {
    fn from(w: WhileStatement) -> Self {
        Self::While(w)
    }
}

impl From<DoWhileStatement> for Statement {
    fn from(d: DoWhileStatement) -> Self {
        Self::DoWhile(d)
    }
}

impl From<StructDeclaration> for Statement {
    fn from(s: StructDeclaration) -> Self {
        Self::Struct(s)
    }
}

impl From<UnionDeclaration> for Statement {
    fn from(u: UnionDeclaration) -> Self {
        Self::Union(u)
    }
}

impl From<TypedefStatement> for Statement {
    fn from(t: TypedefStatement) -> Self {
        Self::Typedef(t)
    }
}
