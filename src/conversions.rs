use crate::{
    Expr, FunctionCall, FunctionDeclaration, IfStatement, Statement, SwitchStatement,
    VariableStatement,
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
