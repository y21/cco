use crate::{FunctionDeclaration, ParameterList, Statement, Type};

/// A function builder.
///
/// This struct is used to build a function declaration.
#[derive(Default)]
pub struct FunctionBuilder {
    ident: Option<Box<str>>,
    ret: Option<Type>,
    args: ParameterList,
    body: Vec<Statement>,
}

impl FunctionBuilder {
    /// Create a function builder.
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a function builder given the function name
    pub fn with_name<S>(name: S) -> Self
    where
        S: Into<Box<str>>,
    {
        Self {
            ident: Some(name.into()),
            ..Default::default()
        }
    }

    /// Sets the return type of the function.
    pub fn returns(mut self, ty: Type) -> Self {
        self.ret = Some(ty);
        self
    }

    /// Sets the argument list of the function.
    pub fn arguments(mut self, args: ParameterList) -> Self {
        self.args = args;
        self
    }

    /// Declares a parameter on the function.
    pub fn add_parameter<S>(mut self, ty: Type, ident: S) -> Self
    where
        S: Into<Box<str>>,
    {
        self.args.push((ty, ident.into()));
        self
    }

    /// Sets the function body
    pub fn body(mut self, body: Vec<Statement>) -> Self {
        self.body = body;
        self
    }

    /// Adds a single statement to the function body
    pub fn add_statement(mut self, stmt: Statement) -> Self {
        self.body.push(stmt);
        self
    }

    /// Builds the function declaration.
    ///
    /// This function consumes self and returns a function declaration if all fields are set.
    pub fn build(self) -> Option<FunctionDeclaration> {
        Some(FunctionDeclaration {
            ident: self.ident?,
            ret: self.ret?,
            args: self.args,
            body: self.body,
        })
    }
}
