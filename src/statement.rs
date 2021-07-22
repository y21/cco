use crate::Expr;

/// An owned identifier
pub type OwnedIdentifier = Box<str>;
/// A list of parameters
pub type ParameterList = Vec<(Type, OwnedIdentifier)>;

/// A C statement
pub enum Statement {
    /// An if statement
    If(IfStatement),
    /// A switch statement
    Switch(SwitchStatement),
    /// A block statement
    Block(Vec<Statement>),
    /// A variable declaration statement
    VariableDeclaration(VariableStatement),
    /// A function declaration statement
    FunctionDeclaration(FunctionDeclaration),
    /// An expression statement
    Expression(Expr),
    /// A return statement
    Return(Expr),
    /// A for statement
    For(ForStatement),
    /// A while statement
    While(WhileStatement),
    /// A do-while statement
    DoWhile(DoWhileStatement),
    /// A continue statement
    Continue,
    /// A break statement
    Break,
    /// A struct declaration statement
    Struct(StructDeclaration),
    /// An union declaration statement
    Union(UnionDeclaration),
    /// A typedef statement
    Typedef(TypedefStatement),
}

/// A typedef statement
pub struct TypedefStatement {
    /// Target type
    pub target: Box<Statement>,
    /// New type
    pub new: OwnedIdentifier,
}

/// An union declaration
pub struct UnionDeclaration(pub ParameterList);

/// A struct declaration
pub struct StructDeclaration(pub ParameterList);

/// A do-while loop
///
/// This is nearly the same as a while loop, with the difference that the condition
/// is checked after the body is executed.
/// Meaning that it is always evaluated at least once.
pub struct DoWhileStatement {
    /// The body of the loop
    pub body: Box<Statement>,
    /// The condition of the loop
    pub condition: Expr,
}

/// A C while loop
pub struct WhileStatement {
    /// The condition of a while loop
    pub condition: Expr,
    /// The body of a while loop, if present
    pub body: Option<Box<Statement>>,
}

/// A C for statement
///
/// All of the fields are optional.
/// In the case of an empty body, a single semicolon is generated.
pub struct ForStatement {
    /// The initial clause
    ///
    /// It is evaluated once, usually used to initialize a variable
    pub init: Option<Box<Statement>>,
    /// The condition clause
    ///
    /// It is evaluated at each iteration, usually used to check a condition
    /// When the expression is false, the loop is exited
    pub cond: Option<Expr>,
    /// The post clause
    ///
    /// It is evaluated at the end of each iteration, usually used to update a variable
    pub finalizer: Option<Expr>,
    /// The body of the loop
    pub body: Option<Box<Statement>>,
}

/// Builtin C types
pub enum Type {
    /// void type
    Void,
    /// char type
    SignedChar,
    /// short type
    SignedShort,
    /// int type
    SignedInt,
    /// long type
    SignedLong,
    /// long long type
    SignedLongLong,
    /// __int128 (implementation-defined) type
    SignedInt128,
    /// _Bool type
    Bool,
    /// unsigned char type
    UnsignedChar,
    /// unsigned short type
    UnsignedShort,
    /// unsigned int type
    UnsignedInt,
    /// unsigned long type
    UnsignedLong,
    /// unsigned long long type
    UnsignedLongLong,
    /// __uint128 (implementation-defined) type
    UnsignedInt128,
    /// float type
    Float,
    /// double type
    Double,
    /// long double type
    LongDouble,
    /// _Complex float type
    FloatComplex,
    /// _Complex double type
    DoubleComplex,
    /// _Complex long double type
    LongDoubleComplex,
    /// _Imaginary float type
    FloatImaginary,
    /// _Imaginary double type
    DoubleImaginary,
    /// _Imaginary long double type
    LongDoubleImaginary,
    /// Array type (`T[n]`)
    Array(Box<Type>, Option<usize>),
    /// Atomic type
    Atomic(Box<Type>),
    /// Pointer type (`T*`)
    Pointer(Box<Type>),
    /// Identifier (user-defined / non-builtin) type
    Identifier(Box<str>),
}

/// A variable statement
pub struct VariableStatement {
    /// The identifier of a variable
    pub ident: OwnedIdentifier,
    /// The type of a variable
    pub ty: Type,
    /// The value of a variable, if present
    pub value: Option<Expr>,
}

/// A function declaration
pub struct FunctionDeclaration {
    /// The name of a function
    pub ident: OwnedIdentifier,
    /// The return type of a function
    pub ret: Type,
    /// The parameters of a function
    pub args: ParameterList,
    /// The body of a function
    pub body: Vec<Statement>,
}

/// An else if branch of an if statement
pub enum IfBranch {
    /// Else if statement
    ElseIf(IfStatement),
    /// Final else statement
    Else(Statement),
}

/// An if statement
pub struct IfStatement {
    /// The condition of an if statement
    pub condition: Expr,
    /// The code that is executed if the condition is true
    pub then: Box<Statement>,
    /// The else conditions of an if statement
    pub else_: Vec<IfBranch>,
}

/// A switch statement
pub struct SwitchStatement {
    /// The main condition of a switch statement
    pub condition: Expr,
    /// The cases of a switch statement
    pub cases: Vec<SwitchCase>,
    /// The default case of a switch statement
    pub default: Option<Box<Statement>>,
}

/// A single case of a switch statement
pub struct SwitchCase {
    /// The condition of a case
    pub condition: Expr,
    /// The body of this case
    pub body: Vec<Statement>,
}
