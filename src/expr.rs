use crate::Type;

/// A C Expression
pub enum Expr {
    /// Add expression: lhs + rhs
    Add(Box<Expr>, Box<Expr>),
    /// Subtraction expression: lhs - rhs
    Sub(Box<Expr>, Box<Expr>),
    /// Multiplication expression: lhs * rhs
    Mul(Box<Expr>, Box<Expr>),
    /// Division expression: lhs / rhs
    Div(Box<Expr>, Box<Expr>),
    /// Modulo expression: lhs % rhs
    Mod(Box<Expr>, Box<Expr>),
    /// Left shift expression: lhs << rhs
    LShift(Box<Expr>, Box<Expr>),
    /// Right shift expression: lhs >> rhs
    RShift(Box<Expr>, Box<Expr>),
    /// Bitwise OR expression: lhs | rhs
    BitOr(Box<Expr>, Box<Expr>),
    /// Bitwise XOR expression: lhs ^ rhs
    BitXor(Box<Expr>, Box<Expr>),
    /// Bitwise AND expression: lhs & rhs
    BitAnd(Box<Expr>, Box<Expr>),
    /// Negative expression: -rhs
    Neg(Box<Expr>),
    /// Not expression: !rhs
    Not(Box<Expr>),
    /// Equality expression: lhs == rhs
    Eq(Box<Expr>, Box<Expr>),
    /// Inequality expression: lhs != rhs
    Ne(Box<Expr>, Box<Expr>),
    /// Less than expression: lhs < rhs
    Lt(Box<Expr>, Box<Expr>),
    /// Less than or equal expression: lhs <= rhs
    Le(Box<Expr>, Box<Expr>),
    /// Greater than expression: lhs > rhs
    Gt(Box<Expr>, Box<Expr>),
    /// Greater than or equal expression: lhs >= rhs
    Ge(Box<Expr>, Box<Expr>),
    /// Logical AND expression: lhs && rhs
    And(Box<Expr>, Box<Expr>),
    /// Logical OR expression: lhs || rhs
    Or(Box<Expr>, Box<Expr>),
    /// Assignment expression: lhs = rhs
    Assign(Box<Expr>, Box<Expr>),
    /// Addition assignment expression: lhs += rhs
    AddAssign(Box<Expr>, Box<Expr>),
    /// Subtraction assignment expression: lhs -= rhs
    SubAssign(Box<Expr>, Box<Expr>),
    /// Multiplication assignment expression: lhs *= rhs
    MulAssign(Box<Expr>, Box<Expr>),
    /// Division assignment expression: lhs /= rhs
    DivAssign(Box<Expr>, Box<Expr>),
    /// Modulo assignment expression: lhs %= rhs
    ModAssign(Box<Expr>, Box<Expr>),
    /// Left shift assignment expression: lhs <<= rhs
    LShiftAssign(Box<Expr>, Box<Expr>),
    /// Right shift assignment expression: lhs >>= rhs
    RShiftAssign(Box<Expr>, Box<Expr>),
    /// Bitwise OR assignment expression: lhs |= rhs
    BitOrAssign(Box<Expr>, Box<Expr>),
    /// Bitwise XOR assignment expression: lhs ^= rhs
    BitXorAssign(Box<Expr>, Box<Expr>),
    /// Bitwise AND assignment expression: lhs &= rhs
    BitAndAssign(Box<Expr>, Box<Expr>),
    /// Identifier expression: `foo` (used to refer to a type or variable)
    Ident(Box<str>),
    /// Function call expression: `foo(p1, p2, ..., pN)`
    Call(FunctionCall),
    /// Cast expression: (T)expr
    Cast(Type, Box<Expr>),
    /// Literal value expression: `42`
    Literal(Literal),
}
// TODO: pointerof expression

/// A literal value
pub enum Literal {
    /// 8-bit integer
    Int8(i8),
    /// Unsigned 8-bit integer
    Uint8(u8),
    /// 16-bit integer
    Int16(i16),
    /// Unsigned 16-bit integer
    Uint16(u16),
    /// 32-bit integer
    Int32(i32),
    /// Unsigned 32-bit integer
    Uint32(u32),
    /// 64-bit integer
    Int64(i64),
    /// Unsigned 64-bit integer
    Uint64(u64),
    /// 32-bit float
    Float(f32),
    /// 64-bit float
    Double(f64),
    /// Boolean
    Bool(bool),
    /// C character (range 0-127)
    Char(i8),
    /// C string
    String(Box<str>),
}

/// A function call expression
pub struct FunctionCall {
    /// The callee (function that is being called)
    pub callee: Box<Expr>,
    /// Function call arguments
    pub args: Vec<Box<Expr>>,
}
