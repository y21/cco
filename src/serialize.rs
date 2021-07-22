use std::borrow::Cow;

use crate::{
    DoWhileStatement, Expr, ForStatement, FunctionCall, FunctionDeclaration, IfBranch, IfStatement,
    Literal, Node, ParameterList, Statement, StructDeclaration, SwitchCase, SwitchStatement, Type,
    TypedefStatement, UnionDeclaration, VariableStatement, WhileStatement,
};

type CowString = Cow<'static, str>;

pub trait Serialize {
    fn serialize(&self) -> CowString;
}

impl Serialize for Box<str> {
    fn serialize(&self) -> CowString {
        Cow::Owned(self.to_string())
    }
}

impl Serialize for Type {
    fn serialize(&self) -> CowString {
        match self {
            Type::Void => Cow::Borrowed("void"),
            Type::SignedChar => Cow::Borrowed("char"),
            Type::SignedShort => Cow::Borrowed("short"),
            Type::SignedInt => Cow::Borrowed("int"),
            Type::SignedLong => Cow::Borrowed("long"),
            Type::SignedLongLong => Cow::Borrowed("long long"),
            Type::SignedInt128 => Cow::Borrowed("__int128"),
            Type::Bool => Cow::Borrowed("_Bool"),
            Type::UnsignedChar => Cow::Borrowed("unsigned char"),
            Type::UnsignedShort => Cow::Borrowed("unsigned short"),
            Type::UnsignedInt => Cow::Borrowed("unsigned int"),
            Type::UnsignedLong => Cow::Borrowed("unsigned long"),
            Type::UnsignedLongLong => Cow::Borrowed("unsigned long long"),
            Type::UnsignedInt128 => Cow::Borrowed("__uint128"),
            Type::Float => Cow::Borrowed("float"),
            Type::Double => Cow::Borrowed("double"),
            Type::LongDouble => Cow::Borrowed("long double"),
            Type::FloatComplex => Cow::Borrowed("float _Complex"),
            Type::DoubleComplex => Cow::Borrowed("double _Complex"),
            Type::LongDoubleComplex => Cow::Borrowed("long double _Complex"),
            Type::FloatImaginary => Cow::Borrowed("float _Imaginary"),
            Type::DoubleImaginary => Cow::Borrowed("double _Imaginary"),
            Type::LongDoubleImaginary => Cow::Borrowed("long double _Imaginary"),
            Type::Array(ty, len) => Cow::Owned(format!(
                "{}[{}]",
                ty.serialize(),
                len.map(|x| x.to_string()).unwrap_or_else(String::new)
            )),
            Type::Pointer(ty) => Cow::Owned(format!("{}*", ty.serialize())),
            Type::Identifier(ty) => ty.serialize(),
            Type::Atomic(ty) => Cow::Owned(format!("_Atomic({})", ty.serialize())),
        }
    }
}

impl Serialize for Statement {
    fn serialize(&self) -> CowString {
        let mut e = match self {
            Statement::If(i) => i.serialize(),
            Statement::Switch(s) => s.serialize(),
            Statement::Block(b) => {
                let mut s = String::from("{\n");
                s.push_str(&b.serialize());
                s.push_str("\n}");
                Cow::Owned(s)
            }
            Statement::VariableDeclaration(v) => v.serialize(),
            Statement::FunctionDeclaration(f) => f.serialize(),
            Statement::Expression(e) => e.serialize(),
            Statement::Return(r) => Cow::Owned(format!("return {}", r.serialize())),
            Statement::For(f) => f.serialize(),
            Statement::While(w) => w.serialize(),
            Statement::DoWhile(d) => d.serialize(),
            Statement::Break => Cow::Borrowed("break"),
            Statement::Continue => Cow::Borrowed("continue"),
            Statement::Struct(s) => s.serialize(),
            Statement::Union(u) => u.serialize(),
            Statement::Typedef(t) => t.serialize(),
        }
        .to_string();

        e.push(';');
        Cow::Owned(e)
    }
}

impl Serialize for TypedefStatement {
    fn serialize(&self) -> CowString {
        Cow::Owned(format!(
            "typedef {} {}",
            self.target.serialize(),
            self.new.serialize()
        ))
    }
}

fn serialize_fields(fields: &[(Type, Box<str>)]) -> String {
    let mut s = String::new();
    for (index, (ty, ident)) in fields.iter().enumerate() {
        if index != 0 {
            s.push_str(",\n");
        }
        s.push_str(&ty.serialize());
        s.push(' ');
        s.push_str(&ident.serialize());
    }
    s
}

impl Serialize for StructDeclaration {
    fn serialize(&self) -> CowString {
        Cow::Owned(format!("struct {{\n{}\n}}", serialize_fields(&self.0)))
    }
}

impl Serialize for UnionDeclaration {
    fn serialize(&self) -> CowString {
        Cow::Owned(format!("union {{\n{}\n}}", serialize_fields(&self.0)))
    }
}

impl Serialize for WhileStatement {
    fn serialize(&self) -> CowString {
        let mut s = format!("while ({}) ", self.condition.serialize());
        if let Some(body) = &self.body {
            s.push_str(&body.serialize());
        }
        Cow::Owned(s)
    }
}

impl Serialize for DoWhileStatement {
    fn serialize(&self) -> CowString {
        Cow::Owned(format!(
            "do {} while ({});",
            self.body.serialize(),
            self.condition.serialize()
        ))
    }
}

impl Serialize for ForStatement {
    fn serialize(&self) -> CowString {
        let mut s = String::from("for (");

        if let Some(init) = &self.init {
            s.push_str(&init.serialize());
        }

        s.push_str("; ");

        if let Some(cond) = &self.cond {
            s.push_str(&cond.serialize());
        }

        s.push_str("; ");

        if let Some(finalizer) = &self.finalizer {
            s.push_str(&finalizer.serialize());
        }

        s.push_str(") ");

        if let Some(body) = &self.body {
            s.push_str(&body.serialize());
        } else {
            s.push(';');
        }

        Cow::Owned(s)
    }
}

impl Serialize for Vec<Statement> {
    fn serialize(&self) -> CowString {
        Cow::Owned(
            self.iter()
                .map(Serialize::serialize)
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join("\n"),
        )
    }
}

impl Serialize for Expr {
    #[rustfmt::skip]
    fn serialize(&self) -> CowString {
        match self {
            Expr::Add(lhs, rhs) => Cow::Owned(format!("{} + {}", lhs.serialize(), rhs.serialize())),
            Expr::Sub(lhs, rhs) => Cow::Owned(format!("{} - {}", lhs.serialize(), rhs.serialize())),
            Expr::Mul(lhs, rhs) => Cow::Owned(format!("{} * {}", lhs.serialize(), rhs.serialize())),
            Expr::Div(lhs, rhs) => Cow::Owned(format!("{} / {}", lhs.serialize(), rhs.serialize())),
            Expr::Mod(lhs, rhs) => Cow::Owned(format!("{} % {}", lhs.serialize(), rhs.serialize())),
            Expr::LShift(lhs, rhs) => Cow::Owned(format!("{} << {}", lhs.serialize(), rhs.serialize())),
            Expr::RShift(lhs, rhs) => Cow::Owned(format!("{} >> {}", lhs.serialize(), rhs.serialize())),
            Expr::BitOr(lhs, rhs) => Cow::Owned(format!("{} | {}", lhs.serialize(), rhs.serialize())),
            Expr::BitXor(lhs, rhs) => Cow::Owned(format!("{} ^ {}", lhs.serialize(), rhs.serialize())),
            Expr::BitAnd(lhs, rhs) => Cow::Owned(format!("{} & {}", lhs.serialize(), rhs.serialize())),
            Expr::Neg(rhs) => Cow::Owned(format!("-{}", rhs.serialize())),
            Expr::Not(rhs) => Cow::Owned(format!("!{}", rhs.serialize())),
            Expr::Cast(t, rhs) => Cow::Owned(format!("({}){}", t.serialize(), rhs.serialize())),
            Expr::Eq(lhs, rhs) => Cow::Owned(format!("{} == {}", lhs.serialize(), rhs.serialize())),
            Expr::Ne(lhs, rhs) => Cow::Owned(format!("{} != {}", lhs.serialize(), rhs.serialize())),
            Expr::Lt(lhs, rhs) => Cow::Owned(format!("{} < {}", lhs.serialize(), rhs.serialize())),
            Expr::Gt(lhs, rhs) => Cow::Owned(format!("{} > {}", lhs.serialize(), rhs.serialize())),
            Expr::Le(lhs, rhs) => Cow::Owned(format!("{} <= {}", lhs.serialize(), rhs.serialize())),
            Expr::Ge(lhs, rhs) => Cow::Owned(format!("{} >= {}", lhs.serialize(), rhs.serialize())),
            Expr::And(lhs, rhs) => Cow::Owned(format!("{} && {}", lhs.serialize(), rhs.serialize())),
            Expr::Or(lhs, rhs) => Cow::Owned(format!("{} || {}", lhs.serialize(), rhs.serialize())),
            Expr::Assign(lhs, rhs) => Cow::Owned(format!("{} = {}", lhs.serialize(), rhs.serialize())),
            Expr::AddAssign(lhs, rhs) => Cow::Owned(format!("{} += {}", lhs.serialize(), rhs.serialize())),
            Expr::SubAssign(lhs, rhs) => Cow::Owned(format!("{} -= {}", lhs.serialize(), rhs.serialize())),
            Expr::MulAssign(lhs, rhs) => Cow::Owned(format!("{} *= {}", lhs.serialize(), rhs.serialize())),
            Expr::DivAssign(lhs, rhs) => Cow::Owned(format!("{} /= {}", lhs.serialize(), rhs.serialize())),
            Expr::ModAssign(lhs, rhs) => Cow::Owned(format!("{} %= {}", lhs.serialize(), rhs.serialize())),
            Expr::LShiftAssign(lhs, rhs) => Cow::Owned(format!("{} <<= {}", lhs.serialize(), rhs.serialize())),
            Expr::RShiftAssign(lhs, rhs) => Cow::Owned(format!("{} >>= {}", lhs.serialize(), rhs.serialize())),
            Expr::BitOrAssign(lhs, rhs) => Cow::Owned(format!("{} |= {}", lhs.serialize(), rhs.serialize())),
            Expr::BitXorAssign(lhs, rhs) => Cow::Owned(format!("{} ^= {}", lhs.serialize(), rhs.serialize())),
            Expr::BitAndAssign(lhs, rhs) => Cow::Owned(format!("{} &= {}", lhs.serialize(), rhs.serialize())),
            Expr::Ident(ident) => ident.serialize(),
            Expr::Call(c) => c.serialize(),
            Expr::Literal(lit) => lit.serialize(),
            Expr::Pos(p) => Cow::Owned(format!("+{}", p.serialize())),
            Expr::BitNot(b) => Cow::Owned(format!("~{}", b.serialize())),
            Expr::PrefixIncrement(p) => Cow::Owned(format!("++{}", p.serialize())),
            Expr::PrefixDecrement(p) => Cow::Owned(format!("--{}", p.serialize())),
            Expr::PostfixIncrement(p) => Cow::Owned(format!("{}++", p.serialize())),
            Expr::PostfixDecrement(p) => Cow::Owned(format!("{}--", p.serialize())),
            Expr::Index(p, i) => Cow::Owned(format!("{}[{}]", p.serialize(), i.serialize())),
            Expr::PropertyAccess(p, m) => Cow::Owned(format!("{}.{}", p.serialize(), m.serialize())),
            Expr::PointerDeref(p) => Cow::Owned(format!("*{}", p.serialize())),
            Expr::AddressOf(a) => Cow::Owned(format!("&{}", a.serialize())),
            Expr::DerefPropertyAccess(p, m) => Cow::Owned(format!("{}->{}", p.serialize(), m.serialize())),
            Expr::Comma(lhs, rhs) => Cow::Owned(format!("{}, {}", lhs.serialize(), rhs.serialize())),
            Expr::Conditional(c)
                => Cow::Owned(format!("{} ? {} : {}", c.condition.serialize(), c.then.serialize(), c.else_.serialize())),
            Expr::Sizeof(ty) => Cow::Owned(format!("sizeof({})", ty.serialize())),
            Expr::Alignof(ty) => Cow::Owned(format!("_Alignof({})", ty.serialize())),
        }
    }
}

impl Serialize for Literal {
    fn serialize(&self) -> CowString {
        match self {
            Literal::Int8(i) => Cow::Owned(i.to_string()),
            Literal::Int16(i) => Cow::Owned(i.to_string()),
            Literal::Int32(i) => Cow::Owned(i.to_string()),
            Literal::Int64(i) => Cow::Owned(i.to_string()),
            Literal::Uint8(i) => Cow::Owned(i.to_string()),
            Literal::Uint16(i) => Cow::Owned(i.to_string()),
            Literal::Uint32(i) => Cow::Owned(i.to_string()),
            Literal::Uint64(i) => Cow::Owned(i.to_string()),
            Literal::Float(f) => Cow::Owned(f.to_string()),
            Literal::Double(f) => Cow::Owned(f.to_string()),
            Literal::Char(c) => Cow::Owned(c.to_string()),
            Literal::String(s) => Cow::Owned(format!("{:?}", s)),
            Literal::Bool(b) => Cow::Owned(b.to_string()),
        }
    }
}

impl Serialize for FunctionCall {
    fn serialize(&self) -> CowString {
        let mut s = format!("{}(", self.callee.serialize());

        for (index, arg) in self.args.iter().enumerate() {
            if index != 0 {
                s.push(',');
            }

            s.push_str(&arg.serialize());
        }

        s.push(')');

        Cow::Owned(s)
    }
}

impl Serialize for IfStatement {
    fn serialize(&self) -> CowString {
        let mut s = String::from("if (");

        // Condition
        s.push_str(&self.condition.serialize());
        s.push_str(") ");

        // Then
        s.push_str(&self.then.serialize());
        s.push('\n');

        // Else
        for (index, branch) in self.else_.iter().enumerate() {
            if index != 0 {
                s.push('\n');
            }
            s.push_str(&branch.serialize());
        }

        s.push('}');

        Cow::Owned(s)
    }
}

impl Serialize for IfBranch {
    fn serialize(&self) -> CowString {
        let e: &dyn Serialize = match self {
            IfBranch::ElseIf(s) => s,
            IfBranch::Else(s) => s,
        };

        Cow::Owned(format!("else {}", e.serialize()))
    }
}

impl Serialize for SwitchStatement {
    fn serialize(&self) -> CowString {
        let mut s = String::from("switch (");

        // Condition
        s.push_str(&self.condition.serialize());
        s.push_str(") {\n");

        // Cases
        for (index, case) in self.cases.iter().enumerate() {
            if index != 0 {
                s.push('\n');
            }
            s.push_str(&case.serialize());
        }

        // Default
        if let Some(default) = &self.default {
            s.push('\n');
            s.push_str(&default.serialize());
        }

        s.push('}');

        Cow::Owned(s)
    }
}

impl Serialize for SwitchCase {
    fn serialize(&self) -> CowString {
        let cond = self.condition.serialize();
        let body = self.body.serialize();
        Cow::Owned(format!("case {}: {{ {} }}", cond, body))
    }
}

impl Serialize for VariableStatement {
    fn serialize(&self) -> CowString {
        let mut s = String::from(self.ty.serialize());

        s.push(' ');

        s.push_str(&self.ident.serialize());

        if let Some(value) = &self.value {
            s.push('=');
            s.push_str(&value.serialize());
        }

        s.push(';');

        Cow::Owned(s)
    }
}

impl Serialize for FunctionDeclaration {
    fn serialize(&self) -> CowString {
        let mut s = String::from(self.ret.serialize());

        s.push(' ');

        s.push_str(&self.ident.serialize());

        s.push('(');

        s.push_str(&self.args.serialize());

        s.push_str(") {\n");

        s.push_str(&self.body.serialize());

        s.push_str("\n}");

        Cow::Owned(s)
    }
}

impl Serialize for ParameterList {
    fn serialize(&self) -> CowString {
        Cow::Owned(
            self.iter()
                .map(|(ident, ty)| format!("{} {}", ty.serialize(), ident.serialize()))
                .collect::<Vec<String>>()
                .join(", "),
        )
    }
}

impl Serialize for Node {
    fn serialize(&self) -> CowString {
        match self {
            Node::SinglelineComment(txt) => Cow::Owned(format!("// {}", txt)),
            Node::MultilineComment(txt) => Cow::Owned(format!("/* {} */", txt)),
            Node::Statement(stmt) => stmt.serialize(),
        }
    }
}
