//! C codegen crate for Rust

#![deny(missing_docs)]

mod conversions;
mod expr;
mod function_builder;
mod module;
mod node;
mod serialize;
mod statement;
pub use expr::*;
pub use function_builder::*;
pub use module::*;
pub use node::*;
use serialize::*;
pub use statement::*;

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn func_add_ints() {
        let mut module = Module::new(
            ModuleKind::Binary,
            ModuleOptions::default().disable_comments(),
        );

        let adder = FunctionBuilder::with_name("add")
            .returns(Type::SignedInt)
            .add_parameter(Type::SignedInt, "a")
            .add_parameter(Type::SignedInt, "b")
            .add_statement({
                let expr = Expr::Add(
                    Box::new(Expr::Ident("a".into())),
                    Box::new(Expr::Ident("b".into())),
                );

                Statement::Return(expr)
            })
            .build()
            .unwrap();

        let call = FunctionCall {
            callee: Box::new(Expr::Ident("add".into())),
            args: vec![
                Box::new(Expr::Literal(Literal::Int8(41))),
                Box::new(Expr::Literal(Literal::Int8(1))),
            ],
        };

        module.add(Statement::from(adder));
        module.add(Statement::from(call));

        assert_eq!(
            module.build(),
            String::from(
                r#"int add(int a, int b) {return a + b;};
add(41,1);
"#
            )
        );
    }
}
