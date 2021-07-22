# cco

C codegen crate.

## Features
- Build up ASTs easily
- Generate C code
- Safe and idiomatic API

You may find this useful if you need to transform an arbitrary AST into a C AST and generate code given the AST, particularly useful for compilers that want to use C as IR and compile C code to machine code.

## Examples
- Generating code for a function that adds two integers
```rs
let mut module = Module::new(ModuleKind::Binary, ModuleOptions::default());

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

module.add(Statement::from(adder));

println!("{}", module.build());
```

- Generate a function call to the adder function from above, equivalent to `add(41, 1)`
```rs
let call = FunctionCall {
    callee: Box::new(Expr::Ident("add".into())),
    args: vec![
        Box::new(Expr::Literal(Literal::Int8(41))),
        Box::new(Expr::Literal(Literal::Int8(1))),
    ],
};

module.add_statement(call);
```