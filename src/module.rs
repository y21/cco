use crate::{Node, Serialize};

const GENERATED_COMMENT: &str = "// This code was auto-generated\n";

/// An Abstract Syntax Tree (AST).
pub type Ast = Vec<Node>;

mod module_flags {
    pub const DISABLE_COMMENTS: u8 = 1 << 1;
}

/// Options for a module
#[derive(Debug, Default)]
pub struct ModuleOptions(u8);

impl ModuleOptions {
    /// Disables the generation of comments for this module.
    pub fn disable_comments(mut self) -> Self {
        self.0 |= module_flags::DISABLE_COMMENTS;
        self
    }

    /// Checks whether comments are disabled for this module
    pub fn has_disabled_comments(&self) -> bool {
        self.has(module_flags::DISABLE_COMMENTS)
    }

    /// Checks whether the underlying bitset contains a flag
    pub fn has(&self, flag: u8) -> bool {
        self.0 & flag == flag
    }
}

/// The type of module
pub enum ModuleKind {
    /// A C library
    Library,
    /// A C binary
    Binary,
}

/// A C module
///
/// This is the main build unit for a C module.
/// Users of this library will want to use this struct to generate C code.
/// It stores the AST that can finally be serialized into C code.
pub struct Module {
    kind: ModuleKind,
    options: ModuleOptions,
    ast: Ast,
}

impl Module {
    /// Creates a new module
    pub fn new(kind: ModuleKind, options: ModuleOptions) -> Self {
        Self {
            kind,
            options,
            ast: Vec::new(),
        }
    }

    /// Adds a new node to the AST
    pub fn add<T>(&mut self, node: T)
    where
        T: Into<Node>,
    {
        self.ast.push(node.into());
    }

    /// Serializes the AST into C code
    pub fn build(self) -> String {
        let mut s = if self.options.has_disabled_comments() {
            String::new()
        } else {
            String::from(GENERATED_COMMENT)
        };

        for node in self.ast {
            s.push_str(&node.serialize());
            s.push('\n');
        }

        s
    }
}
