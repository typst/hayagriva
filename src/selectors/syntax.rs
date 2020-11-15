use super::{is_ident, SpanVec, Spanned};
use std::ops::Deref;

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct Ident(pub String);

impl Ident {
    /// Create a new identifier from a string checking that it is a valid.
    pub fn new(ident: impl AsRef<str> + Into<String>) -> Option<Self> {
        if is_ident(ident.as_ref()) {
            Some(Self(ident.into()))
        } else {
            None
        }
    }

    /// Return a reference to the underlying string.
    pub fn as_str(&self) -> &str {
        self
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        self
    }
}

impl Deref for Ident {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.as_str()
    }
}

/// A syntax node, which encompasses a single logical entity of parsed source
/// code.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// A unary operation: `!x`.
    Unary(ExprUnary),
    /// A tag operation: `x:Misc`.
    Tag(ExprTag),
    /// A binary operation: `a & b`, `a | b`.
    Binary(ExprBinary),
    /// A literal: `true`, `1cm`, `"hi"`, `{_Hey!_}`.
    Lit(Lit),
}

/// A unary operation: `!x`.
#[derive(Debug, Clone, PartialEq)]
pub struct ExprUnary {
    /// The operator: `!`.
    pub op: Spanned<UnOp>,
    /// The expression to operator on: `x`.
    pub expr: Spanned<Box<Expr>>,
}

/// A tag operation: `x:Misc`.
#[derive(Debug, Clone, PartialEq)]
pub struct ExprTag {
    /// The operator: `x:`.
    pub op: Spanned<TagOp>,
    /// The expression to operator on: `x`.
    pub expr: Spanned<Box<Expr>>,
}

/// A unary operator.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum UnOp {
    /// The negation operator: `!`.
    Neg,
}

/// A tag operator.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TagOp {
    /// A binding: `ident:`
    Bind(String),
    /// An attribute list: `Abc[att1, attr2]`
    Attributes(SpanVec<Ident>),
}

/// A binary operation: `a & b`, `a, b`.
#[derive(Debug, Clone, PartialEq)]
pub struct ExprBinary {
    /// The left-hand side of the operation: `a`.
    pub lhs: Spanned<Box<Expr>>,
    /// The operator: `+`.
    pub op: Spanned<BinOp>,
    /// The right-hand side of the operation: `b`.
    pub rhs: Spanned<Box<Expr>>,
}

/// A binary operator.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinOp {
    /// The ancestrage operator: : `>`.
    Ancestrage,
    /// The multi-parent operator: `&`.
    MultiParent,
    /// The alternative operator: `|`.
    Alternative,
}

/// A literal.
#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    /// A identifier literal: `Patent`.
    Ident(Ident),
    /// A wildcard literal: `*`.
    Wildcard,
}
