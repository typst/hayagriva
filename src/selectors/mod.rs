//! Parsing and tokenization.

mod lines;
mod parser;
mod scanner;
mod span;
mod syntax;
mod token;

use crate::error;

pub use lines::*;
pub use parser::*;
pub use scanner::*;
pub use span::*;
pub use syntax::*;
pub use token::*;

/// The result of some pass: Some output `T` and feedback data.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Pass<T> {
    /// The output of this compilation pass.
    pub output: T,
    /// User feedback data accumulated in this pass.
    pub feedback: SpanVec<Diag>,
}

impl<T> Pass<T> {
    /// Create a new pass from output and feedback data.
    pub fn new(output: T, feedback: SpanVec<Diag>) -> Self {
        Self { output, feedback }
    }

    /// Create a new pass with empty feedback.
    pub fn okay(output: T) -> Self {
        Self { output, feedback: vec![] }
    }

    /// Map the output type and keep the feedback data.
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Pass<U> {
        Pass {
            output: f(self.output),
            feedback: self.feedback,
        }
    }
}

/// Parse a string of source code.
pub fn parse(src: &str) -> Pass<Option<Expr>> {
    let mut p = Parser::new(src);
    Pass::new(expr(&mut p).map(|n| n.v), p.finish())
}

/// Parse a parenthesized expression: `(a | b)`.
fn parenthesized(p: &mut Parser) -> Option<Spanned<Expr>> {
    p.start_group(Group::Paren);
    let expr = expr(p);
    p.end_group();
    expr
}

/// Parse a value.
fn attr_value(p: &mut Parser) -> Option<Expr> {
    let start = p.pos();
    Some(match p.eat()? {
        // Dictionary or just a parenthesized expression.
        Token::LeftParen => {
            p.jump(start);
            parenthesized(p)?.v
        }

        // Function or just ident.
        Token::Ident(id) => {
            let ident = Ident(id.into());

            Expr::Lit(Lit::Ident(ident))
        }

        Token::Star => {
            Expr::Lit(Lit::Wildcard)
        }

        // No value.
        _ => {
            p.jump(start);
            return None;
        }
    })
}

/// Parse a value.
fn value(p: &mut Parser) -> Option<Spanned<Expr>> {
    let op = |token| match token {
        Token::ExclamationMark => Some(UnOp::Neg),
        _ => None,
    };

    p.span(|p| {
        if let Some(op) = p.span(|p| p.eat_map(op)).transpose() {
            if let Some(expr) = value(p) {
                Some(Expr::Unary(ExprUnary { op, expr: expr.map(Box::new) }))
            } else {
                p.diag(error!(op.span, "missing factor"));
                None
            }
        } else {
            attr_value(p)
        }
    })
    .transpose()
}

/// Parse a factor of the form `a:?value[attrs..]?`.
fn factor(p: &mut Parser) -> Option<Spanned<Expr>> {
    let start = p.pos();
    let op = |token| match token {
        Token::Ident(i) => {
            Some(TagOp::Bind(i.into()))
        }
        _ => None,
    };

    let inner = p
        .span(|p| {
            if let Some(op) = p.span(|p| p.eat_map(op)).transpose() {
                if p.peek() == Some(Token::Colon) {
                    p.eat();
                    if let Some(expr) = value(p) {
                        Some(Expr::Tag(ExprTag { op, expr: expr.map(Box::new) }))
                    } else {
                        p.diag(error!(op.span, "missing factor"));
                        None
                    }
                } else {
                    p.jump(start);
                    value(p).map(|v| v.v)
                }
            } else {
                value(p).map(|v| v.v)
            }
        })
        .transpose();

    let pos = p.pos();
    let mut fail = false;
    let outer = p
        .span(|p| {
            if let Some(t) = p.span(|p| p.eat()).transpose() {
                if t.v == Token::LeftBracket {
                    let mut attrs: SpanVec<Ident> = vec![];
                    let mut last_ident = false;
                    let mut ok = true;
                    while let Some(st) = p.span(|p| p.eat()).transpose() {
                        if let Token::Ident(i) = st.v {
                            if !last_ident {
                                attrs.push(Spanned::new(Ident::new(i)?, st.span));
                                last_ident = true;
                            } else {
                                p.diag_expected_at(
                                    "attribute name or right brace",
                                    st.span.start,
                                );
                                ok = false;
                                break;
                            }
                        }

                        if Token::Comma == st.v {
                            if last_ident {
                                last_ident = false;
                            } else {
                                p.diag_expected_at("comma or right brace", st.span.start);
                                ok = false;
                                break;
                            }
                        }

                        if Token::RightBracket == st.v {
                            break;
                        }
                    }
                    if ok {
                        Some(attrs)
                    } else {
                        p.jump(pos);
                        fail = true;
                        return None;
                    }
                } else {
                    p.jump(pos);
                    None
                }
            } else {
                p.jump(pos);
                None
            }
        })
        .transpose();

    if let Some(outer) = outer {
        Some(Spanned::new(
            Expr::Tag(ExprTag {
                op: outer.map(|args| TagOp::Attributes(args)),
                expr: inner?.map(Box::new),
            }),
            Span::new(start, p.pos()),
        ))
    } else {
        if fail { None } else { inner }
    }
}

/// Parse a term: `factor (* factor)*`.
fn term(p: &mut Parser) -> Option<Spanned<Expr>> {
    binops(p, "factor", factor, |token| match token {
        Token::Ampersand => Some(BinOp::MultiParent),
        Token::Pipe => Some(BinOp::Alternative),
        _ => None,
    })
}

fn expr(p: &mut Parser) -> Option<Spanned<Expr>> {
    binops(p, "summand", term, |token| match token {
        Token::Chevron => Some(BinOp::Ancestrage),
        _ => None,
    })
}

/// Parse binary operations of the from `a (<op> b)*`.
fn binops(
    p: &mut Parser,
    operand_name: &str,
    operand: fn(&mut Parser) -> Option<Spanned<Expr>>,
    op: fn(Token) -> Option<BinOp>,
) -> Option<Spanned<Expr>> {
    let mut lhs = operand(p)?;

    loop {
        if let Some(op) = p.span(|p| p.eat_map(op)).transpose() {
            if let Some(rhs) = operand(p) {
                let span = lhs.span.join(rhs.span);
                let expr = Expr::Binary(ExprBinary {
                    lhs: lhs.map(Box::new),
                    op,
                    rhs: rhs.map(Box::new),
                });
                lhs = expr.span_with(span);
            } else {
                let span = lhs.span.join(op.span);
                p.diag(error!(span, "missing right {}", operand_name));
                break;
            }
        } else {
            break;
        }
    }

    Some(lhs)
}

#[cfg(test)]
#[allow(non_snake_case)]
mod tests {
    use super::*;
    use std::fmt::Debug;

    /// Assert that expected and found are equal, printing both and panicking
    /// and the source of their test case if they aren't.
    ///
    /// When `cmp_spans` is false, spans are ignored.
    pub fn check<T>(src: &str, exp: T, found: T, cmp_spans: bool)
    where
        T: Debug + PartialEq,
    {
        Span::set_cmp(cmp_spans);
        let equal = exp == found;
        Span::set_cmp(true);

        if !equal {
            println!("source:   {:?}", src);
            if cmp_spans {
                println!("expected: {:#?}", exp);
                println!("found:    {:#?}", found);
            } else {
                println!("expected: {:?}", exp);
                println!("found:    {:?}", found);
            }
            panic!("test failed");
        }
    }
    macro_rules! t {
        ($src:expr => $exp:expr) => {
            let res = parse($src).output.unwrap();
            check($src, $exp, res, false);
        };
    }
    macro_rules! attrs {
        ($src:expr, $($exp:expr),* $(,)?) => {
            {
                let mut array = vec![];
                $(
                    array.push(Spanned::zero(Ident::new($exp).unwrap()));
                )*

                Expr::Tag(ExprTag { op: Spanned::zero(TagOp::Attributes(array)), expr: Spanned::zero(Box::new($src)) })
            }
        };
    }


    fn Id(ident: &str) -> Expr {
        Expr::Lit(Lit::Ident(Ident(ident.into())))
    }
    fn Wc() -> Expr {
        Expr::Lit(Lit::Wildcard)
    }
    fn Bind(binding: &str, expr: Expr) -> Expr {
        Expr::Tag(ExprTag { op: Spanned::zero(TagOp::Bind(binding.into())), expr: Spanned::zero(Box::new(expr)) })
    }
    fn Neg(expr: Expr) -> Expr {
        Expr::Unary(ExprUnary { op: Spanned::zero(UnOp::Neg), expr: Spanned::zero(Box::new(expr)) })
    }
    fn Anc(lhs: Expr, rhs: Expr) -> Expr {
        Expr::Binary(ExprBinary { op: Spanned::zero(BinOp::Ancestrage), lhs: Spanned::zero(Box::new(lhs)), rhs: Spanned::zero(Box::new(rhs))})
    }
    fn Alt(lhs: Expr, rhs: Expr) -> Expr {
        Expr::Binary(ExprBinary { op: Spanned::zero(BinOp::Alternative), lhs: Spanned::zero(Box::new(lhs)), rhs: Spanned::zero(Box::new(rhs))})
    }
    fn Mul(lhs: Expr, rhs: Expr) -> Expr {
        Expr::Binary(ExprBinary { op: Spanned::zero(BinOp::MultiParent), lhs: Spanned::zero(Box::new(lhs)), rhs: Spanned::zero(Box::new(rhs))})
    }

    #[test]
    fn expressions() {
        t!("a:Misc" => Bind("a", Id("Misc")));
        t!("bread:!!Blog" => Bind("bread", Neg(Neg(Id("Blog")))));
        t!("Anthology[title, author]" => attrs!(Id("Anthology"), "title", "author"));
        t!("Article > Proceedings" => Anc(Id("Article"), Id("Proceedings")));
        t!("Artwork | Audio > Exhibiton" => Anc(Alt(Id("Artwork"), Id("Audio")), Id("Exhibiton")));
        t!("Article > (Book & (Repository | Anthology > Blog) & WebItem[url, title])" => Anc(Id("Article"), Mul(Mul(Id("Book"), Anc(Alt(Id("Repository"), Id("Anthology")), Id("Blog"))), attrs!(Id("WebItem"), "url", "title"))));
        t!("a:(Book | Anthology) > b:((Repository > WebItem) | Blog)" => Anc(Bind("a", Alt(Id("Book"), Id("Anthology"))), Bind("b", Alt(Anc(Id("Repository"), Id("WebItem")), Id("Blog")))));
        t!("a:!Audio > ((Blog[author] & WebItem) | (Video > WebItem))" => Anc(Bind("a", Neg(Id("Audio"))), Alt(Mul(attrs!(Id("Blog"), "author"), Id("WebItem")), Anc(Id("Video"), Id("WebItem")))));
        t!("*[title]" => attrs!(Wc(), "title"));
        t!("* > i:*[url]" => Anc(Wc(), Bind("i", attrs!(Wc(), "url"))));
        t!("* > i:!Blog" => Anc(Wc(), Bind("i", Neg(Id("Blog")))));
    }
}
