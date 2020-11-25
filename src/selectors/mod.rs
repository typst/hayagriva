//! Parsing and tokenization.

mod exec;
mod parser;
mod scanner;
mod span;
mod syntax;
mod token;

use crate::error;
use crate::types::EntryType;
use std::str::FromStr;

use thiserror::Error;

use parser::*;
use scanner::*;
pub use span::Spanned;
use span::*;
pub use syntax::*;
use token::*;

/// This error occurs if the parsing of a selector expression failed.
#[derive(Clone, Debug, Error)]
pub enum SelectorError {
    /// The selector expression matched for an unknown Entry type.
    #[error("Unknown Entry type")]
    UnknownEntryType,
    /// The selector expression was malformed and, thus, could not be parsed.
    #[error("The selector was malformed")]
    MalformedSelector,
}

/// Parse a string of source code.
pub fn parse(src: &str) -> Result<Expr, SelectorError> {
    let mut p = Parser::new(src);
    expr(&mut p).map(|n| n.v)
}

/// Parse a parenthesized expression: `(a | b)`.
fn parenthesized(p: &mut Parser) -> Result<Spanned<Expr>, SelectorError> {
    p.start_group(Group::Paren);
    let expr = expr(p);
    p.end_group();
    expr
}

/// Parse a value.
fn attr_value(p: &mut Parser) -> Result<Expr, SelectorError> {
    let start = p.pos();
    Ok(
        match p.eat().ok_or_else(|| SelectorError::MalformedSelector)? {
            // Dictionary or just a parenthesized expression.
            Token::LeftParen => {
                p.jump(start);
                parenthesized(p)?.v
            }

            // Function or just ident.
            Token::Ident(id) => {
                let ident = Ident(id.into());

                let kind = EntryType::from_str(&ident.to_lowercase());
                if let Ok(kind) = kind {
                    Expr::Lit(Lit::Ident(kind))
                } else {
                    return Err(SelectorError::UnknownEntryType);
                }
            }

            Token::Star => Expr::Lit(Lit::Wildcard),

            // No value.
            _ => {
                p.jump(start);
                return Err(SelectorError::MalformedSelector);
            }
        },
    )
}

/// Parse a value.
fn value(p: &mut Parser) -> Result<Spanned<Expr>, SelectorError> {
    let op = |token| match token {
        Token::ExclamationMark => Some(UnOp::Neg),
        _ => None,
    };

    p.span(|p| {
        if let Some(op) = p.span(|p| p.eat_map(op)).transpose() {
            let expr = value(p)?;
            Ok(Expr::Unary(ExprUnary { op, expr: expr.map(Box::new) }))
        } else {
            attr_value(p)
        }
    })
    .transpose()
}

fn factor_attr(p: &mut Parser) -> Result<Spanned<Expr>, SelectorError> {
    let start = p.pos();
    let inner = value(p)?;
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
                                ok = false;
                                break;
                            }
                        }

                        if Token::Comma == st.v {
                            if last_ident {
                                last_ident = false;
                            } else {
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
        Ok(Spanned::new(
            Expr::Tag(ExprTag {
                op: outer.map(|args| TagOp::Attributes(args)),
                expr: inner.map(Box::new),
            }),
            Span::new(start, p.pos()),
        ))
    } else {
        if fail {
            Err(SelectorError::MalformedSelector)
        } else {
            Ok(inner)
        }
    }
}

/// Parse a factor of the form `a:?value[attrs..]?`.
fn factor(p: &mut Parser) -> Result<Spanned<Expr>, SelectorError> {
    let start = p.pos();
    let op = |token| match token {
        Token::Ident(i) => Some(TagOp::Bind(i.into())),
        _ => None,
    };

    p.span(|p| {
        if let Some(op) = p.span(|p| p.eat_map(op)).transpose() {
            if p.peek() == Some(Token::Colon) {
                p.eat();
                if let Ok(expr) = factor_attr(p) {
                    Ok(Expr::Tag(ExprTag { op, expr: expr.map(Box::new) }))
                } else {
                    Err(SelectorError::MalformedSelector)
                }
            } else {
                p.jump(start);
                factor_attr(p).map(|v| v.v)
            }
        } else {
            factor_attr(p).map(|v| v.v)
        }
    })
    .transpose()
}

/// Parse a term: `factor (* factor)*`.
fn term(p: &mut Parser) -> Result<Spanned<Expr>, SelectorError> {
    binops(p, factor, |token| match token {
        Token::Ampersand => Some(BinOp::MultiParent),
        Token::Pipe => Some(BinOp::Alternative),
        _ => None,
    })
}

fn expr(p: &mut Parser) -> Result<Spanned<Expr>, SelectorError> {
    binops(p, term, |token| match token {
        Token::Chevron => Some(BinOp::Ancestrage),
        _ => None,
    })
}

/// Parse binary operations of the from `a (<op> b)*`.
fn binops(
    p: &mut Parser,
    operand: fn(&mut Parser) -> Result<Spanned<Expr>, SelectorError>,
    op: fn(Token) -> Option<BinOp>,
) -> Result<Spanned<Expr>, SelectorError> {
    let mut lhs = operand(p)?;

    loop {
        if let Some(op) = p.span(|p| p.eat_map(op)).transpose() {
            if let Ok(rhs) = operand(p) {
                let span = lhs.span.join(rhs.span);
                let expr = Expr::Binary(ExprBinary {
                    lhs: lhs.map(Box::new),
                    op,
                    rhs: rhs.map(Box::new),
                });
                lhs = expr.span_with(span);
            } else {
                // No right-hand-side operator
                Err(SelectorError::MalformedSelector)?;
                break;
            }
        } else {
            break;
        }
    }

    Ok(lhs)
}

/// This macro allows to specify attribute requirements for an expression.
#[macro_export]
macro_rules! attrs {
    ($src:expr, $($exp:expr),* $(,)?) => {
        {
            use crate::selectors::{ExprTag, TagOp, Ident};
            let mut array = vec![];
            $(
                array.push(Spanned::zero(Ident::new($exp).unwrap()));
            )*

            Expr::Tag(ExprTag { op: Spanned::zero(TagOp::Attributes(array)), expr: Spanned::zero(Box::new($src)) })
        }
    };
}

/// This macro allows to create [binary expressions](crate::selectors::BinOp)
/// (MultiParent `sel!(mul Id(Video), Id(Web))`, Alternative
/// `sel!(alt Id(Reference), Id(Book))`, and Ancestrage `sel!(Id(Anthos) => Id(Anthology)))`.
#[macro_export]
macro_rules! sel {
    ($variant:expr, $($item:expr),+ $(,)?) => {
        {
            use crate::selectors::{ExprBinary, Expr, BinOp, Spanned};
            let mut exprs = vec![ $($item ,)+  ];
            if exprs.len() == 1 {
                exprs.pop().unwrap()
            } else {
                let mut root = Expr::Binary(ExprBinary {
                    op: Spanned::zero($variant),
                    lhs: Spanned::zero(Box::new(exprs.remove(0))),
                    rhs: Spanned::zero(Box::new(exprs.remove(0))),
                });

                let mut pointer = &mut root;

                for e in exprs {
                    if let Expr::Binary(bex) = pointer {
                        bex.rhs = Spanned::zero(Box::new(Expr::Binary(ExprBinary {
                            op: Spanned::zero($variant),
                            lhs: bex.rhs.clone(),
                            rhs: Spanned::zero(Box::new(e)),
                        })));

                        pointer = &mut bex.rhs.v;
                    } else {
                        panic!("Not the expected binary expression")
                    }
                }

                root
            }
        }
    };

    (alt $($item:expr),+ $(,)?) => {
        sel!(BinOp::Alternative, $($item ,)+)
    };

    (mul $($item:expr),+ $(,)?) => {
        sel!(BinOp::MultiParent, $($item ,)+)
    };

    ($lhs:expr => $rhs:expr) => {
        {
            use crate::selectors::{ExprBinary, Expr, BinOp, Spanned};
            Expr::Binary(ExprBinary {
                op: Spanned::zero(BinOp::Ancestrage),
                lhs: Spanned::zero(Box::new($lhs)),
                rhs: Spanned::zero(Box::new($rhs)),
            })
        }
    };
}

/// Creates an identifier expression.
#[allow(non_snake_case)]
pub fn Id(ident: EntryType) -> Expr {
    Expr::Lit(Lit::Ident(ident))
}

/// Creates a wildcard expression.
#[allow(non_snake_case)]
pub fn Wc() -> Expr {
    Expr::Lit(Lit::Wildcard)
}

/// Binds an expression to a variable name.
#[allow(non_snake_case)]
pub fn Bind(binding: &str, expr: Expr) -> Expr {
    Expr::Tag(ExprTag {
        op: Spanned::zero(TagOp::Bind(binding.into())),
        expr: Spanned::zero(Box::new(expr)),
    })
}

/// Negates an expression.
#[allow(non_snake_case)]
pub fn Neg(expr: Expr) -> Expr {
    Expr::Unary(ExprUnary {
        op: Spanned::zero(UnOp::Neg),
        expr: Spanned::zero(Box::new(expr)),
    })
}

#[cfg(test)]
#[allow(non_snake_case)]
mod tests {
    use super::*;
    use crate::types::EntryType::*;
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
            let res = parse($src).unwrap();
            check($src, $exp, res, false);
        };
    }

    fn Anc(lhs: Expr, rhs: Expr) -> Expr {
        Expr::Binary(ExprBinary {
            op: Spanned::zero(BinOp::Ancestrage),
            lhs: Spanned::zero(Box::new(lhs)),
            rhs: Spanned::zero(Box::new(rhs)),
        })
    }

    fn Alt(lhs: Expr, rhs: Expr) -> Expr {
        Expr::Binary(ExprBinary {
            op: Spanned::zero(BinOp::Alternative),
            lhs: Spanned::zero(Box::new(lhs)),
            rhs: Spanned::zero(Box::new(rhs)),
        })
    }

    fn Mul(lhs: Expr, rhs: Expr) -> Expr {
        Expr::Binary(ExprBinary {
            op: Spanned::zero(BinOp::MultiParent),
            lhs: Spanned::zero(Box::new(lhs)),
            rhs: Spanned::zero(Box::new(rhs)),
        })
    }

    #[test]
    fn expressions() {
        t!("a:Misc" => Bind("a", Id(Misc)));
        t!("bread:!!Blog" => Bind("bread", Neg(Neg(Id(Blog)))));
        t!("Anthology[title, author]" => attrs!(Id(Anthology), "title", "author"));
        t!("Article > Proceedings" => Anc(Id(Article), Id(Proceedings)));
        t!("Artwork | Audio > Exhibition" => sel!(sel!(alt Id(Artwork), Id(Audio)) => Id(Exhibition)));
        t!("Article > (Book & (Repository | Anthology > Blog) & Web[url, title])" => Anc(Id(Article), Mul(Mul(Id(Book), Anc(Alt(Id(Repository), Id(Anthology)), Id(Blog))), attrs!(Id(Web), "url", "title"))));
        t!("a:(Book | Anthology) > b:((Repository > Web) | Blog)" => Anc(Bind("a", Alt(Id(Book), Id(Anthology))), Bind("b", Alt(Anc(Id(Repository), Id(Web)), Id(Blog)))));
        t!("a:!Audio > ((Blog[author] & Web) | (Video > Web))" => Anc(Bind("a", Neg(Id(Audio))), Alt(Mul(attrs!(Id(Blog), "author"), Id(Web)), Anc(Id(Video), Id(Web)))));
        t!("*[title]" => attrs!(Wc(), "title"));
        t!("* > i:*[url]" => Anc(Wc(), Bind("i", attrs!(Wc(), "url"))));
        t!("* > i:!Blog" => Anc(Wc(), Bind("i", Neg(Id(Blog)))));
    }
}
