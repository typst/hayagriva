//! Token definition.

use std::fmt::{self, Debug, Formatter};

#[cfg(test)]
use super::Span;
use super::{Pos, Scanner};

use unicode_xid::UnicodeXID;

/// An iterator over the tokens of a string of source code.
#[derive(Clone)]
pub struct Tokens<'s> {
    s: Scanner<'s>,
}

/// A minimal semantic entity of source code.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Token<'s> {
    /// A left bracket starting an attribute list: `[`.
    LeftBracket,
    /// A right bracket ending an attribute list: `]`.
    RightBracket,
    /// A left parenthesis in a option list: `(`.
    LeftParen,
    /// A right parenthesis in a option list: `)`.
    RightParen,

    /// A star replacing a type: `*`.
    Star,
    /// A colon in a binding: `:`.
    Colon,
    /// A comma in an attribute list: `,`.
    Comma,
    /// A pipe in a option list: `|`.
    Pipe,
    /// An ampersand in a option list: `&`.
    Ampersand,
    /// A chevron in a ancestrage chain: `>`.
    Chevron,
    /// A exclamation mark in a negation: `!`.
    ExclamationMark,

    /// An identifier in a function header: `Periodical`.
    Ident(&'s str),

    /// Things that are not valid in the context they appeared in.
    Invalid(&'s str),
}

impl<'s> Token<'s> {
    /// The natural-language name of this token for use in error messages.
    pub fn name(self) -> &'static str {
        match self {
            Self::LeftBracket => "opening bracket",
            Self::RightBracket => "closing bracket",
            Self::LeftParen => "opening paren",
            Self::RightParen => "closing paren",
            Self::Star => "star",
            Self::Colon => "colon",
            Self::Comma => "comma",
            Self::Pipe => "pipe",
            Self::Ampersand => "ampersand",
            Self::Chevron => "chevron",
            Self::ExclamationMark => "negation",
            Self::Ident(_) => "identifier",
            Self::Invalid(_) => "invalid token",
        }
    }
}

impl<'s> Tokens<'s> {
    /// Create a new token iterator with the given mode.
    pub fn new(src: &'s str) -> Self {
        Self { s: Scanner::new(src) }
    }

    /// The position in the string at which the last token ends and next token
    /// will start.
    pub fn pos(&self) -> Pos {
        self.s.index().into()
    }

    /// Jump to a position in the source string.
    pub fn jump(&mut self, pos: Pos) {
        self.s.jump(pos.to_usize());
    }

    /// The underlying scanner.
    pub fn scanner(&self) -> &Scanner<'s> {
        &self.s
    }
}

impl<'s> Iterator for Tokens<'s> {
    type Item = Token<'s>;

    /// Parse the next token in the source code.
    fn next(&mut self) -> Option<Self::Item> {
        let (start, c) = loop {
            let start = self.s.index();
            let c = self.s.eat()?;

            if !c.is_whitespace() {
                break (start, c);
            }
        };

        Some(match c {
            // Backets.
            '[' => Token::LeftBracket,
            ']' => Token::RightBracket,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,

            // List seperators
            ',' => Token::Comma,
            '&' => Token::Ampersand,
            '|' => Token::Pipe,

            // Misc
            '*' => Token::Star,
            ':' => Token::Colon,
            '>' => Token::Chevron,
            '!' => Token::ExclamationMark,

            // Expressions or just plain text.
            _ => self.read_text_or_expr(start),
        })
    }
}

impl<'s> Tokens<'s> {
    fn read_text_or_expr(&mut self, start: usize) -> Token<'s> {
        self.s.eat_until(|c| {
            let end = match c {
                c if c.is_whitespace() => true,
                '[' | ']' | '*' | '(' | ')' | '!' | '&' | '|' | ':' | '>' | ',' => true,
                _ => false,
            };
            end
        });

        let read = self.s.eaten_from(start);
        parse_expr(read)
    }
}

impl Debug for Tokens<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Tokens({}|{})", self.s.eaten(), self.s.rest())
    }
}

pub fn is_ident(text: &str) -> bool {
    let mut chars = text.chars();
    if matches!(chars.next(), Some(c) if c.is_xid_start() || c == '_' || c == '-') {
        chars.all(|c| c.is_xid_continue() || c == '_' || c == '-')
    } else {
        false
    }
}

fn parse_expr(text: &str) -> Token<'_> {
    if is_ident(text) {
        Token::Ident(text)
    } else {
        Token::Invalid(text)
    }
}

/// Assert that expected and found are equal, printing both and panicking
/// and the source of their test case if they aren't.
///
/// When `cmp_spans` is false, spans are ignored.
#[cfg(test)]
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

#[cfg(test)]
#[allow(non_snake_case)]
mod tests {
    use super::*;

    use Token::{
        Ampersand as AMP, Chevron, Colon as CL, Comma as CM, ExclamationMark as EX,
        Ident as Id, LeftBracket as LB, LeftParen as L, Pipe, RightBracket as RB,
        RightParen as R, Star,
    };

    macro_rules! t {
        ($src:expr => $($token:expr),*) => {
            let exp = vec![$($token),*];
            let found = Tokens::new($src).collect::<Vec<_>>();
            check($src, exp, found, false);
        }
    }

    #[test]
    fn tokenize() {
        t!("Article > Book"            => Id("Article"), Chevron, Id("Book"));
        t!("alpha:!* > (a & b)"        => Id("alpha"), CL, EX, Star, Chevron, L, Id("a"), AMP, Id("b"), R);
        t!("g5:(Blog | Misc)"          => Id("g5"), CL, L, Id("Blog"), Pipe, Id("Misc"), R);
        t!("Anthology[editor,date]"    => Id("Anthology"), LB, Id("editor"), CM, Id("date"), RB);
    }
}
