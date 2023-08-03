use std::str::FromStr;

use super::{Selector, SelectorError, SelectorResult};
use crate::types::EntryType;

/// Parse a selector.
pub fn parse(src: &str) -> SelectorResult<Selector> {
    let mut p = Parser::new(src);
    Ok(expr(&mut p)?)
}

/// Parse an expression, with optional ancestrage relation.
fn expr(p: &mut Parser) -> SelectorResult<Selector> {
    let mut lhs = term(p)?;
    while p.eat_if(Token::Chevron) {
        lhs = Selector::Ancestrage(Box::new(lhs), Box::new(term(p)?));
    }
    Ok(lhs)
}

/// Parse a term, consisting of alternatives or multi-parents.
fn term(p: &mut Parser) -> SelectorResult<Selector> {
    let mut lhs = binding(p)?;

    loop {
        if p.eat_if(Token::Pipe) {
            let mut alternatives = vec![lhs];
            loop {
                alternatives.push(binding(p)?);
                if !p.eat_if(Token::Pipe) {
                    break;
                }
            }
            lhs = Selector::Alt(alternatives);
        } else if p.eat_if(Token::Ampersand) {
            let mut parents = vec![lhs];
            loop {
                parents.push(binding(p)?);
                if !p.eat_if(Token::Ampersand) {
                    break;
                }
            }
            lhs = Selector::Multi(parents);
        } else {
            break;
        }
    }

    Ok(lhs)
}

/// Parse an expression with an optional binding `a:expr`.
fn binding(p: &mut Parser) -> SelectorResult<Selector> {
    let start = p.index();
    if let Some(id) = ident(p) {
        if p.eat_if(Token::Colon) {
            return Ok(Selector::Binding(id, Box::new(attributes(p)?)));
        } else {
            p.jump(start);
        }
    }
    attributes(p)
}

/// Parse a factor with optional attributes: `factor[attr, ...]`.
fn attributes(p: &mut Parser) -> SelectorResult<Selector> {
    let inner = factor(p)?;
    if p.eat_if(Token::LeftBracket) {
        let mut attrs: Vec<String> = vec![];
        loop {
            match p.eat() {
                Some(Token::RightBracket) => break,
                Some(Token::Ident(id)) => attrs.push(id.into()),
                _ => return Err(SelectorError::MalformedAttribute),
            }

            match p.eat() {
                Some(Token::RightBracket) => break,
                Some(Token::Comma) => continue,
                _ => return Err(SelectorError::MissingComma),
            }
        }
        Ok(Selector::Attr(Box::new(inner), attrs))
    } else {
        Ok(inner)
    }
}

/// Parse a value with optional negation: `!value`.
fn factor(p: &mut Parser) -> SelectorResult<Selector> {
    if p.eat_if(Token::ExclamationMark) {
        Ok(Selector::Neg(Box::new(factor(p)?)))
    } else {
        value(p)
    }
}

/// Parse a parenthesized or atomic value: `book`, `(expr)`.
fn value(p: &mut Parser) -> SelectorResult<Selector> {
    match p.eat() {
        Some(Token::LeftParen) => {
            let expr = expr(p)?;
            if !p.eat_if(Token::RightParen) {
                return Err(SelectorError::UnbalancedParens);
            }
            Ok(expr)
        }
        Some(Token::Star) => Ok(Selector::Wildcard),
        Some(Token::Ident(id)) => {
            let lower = id.to_lowercase();
            if let Ok(kind) = EntryType::from_str(&lower) {
                Ok(Selector::Entry(kind))
            } else {
                Err(SelectorError::UnknownEntryType(lower))
            }
        }
        _ => Err(SelectorError::MissingValue),
    }
}

/// Parse an identifier.
fn ident(p: &mut Parser) -> Option<String> {
    p.eat_map(|token| match token {
        Token::Ident(id) => Some(id.into()),
        _ => None,
    })
}

/// A token-based parser.
#[derive(Debug)]
struct Parser<'s> {
    tokens: Tokens<'s>,
}

impl<'s> Parser<'s> {
    /// Create a new parser for the source string.
    fn new(src: &'s str) -> Self {
        Self { tokens: Tokens::new(src) }
    }

    /// Consume the next token.
    fn eat(&mut self) -> Option<Token<'s>> {
        self.tokens.next()
    }

    /// Consume the next token if it is the given one.
    fn eat_if(&mut self, t: Token) -> bool {
        let matches = self.peek() == Some(t);
        if matches {
            self.eat();
        }
        matches
    }

    /// Consume the next token if the closure maps it a to `Some`-variant.
    fn eat_map<T>(&mut self, f: impl FnOnce(Token<'s>) -> Option<T>) -> Option<T> {
        let mapped = f(self.peek()?);
        if mapped.is_some() {
            self.eat();
        }
        mapped
    }

    /// Peek at the next token without consuming it.
    fn peek(&mut self) -> Option<Token<'s>> {
        self.tokens.clone().next()
    }

    /// The position in the string at which the last token ends and next token
    /// will start.
    fn index(&self) -> usize {
        self.tokens.index
    }

    /// Jump to a position in the source string.
    fn jump(&mut self, index: usize) {
        self.tokens.index = index;
    }
}

/// An iterator over the tokens of a string of source code.
#[derive(Debug, Clone)]
struct Tokens<'s> {
    src: &'s str,
    index: usize,
}

/// A minimal semantic entity of source code.
#[derive(Debug, Copy, Clone, PartialEq)]
enum Token<'s> {
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
    Invalid,
}

impl<'s> Tokens<'s> {
    /// Create a new token iterator with the given mode.
    fn new(src: &'s str) -> Self {
        Self { src, index: 0 }
    }

    /// Eat the next char.
    fn eat(&mut self) -> Option<char> {
        let next = self.src[self.index..].chars().next()?;
        self.index += next.len_utf8();
        Some(next)
    }
}

impl<'s> Iterator for Tokens<'s> {
    type Item = Token<'s>;

    /// Parse the next token in the source code.
    fn next(&mut self) -> Option<Self::Item> {
        let mut start = self.index;
        let mut c = self.eat()?;

        while c.is_whitespace() {
            start = self.index;
            c = self.eat()?;
        }

        Some(match c {
            // Brackets.
            '[' => Token::LeftBracket,
            ']' => Token::RightBracket,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,

            // List separators.
            ',' => Token::Comma,
            '&' => Token::Ampersand,
            '|' => Token::Pipe,

            // Misc.
            '*' => Token::Star,
            ':' => Token::Colon,
            '>' => Token::Chevron,
            '!' => Token::ExclamationMark,

            // Identifiers.
            c if is_id_start(c) => {
                let mut end = self.index;
                while let Some(next) = self.eat() {
                    if !is_id_continue(next) {
                        self.index = end;
                        break;
                    }
                    end = self.index;
                }
                Token::Ident(&self.src[start..end])
            }

            _ => Token::Invalid,
        })
    }
}

fn is_id_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '-'
}

fn is_id_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '-'
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use super::super::Selector;
    use super::*;

    use Token::{
        Ampersand as A, ExclamationMark, Ident as Id, LeftBracket as LB, LeftParen as L,
        Pipe, RightBracket as RB, RightParen as R, *,
    };

    fn check<T>(src: &str, exp: T, found: T)
    where
        T: Debug + PartialEq,
    {
        if exp != found {
            println!("source:   {:#?}", src);
            println!("expected: {:#?}", exp);
            println!("found:    {:#?}", found);
            panic!("test failed");
        }
    }

    #[test]
    fn test_tokenize() {
        macro_rules! t {
            ($src:expr => $($token:expr),*) => {
                let exp = vec![$($token),*];
                let found = Tokens::new($src).collect::<Vec<_>>();
                check($src, exp, found);
            }
        }

        t!("Article > Book"         => Id("Article"), Chevron, Id("Book"));
        t!("g5:(Blog | Misc)"       => Id("g5"), Colon, L, Id("Blog"), Pipe, Id("Misc"), R);
        t!("anthology[editor,date]" => Id("anthology"), LB, Id("editor"), Comma, Id("date"), RB);
        t!("alpha:!* > (a & b)"     => Id("alpha"), Colon, ExclamationMark, Star, Chevron,
                                       LeftParen, Id("a"), A, Id("b"), R);
    }

    #[test]
    fn test_parse() {
        macro_rules! t {
            ($src:expr => $exp:expr) => {
                check($src, $exp, Selector::parse($src).unwrap());
            };
        }

        t!("*[title]"                     => select!(*["title"]));
        t!("* > i:*[url]"                 => select!(* > ("i":(*["url"]))));
        t!("* > i:!Blog"                  => select!(* > ("i":(!Blog))));
        t!("a:Misc"                       => select!("a":Misc));
        t!("bread:!!blog"                 => select!("bread":(!(!Blog))));
        t!("anthology[title, author]"     => select!(Anthology["title", "author"]));
        t!("article > proceedings"        => select!(Article > Proceedings));
        t!("artwork | audio > exhibition" => select!((Artwork | Audio) > Exhibition));

        t!("article > (book & (repository | anthology > blog) & web[url, title])"
            => select!(Article > (Book & ((Repository | Anthology) > Blog) & (Web["url", "title"]))));

        t!("a:(book | anthology) > b:((repository > web) | blog)"
            => select!(("a":(Book | Anthology)) > ("b":((Repository > Web) | Blog))));

        t!("a:!audio > ((blog[author] & web) | (video > web))"
            => select!(("a":(!Audio)) > (((Blog["author"]) & Web) | (Video > Web))));
    }

    #[test]
    fn test_parse_errors() {
        assert_eq!(parse("()"), Err(SelectorError::MissingValue));
        assert_eq!(parse("book[*]"), Err(SelectorError::MalformedAttribute));
        assert_eq!(parse("book[date url]"), Err(SelectorError::MissingComma));
        assert_eq!(parse("(book | blog"), Err(SelectorError::UnbalancedParens));
        assert_eq!(parse("a"), Err(SelectorError::UnknownEntryType("a".into())));
    }
}
