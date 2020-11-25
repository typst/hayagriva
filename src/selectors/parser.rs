use std::fmt::{self, Debug, Formatter};

use super::token::Tokens;
use super::{Pos, SpanWith, Spanned, Token};

/// Construct a new error.
#[macro_export]
macro_rules! error {
    ($fmt:literal $($tts:tt)*) => {
        $crate::selectors::parser::Diag::new(format!($fmt $($tts)*))
    };

    ($span:expr, $fmt:literal $($tts:tt)*) => {
        $crate::selectors::Spanned::new(
            $crate::error!($fmt $($tts)*),
            $span,
        )
    };
}

/// A convenient token-based parser.
pub struct Parser<'s> {
    tokens: Tokens<'s>,
    peeked: Option<Token<'s>>,
    groups: Vec<Group>,
    pos: Pos,
}

impl<'s> Parser<'s> {
    /// Create a new parser for the source string.
    pub fn new(src: &'s str) -> Self {
        Self {
            tokens: Tokens::new(src),
            peeked: None,
            groups: vec![],
            pos: Pos::ZERO,
        }
    }

    /// Continues parsing in a group.
    ///
    /// When the end delimiter of the group is reached, all subsequent calls to
    /// `eat()` and `peek()` return `None`. Parsing can only continue with
    /// a matching call to `end_group`.
    ///
    /// # Panics
    /// This panics if the next token does not start the given group.
    pub fn start_group(&mut self, group: Group) {
        match group {
            Group::Paren => self.eat_assert(Token::LeftParen),
            Group::Bracket => self.eat_assert(Token::LeftBracket),
        }
        self.groups.push(group);
    }

    /// Ends the parsing of a group and returns the span of the whole group.
    ///
    /// # Panics
    /// This panics if no group was started.
    pub fn end_group(&mut self) {
        // Check that we are indeed at the end of the group.
        debug_assert_eq!(self.peek(), None, "unfinished group");

        let group = self.groups.pop().expect("no started group");
        let end = match group {
            Group::Paren => Some(Token::RightParen),
            Group::Bracket => Some(Token::RightBracket),
        };

        if let Some(token) = end {
            // This `peek()` can't be used directly because it hides the end of
            // group token. To circumvent this, we drop down to `self.peeked`.
            self.peek();
            if self.peeked == Some(token) {
                self.bump();
            }
        }
    }

    /// Execute `f` and return the result alongside the span of everything `f`
    /// ate.
    pub fn span<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> Spanned<T> {
        let start = self.pos;
        f(self).span_with(start .. self.pos)
    }

    /// Consume the next token.
    pub fn eat(&mut self) -> Option<Token<'s>> {
        self.peek()?;
        self.bump()
    }

    /// Consume the next token if the closure maps it a to `Some`-variant.
    pub fn eat_map<T>(&mut self, f: impl FnOnce(Token<'s>) -> Option<T>) -> Option<T> {
        let token = self.peek()?;
        let out = f(token);
        if out.is_some() {
            self.bump();
        }
        out
    }

    /// Consume the next token, debug-asserting that it is the given one.
    pub fn eat_assert(&mut self, t: Token) {
        let next = self.eat();
        debug_assert_eq!(next, Some(t));
    }

    /// Peek at the next token without consuming it.
    pub fn peek(&mut self) -> Option<Token<'s>> {
        let token = match self.peeked {
            Some(token) => token,
            None => {
                let token = self.tokens.next()?;
                self.peeked = Some(token);
                token
            }
        };

        let group = match token {
            Token::RightParen => Group::Paren,
            Token::RightBracket => Group::Bracket,
            _ => return Some(token),
        };

        if self.groups.contains(&group) {
            None
        } else {
            Some(token)
        }
    }

    /// The position in the string at which the last token ends and next token
    /// will start.
    pub fn pos(&self) -> Pos {
        self.pos
    }

    /// Jump to a position in the source string.
    pub fn jump(&mut self, pos: Pos) {
        self.tokens.jump(pos);
        self.bump();
    }

    /// The full source string up to the current index.
    pub fn eaten(&self) -> &'s str {
        self.tokens.scanner().get(.. self.pos.to_usize())
    }

    /// The remaining source string after the current index.
    pub fn rest(&self) -> &'s str {
        self.tokens.scanner().get(self.pos.to_usize() ..)
    }

    /// Set the position to the tokenizer's position and take the peeked token.
    fn bump(&mut self) -> Option<Token<'s>> {
        self.pos = self.tokens.pos();
        let token = self.peeked;
        self.peeked = None;
        token
    }
}

impl Debug for Parser<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Parser({}|{})", self.eaten(), self.rest())
    }
}

/// A group, confined by optional start and end delimiters.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Group {
    /// A parenthesized group: `(...)`.
    Paren,
    /// A bracketed group: `[...]`.
    Bracket,
}
