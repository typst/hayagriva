use std::fmt::{self, Debug, Display, Formatter};

use super::token::Tokens;
use super::Scanner;
use super::{Pos, Span, SpanVec, SpanWith, Spanned, Token};

/// Construct a diagnostic with [`Error`] level.
#[macro_export]
macro_rules! error {
    ($($tts:tt)*) => {
        $crate::__impl_diagnostic!($crate::selectors::parser::Level::Error; $($tts)*)
    };
}

/// Construct a diagnostic with [`Warning`] level.
///
/// This works exactly like `error!`. See its documentation for more
/// information.
#[macro_export]
macro_rules! warning {
    ($($tts:tt)*) => {
        $crate::__impl_diagnostic!($crate::selectors::parser::Level::Warning; $($tts)*)
    };
}

/// Backs the `error!` and `warning!` macros.
#[macro_export]
#[doc(hidden)]
macro_rules! __impl_diagnostic {
    ($level:expr; $fmt:literal $($tts:tt)*) => {
        $crate::selectors::parser::Diag::new($level, format!($fmt $($tts)*))
    };

    ($level:expr; $span:expr, $fmt:literal $($tts:tt)*) => {
        $crate::selectors::Spanned::new(
            $crate::__impl_diagnostic!($level; $fmt $($tts)*),
            $span,
        )
    };
}

/// How severe / important a diagnostic is.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "camelCase"))]
pub enum Level {
    Warning,
    Error,
}

/// A diagnostic that arose in parsing.
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct Diag {
    /// How severe / important the diagnostic is.
    pub level: Level,
    /// A message describing the diagnostic.
    pub message: String,
}

impl Diag {
    /// Create a new diagnostic from message and level.
    pub fn new(level: Level, message: impl Into<String>) -> Self {
        Self { level, message: message.into() }
    }
}

impl Display for Level {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.pad(match self {
            Self::Warning => "warning",
            Self::Error => "error",
        })
    }
}

/// A convenient token-based parser.
pub struct Parser<'s> {
    tokens: Tokens<'s>,
    peeked: Option<Token<'s>>,
    groups: Vec<Group>,
    f: SpanVec<Diag>,
    pos: Pos,
}

impl<'s> Parser<'s> {
    /// Create a new parser for the source string.
    pub fn new(src: &'s str) -> Self {
        Self {
            tokens: Tokens::new(src),
            peeked: None,
            groups: vec![],
            f: vec![],
            pos: Pos::ZERO,
        }
    }

    /// Finish parsing and return the accumulated feedback.
    pub fn finish(self) -> SpanVec<Diag> {
        self.f
    }

    /// Add a diagnostic to the feedback.
    pub fn diag(&mut self, diag: Spanned<Diag>) {
        self.f.push(diag);
    }

    /// Eat the next token and add a diagnostic that it was not the expected
    /// `thing`.
    pub fn diag_expected(&mut self, thing: &str) {
        let before = self.pos();
        if let Some(found) = self.eat() {
            let after = self.pos();
            self.diag(error!(
                before .. after,
                "expected {}, found {}",
                thing,
                found.name(),
            ));
        } else {
            self.diag_expected_at(thing, self.pos());
        }
    }

    /// Add a diagnostic that the `thing` was expected at the given position.
    pub fn diag_expected_at(&mut self, thing: &str, pos: Pos) {
        self.diag(error!(pos, "expected {}", thing));
    }

    /// Add a diagnostic that the given `token` was unexpected.
    pub fn diag_unexpected(&mut self, token: Spanned<Token>) {
        self.diag(error!(token.span, "unexpected {}", token.v.name()));
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
            } else {
                self.diag(error!(self.pos(), "expected {}", token.name()));
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

    /// Consume the next token if it is the given one.
    pub fn eat_if(&mut self, t: Token) -> bool {
        if self.peek() == Some(t) {
            self.bump();
            true
        } else {
            false
        }
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

    /// Consume tokens while the condition is true.
    ///
    /// Returns how many tokens were eaten.
    pub fn eat_while(&mut self, mut f: impl FnMut(Token<'s>) -> bool) -> usize {
        self.eat_until(|t| !f(t))
    }

    /// Consume tokens until the condition is true.
    ///
    /// Returns how many tokens were eaten.
    pub fn eat_until(&mut self, mut f: impl FnMut(Token<'s>) -> bool) -> usize {
        let mut count = 0;
        while let Some(t) = self.peek() {
            if f(t) {
                break;
            }
            self.bump();
            count += 1;
        }
        count
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

    /// Checks whether the next token fulfills a condition.
    ///
    /// Returns `false` if there is no next token.
    pub fn check(&mut self, f: impl FnOnce(Token<'s>) -> bool) -> bool {
        self.peek().map_or(false, f)
    }

    /// Whether the end of the source string or group is reached.
    pub fn eof(&mut self) -> bool {
        self.peek().is_none()
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

    /// Slice a part out of the source string.
    pub fn get(&self, span: impl Into<Span>) -> &'s str {
        self.tokens.scanner().get(span.into().to_range())
    }

    /// The full source string up to the current index.
    pub fn eaten(&self) -> &'s str {
        self.tokens.scanner().get(.. self.pos.to_usize())
    }

    /// The source string from `start` to the current index.
    pub fn eaten_from(&self, start: Pos) -> &'s str {
        self.tokens.scanner().get(start.to_usize() .. self.pos.to_usize())
    }

    /// The remaining source string after the current index.
    pub fn rest(&self) -> &'s str {
        self.tokens.scanner().get(self.pos.to_usize() ..)
    }

    /// The underlying scanner.
    pub fn scanner(&self) -> Scanner<'s> {
        let mut scanner = self.tokens.scanner().clone();
        scanner.jump(self.pos.to_usize());
        scanner
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
