//! Low-level char-based scanner.

use std::fmt::{self, Debug, Formatter};
use std::slice::SliceIndex;
use std::str::Chars;

/// A low-level featureful char-based scanner.
#[derive(Clone)]
pub struct Scanner<'s> {
    src: &'s str,
    iter: Chars<'s>,
    index: usize,
}

impl<'s> Scanner<'s> {
    /// Create a new char scanner.
    pub fn new(src: &'s str) -> Self {
        Self { src, iter: src.chars(), index: 0 }
    }

    /// Consume the next char.
    pub fn eat(&mut self) -> Option<char> {
        let next = self.iter.next();
        if let Some(c) = next {
            self.index += c.len_utf8();
        }
        next
    }

    /// Eat chars until the condition is true.
    pub fn eat_until(&mut self, mut f: impl FnMut(char) -> bool) -> &'s str {
        let start = self.index;
        while let Some(c) = self.iter.next() {
            if f(c) {
                // Undo the previous `next()` without peeking all the time
                // during iteration.
                self.reset();
                break;
            }
            self.index += c.len_utf8();
        }
        &self.src[start .. self.index]
    }

    /// The current index in the source string.
    pub fn index(&self) -> usize {
        self.index
    }

    /// Jump to an index in the source string.
    pub fn jump(&mut self, index: usize) {
        self.index = index;
        self.reset();
    }

    /// Slice a part out of the source string.
    pub fn get<I>(&self, index: I) -> &'s str
    where
        I: SliceIndex<str, Output = str>,
    {
        &self.src[index]
    }

    /// The full source string up to the current index.
    pub fn eaten(&self) -> &'s str {
        &self.src[.. self.index]
    }

    /// The source string from `start` to the current index.
    pub fn eaten_from(&self, start: usize) -> &'s str {
        &self.src[start .. self.index]
    }

    /// The remaining source string after the current index.
    pub fn rest(&self) -> &'s str {
        &self.src[self.index ..]
    }

    /// Go back to the where the index says.
    fn reset(&mut self) {
        self.iter = self.src[self.index ..].chars();
    }
}

impl Debug for Scanner<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Scanner({}|{})", self.eaten(), self.rest())
    }
}
