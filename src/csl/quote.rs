use citationberg::{taxonomy::OtherTerm, TermForm};

use super::{taxonomy::EntryLike, Context};

/// A smart quote substitutor with zero lookahead.
/// TODO: Move to its own crate.
#[derive(Debug, Clone)]
pub struct SmartQuoter {
    /// The amount of quotes that have been opened.
    depth: u8,
    /// Each bit indicates whether the quote at this nesting depth is a double.
    /// Maximum supported depth is thus 32.
    kinds: u32,
}

impl SmartQuoter {
    /// Start quoting.
    pub fn new() -> Self {
        Self { depth: 0, kinds: 0 }
    }

    /// Determine which smart quote to substitute given this quoter's nesting
    /// state and the character immediately preceding the quote.
    pub fn quote<'a>(
        &mut self,
        before: Option<char>,
        quotes: &SmartQuotes<'a>,
        double: bool,
    ) -> &'a str {
        let opened = self.top();
        let before = before.unwrap_or(' ');

        // If we are after a number and haven't most recently opened a quote of
        // this kind, produce a prime. Otherwise, we prefer a closing quote.
        if before.is_numeric() && opened != Some(double) {
            return if double { "″" } else { "′" };
        }

        // If we have a single smart quote, didn't recently open a single
        // quotation, and are after an alphabetic char or an object (e.g. a
        // math equation), interpret this as an apostrophe.
        if !double
            && opened != Some(false)
            && (before.is_alphabetic() || before == '\u{FFFC}')
        {
            return "’";
        }

        // If the most recently opened quotation is of this kind and the
        // previous char does not indicate a nested quotation, close it.
        if opened == Some(double)
            && !before.is_whitespace()
            && !is_newline(before)
            && !is_opening_bracket(before)
        {
            self.pop();
            return quotes.close(double);
        }

        // Otherwise, open a new the quotation.
        self.push(double);
        quotes.open(double)
    }

    /// The top of our quotation stack. Returns `Some(double)` for the most
    /// recently opened quote or `None` if we didn't open one.
    fn top(&self) -> Option<bool> {
        self.depth.checked_sub(1).map(|i| (self.kinds >> i) & 1 == 1)
    }

    /// Push onto the quotation stack.
    fn push(&mut self, double: bool) {
        if self.depth < 32 {
            self.kinds |= (double as u32) << self.depth;
            self.depth += 1;
        }
    }

    /// Pop from the quotation stack.
    fn pop(&mut self) {
        self.depth -= 1;
        self.kinds &= (1 << self.depth) - 1;
    }
}

impl Default for SmartQuoter {
    fn default() -> Self {
        Self::new()
    }
}

/// Whether the character is an opening bracket, parenthesis, or brace.
fn is_opening_bracket(c: char) -> bool {
    matches!(c, '(' | '{' | '[')
}

/// Whether a character is interpreted as a newline by Typst.
#[inline]
pub fn is_newline(character: char) -> bool {
    matches!(
        character,
        // Line Feed, Vertical Tab, Form Feed, Carriage Return.
        '\n' | '\x0B' | '\x0C' | '\r' |
        // Next Line, Line Separator, Paragraph Separator.
        '\u{0085}' | '\u{2028}' | '\u{2029}'
    )
}

/// Decides which quotes to substitute smart quotes with.
pub struct SmartQuotes<'s> {
    /// The opening single quote.
    pub single_open: &'s str,
    /// The closing single quote.
    pub single_close: &'s str,
    /// The opening double quote.
    pub double_open: &'s str,
    /// The closing double quote.
    pub double_close: &'s str,
}

impl<'s> SmartQuotes<'s> {
    /// Create a new `Quotes` struct with the given quotes, optionally falling
    /// back to the defaults for a language and region.
    ///
    /// The language should be specified as an all-lowercase ISO 639-1 code, the
    /// region as an all-uppercase ISO 3166-alpha2 code.
    ///
    /// Currently, the supported languages are: English, Czech, Danish, German,
    /// Swiss / Liechtensteinian German, Estonian, Icelandic, Italian, Latin,
    /// Lithuanian, Latvian, Slovak, Slovenian, Spanish, Bosnian, Finnish,
    /// Swedish, French, Swiss French, Hungarian, Polish, Romanian, Japanese,
    /// Traditional Chinese, Russian, Norwegian, Hebrew and Croatian.
    ///
    /// For unknown languages, the English quotes are used as fallback.
    pub fn get<T: EntryLike>(ctx: &'s Context<'s, T>) -> Self {
        let default = ("'", "\"");

        Self {
            single_open: ctx
                .term(OtherTerm::OpenInnerQuote.into(), TermForm::default(), false)
                .unwrap_or(default.0),
            single_close: ctx
                .term(OtherTerm::CloseInnerQuote.into(), TermForm::default(), false)
                .unwrap_or(default.0),
            double_open: ctx
                .term(OtherTerm::OpenQuote.into(), TermForm::default(), false)
                .unwrap_or(default.1),
            double_close: ctx
                .term(OtherTerm::CloseQuote.into(), TermForm::default(), false)
                .unwrap_or(default.1),
        }
    }

    /// The opening quote.
    pub fn open(&self, double: bool) -> &'s str {
        if double {
            self.double_open
        } else {
            self.single_open
        }
    }

    /// The closing quote.
    pub fn close(&self, double: bool) -> &'s str {
        if double {
            self.double_close
        } else {
            self.single_close
        }
    }
}
