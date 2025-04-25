use citationberg::taxonomy::OtherTerm;
use citationberg::TermForm;

use super::taxonomy::EntryLike;
use super::Context;

pub fn apply_quotes(s: &str, quotes: &SmartQuotes, inner: bool) -> String {
    let mut res = String::with_capacity(s.len());
    let mut before = None;
    let mut quoter = SmartQuoter::new();
    let mut escape = false;
    for c in s.chars() {
        match c {
            '"' | '\'' if escape => {
                res.push(c);
                escape = false
            }
            '"' => res.push_str(quoter.quote(before, quotes, !inner)),
            '\'' => res.push_str(quoter.quote(before, quotes, inner)),
            '\\' if escape => {
                res.push('\\');
                escape = false
            }
            '\\' => escape = true,
            c => res.push(c),
        }
        before = Some(c);
    }
    res
}

/// A smart quote substitutor with zero lookahead.
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
#[inline]
fn is_opening_bracket(c: char) -> bool {
    matches!(c, '(' | '{' | '[')
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
    /// Create a new `Quotes` struct with quotes taken from the current CSL locale's
    /// terms, falling back to `"` and `'` when not available.
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

#[cfg(test)]
mod tests {
    use super::*;

    const US_MARKS: SmartQuotes = SmartQuotes {
        single_open: "‘",
        single_close: "’",
        double_open: "“",
        double_close: "”",
    };
    const DE_MARKS: SmartQuotes = SmartQuotes {
        single_open: "‚",
        single_close: "‘",
        double_open: "„",
        double_close: "“",
    };

    #[test]
    fn typst_tests() {
        let cases = vec![
            ("“The horse eats no cucumber salad” was the first sentence ever uttered on the ‘telephone.’", r#""The horse eats no cucumber salad" was the first sentence ever uttered on the 'telephone.'"#, &US_MARKS),
            ("„Das Pferd frisst keinen Gurkensalat“ war der erste jemals am ‚Fernsprecher‘ gesagte Satz.", r#""Das Pferd frisst keinen Gurkensalat" war der erste jemals am 'Fernsprecher' gesagte Satz."#, &DE_MARKS),
            ("“”", r#""""#, &US_MARKS),
            ("The 5′11″ ‘quick’ brown fox jumps over the “lazy” dog’s ear.", r#"The 5'11" 'quick' brown fox jumps over the "lazy" dog's ear."#, &US_MARKS),
            ("He said “I’m a big fella.”", r#"He said "I'm a big fella.""#, &US_MARKS),
            (r#"The 5'11" ‘quick' brown fox jumps over the "lazy’ dog's ear."#, r#"The 5\'11\" 'quick\' brown fox jumps over the \"lazy' dog\'s ear."#, &US_MARKS),
            ("“Hello”/“World”", r#""Hello"/"World""#, &US_MARKS),
            ("‘“Hello”/“World”’", r#"'"Hello"/"World"'"#, &US_MARKS),
            ("“”Hello“/”World“”", r#"""Hello"/"World"""#, &US_MARKS),
            ("Straight “A”s and “B”s", r#"Straight "A"s and "B"s"#, &US_MARKS),
            ("A 2″ nail.", r#"A 2" nail."#, &US_MARKS),
            ("‘A 2″ nail.’", r#"'A 2" nail.'"#, &US_MARKS),
            ("“A 2” nail.“", r#""A 2" nail.""#, &US_MARKS),
            ("“a [“b”] c”", r#""a ["b"] c""#, &US_MARKS),
            ("“a b”c“d e”", r#""a b"c"d e""#, &US_MARKS)
        ];

        for (expected, input, quotes) in cases {
            assert_eq!(expected, apply_quotes(input, quotes, false));
        }
    }
}
