use std::borrow::Cow;
use std::cell::RefCell;
use std::fmt::{self, Write};
use std::mem;
use std::num::NonZeroUsize;

use citationberg::taxonomy::{Locator, OtherTerm, Term, Variable};
use citationberg::{
    taxonomy as csl_taxonomy, Affixes, CslMacro, Display, FontStyle, FontVariant,
    FontWeight, InheritableNameOptions, Locale, LocaleCode, Style, TermForm,
    TextDecoration, VerticalAlign,
};
use citationberg::{
    DateForm, IndependentStyleSettings, LongShortForm, OrdinalLookup, TextCase,
};

use crate::csl::taxonomy::resolve_name_variable;
use crate::lang::CaseFolder;
use crate::types::{ChunkKind, ChunkedString, Date, MaybeTyped, Numeric, Person};
use crate::Entry;

mod rendering;
mod taxonomy;

use taxonomy::{
    resolve_date_variable, resolve_number_variable, resolve_standard_variable,
};

pub(crate) struct Context<'a> {
    /// The settings of the style.
    pub settings: &'a IndependentStyleSettings,
    /// The current entry.
    pub entry: &'a Entry,
    /// The buffer we're writing to. If block-level or formatting changes, we
    /// flush the buffer to the last [`Elem`] in the finished list.
    pub buf: CaseFolder,
    /// The position of this citation in the list of citations.
    pub cite_props: Option<CiteProperties<'a>>,
    /// Suppressed variables that must not be rendered.
    suppressed_variables: RefCell<Vec<Variable>>,
    /// Which locale we're using.
    locale: LocaleCode,
    /// A list of CSL macros.
    macros: &'a [CslMacro],
    /// A list of locales defined in the style.
    style_locales: Vec<&'a Locale>,
    /// A list of locales defined in their respective locale file.
    locale_file: Vec<&'a Locale>,
    /// A stack of formatting. Always contains the format of the root layout
    /// element at the bottom. Each element with a formatting may push and, if
    /// it did push, must remove. Its formatting shall apply for it and its
    /// children.
    format_stack: NonEmptyStack<Formatting>,
    /// Finished elements. The last element may be unfinished.
    elem_stack: NonEmptyStack<Elem>,
    /// Text cases.
    cases: NonEmptyStack<Option<TextCase>>,
    /// Usage info for the current nesting level.
    usage_info: RefCell<NonEmptyStack<UsageInfo>>,
    /// Whether to watch out for punctuation that should be pulled inside the
    /// preceeding quoted content.
    pull_punctuation: bool,
    /// Whether we should be using inner quotes.
    inner_quotes: bool,
    /// Whether to strip periods.
    strip_periods: bool,
    /// Whether to add queried variables to the suppression list.
    suppress_queried_variables: bool,
    /// Inheritable name options.
    name_options: NonEmptyStack<InheritableNameOptions>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CiteProperties<'a> {
    /// Whether this citation is in a note and within `near-note-distance` to
    /// the previous citation of the same item.
    pub is_near_note: bool,
    /// Whether this citation is a duplicate of the previous citation.
    pub is_ibid: bool,
    /// Whether this directly follows another citation to the same item with a
    /// different locator.
    pub is_ibid_with_locator: bool,
    /// Whether this is the first citation of the entry.
    pub is_first: bool,
    /// Whether this is a citation that would be identical to another citation
    /// if not disambiguated by `choose`.
    pub is_disambiguation: bool,
    /// Locator with its type.
    pub locator: Option<(Locator, &'a str)>,
}

impl<'a> Context<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        entry: &'a Entry,
        style: &'a Style,
        locale: LocaleCode,
        locale_file: Vec<&'a Locale>,
        root_format: citationberg::Formatting,
        cite_props: Option<CiteProperties<'a>>,
    ) -> Option<Self> {
        let macros = &style.macros;
        let style_locales: Vec<&Locale> = style.locale.iter().collect();
        let settings = style.independant_settings.as_ref()?;

        Some(Self {
            entry,
            buf: CaseFolder::new(),
            locale,
            macros,
            style_locales,
            locale_file,
            format_stack: NonEmptyStack::new(Formatting::default().apply(root_format)),
            elem_stack: NonEmptyStack::new(Elem::new(None)),
            pull_punctuation: false,
            inner_quotes: false,
            strip_periods: false,
            cases: NonEmptyStack::new(None),
            cite_props,
            suppressed_variables: RefCell::new(Vec::new()),
            suppress_queried_variables: false,
            usage_info: RefCell::new(NonEmptyStack::new(UsageInfo::new())),
            name_options: NonEmptyStack::new(settings.options.clone()),
            settings,
        })
    }

    /// Retrieve the current formatting.
    fn formatting(&self) -> &Formatting {
        self.format_stack.last()
    }

    /// Saves the current buffer to the last element in the finished list. This
    /// must happen if [`Display`] or formatting changes, as well as when we're
    /// done with an element.
    fn save_to_block(&mut self) {
        if self.buf.is_empty() {
            return;
        }

        let format = *self.formatting();

        // Append to last child if formats match.
        if let Some(child) = self
            .elem_stack
            .last_mut()
            .children
            .last_mut()
            .and_then(|c| if let ElemChild::Text(c) = c { Some(c) } else { None })
        {
            if format == child.formatting {
                child.text.push_str(&mem::take(&mut self.buf).finish());
                return;
            }
        }

        let formatted = format.add_text(mem::take(&mut self.buf).finish());
        self.elem_stack.last_mut().children.push(ElemChild::Text(formatted))
    }

    /// Push a format on top of the stack if it is not empty.
    fn push_format(&mut self, format: citationberg::Formatting) -> FormatIdx {
        if format.is_empty() {
            return FormatIdx(self.format_stack.len());
        }

        self.save_to_block();
        let pos = self.format_stack.len();
        self.format_stack.push(self.formatting().apply(format));
        FormatIdx(pos)
    }

    /// Pop a format from the stack if it is not empty.
    fn pop_format(&mut self, pos: FormatIdx) {
        if pos.0 == self.format_stack.len() {
            return;
        }

        self.save_to_block();
        self.format_stack.drain(pos.0).for_each(drop);
    }

    /// Push on the element stack if the current element has some [`Display`].
    /// Also apply formatting. Return the index of the current element, so that,
    /// at the end of writing, all following elements can be popped and added as
    /// children.
    fn push_elem(
        &mut self,
        display: Option<Display>,
        format: citationberg::Formatting,
    ) -> DisplayLoc {
        let idx = match display {
            Some(_) => {
                self.save_to_block();
                let len = self.elem_stack.len();
                self.elem_stack.push(Elem::new(display));
                len
            }
            None => self.elem_stack.len(),
        };

        let format_idx = self.push_format(format);
        DisplayLoc::new(idx, format_idx)
    }

    /// Pop from the element stack if the current element has some [`Display`].
    /// Also pop formatting.
    fn pop_elem(&mut self, loc: DisplayLoc) {
        self.pop_format(loc.format_idx);

        if loc.display_idx != self.elem_stack.len() {
            self.save_to_block();
            let mut children = self
                .elem_stack
                .drain(loc.display_idx)
                .map(ElemChild::Elem)
                .collect::<Vec<_>>();
            let elem = self.elem_stack.last_mut();
            elem.children.append(&mut children);
        }
    }

    /// Add the appropriate opening quotation marks.
    fn push_quotes(&mut self) {
        let mark = self.term(
            if self.inner_quotes {
                OtherTerm::OpenInnerQuote
            } else {
                OtherTerm::OpenQuote
            }
            .into(),
            TermForm::default(),
            false,
        );

        if let Some(mark) = mark {
            self.push_str(mark);
        }

        self.inner_quotes = !self.inner_quotes;
    }

    /// Add the appropriate closing quotation marks.
    fn pop_quotes(&mut self) {
        self.inner_quotes = !self.inner_quotes;

        let mark = self.term(
            if self.inner_quotes {
                OtherTerm::CloseInnerQuote
            } else {
                OtherTerm::CloseQuote
            }
            .into(),
            TermForm::default(),
            false,
        );

        if let Some(mark) = mark {
            self.push_str(mark);
        }
    }

    /// Push an item on the name options stack.
    pub fn push_name_options(&mut self, options: &InheritableNameOptions) {
        self.name_options.push(self.name_options.last().apply(options));
    }

    /// Pop an item from the name options stack.
    pub fn pop_name_options(&mut self) {
        self.name_options.pop();
    }

    /// Pull punctuation into a quote if applicable
    fn do_pull_punctuation<'s>(&mut self, mut s: &'s str) -> &'s str {
        if self.pull_punctuation && s.starts_with(['.', ',']) {
            let close_quote =
                self.term(OtherTerm::CloseQuote.into(), TermForm::default(), false);
            let close_inner_quote =
                self.term(OtherTerm::CloseInnerQuote.into(), TermForm::default(), false);

            let mut used_buf = false;
            let buf = if self.buf.is_empty() {
                match self.elem_stack.last_mut().children.last_mut() {
                    Some(ElemChild::Text(f)) => &mut f.text,
                    _ => {
                        used_buf = true;
                        self.buf.as_string_mut()
                    }
                }
            } else {
                used_buf = true;
                self.buf.as_string_mut()
            };

            for quote in [close_quote, close_inner_quote].iter().flatten() {
                // Check if buf ends with a close quote.
                // If it does, replace it with the punctuation.
                if let Some(head) = buf.strip_suffix(quote) {
                    buf.truncate(head.len());

                    let punctuation = s.chars().next().unwrap();
                    s = &s[punctuation.len_utf8()..];
                    buf.push(punctuation);
                    buf.push_str(quote);

                    if used_buf {
                        self.buf.mark_changed();
                    }

                    break;
                }
            }
        }

        s
    }

    /// Add a string to the buffer.
    fn push_str(&mut self, s: &str) {
        let s = self.do_pull_punctuation(s);

        self.buf
            .reconfigure((*self.cases.last()).map(Into::into).unwrap_or_default());

        if self.strip_periods {
            // Replicate citeproc.js behavior: remove a period if the
            // preceeding character in the original string is not a period.
            let mut last_period = false;
            for c in s.chars() {
                let is_period = c == '.';
                if !is_period || last_period {
                    self.buf.push(c);
                }
                last_period = is_period;
            }
        } else {
            self.buf.push_str(s);
        }

        self.pull_punctuation = false;
    }

    /// Push a chunked string to the buffer.
    pub fn push_chunked(&mut self, chunked: &ChunkedString) {
        for chunk in &chunked.0 {
            match chunk.kind {
                ChunkKind::Normal => self.push_str(&chunk.value),
                _ => {
                    self.buf.push_chunk(chunk);
                    self.pull_punctuation = false;
                }
            }
        }
    }

    /// Ensure that the buffer is either empty or the last character is a space.
    pub fn ensure_space(&mut self) {
        if !self.buf.is_empty() && !self.buf.ends_with(' ') {
            self.buf.push(' ');
        }
    }

    /// Folds all remaining elements into the first element and returns it.
    fn flush(mut self) -> Elem {
        self.save_to_block();

        assert_eq!(
            self.format_stack.len().get(),
            1,
            "formatting stack is not one but {}",
            self.format_stack.len()
        );

        self.elem_stack.finish()
    }

    /// Retrieve a macro.
    fn get_macro(&self, name: &str) -> Option<&'a CslMacro> {
        let res = self.macros.iter().find(|m| m.name == name);
        res
    }

    /// Note that we have used a macro that had non-empty content.
    fn printed_non_empty_macro(&mut self) {
        self.usage_info.get_mut().last_mut().has_used_macros = true;
    }

    /// Note that we have used a group that had non-empty content.
    fn printed_non_empty_group(&mut self) {
        self.usage_info.get_mut().last_mut().has_non_empty_group = true;
    }

    /// Get the locale for the given language in the style.
    fn lookup_locale<F, R>(&self, mut f: F) -> Option<R>
    where
        F: FnMut(&'a Locale) -> Option<R>,
    {
        let mut lookup = |file: &[&'a Locale], lang| {
            file.iter().find(|l| l.lang.as_ref() == lang).and_then(|&l| f(l))
        };

        let fallback = self.locale.fallback();
        let en_us = LocaleCode::en_us();

        for (i, resource) in
            [&self.style_locales, &self.locale_file].into_iter().enumerate()
        {
            if let Some(output) = lookup(resource, Some(&self.locale)) {
                return Some(output);
            }

            if fallback.is_some() {
                if let Some(output) = lookup(resource, fallback.as_ref()) {
                    return Some(output);
                }
            }

            if i == 0 {
                if let Some(output) = lookup(resource, None) {
                    return Some(output);
                }
            } else if let Some(output) = lookup(resource, Some(&en_us)) {
                return Some(output);
            }
        }

        None
    }

    /// Get a term from the style.
    fn term(&self, term: Term, form: TermForm, plural: bool) -> Option<&'a str> {
        let mut form = Some(form);
        while let Some(current_form) = form {
            if let Some(localization) = self.lookup_locale(|l| {
                let term = l.term(term, current_form)?;
                Some(if plural { term.multiple() } else { term.single() })
            }) {
                return localization;
            }

            form = current_form.fallback();
        }

        None
    }

    /// Get a localized date format.
    fn localized_date(&self, form: DateForm) -> Option<&'a citationberg::Date> {
        self.lookup_locale(|l| l.date.iter().find(|d| d.form == Some(form)))
    }

    /// Get the ordinal lookup object.
    fn ordinal_lookup(&self) -> OrdinalLookup<'a> {
        self.lookup_locale(|l| l.ordinals())
            .unwrap_or_else(OrdinalLookup::empty)
    }

    /// Check whether to do punctuation in quotes.
    fn punctuation_in_quotes(&self) -> bool {
        self.lookup_locale(|f| f.style_options?.punctuation_in_quote)
            .unwrap_or_default()
    }

    /// Pull the next punctuation character into the preceeding quoted content
    /// if appropriate for the locale.
    fn may_pull_punctuation(&mut self) {
        self.pull_punctuation |= self.punctuation_in_quotes();
    }

    /// Set whether to strip periods.
    fn may_strip_periods(&mut self, strip: bool) {
        self.strip_periods = strip;
    }

    /// Stop stripping periods.
    fn stop_stripping_periods(&mut self) {
        self.strip_periods = false;
    }

    /// Start suppressing the queried variables.
    fn start_suppressing_queried_variables(&mut self) {
        self.suppress_queried_variables = true;
    }

    /// Stop suppressing the queried variables.
    fn stop_suppressing_queried_variables(&mut self) {
        self.suppress_queried_variables = false;
    }

    /// Set the case of the next text.
    fn push_case(&mut self, case: Option<TextCase>) -> CaseIdx {
        let idx = self.cases.len();
        self.cases.push(case);
        CaseIdx(idx)
    }

    /// Clear the case of the next text.
    fn pop_case(&mut self, idx: CaseIdx) {
        if idx.0 == self.cases.len() {
            return;
        }

        self.cases.drain(idx.0).for_each(drop);
    }

    /// Push an element on the usage info stack.
    fn push_usage_info(&mut self) -> UsageInfoIdx {
        let info = self.usage_info.get_mut();
        let idx = info.len();
        info.push(UsageInfo::new());
        UsageInfoIdx(idx)
    }

    /// Pop an element from the usage info stack.
    fn pop_usage_info(&mut self, idx: UsageInfoIdx) -> UsageInfo {
        let info = self.usage_info.get_mut();
        let mut v = info.drain(idx.0).collect::<Vec<_>>();
        if v.is_empty() {
            return UsageInfo::default();
        }

        let mut first = v.remove(0);

        for e in v.drain(0..v.len()) {
            first = first.merge_child(e);
        }

        first
    }

    /// Push a new suppressed variable if we are suppressing queried variables.
    fn maybe_suppress(&self, variable: Variable) {
        if self.suppress_queried_variables {
            self.suppressed_variables.borrow_mut().push(variable);
        }
    }

    fn prepare_variable_query<V>(&self, variable: V) -> Option<Variable>
    where
        V: Into<Variable>,
    {
        let general: Variable = variable.into();
        if self.suppressed_variables.borrow().contains(&general) {
            return None;
        }

        self.maybe_suppress(general);
        Some(general)
    }

    /// Resolve a number variable.
    ///
    /// Honors suppressions.
    fn resolve_number_variable(
        &self,
        variable: csl_taxonomy::NumberVariable,
    ) -> Option<MaybeTyped<Cow<'a, Numeric>>> {
        self.prepare_variable_query(variable)?;
        let res = resolve_number_variable(self.entry, variable);

        if res.is_some() {
            self.usage_info.borrow_mut().last_mut().has_non_empty_vars = true;
        }
        res
    }

    /// Resolve a name variable.
    ///
    /// Honors suppressions.
    fn resolve_standard_variable(
        &self,
        form: LongShortForm,
        variable: csl_taxonomy::StandardVariable,
    ) -> Option<Cow<'a, ChunkedString>> {
        self.prepare_variable_query(variable)?;
        let res = resolve_standard_variable(self.entry, form, variable);

        if res.is_some() {
            self.usage_info.borrow_mut().last_mut().has_non_empty_vars = true;
        }
        res
    }

    /// Resolve a date variable.
    ///
    /// Honors suppressions.
    fn resolve_date_variable(
        &self,
        variable: csl_taxonomy::DateVariable,
    ) -> Option<&'a Date> {
        self.prepare_variable_query(variable)?;
        let res = resolve_date_variable(self.entry, variable);

        if res.is_some() {
            self.usage_info.borrow_mut().last_mut().has_non_empty_vars = true;
        }
        res
    }

    /// Resolve a name variable.
    ///
    /// Honors suppressions.
    fn resolve_name_variable(
        &self,
        variable: csl_taxonomy::NameVariable,
    ) -> Vec<&'a Person> {
        if self.prepare_variable_query(variable).is_none() {
            return Vec::new();
        }

        let res = resolve_name_variable(self.entry, variable);

        if !res.is_empty() {
            self.usage_info.borrow_mut().last_mut().has_non_empty_vars = true;
        }
        res
    }

    /// Return the sum of the lengths of strings in the finished elements.
    fn len(&self) -> usize {
        self.buf.len() + self.elem_stack.iter().map(|e| e.str_len()).sum::<usize>()
    }

    /// Retrieve a length that can be used to delete the following elements.
    fn deletable_len(&mut self) -> ElemLoc {
        self.save_to_block();
        let pos = self.elem_stack.len().get() - 1;
        let children = &self.elem_stack.last().children;
        let child_pos = children.len().saturating_sub(1);
        let child_inner_pos = if let Some(ElemChild::Text(t)) = children.last() {
            Some(t.text.len())
        } else {
            None
        };

        ElemLoc::new(pos, child_pos, child_inner_pos)
    }

    /// Delete the elements after the position.
    fn delete_with_loc(&mut self, loc: ElemLoc) {
        self.save_to_block();

        // Drop items after the stack position.
        if loc.stack_pos + 1 < self.elem_stack.len().get() {
            self.elem_stack
                .drain(NonZeroUsize::new(loc.stack_pos + 1).unwrap())
                .for_each(drop);
        }

        let elem = self.elem_stack.last_mut();
        // Remove any element after the position, we always want to vacate them.
        if loc.child_pos + 1 < elem.children.len() {
            elem.children.drain(loc.child_pos + 1..).for_each(drop);
        }

        if loc.child_pos < elem.children.len() {
            let pop = match (elem.children.last_mut(), loc.child_inner_pos) {
                // Remove empty text elements.
                (Some(ElemChild::Text(_)), Some(0) | None) => true,
                // Truncate non-empty text elements.
                (Some(ElemChild::Text(t)), Some(idx)) => {
                    t.text.truncate(idx);
                    false
                }
                (None, _) => unreachable!(),
                // Elements cannot be partially removed.
                (Some(ElemChild::Elem(_)), _) => true,
            };

            if pop {
                elem.children.pop();
            }
        }
    }

    /// Check whether the location wasn't only followed by whitespace.
    fn has_content_between(&self, loc: ElemLoc) -> bool {
        if !self.buf.is_whitespace() {
            return true;
        }

        // We need to investigate the current element, it may have interesting
        // children.
        for e in self.elem_stack.iter().skip(loc.stack_pos) {
            for (i, b) in e.children.iter().skip(loc.child_pos).enumerate() {
                let first = i == 0;

                match b {
                    ElemChild::Text(t) => {
                        let idx = if first {
                            loc.child_inner_pos.unwrap_or_default()
                        } else {
                            0
                        };

                        if idx >= t.text.len() {
                            continue;
                        }

                        if t.text[idx..].chars().any(|c| !c.is_whitespace()) {
                            return true;
                        }
                    }
                    ElemChild::Elem(e) => {
                        if !e.is_whitespace() {
                            return true;
                        }
                    }
                }
            }
        }

        false
    }

    /// Apply a prefix, but return a tuple that allows us to undo it if it
    /// wasn't followed by anything.
    fn apply_prefix(&mut self, affixes: &Affixes) -> (Option<ElemLoc>, ElemLoc) {
        let loc_before = if let Some(prefix) = &affixes.prefix {
            let loc = self.deletable_len();
            self.push_str(prefix);
            Some(loc)
        } else {
            None
        };

        let loc_after = self.deletable_len();
        (loc_before, loc_after)
    }

    /// Apply a suffix, but only if the location was followed by something.
    /// Delete any prefix if not.
    fn apply_suffix(&mut self, affixes: &Affixes, loc: (Option<ElemLoc>, ElemLoc)) {
        if self.has_content_between(loc.1) {
            if let Some(suffix) = &affixes.suffix {
                self.push_str(suffix);
            }
        } else if let Some(loc) = loc.0 {
            self.delete_with_loc(loc);
        }
    }

    /// Get a representation of the current progress as a string.
    fn dbg_string(&self) {
        let mut w = String::with_capacity(self.len());
        for e in self.elem_stack.iter() {
            e.to_string(&mut w).unwrap();
        }

        w.write_str(&self.buf.clone().finish()).unwrap();

        eprintln!("{}\n", w);
    }
}

#[must_use = "format stack must be popped"]
struct FormatIdx(NonZeroUsize);

#[must_use = "element stack must be popped"]
struct DisplayLoc {
    display_idx: NonZeroUsize,
    format_idx: FormatIdx,
}

impl DisplayLoc {
    fn new(display_idx: NonZeroUsize, format_idx: FormatIdx) -> Self {
        Self { display_idx, format_idx }
    }
}

#[must_use = "case stack must be popped"]
struct CaseIdx(NonZeroUsize);

#[must_use = "usage info stack must be popped"]
struct UsageInfoIdx(NonZeroUsize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ElemLoc {
    stack_pos: usize,
    child_pos: usize,
    child_inner_pos: Option<usize>,
}

impl ElemLoc {
    fn new(stack_pos: usize, child_pos: usize, child_inner_pos: Option<usize>) -> Self {
        Self { stack_pos, child_pos, child_inner_pos }
    }
}

impl Write for Context<'_> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.push_str(s);
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Elem {
    pub children: Vec<ElemChild>,
    pub display: Option<Display>,
}

impl Elem {
    fn new(display: Option<Display>) -> Self {
        Self { children: Vec::new(), display }
    }

    fn str_len(&self) -> usize {
        self.children
            .iter()
            .map(|c| match c {
                ElemChild::Text(t) => t.text.len(),
                ElemChild::Elem(e) => e.str_len(),
            })
            .sum()
    }

    fn to_string(&self, w: &mut impl fmt::Write) -> Result<(), fmt::Error> {
        if self.display == Some(Display::Block) {
            w.write_char('\n')?;
        }

        for child in &self.children {
            match child {
                ElemChild::Text(t) => w.write_str(&t.text)?,
                ElemChild::Elem(e) => e.to_string(w)?,
            }
        }

        if self.display == Some(Display::Block) {
            w.write_char('\n')?;
        }

        Ok(())
    }

    fn is_whitespace(&self) -> bool {
        self.children.iter().all(|c| match c {
            ElemChild::Text(t) => t.text.chars().all(char::is_whitespace),
            ElemChild::Elem(e) => e.is_whitespace(),
        })
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
struct UsageInfo {
    has_vars: bool,
    has_non_empty_vars: bool,
    has_used_macros: bool,
    has_non_empty_group: bool,
}

impl UsageInfo {
    /// Create a new usage info object.
    fn new() -> Self {
        Self::default()
    }

    /// Merge usage info with the info of a child.
    fn merge_child(self, child: Self) -> Self {
        Self {
            has_vars: self.has_vars || child.has_vars,
            has_non_empty_vars: self.has_non_empty_vars || child.has_non_empty_vars,
            has_used_macros: self.has_used_macros || child.has_used_macros,
            has_non_empty_group: self.has_non_empty_group || child.has_non_empty_group,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ElemChild {
    Text(Formatted),
    Elem(Elem),
}

#[derive(Debug, Clone)]
pub struct Formatted {
    pub text: String,
    pub formatting: Formatting,
}

impl Formatted {
    fn new(text: String) -> Self {
        Self { text, formatting: Formatting::new() }
    }

    fn elem(self, display: Option<Display>) -> Elem {
        Elem { children: vec![ElemChild::Text(self)], display }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Formatting {
    pub font_style: FontStyle,
    pub font_variant: FontVariant,
    pub font_weight: FontWeight,
    pub text_decoration: TextDecoration,
    pub vertical_align: VerticalAlign,
}

impl Formatting {
    fn new() -> Self {
        Self::default()
    }

    fn add_text(self, text: String) -> Formatted {
        Formatted { text, formatting: self }
    }

    /// Apply a partial formatting on top of this formatting.
    fn apply(mut self, other: citationberg::Formatting) -> Self {
        if let Some(style) = other.font_style {
            self.font_style = style;
        }

        if let Some(variant) = other.font_variant {
            self.font_variant = variant;
        }

        if let Some(weight) = other.font_weight {
            self.font_weight = weight;
        }

        if let Some(decoration) = other.text_decoration {
            self.text_decoration = decoration;
        }

        if let Some(align) = other.vertical_align {
            self.vertical_align = align;
        }

        self
    }

    /// Whether this format will change if a different format is applied on top.
    fn will_change(&self, other: citationberg::Formatting) -> bool {
        if let Some(style) = other.font_style {
            if self.font_style != style {
                return true;
            }
        }

        if let Some(variant) = other.font_variant {
            if self.font_variant != variant {
                return true;
            }
        }

        if let Some(weight) = other.font_weight {
            if self.font_weight != weight {
                return true;
            }
        }

        if let Some(decoration) = other.text_decoration {
            if self.text_decoration != decoration {
                return true;
            }
        }

        if let Some(align) = other.vertical_align {
            if self.vertical_align != align {
                return true;
            }
        }

        false
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct NonEmptyStack<T> {
    head: Vec<T>,
    last: T,
}

impl<T> NonEmptyStack<T> {
    fn new(last: T) -> Self {
        Self { head: Vec::new(), last }
    }

    fn last(&self) -> &T {
        &self.last
    }

    fn last_mut(&mut self) -> &mut T {
        &mut self.last
    }

    fn len(&self) -> NonZeroUsize {
        NonZeroUsize::new(self.head.len() + 1).unwrap()
    }

    fn push(&mut self, elem: T) {
        self.head.push(mem::replace(&mut self.last, elem));
    }

    fn pop(&mut self) -> Option<T> {
        let new_last = self.head.pop()?;
        Some(mem::replace(&mut self.last, new_last))
    }

    /// Drains all elements including and after the given index.
    fn drain(&mut self, idx: NonZeroUsize) -> impl Iterator<Item = T> + '_ {
        let idx = idx.get();
        mem::swap(&mut self.head[idx - 1], &mut self.last);
        let mut drain = self.head.drain(idx - 1..);
        let first = drain.next();
        drain.chain(first)
    }

    fn iter(&self) -> impl Iterator<Item = &T> {
        self.head.iter().chain(std::iter::once(&self.last))
    }

    fn finish(self) -> T {
        if !self.head.is_empty() {
            panic!("NonEmptyStack::finish called with non-empty stack")
        }

        self.last
    }
}

#[cfg(test)]
mod tests {
    use citationberg::{LocaleFile, Style};

    use super::rendering::RenderCsl;
    use crate::io::from_yaml_str;

    use super::*;
    use std::fs;

    #[test]
    fn test_csl() {
        let style = fs::read_to_string("tests/art-history.csl").unwrap();
        let style = Style::from_xml(&style).unwrap();
        let locale = LocaleCode::en_us();

        let en_locale = fs::read_to_string("tests/locales-en-US.xml").unwrap();
        let en_locale = LocaleFile::from_xml(&en_locale).unwrap();

        let yaml = fs::read_to_string("tests/basic.yml").unwrap();
        let bib = from_yaml_str(&yaml).unwrap();
        let en_locale = en_locale.into();

        for entry in &bib {
            let mut ctx = Context::new(
                entry,
                &style,
                locale.clone(),
                vec![&en_locale],
                citationberg::Formatting::default(),
                None,
            )
            .unwrap();
            let cit_style = style.citation.as_ref().unwrap();
            ctx.push_name_options(&cit_style.name_options);

            cit_style.layout.render(&mut ctx);
            let mut buf = String::new();
            ctx.flush().to_string(&mut buf).unwrap();
            eprintln!("{}", buf);
        }
    }
}
