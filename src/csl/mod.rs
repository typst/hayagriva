use std::borrow::Cow;
use std::cell::RefCell;
use std::fmt::Write;
use std::mem;
use std::num::NonZeroUsize;

use citationberg::taxonomy::{Locator, OtherTerm, Term, Variable};
use citationberg::{
    taxonomy as csl_taxonomy, Affixes, Bibliography, Citation, CslMacro, Display,
    InheritableNameOptions, Locale, LocaleCode, Style, TermForm, ToFormatting,
};
use citationberg::{
    DateForm, IndependentStyleSettings, LongShortForm, OrdinalLookup, TextCase,
};

use crate::csl::elem::{BufWriteFormat, Elem, Formatted};
use crate::csl::rendering::RenderCsl;
use crate::csl::taxonomy::resolve_name_variable;
use crate::lang::CaseFolder;
use crate::types::{ChunkKind, ChunkedString, Date, MaybeTyped, Numeric, Person};
use crate::Entry;

mod elem;
mod rendering;
mod sort;
mod taxonomy;

use taxonomy::resolve_date_variable;

use self::elem::{
    simplify_children, ElemChild, ElemChildren, ElemMeta, Formatting, NonEmptyStack,
};

/// This struct formats a set of citations according to a style.
#[derive(Debug, Clone)]
pub struct BibliographyDriver<'a> {
    /// The style we are using.
    style: StyleContext<'a>,
    /// The citations we have seen so far.
    citations: Vec<CitationMark<'a>>,
}

impl<'a> BibliographyDriver<'a> {
    /// Create a new bibliography driver.
    pub fn new(
        style: &'a Style,
        locale: LocaleCode,
        locale_file: &'a [Locale],
    ) -> Option<Self> {
        let style = StyleContext::new(style, locale, locale_file)?;
        Some(Self { style, citations: vec![] })
    }

    /// Create a new citation with the given items. Bibliography-wide
    /// disambiguation will not be applied.
    pub fn instant_citation(&self, items: &mut [CitationItem<'_>]) -> ElemChildren {
        self.style.sort(items, self.style.citation.sort.as_ref());
        let mut res = vec![];
        for item in items {
            res.push(self.style.citation(item));
        }

        let non_empty: Vec<_> = res.into_iter().filter(|c| c.has_content()).collect();
        // TODO in-cite disambiguation.

        let formatting =
            Formatting::default().apply(self.style.citation.layout.to_formatting());
        if !non_empty.is_empty() {
            let mut res = if let Some(prefix) = self.style.citation.layout.prefix.as_ref()
            {
                ElemChildren(vec![Formatted { text: prefix.clone(), formatting }.into()])
            } else {
                ElemChildren::new()
            };

            for (i, item) in non_empty.into_iter().enumerate() {
                let first = i == 0;
                if !first {
                    res.0.push(
                        Formatted {
                            text: self
                                .style
                                .citation
                                .layout
                                .delimiter
                                .as_deref()
                                .unwrap_or(Citation::DEFAULT_CITE_GROUP_DELIMITER)
                                .to_string(),
                            formatting,
                        }
                        .into(),
                    );
                }

                res.0.extend(item.0)
            }

            if let Some(suffix) = self.style.citation.layout.suffix.as_ref() {
                res.0.push(Formatted { text: suffix.clone(), formatting }.into());
            }

            simplify_children(res)
        } else {
            ElemChildren::new()
        }
    }

    /// Create a new citation with the given items.
    pub fn citation(&mut self, citation: &mut [CitationItem<'a>]) {
        self.style.sort(citation, self.style.citation.sort.as_ref());
        let mut items: Vec<(CitationItem<'a>, ElemChildren)> =
            Vec::with_capacity(citation.len());

        for item in citation {
            items.push((*item, self.style.citation(item)));
        }

        self.citations.push(CitationMark::Citation {
            items,
            affixes: (
                self.style.citation.layout.prefix.as_deref(),
                self.style.citation.layout.suffix.as_deref(),
            ),
        })
    }

    /// Skip a number of notes.
    pub fn skip_notes(&mut self, n: usize) {
        self.citations.push(CitationMark::SkipNotes(n))
    }

    /// Reset the note counter.
    pub fn reset_notes(&mut self, n: usize) {
        self.citations.push(CitationMark::ResetNotes(n))
    }
}

#[derive(Debug, Clone)]
enum CitationMark<'a> {
    /// A citation to one or more items.
    Citation {
        items: Vec<(CitationItem<'a>, ElemChildren)>,
        affixes: (Option<&'a str>, Option<&'a str>),
    },
    /// Skipping a number of notes.
    SkipNotes(usize),
    /// Resetting the note counter.
    ResetNotes(usize),
}

/// A context that contains all information related to rendering a single entry.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct InstanceContext<'a> {
    // Entry-dependent data.
    /// The current entry.
    pub entry: &'a Entry,
    /// The position of this citation in the list of citations.
    pub cite_props: Option<CiteProperties<'a>>,
    /// Whether we are sorting or formatting right now.
    pub sorting: bool,
}

impl<'a> InstanceContext<'a> {
    fn new(
        entry: &'a Entry,
        cite_props: Option<CiteProperties<'a>>,
        sorting: bool,
    ) -> Self {
        Self { entry, cite_props, sorting }
    }

    fn sort_instance(entry: &CitationItem<'a>) -> Self {
        Self::new(entry.0, CiteProperties::with_locator(entry.1), true)
    }
}

/// A context that contains information about the style we are using to render.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct StyleContext<'a> {
    // Settings from the style.
    /// The settings of the style.
    pub settings: &'a IndependentStyleSettings,
    /// A list of CSL macros.
    macros: &'a [CslMacro],
    /// A list of locales defined in the style.
    style_locales: &'a [Locale],
    /// A list of locales defined in their respective locale file.
    locale_file: &'a [Locale],
    /// Which locale we're using.
    locale: LocaleCode,
    /// Citation style.
    citation: &'a Citation,
    /// Bibliography layout.
    bibliography: Option<&'a Bibliography>,
}

impl<'a> StyleContext<'a> {
    fn new(
        style: &'a Style,
        locale: LocaleCode,
        locale_file: &'a [Locale],
    ) -> Option<Self> {
        let macros = &style.macros;
        let settings = style.independant_settings.as_ref()?;

        Some(Self {
            settings,
            macros,
            locale,
            style_locales: style.locale.as_slice(),
            locale_file,
            citation: style.citation.as_ref()?,
            bibliography: style.bibliography.as_ref(),
        })
    }

    fn ctx<'b>(
        &'b self,
        entry: &'b Entry,
        cite_props: Option<CiteProperties<'a>>,
    ) -> Context<'b> {
        Context {
            instance: InstanceContext::new(entry, cite_props, false),
            style: self,
            writing: WritingContext::new(),
        }
    }

    fn sorting_ctx<'b>(&'b self, item: &'b CitationItem<'b>) -> Context<'b> {
        Context {
            instance: InstanceContext::new(
                item.0,
                CiteProperties::with_locator(item.1),
                true,
            ),
            style: self,
            writing: WritingContext::new(),
        }
    }

    pub fn citation(&self, item: &CitationItem<'_>) -> ElemChildren {
        let mut ctx = self.ctx(item.0, CiteProperties::with_locator(item.1));
        ctx.writing.push_name_options(&self.citation.name_options);
        self.citation.layout.render(&mut ctx);
        ctx.flush()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CitationItem<'a>(&'a Entry, Option<SpecificLocator<'a>>);

impl<'a> StyleContext<'a> {
    /// Retrieve a macro.
    fn get_macro(&self, name: &str) -> Option<&'a CslMacro> {
        self.macros.iter().find(|m| m.name == name)
    }

    /// Get the locale for the given language in the style.
    fn lookup_locale<F, R>(&self, mut f: F) -> Option<R>
    where
        F: FnMut(&'a Locale) -> Option<R>,
    {
        let mut lookup = |file: &'a [Locale], lang| {
            #[allow(clippy::redundant_closure)]
            file.iter().find(|l| l.lang.as_ref() == lang).and_then(|l| f(l))
        };

        let fallback = self.locale.fallback();
        let en_us = LocaleCode::en_us();

        for (i, resource) in
            [self.style_locales, self.locale_file].into_iter().enumerate()
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

    /// Check whether to do punctuation in quotes.
    fn punctuation_in_quotes(&self) -> bool {
        self.lookup_locale(|f| f.style_options?.punctuation_in_quote)
            .unwrap_or_default()
    }
}

/// This struct contains all information needed to render a single entry. It
/// contains buffers and is mutable.
#[derive(Debug, Clone, Default)]
pub(crate) struct WritingContext {
    // Dynamic settings that change while rendering.
    /// Whether to watch out for punctuation that should be pulled inside the
    /// preceeding quoted content.
    pull_punctuation: bool,
    /// Whether we should be using inner quotes.
    inner_quotes: bool,
    /// Whether to strip periods.
    strip_periods: bool,
    /// Whether to add queried variables to the suppression list.
    suppress_queried_variables: bool,
    /// Suppressed variables that must not be rendered.
    suppressed_variables: RefCell<Vec<Variable>>,

    // Inheritable settings.
    /// A stack of formatting. Always contains the format of the root layout
    /// element at the bottom. Each element with a formatting may push and, if
    /// it did push, must remove. Its formatting shall apply for it and its
    /// children.
    format_stack: NonEmptyStack<Formatting>,
    /// Text cases.
    cases: NonEmptyStack<Option<TextCase>>,
    /// Inheritable name options.
    name_options: NonEmptyStack<InheritableNameOptions>,
    /// Usage info for the current nesting level.
    usage_info: RefCell<NonEmptyStack<UsageInfo>>,

    // Buffers.
    /// The buffer we're writing to. If block-level or formatting changes, we
    /// flush the buffer to the last [`Elem`] in the elem stack.
    buf: CaseFolder,
    /// A list of in-progress subtrees. Elements that are to be nested are
    /// pushed and then either popped or inserted at the end of their ancestor.
    elem_stack: NonEmptyStack<ElemChildren>,
}

impl WritingContext {
    fn new() -> Self {
        Self::default()
    }

    /// Retrieve the current formatting.
    fn formatting(&self) -> &Formatting {
        self.format_stack.last()
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

    /// Saves the current buffer to the last element in the finished list. This
    /// must happen if [`Display`] or formatting changes, as well as when we're
    /// done with an element.
    fn save_to_block(&mut self) {
        if self.buf.is_empty() {
            return;
        }

        let format = *self.formatting();

        // Append to last child if formats match.
        if let Some(child) = self.elem_stack.last_mut().0.last_mut().and_then(|c| {
            if let ElemChild::Text(c) = c {
                Some(c)
            } else {
                None
            }
        }) {
            if format == child.formatting {
                child.text.push_str(&mem::take(&mut self.buf).finish());
                return;
            }
        }

        let formatted = format.add_text(mem::take(&mut self.buf).finish());
        self.elem_stack.last_mut().0.push(ElemChild::Text(formatted))
    }

    /// Add another subtree to the children element. This must be done to
    /// include a new element or to check that the subtree is empty.
    fn push_elem(&mut self, format: citationberg::Formatting) -> DisplayLoc {
        self.save_to_block();
        let pos = self.elem_stack.len();
        self.elem_stack.push(ElemChildren::new());
        DisplayLoc::new(pos, self.push_format(format))
    }

    /// Remove the last subtree and discard it.
    ///
    /// Will panic if the location does not match.
    fn discard_elem(&mut self, loc: DisplayLoc) {
        assert_eq!(
            self.elem_stack.len().get(),
            loc.0.get() + 1,
            "stack location does not match"
        );
        self.pop_format(loc.1);

        self.save_to_block();
        self.elem_stack.drain(loc.0).for_each(drop);
    }

    fn has_content_since(&mut self, loc: &(DisplayLoc, usize)) -> bool {
        self.save_to_block();
        let children = self.elem_stack.last();
        let has_content = match children.0.first() {
            Some(ElemChild::Text(t)) if loc.1 < t.text.len() => {
                t.text[loc.1..].chars().any(|c| !c.is_whitespace())
            }
            Some(ElemChild::Text(_)) => false,
            Some(ElemChild::Elem(e)) => e.has_content(),
            None => false,
        };

        has_content || children.0.iter().skip(1).any(ElemChild::has_content)
    }

    /// Apply a prefix, but return a tuple that allows us to undo it if it
    /// wasn't followed by anything.
    fn apply_prefix(&mut self, affixes: &Affixes) -> (DisplayLoc, usize) {
        let pos = self.push_elem(citationberg::Formatting::default());
        if let Some(prefix) = &affixes.prefix {
            self.buf.push_str(prefix);
        };

        (pos, affixes.prefix.as_ref().map(|p| p.len()).unwrap_or_default())
    }

    /// Apply a suffix, but only if the location was followed by something.
    /// Delete any prefix if not.
    fn apply_suffix(&mut self, affixes: &Affixes, loc: (DisplayLoc, usize)) {
        if !self.has_content_since(&loc) {
            self.discard_elem(loc.0);
        } else {
            if let Some(suffix) = &affixes.suffix {
                self.buf.push_str(suffix);
            }
            self.commit_elem(loc.0, None, None);
        }
    }

    /// Nest the last subtree into its ancestor. If the `display` argument is
    /// some, a new element child will be created there. Otherwise, we will
    /// append the children of the last subtree to the children of its ancestor.
    fn commit_elem(
        &mut self,
        loc: DisplayLoc,
        display: Option<Display>,
        meta: Option<ElemMeta>,
    ) {
        assert_eq!(
            self.elem_stack.len().get(),
            loc.0.get() + 1,
            "stack location does not match"
        );
        self.pop_format(loc.1);

        self.save_to_block();
        let children = self.elem_stack.pop().unwrap();

        if display.is_some() || meta.is_some() {
            self.elem_stack
                .last_mut()
                .0
                .push(Elem { children, display, meta }.into());
        } else {
            self.elem_stack.last_mut().0.extend(children.0);
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

    /// Ensure that the buffer is either empty or the last character is a space.
    pub fn ensure_space(&mut self) {
        if !self.buf.is_empty() && !self.buf.ends_with(' ') {
            self.buf.push(' ');
        }
    }

    /// Folds all remaining elements into the first element and returns it.
    fn flush(mut self) -> ElemChildren {
        self.save_to_block();

        assert_eq!(
            self.format_stack.len().get(),
            1,
            "formatting stack is not one but {}",
            self.format_stack.len()
        );

        self.elem_stack.finish()
    }

    /// Note that we have used a macro that had non-empty content.
    fn printed_non_empty_macro(&mut self) {
        self.usage_info.get_mut().last_mut().has_used_macros = true;
    }

    /// Note that we have used a group that had non-empty content.
    fn printed_non_empty_group(&mut self) {
        self.usage_info.get_mut().last_mut().has_non_empty_group = true;
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

    /// Return the sum of the lengths of strings in the finished elements. This
    /// may not monotonoically increase.
    fn len(&self) -> usize {
        self.buf.len()
            + self
                .elem_stack
                .iter()
                .flat_map(|e| e.0.iter().map(ElemChild::str_len))
                .sum::<usize>()
    }

    /// Check if the last subtree is empty.
    fn last_is_empty(&self) -> bool {
        !self.buf.has_content() && !self.elem_stack.last().has_content()
    }
}

pub(crate) struct Context<'a> {
    instance: InstanceContext<'a>,
    style: &'a StyleContext<'a>,
    writing: WritingContext,
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
    pub locator: Option<SpecificLocator<'a>>,
}

impl<'a> CiteProperties<'a> {
    fn with_locator(locator: Option<SpecificLocator<'a>>) -> Option<Self> {
        Some(Self { locator, ..Self::default() })
    }
}

impl<'a> Default for CiteProperties<'a> {
    fn default() -> CiteProperties<'static> {
        CiteProperties {
            is_near_note: false,
            is_ibid: false,
            is_ibid_with_locator: false,
            is_first: true,
            is_disambiguation: false,
            locator: None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SpecificLocator<'a>(Locator, &'a str);

impl<'a> Context<'a> {
    /// Push a format on top of the stack if it is not empty.
    fn push_format(&mut self, format: citationberg::Formatting) -> FormatIdx {
        self.writing.push_format(format)
    }

    /// Pop a format from the stack if it is not empty.
    fn pop_format(&mut self, pos: FormatIdx) {
        self.writing.pop_format(pos)
    }

    /// Add another subtree to the children element. This must be done to
    /// include a new element or to check that the subtree is empty.
    fn push_elem(&mut self, format: citationberg::Formatting) -> DisplayLoc {
        self.writing.push_elem(format)
    }

    /// Remove the last subtree and discard it.
    ///
    /// Will panic if the location does not match.
    fn discard_elem(&mut self, loc: DisplayLoc) {
        self.writing.discard_elem(loc)
    }

    /// Nest the last subtree into its ancestor. If the `display` argument is
    /// some, a new element child will be created there. Otherwise, we will
    /// append the children of the last subtree to the children of its ancestor.
    fn commit_elem(
        &mut self,
        loc: DisplayLoc,
        display: Option<Display>,
        meta: Option<ElemMeta>,
    ) {
        self.writing.commit_elem(loc, display, meta)
    }

    /// Ensure that the buffer is either empty or the last character is a space.
    pub fn ensure_space(&mut self) {
        self.writing.ensure_space()
    }

    /// Add the appropriate opening quotation marks.
    fn push_quotes(&mut self) {
        let mark = self.term(
            if self.writing.inner_quotes {
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

        self.writing.inner_quotes = !self.writing.inner_quotes;
    }

    /// Add the appropriate closing quotation marks.
    fn pop_quotes(&mut self) {
        self.writing.inner_quotes = !self.writing.inner_quotes;

        let mark = self.term(
            if self.writing.inner_quotes {
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

    /// Pull punctuation into a quote if applicable
    fn do_pull_punctuation<'s>(&mut self, mut s: &'s str) -> &'s str {
        if self.writing.pull_punctuation && s.starts_with(['.', ',']) {
            let close_quote =
                self.term(OtherTerm::CloseQuote.into(), TermForm::default(), false);
            let close_inner_quote =
                self.term(OtherTerm::CloseInnerQuote.into(), TermForm::default(), false);

            let mut used_buf = false;
            let buf = if self.writing.buf.is_empty() {
                match self.writing.elem_stack.last_mut().0.last_mut() {
                    Some(ElemChild::Text(f)) => &mut f.text,
                    _ => {
                        used_buf = true;
                        self.writing.buf.as_string_mut()
                    }
                }
            } else {
                used_buf = true;
                self.writing.buf.as_string_mut()
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
                        self.writing.buf.mark_changed();
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

        self.writing.buf.reconfigure(
            (*self.writing.cases.last()).map(Into::into).unwrap_or_default(),
        );

        if self.writing.strip_periods {
            // Replicate citeproc.js behavior: remove a period if the
            // preceeding character in the original string is not a period.
            let mut last_period = false;
            for c in s.chars() {
                let is_period = c == '.';
                if !is_period || last_period {
                    self.writing.buf.push(c);
                }
                last_period = is_period;
            }
        } else {
            self.writing.buf.push_str(s);
        }

        self.writing.pull_punctuation = false;
    }

    /// Push a chunked string to the buffer.
    pub fn push_chunked(&mut self, chunked: &ChunkedString) {
        for chunk in &chunked.0 {
            match chunk.kind {
                ChunkKind::Normal => self.push_str(&chunk.value),
                _ => {
                    self.writing.buf.push_chunk(chunk);
                    self.writing.pull_punctuation = false;
                }
            }
        }
    }

    /// Folds all remaining elements into the first element and returns it.
    fn flush(self) -> ElemChildren {
        self.writing.flush()
    }

    /// Get a term from the style.
    fn term(&self, term: Term, form: TermForm, plural: bool) -> Option<&'a str> {
        let mut form = Some(form);
        while let Some(current_form) = form {
            if let Some(localization) = self.style.lookup_locale(|l| {
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
        self.style
            .lookup_locale(|l| l.date.iter().find(|d| d.form == Some(form)))
    }

    /// Get the ordinal lookup object.
    fn ordinal_lookup(&self) -> OrdinalLookup<'a> {
        self.style
            .lookup_locale(|l| l.ordinals())
            .unwrap_or_else(OrdinalLookup::empty)
    }

    /// Pull the next punctuation character into the preceeding quoted content
    /// if appropriate for the locale.
    fn may_pull_punctuation(&mut self) {
        self.writing.pull_punctuation |= self.style.punctuation_in_quotes();
    }

    /// Set whether to strip periods.
    fn may_strip_periods(&mut self, strip: bool) {
        self.writing.may_strip_periods(strip)
    }

    /// Stop stripping periods.
    fn stop_stripping_periods(&mut self) {
        self.writing.stop_stripping_periods()
    }

    /// Set the case of the next text.
    fn push_case(&mut self, case: Option<TextCase>) -> CaseIdx {
        self.writing.push_case(case)
    }

    /// Clear the case of the next text.
    fn pop_case(&mut self, idx: CaseIdx) {
        self.writing.pop_case(idx)
    }

    /// Resolve a number variable.
    ///
    /// Honors suppressions.
    fn resolve_number_variable(
        &self,
        variable: csl_taxonomy::NumberVariable,
    ) -> Option<MaybeTyped<Cow<'a, Numeric>>> {
        self.writing.prepare_variable_query(variable)?;
        let res = self.instance.resolve_number_variable(variable);

        if res.is_some() {
            self.writing.usage_info.borrow_mut().last_mut().has_non_empty_vars = true;
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
        self.writing.prepare_variable_query(variable)?;
        let res = self.instance.resolve_standard_variable(form, variable);

        if res.is_some() {
            self.writing.usage_info.borrow_mut().last_mut().has_non_empty_vars = true;
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
        self.writing.prepare_variable_query(variable)?;
        let res = resolve_date_variable(self.instance.entry, variable);

        if res.is_some() {
            self.writing.usage_info.borrow_mut().last_mut().has_non_empty_vars = true;
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
        if self.writing.prepare_variable_query(variable).is_none() {
            return Vec::new();
        }

        let res = resolve_name_variable(self.instance.entry, variable);

        if !res.is_empty() {
            self.writing.usage_info.borrow_mut().last_mut().has_non_empty_vars = true;
        }
        res
    }

    /// Apply a prefix, but return a tuple that allows us to undo it if it
    /// wasn't followed by anything.
    fn apply_prefix(&mut self, affixes: &Affixes) -> (DisplayLoc, usize) {
        self.writing.apply_prefix(affixes)
    }

    /// Apply a suffix, but only if the location was followed by something.
    /// Delete any prefix if not.
    fn apply_suffix(&mut self, affixes: &Affixes, loc: (DisplayLoc, usize)) {
        self.writing.apply_suffix(affixes, loc)
    }
}

#[must_use = "format stack must be popped"]
struct FormatIdx(NonZeroUsize);

#[must_use = "element stack must be popped"]
struct DisplayLoc(NonZeroUsize, FormatIdx);

impl DisplayLoc {
    fn new(pos: NonZeroUsize, format_idx: FormatIdx) -> Self {
        Self(pos, format_idx)
    }
}

#[must_use = "case stack must be popped"]
struct CaseIdx(NonZeroUsize);

#[must_use = "usage info stack must be popped"]
struct UsageInfoIdx(NonZeroUsize);

impl Write for Context<'_> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.push_str(s);
        Ok(())
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

#[cfg(test)]
mod tests {
    use citationberg::{LocaleFile, Style};

    use crate::csl::elem::BufWriteFormat;
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
        let en_locale = [en_locale.into()];

        let driver = BibliographyDriver::new(&style, locale, &en_locale).unwrap();

        for n in (0..bib.len()).step_by(2) {
            let mut item = [
                CitationItem(bib.nth(n).unwrap(), None),
                CitationItem(bib.nth(n + 1).unwrap(), None),
            ];
            let citation = driver.instant_citation(&mut item);
            let mut buf = String::new();

            for e in citation.0 {
                e.write_buf(&mut buf, BufWriteFormat::VT100).unwrap();
            }

            eprintln!("{}", buf);
        }
    }
}
