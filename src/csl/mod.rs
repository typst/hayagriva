use std::borrow::Cow;
use std::cell::RefCell;
use std::fmt::Write;
use std::mem;
use std::num::NonZeroUsize;

use crate::csl::taxonomy::resolve_name_variable;
use crate::lang::{Case, CaseFolder, SentenceCaseConf, TitleCaseConf};
use crate::types::{Date, Person};
use crate::Entry;
use citationberg::taxonomy::{Locator, NameVariable};
use citationberg::{taxonomy as csl_taxonomy, ToFormatting};
use citationberg::{
    taxonomy::{OtherTerm, Term, Variable},
    CslMacro, Display, FontStyle, FontVariant, FontWeight, Locale, LocaleCode, TermForm,
    TextDecoration, TextTarget, VerticalAlign,
};
use citationberg::{
    ChooseBranch, DateDayForm, DateForm, DateMonthForm, DatePartName, DateParts,
    DateStrongAnyForm, DelimiterBehavior, DemoteNonDroppingParticle,
    IndependentStyleSettings, LabelPluralize, LayoutRenderingElement, LongShortForm,
    NameAnd, NameAsSortOrder, NameForm, Names, OrdinalLookup, TestPosition, TextCase,
};

mod taxonomy;
use taxonomy::{resolve_number_variable, resolve_standard_variable};

use self::taxonomy::{matches_entry_type, resolve_date_variable, MaybeTyped, Numeric};

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
        locale: LocaleCode,
        macros: &'a [CslMacro],
        style_locales: Vec<&'a Locale>,
        locale_file: Vec<&'a Locale>,
        root_format: citationberg::Formatting,
        settings: &'a IndependentStyleSettings,
        cite_props: Option<CiteProperties<'a>>,
    ) -> Self {
        Self {
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
            settings,
            cases: NonEmptyStack::new(None),
            cite_props,
            suppressed_variables: RefCell::new(Vec::new()),
            suppress_queried_variables: false,
            usage_info: RefCell::new(NonEmptyStack::new(UsageInfo::new())),
        }
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

    /// Add a string to the buffer.
    fn push_str(&mut self, mut s: &str) {
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
                        self.buf.as_str_mut()
                    }
                }
            } else {
                used_buf = true;
                self.buf.as_str_mut()
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

        self.buf
            .config((*self.cases.last()).map(Into::into).unwrap_or_default());

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
    ) -> Option<MaybeTyped<Numeric>> {
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
    ) -> Option<&'a str> {
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
    ) -> Option<Date> {
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
    ) -> Option<Vec<&'a Person>> {
        self.prepare_variable_query(variable)?;
        let res = resolve_name_variable(self.entry, variable);

        if res.is_some() {
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
        let pos = self.elem_stack.len();
        let children = &self.elem_stack.last().children;
        let child_pos = children.len();
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
        self.elem_stack.drain(loc.stack_pos).for_each(drop);
        let elem = self.elem_stack.last_mut();
        elem.children.drain(loc.child_pos..);
        if let (Some(pos), Some(ElemChild::Text(t))) =
            (loc.child_inner_pos, elem.children.last_mut())
        {
            t.text.truncate(pos);
        }
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

struct ElemLoc {
    stack_pos: NonZeroUsize,
    child_pos: usize,
    child_inner_pos: Option<usize>,
}

impl ElemLoc {
    fn new(
        stack_pos: NonZeroUsize,
        child_pos: usize,
        child_inner_pos: Option<usize>,
    ) -> Self {
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

trait RenderCsl {
    fn render(&self, ctx: &mut Context);
}

impl RenderCsl for citationberg::Text {
    fn render(&self, ctx: &mut Context) {
        let depth = ctx.push_elem(self.display, self.formatting);

        if let Some(prefix) = &self.affixes.prefix {
            ctx.push_str(prefix);
        }

        if self.quotes {
            ctx.push_quotes();
        }

        ctx.may_strip_periods(self.strip_periods);
        let cidx = ctx.push_case(self.text_case);

        match &self.target {
            TextTarget::Variable { var, form } => ctx.push_str(
                match var {
                    Variable::Standard(var) => {
                        ctx.resolve_standard_variable(*form, *var).map(Cow::Borrowed)
                    }
                    Variable::Number(var) => match ctx.resolve_number_variable(*var) {
                        Some(MaybeTyped::String(s)) => Some(Cow::Owned(s)),
                        Some(MaybeTyped::Typed(n)) => Some(Cow::Owned(n.to_string())),
                        None => None,
                    },
                    _ => None,
                }
                .unwrap_or_default()
                .as_ref(),
            ),
            TextTarget::Macro { name } => {
                let len = ctx.len();
                let mac = ctx.get_macro(name);

                if let Some(mac) = &mac {
                    for child in &mac.children {
                        child.render(ctx);
                    }

                    if len < ctx.len() {
                        ctx.printed_non_empty_macro();
                    }
                }
            }
            TextTarget::Term { term, form, plural } => {
                ctx.push_str(ctx.term(*term, *form, *plural).unwrap_or_default())
            }
            TextTarget::Value { val } => ctx.push_str(val),
        }

        ctx.pop_case(cidx);
        ctx.stop_stripping_periods();

        if self.quotes {
            ctx.pop_quotes();
            ctx.may_pull_punctuation();
        }

        if let Some(suffix) = &self.affixes.suffix {
            ctx.push_str(suffix);
        }

        ctx.pop_elem(depth);
    }
}

impl RenderCsl for citationberg::Number {
    fn render(&self, ctx: &mut Context) {
        let depth = ctx.push_elem(self.display, self.formatting);

        if let Some(prefix) = &self.affixes.prefix {
            ctx.push_str(prefix);
        }

        let cidx = ctx.push_case(self.text_case);

        let value = ctx.resolve_number_variable(self.variable);
        match value {
            Some(MaybeTyped::Typed(num)) if num.will_transform() => {
                num.with_form(ctx, self.form, ctx.ordinal_lookup()).unwrap();
            }
            Some(MaybeTyped::Typed(num)) => write!(ctx, "{}", num).unwrap(),
            Some(MaybeTyped::String(s)) => ctx.push_str(&s),
            None => {}
        }

        ctx.pop_case(cidx);

        if let Some(suffix) = &self.affixes.suffix {
            ctx.push_str(suffix);
        }

        ctx.pop_elem(depth);
    }
}

impl RenderCsl for citationberg::Label {
    fn render(&self, ctx: &mut Context) {
        let Some(variable) = ctx.resolve_number_variable(self.variable) else {
            return;
        };

        let plural = match self.label.plural {
            LabelPluralize::Always => true,
            LabelPluralize::Never => false,
            LabelPluralize::Contextual => match variable {
                MaybeTyped::String(_) => false,
                MaybeTyped::Typed(n) => {
                    n.is_plural(self.variable.is_number_of_variable())
                }
            },
        };

        let content = ctx
            .term(Term::from(self.variable), self.label.form, plural)
            .unwrap_or_default();

        render_label_with_var(&self.label, ctx, content)
    }
}

fn render_label_with_var(
    label: &citationberg::VariablelessLabel,
    ctx: &mut Context,
    content: &str,
) {
    let idx = ctx.push_format(label.formatting);

    let affixes = &label.affixes;
    if let Some(prefix) = &affixes.prefix {
        ctx.push_str(prefix);
    }

    ctx.may_strip_periods(label.strip_periods);
    let cidx = ctx.push_case(label.text_case);

    ctx.push_str(content);

    ctx.pop_case(cidx);
    ctx.stop_stripping_periods();

    if let Some(suffix) = &affixes.suffix {
        ctx.push_str(suffix);
    }

    ctx.pop_format(idx);
}

impl RenderCsl for citationberg::Date {
    fn render(&self, ctx: &mut Context) {
        let Some(variable) = self.variable else { return };
        let Some(date) = ctx.resolve_date_variable(variable) else { return };

        let base = if let Some(form) = self.form {
            let Some(base) = ctx.localized_date(form) else { return };
            Some(base)
        } else {
            None
        };

        let formatting = base
            .map(|b| self.formatting.apply(b.formatting))
            .unwrap_or(self.formatting);
        let depth = ctx.push_elem(self.display, formatting);

        if let Some(prefix) = &self.affixes.prefix {
            ctx.push_str(prefix);
        }

        let cidx = ctx.push_case(self.text_case.or(base.and_then(|b| b.text_case)));

        let parts = if let Some(base) = base {
            base.parts.unwrap_or_default()
        } else {
            DateParts::default()
        };

        // TODO: Date ranges
        let mut last_was_empty = true;
        for part in &base.unwrap_or(self).date_part {
            match part.name {
                DatePartName::Month if !parts.has_month() => continue,
                DatePartName::Day if !parts.has_day() => continue,
                _ => {}
            }

            let cursor = ctx.len();
            if !last_was_empty {
                if let Some(delim) = &self.delimiter {
                    ctx.push_str(delim);
                }
            }

            let over_ride = base
                .is_some()
                .then(|| self.date_part.iter().find(|p| p.name == part.name))
                .flatten();

            render_date_part(part, &date, ctx, over_ride);
            last_was_empty = cursor == ctx.len();
        }

        ctx.pop_case(cidx);

        if let Some(suffix) = &self.affixes.suffix {
            ctx.push_str(suffix);
        }

        ctx.pop_elem(depth);
    }
}

fn render_date_part(
    date_part: &citationberg::DatePart,
    date: &Date,
    ctx: &mut Context,
    over_ride: Option<&citationberg::DatePart>,
) {
    let Some(val) = (match date_part.name {
        DatePartName::Day => date.day.map(|i| i as i32),
        DatePartName::Month => date.month.map(|i| i as i32),
        DatePartName::Year => Some(date.year),
    }) else {
        return;
    };

    let formatting = over_ride
        .map(|p| p.formatting.apply(date_part.formatting))
        .unwrap_or(date_part.formatting);

    let idx = ctx.push_format(formatting);

    let affixes = &date_part.affixes;

    if let Some(prefix) = &affixes.prefix {
        ctx.push_str(prefix);
    }

    let cidx = ctx.push_case(over_ride.and_then(|o| o.text_case).or(date_part.text_case));

    let form = over_ride
        .map(citationberg::DatePart::form)
        .unwrap_or_else(|| date_part.form());
    match form {
        DateStrongAnyForm::Day(DateDayForm::NumericLeadingZeros)
        | DateStrongAnyForm::Month(DateMonthForm::NumericLeadingZeros) => {
            write!(ctx, "{:02}", val).unwrap();
        }
        DateStrongAnyForm::Day(DateDayForm::Ordinal)
            if val != 1
                || !ctx
                    .lookup_locale(|l| {
                        Some(
                            l.style_options
                                .and_then(|o| o.limit_day_ordinals_to_day_1)
                                .unwrap_or_default(),
                        )
                    })
                    .unwrap_or_default() =>
        {
            write!(
                ctx,
                "{}{}",
                val,
                ctx.ordinal_lookup().lookup(val).unwrap_or_default()
            )
            .unwrap();
        }
        DateStrongAnyForm::Day(DateDayForm::Numeric | DateDayForm::Ordinal)
        | DateStrongAnyForm::Month(DateMonthForm::Numeric) => {
            write!(ctx, "{}", val).unwrap();
        }
        DateStrongAnyForm::Month(DateMonthForm::Long) => {
            if let Some(month) = ctx.term(
                OtherTerm::month(val as u8).unwrap().into(),
                TermForm::Long,
                false,
            ) {
                ctx.push_str(month);
            } else {
                write!(ctx, "{}", val).unwrap();
            }
        }
        DateStrongAnyForm::Month(DateMonthForm::Short) => {
            if let Some(month) = ctx.term(
                OtherTerm::month(val as u8).unwrap().into(),
                TermForm::Short,
                false,
            ) {
                ctx.push_str(month);
            } else {
                write!(ctx, "{}", val).unwrap();
            }
        }
        DateStrongAnyForm::Year(LongShortForm::Short) => {
            write!(ctx, "{:02}", (val % 100).abs()).unwrap();
        }
        DateStrongAnyForm::Year(LongShortForm::Long) => {
            write!(ctx, "{}", val.abs()).unwrap();
        }
    }

    if let DateStrongAnyForm::Year(_) = form {
        if val < 1000 {
            ctx.push_str(if val < 0 { "BC" } else { "AD" });
        }
    }

    if let Some(suffix) = &affixes.suffix {
        ctx.push_str(suffix);
    }

    ctx.pop_case(cidx);
    ctx.pop_format(idx);
}

impl RenderCsl for Names {
    fn render(&self, ctx: &mut Context) {
        let people: Vec<(Vec<&Person>, Term)> = if self.variable.len() == 2
            && self.variable.contains(&NameVariable::Editor)
            && self.variable.contains(&NameVariable::Translator)
        {
            let editors = ctx.resolve_name_variable(NameVariable::Editor);
            let translators = ctx.resolve_name_variable(NameVariable::Translator);

            match (editors, translators) {
                (Some(editors), Some(translators)) if editors == translators => {
                    vec![(editors, NameVariable::EditorTranslator.into())]
                }
                (editors, translators) => {
                    let mut res = Vec::new();
                    if let Some(editors) = editors {
                        res.push((editors, NameVariable::Editor.into()));
                    }

                    if let Some(translators) = translators {
                        res.push((translators, NameVariable::Translator.into()));
                    }

                    res
                }
            }
        } else {
            self.variable
                .iter()
                .map(|v| {
                    (ctx.resolve_name_variable(*v).unwrap_or_default(), Term::from(*v))
                })
                .collect()
        };

        let is_empty = people.iter().all(|(p, _)| p.is_empty());
        if is_empty {
            if let Some(substitute) = &self.substitute {
                ctx.start_suppressing_queried_variables();

                for child in &substitute.children {
                    let len = ctx.len();
                    // TODO deal with name shorthand
                    child.render(ctx);
                    if len < ctx.len() {
                        break;
                    }
                }

                ctx.stop_suppressing_queried_variables();
            }
            return;
        }

        let idx = ctx.push_elem(self.display, self.formatting);

        if let Some(prefix) = &self.affixes.prefix {
            ctx.push_str(prefix);
        }

        for (i, (persons, term)) in people.into_iter().enumerate() {
            let plural = persons.len() != 1;
            add_names(self, ctx, persons);

            if let Some(label) = &self.label {
                render_label_with_var(
                    label,
                    ctx,
                    ctx.term(term, label.form, plural).unwrap_or_default(),
                )
            }

            if i > 0 {
                if let Some(delim) = &self.delimiter {
                    ctx.push_str(delim);
                }
            }
        }

        if let Some(suffix) = &self.affixes.suffix {
            ctx.push_str(suffix);
        }

        ctx.pop_elem(idx);
    }
}

#[derive(Debug, Clone, Copy)]
enum EndDelim {
    Delim,
    And(NameAnd),
    DelimAnd(NameAnd),
}

// TODO differentiate subsequent cites.
fn add_names(names: &citationberg::Names, ctx: &mut Context, persons: Vec<&Person>) {
    let take =
        if persons.len() >= names.options.et_al_min.map_or(usize::MAX, |u| u as usize) {
            (names.options.et_al_use_first.map_or(usize::MAX, |u| u as usize))
                .min(persons.len())
        } else {
            persons.len()
        };

    let has_et_al = persons.len() > take;
    let et_al_use_last =
        names.options.et_al_use_last.unwrap_or_default() && take + 2 <= persons.len();
    let mut last_inverted = false;
    let reverse = matches!(
        ctx.settings.demote_non_dropping_particle,
        DemoteNonDroppingParticle::DisplayAndSort
    );

    for (i, name) in persons.iter().take(take).enumerate() {
        let last = i + 1 == take;

        if i != 0 {
            let mut delim = EndDelim::Delim;
            if last && i > 0 && !has_et_al {
                if let Some(d) = names.options.and {
                    delim =
                        match names.options.delimiter_precedes_last.unwrap_or_default() {
                            DelimiterBehavior::Contextual if i >= 2 => {
                                EndDelim::DelimAnd(d)
                            }
                            DelimiterBehavior::AfterInvertedName if last_inverted => {
                                EndDelim::DelimAnd(d)
                            }
                            DelimiterBehavior::Always => EndDelim::DelimAnd(d),
                            _ => EndDelim::And(d),
                        }
                }
            }

            match delim {
                EndDelim::Delim => ctx.push_str(&names.name.delimiter),
                EndDelim::And(and) => {
                    ctx.push_str(" ");
                    ctx.push_str(match and {
                        NameAnd::Text => ctx
                            .term(Term::Other(OtherTerm::And), TermForm::default(), false)
                            .unwrap_or_default(),
                        NameAnd::Symbol => "&",
                    });
                    ctx.push_str(" ");
                }
                EndDelim::DelimAnd(and) => {
                    ctx.push_str(&names.name.delimiter);
                    ctx.push_str(match and {
                        NameAnd::Text => ctx
                            .term(Term::Other(OtherTerm::And), TermForm::default(), false)
                            .unwrap_or_default(),
                        NameAnd::Symbol => "&",
                    });
                    ctx.push_str(" ");
                }
            }
        }

        let inverted = match names.options.name_as_sort_order {
            Some(NameAsSortOrder::First) if i == 0 => true,
            Some(NameAsSortOrder::All) => true,
            _ => false,
        };

        write_name(
            name,
            ctx,
            names.name.form == NameForm::Long,
            inverted,
            reverse,
            names,
        );

        last_inverted = inverted;
    }

    if et_al_use_last {
        if let Some(name) = persons.last() {
            ctx.push_str(&names.name.delimiter);
            ctx.push_str("… ");
            write_name(
                name,
                ctx,
                names.name.form == NameForm::Long,
                matches!(names.options.name_as_sort_order, Some(NameAsSortOrder::All)),
                reverse,
                names,
            );
        }
    } else if has_et_al {
        if let Some(term) = ctx.term(names.et_al.term.into(), TermForm::default(), false)
        {
            let delim = match names.options.delimiter_precedes_et_al {
                Some(DelimiterBehavior::Always) => true,
                Some(DelimiterBehavior::Contextual) if take >= 2 => true,
                Some(DelimiterBehavior::AfterInvertedName) if last_inverted => true,
                _ => false,
            };

            if delim {
                ctx.push_str(&names.name.delimiter);
            }

            let idx = ctx.push_format(names.et_al.formatting);
            ctx.push_str(term);
            ctx.pop_format(idx);
        }
    }
}

fn write_name(
    name: &Person,
    ctx: &mut Context,
    long: bool,
    reverse: bool,
    demote_non_dropping: bool,
    names: &citationberg::Names,
) {
    let hyphen_init = ctx.settings.initialize_with_hyphen;
    let initialize = names.options.initialize.unwrap_or(true);
    let initialize_with = names.options.initialize_with.as_deref();
    let sort_sep = names.options.sort_separator.as_deref().unwrap_or(", ");

    let first_part = names.name.name_part_given();
    let family_part = names.name.name_part_family();
    let first_format = first_part.map(|p| p.formatting).unwrap_or_default();
    let first_case = first_part.map(|p| p.text_case).unwrap_or_default();
    let first_affixes = [
        first_part.map(|p| &p.affixes).and_then(|f| f.prefix.as_ref()),
        first_part.map(|p| &p.affixes).and_then(|f| f.suffix.as_ref()),
    ];
    let family_format = family_part.map(|p| p.formatting).unwrap_or_default();
    let family_case = family_part.map(|p| p.text_case).unwrap_or_default();
    let family_affixes = [
        family_part.map(|p| &p.affixes).and_then(|f| f.prefix.as_ref()),
        family_part.map(|p| &p.affixes).and_then(|f| f.suffix.as_ref()),
    ];

    let first_name = |ctx: &mut Context| {
        if let Some(first) = &name.given_name {
            if let Some(initialize_with) = initialize_with {
                if initialize {
                    name.initials(ctx, Some(initialize_with), hyphen_init).unwrap();
                } else {
                    name.first_name_with_delimiter(ctx, Some(initialize_with)).unwrap();
                }
            } else {
                ctx.push_str(first);
            }

            true
        } else {
            false
        }
    };

    let simple = |ctx: &mut Context| {
        let idx = ctx.push_format(family_format);
        let cidx = ctx.push_case(family_case);
        if let Some(prefix) = family_affixes[0] {
            ctx.push_str(prefix);
        }
        ctx.push_str(&name.name);
        ctx.pop_case(cidx);
        ctx.pop_format(idx);
        if let Some(suffix) = family_affixes[1] {
            ctx.push_str(suffix);
        }
    };

    match (long, reverse, demote_non_dropping) {
        _ if name.is_institutional() => simple(ctx),
        (true, _, _) if name.is_cjk() => {
            let idx = ctx.push_format(family_format);
            if let Some(prefix) = family_affixes[0] {
                ctx.push_str(prefix);
            }
            ctx.push_str(&name.name);
            ctx.pop_format(idx);
            if let Some(suffix) = family_affixes[1] {
                ctx.push_str(suffix);
            }

            if let Some(given) = &name.given_name {
                let idx = ctx.push_format(first_format);
                if let Some(prefix) = first_affixes[0] {
                    ctx.push_str(prefix);
                }

                ctx.push_str(given);
                ctx.pop_format(idx);

                if let Some(suffix) = first_affixes[1] {
                    ctx.push_str(suffix);
                }
            }
        }
        (true, false, _) => {
            let idx = ctx.push_format(first_format);
            let cidx = ctx.push_case(first_case);

            if let Some(prefix) = first_affixes[0] {
                ctx.push_str(prefix);
            }

            first_name(ctx);
            ctx.ensure_space();
            if let Some(prefix) = &name.prefix {
                ctx.push_str(prefix);
            }

            ctx.pop_format(idx);
            ctx.pop_case(cidx);

            if let Some(suffix) = first_affixes[1] {
                ctx.push_str(suffix);
            }

            ctx.ensure_space();
            let idx = ctx.push_format(family_format);
            let cidx = ctx.push_case(family_case);

            if let Some(prefix) = family_affixes[0] {
                ctx.push_str(prefix);
            }

            ctx.push_str(&name.name);

            ctx.pop_case(cidx);
            ctx.pop_format(idx);

            if let Some(suffix) = &name.suffix {
                ctx.ensure_space();
                ctx.push_str(suffix);
            }

            if let Some(suffix) = family_affixes[1] {
                ctx.push_str(suffix);
            }
        }
        (true, true, false) => {
            let idx = ctx.push_format(family_format);
            let cidx = ctx.push_case(family_case);

            if let Some(prefix) = family_affixes[0] {
                ctx.push_str(prefix);
            }

            ctx.push_str(&name.name);

            ctx.pop_case(cidx);
            ctx.pop_format(idx);

            if let Some(suffix) = family_affixes[1] {
                ctx.push_str(suffix);
            }

            if name.given_name.is_some() {
                ctx.push_str(sort_sep);
                ctx.ensure_space();

                let idx = ctx.push_format(first_format);
                let cidx = ctx.push_case(first_case);

                if let Some(prefix) = first_affixes[0] {
                    ctx.push_str(prefix);
                }

                first_name(ctx);

                if let Some(prefix) = &name.prefix {
                    ctx.ensure_space();
                    ctx.push_str(prefix);
                }

                ctx.pop_case(cidx);
                ctx.pop_format(idx);

                if let Some(suffix) = first_affixes[1] {
                    ctx.push_str(suffix);
                }
            }

            if let Some(suffix) = &name.suffix {
                ctx.push_str(sort_sep);
                ctx.ensure_space();
                ctx.push_str(suffix);
            }
        }
        (true, true, true) => {
            let idx = ctx.push_format(family_format);
            let cidx = ctx.push_case(family_case);

            if let Some(prefix) = family_affixes[0] {
                ctx.push_str(prefix);
            }

            ctx.push_str(name.name_without_particle());

            ctx.pop_case(cidx);
            ctx.pop_format(idx);

            if let Some(suffix) = family_affixes[1] {
                ctx.push_str(suffix);
            }

            if name.given_name.is_some() {
                ctx.push_str(sort_sep);
                ctx.ensure_space();

                let idx = ctx.push_format(first_format);
                let cidx = ctx.push_case(first_case);

                if let Some(prefix) = first_affixes[0] {
                    ctx.push_str(prefix);
                }

                first_name(ctx);

                if let Some(prefix) = &name.prefix {
                    ctx.ensure_space();
                    ctx.push_str(prefix);
                }

                ctx.pop_case(cidx);
                ctx.pop_format(idx);

                if let Some(particle) = &name.name_particle() {
                    ctx.ensure_space();
                    ctx.push_str(particle);
                }

                if let Some(suffix) = first_affixes[1] {
                    ctx.push_str(suffix);
                }
            }

            if let Some(suffix) = &name.suffix {
                ctx.push_str(sort_sep);
                ctx.ensure_space();
                ctx.push_str(suffix);
            }
        }
        (false, _, _) => {
            simple(ctx);
        }
    }
}

impl RenderCsl for citationberg::Choose {
    fn render(&self, ctx: &mut Context) {
        for branch in self.branches() {
            if branch.match_.test(BranchConditionIter::from_branch(branch, ctx)) {
                render_with_delimiter(&branch.children, self.delimiter.as_deref(), ctx);
                return;
            }
        }

        if let Some(fallthrough) = &self.otherwise {
            render_with_delimiter(&fallthrough.children, self.delimiter.as_deref(), ctx);
        }
    }
}

fn render_with_delimiter(
    children: &[LayoutRenderingElement],
    delimiter: Option<&str>,
    ctx: &mut Context,
) {
    let mut last_empty = true;

    for child in children {
        if !last_empty {
            if let Some(delim) = delimiter {
                ctx.push_str(delim);
            }
        }

        let pos = ctx.len();
        match child {
            LayoutRenderingElement::Text(text) => text.render(ctx),
            LayoutRenderingElement::Number(num) => num.render(ctx),
            LayoutRenderingElement::Label(label) => label.render(ctx),
            LayoutRenderingElement::Date(date) => date.render(ctx),
            LayoutRenderingElement::Names(names) => names.render(ctx),
            LayoutRenderingElement::Choose(choose) => choose.render(ctx),
            LayoutRenderingElement::Group(_group) => _group.render(ctx),
        }

        last_empty = pos == ctx.len();
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BranchConditionPos {
    Disambiguate,
    IsNumeric,
    IsUncertainDate,
    Locator,
    Position,
    Type,
    Variable,
}

impl Iterator for BranchConditionPos {
    type Item = Self;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Disambiguate => {
                *self = Self::IsNumeric;
                Some(Self::Disambiguate)
            }
            Self::IsNumeric => {
                *self = Self::IsUncertainDate;
                Some(Self::IsNumeric)
            }
            Self::IsUncertainDate => {
                *self = Self::Locator;
                Some(Self::IsUncertainDate)
            }
            Self::Locator => {
                *self = Self::Position;
                Some(Self::Locator)
            }
            Self::Position => {
                *self = Self::Type;
                Some(Self::Position)
            }
            Self::Type => {
                *self = Self::Variable;
                Some(Self::Type)
            }
            Self::Variable => None,
        }
    }
}

struct BranchConditionIter<'a, 'b> {
    cond: &'a ChooseBranch,
    ctx: &'a mut Context<'b>,
    pos: BranchConditionPos,
    idx: usize,
}

impl<'a, 'b> BranchConditionIter<'a, 'b> {
    fn from_branch(cond: &'a ChooseBranch, ctx: &'a mut Context<'b>) -> Self {
        Self {
            cond,
            ctx,
            pos: BranchConditionPos::Disambiguate,
            idx: 0,
        }
    }

    fn next_case(&mut self) {
        self.pos.next();
        self.idx = 0;
    }
}

impl<'a, 'b> Iterator for BranchConditionIter<'a, 'b> {
    type Item = bool;

    fn next(&mut self) -> Option<Self::Item> {
        match self.pos {
            BranchConditionPos::Disambiguate => {
                self.pos.next();
                if let Some(d) = self.cond.disambiguate {
                    Some(d == self.ctx.cite_props.map_or(false, |p| p.is_disambiguation))
                } else {
                    self.next()
                }
            }
            BranchConditionPos::IsNumeric => {
                if let Some(vars) = &self.cond.is_numeric {
                    if self.idx >= vars.len() {
                        self.next_case();
                        return self.next();
                    }

                    let var = vars[self.idx];
                    self.idx += 1;

                    Some(match var {
                        Variable::Standard(var) => self
                            .ctx
                            .resolve_standard_variable(LongShortForm::default(), var)
                            .map(|v| TryInto::<Numeric>::try_into(v).is_ok())
                            .unwrap_or_default(),
                        Variable::Number(var) => matches!(
                            self.ctx.resolve_number_variable(var),
                            Some(MaybeTyped::Typed(_))
                        ),
                        _ => false,
                    })
                } else {
                    self.next_case();
                    self.next()
                }
            }
            BranchConditionPos::IsUncertainDate => {
                if let Some(vars) = &self.cond.is_uncertain_date {
                    if self.idx >= vars.len() {
                        self.next_case();
                        return self.next();
                    }

                    let var = vars[self.idx];
                    self.idx += 1;

                    Some(
                        self.ctx
                            .resolve_date_variable(var)
                            .map_or(false, |d| d.approximate),
                    )
                } else {
                    self.next_case();
                    self.next()
                }
            }
            BranchConditionPos::Locator => {
                if let Some(locs) = &self.cond.locator {
                    if self.idx >= locs.len() {
                        self.next_case();
                        return self.next();
                    }

                    let loc = locs[self.idx];
                    self.idx += 1;

                    Some(
                        self.ctx
                            .cite_props
                            .and_then(|c| c.locator)
                            .map(|l| l.0)
                            .map_or(false, |l| l == loc),
                    )
                } else {
                    self.next_case();
                    self.next()
                }
            }
            BranchConditionPos::Position => {
                if let Some(pos) = &self.cond.position {
                    if self.idx >= pos.len() {
                        self.next_case();
                        return self.next();
                    }

                    let spec_pos = pos[self.idx];
                    self.idx += 1;

                    let Some(props) = self.ctx.cite_props else {
                        self.next_case();
                        return Some(false);
                    };

                    Some(match spec_pos {
                        TestPosition::First => props.is_first,
                        TestPosition::Subsequent => !props.is_first,
                        TestPosition::Ibid => props.is_ibid,
                        TestPosition::IbidWithLocator => {
                            props.is_ibid || props.is_ibid_with_locator
                        }
                        TestPosition::NearNote => props.is_near_note,
                    })
                } else {
                    self.next_case();
                    self.next()
                }
            }
            BranchConditionPos::Type => {
                if let Some(kind) = &self.cond.type_ {
                    if self.idx >= kind.len() {
                        self.next_case();
                        return self.next();
                    }

                    let kind = kind[self.idx];
                    self.idx += 1;

                    Some(matches_entry_type(self.ctx.entry, kind))
                } else {
                    self.next_case();
                    self.next()
                }
            }
            BranchConditionPos::Variable => {
                if let Some(vars) = &self.cond.variable {
                    if self.idx >= vars.len() {
                        return None;
                    }

                    let var = vars[self.idx];
                    self.idx += 1;

                    Some(match var {
                        Variable::Standard(s) => {
                            let val = self
                                .ctx
                                .resolve_standard_variable(LongShortForm::default(), s);
                            val.map_or(false, |s| !s.chars().all(char::is_whitespace))
                        }
                        Variable::Number(n) => {
                            let val = self.ctx.resolve_number_variable(n);
                            val.is_some()
                        }
                        Variable::Date(d) => self.ctx.resolve_date_variable(d).is_some(),
                        Variable::Name(n) => self
                            .ctx
                            .resolve_name_variable(n)
                            .map_or(false, |n| !n.is_empty()),
                    })
                } else {
                    None
                }
            }
        }
    }
}

impl RenderCsl for citationberg::Group {
    fn render(&self, ctx: &mut Context) {
        let remove_pos = ctx.deletable_len();
        let info = ctx.push_usage_info();
        let idx = ctx.push_elem(self.display, self.to_formatting());

        if let Some(prefix) = &self.prefix {
            ctx.push_str(prefix);
        }

        render_with_delimiter(&self.children, self.delimiter.as_deref(), ctx);

        if let Some(suffix) = &self.suffix {
            ctx.push_str(suffix);
        }

        ctx.pop_elem(idx);

        let info = ctx.pop_usage_info(info);
        if info.has_vars
            && (!info.has_non_empty_vars
                && !info.has_used_macros
                && !info.has_non_empty_group)
        {
            ctx.delete_with_loc(remove_pos);
        } else {
            ctx.printed_non_empty_group()
        }
    }
}

impl RenderCsl for citationberg::LayoutRenderingElement {
    fn render(&self, ctx: &mut Context) {
        match self {
            citationberg::LayoutRenderingElement::Text(text) => text.render(ctx),
            citationberg::LayoutRenderingElement::Number(num) => num.render(ctx),
            citationberg::LayoutRenderingElement::Label(label) => label.render(ctx),
            citationberg::LayoutRenderingElement::Date(date) => date.render(ctx),
            citationberg::LayoutRenderingElement::Names(names) => names.render(ctx),
            citationberg::LayoutRenderingElement::Choose(choose) => choose.render(ctx),
            citationberg::LayoutRenderingElement::Group(group) => group.render(ctx),
        }
    }
}

impl RenderCsl for citationberg::Layout {
    fn render(&self, ctx: &mut Context) {
        let fidx = ctx.push_format(self.to_formatting());

        if let Some(prefix) = &self.prefix {
            ctx.push_str(prefix);
        }

        render_with_delimiter(&self.elements, self.delimiter.as_deref(), ctx);

        if let Some(suffix) = &self.suffix {
            ctx.push_str(suffix);
        }

        ctx.pop_format(fidx);
    }
}

impl RenderCsl for citationberg::RenderingElement {
    fn render(&self, ctx: &mut Context) {
        match self {
            citationberg::RenderingElement::Layout(l) => l.render(ctx),
            citationberg::RenderingElement::Other(o) => o.render(ctx),
        }
    }
}

impl From<TextCase> for Case {
    fn from(case: TextCase) -> Self {
        match case {
            TextCase::Uppercase => Case::Uppercase,
            TextCase::Lowercase => Case::Lowercase,
            TextCase::TitleCase => Case::Title(TitleCaseConf::default()),
            TextCase::SentenceCase => Case::Sentence(SentenceCaseConf::default()),
            TextCase::CapitalizeFirst => Case::FirstUpper,
            TextCase::CapitalizeAll => Case::AllUpper,
        }
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
