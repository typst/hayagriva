use std::borrow::Cow;
use std::fmt::Write;
use std::mem;
use std::num::NonZeroUsize;

use crate::csl::taxonomy::resolve_name_variable;
use crate::lang::{Case, CaseFolder, SentenceCaseConf, TitleCaseConf};
use crate::types::{Date, Person};
use crate::Entry;
use citationberg::taxonomy::NameVariable;
use citationberg::{
    taxonomy::{OtherTerm, Term, Variable},
    CslMacro, Display, FontStyle, FontVariant, FontWeight, Locale, LocaleCode, TermForm,
    TextDecoration, TextTarget, VerticalAlign,
};
use citationberg::{
    DateDayForm, DateForm, DateMonthForm, DatePartName, DateParts, DateStrongAnyForm,
    DelimiterBehavior, DemoteNonDroppingParticle, IndependentStyleSettings,
    LabelPluralize, LongShortForm, NameAnd, NameAsSortOrder, NameForm, Names,
    OrdinalLookup, TextCase,
};

mod taxonomy;
use taxonomy::{resolve_number_variable, resolve_standard_variable};

use self::taxonomy::{resolve_date_variable, MaybeTyped};

struct Context<'a> {
    /// The settings of the style.
    pub settings: &'a IndependentStyleSettings,
    /// The current entry.
    pub entry: &'a Entry,
    /// The buffer we're writing to. If block-level or formatting changes, we
    /// flush the buffer to the last [`Elem`] in the finished list.
    pub buf: CaseFolder,
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
    /// Whether to watch out for punctuation that should be pulled inside the
    /// preceeding quoted content.
    pull_punctuation: bool,
    /// Whether we should be using inner quotes.
    inner_quotes: bool,
    /// Whether to strip periods.
    strip_periods: bool,
    /// The case of the next text.
    case: Option<TextCase>,
}

impl<'a> Context<'a> {
    fn new(
        entry: &'a Entry,
        locale: LocaleCode,
        macros: &'a [CslMacro],
        style_locales: Vec<&'a Locale>,
        locale_file: Vec<&'a Locale>,
        root_format: citationberg::Formatting,
        settings: &'a IndependentStyleSettings,
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
            case: None,
            settings,
        }
    }

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

        let formatted = (*self.formatting()).add_text(mem::take(&mut self.buf).finish());
        self.elem_stack.last_mut().children.push(ElemChild::Text(formatted))
    }

    /// Push a format on top of the stack if it is not empty.
    fn push_format(&mut self, format: citationberg::Formatting) {
        if format.is_empty() {
            return;
        }

        self.save_to_block();
        self.format_stack.push(self.formatting().apply(format));
    }

    /// Pop a format from the stack if it is not empty.
    fn pop_format(&mut self, format: citationberg::Formatting) {
        if format.is_empty() {
            return;
        }

        self.save_to_block();
        self.format_stack.pop();
    }

    /// Push on the element stack if the current element has some [`Display`].
    /// Also apply formatting. Return the index of the current element, so that,
    /// at the end of writing, all following elements can be popped and added as
    /// children.
    fn push_elem(
        &mut self,
        display: Option<Display>,
        format: citationberg::Formatting,
    ) -> Option<usize> {
        let idx = match display {
            Some(_) => {
                self.save_to_block();
                self.elem_stack.push(Elem::new(display));
                Some(self.elem_stack.len().get() - 1)
            }
            None => None,
        };
        self.push_format(format);
        idx
    }

    /// Pop from the element stack if the current element has some [`Display`].
    /// Also pop formatting.
    fn pop_elem(&mut self, idx: Option<usize>, format: citationberg::Formatting) {
        self.pop_format(format);

        if let Some(idx) = idx {
            self.save_to_block();
            let mut children = self
                .elem_stack
                .drain(NonZeroUsize::new(idx + 1).unwrap())
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
            match self.case {
                None => self.buf.push_str(s),
                Some(case) => self.buf.config(case.into()),
            }
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

    fn get_macro(&self, name: &str) -> Option<&CslMacro> {
        self.macros.iter().find(|m| m.name == name)
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

    /// Set the case of the next text.
    fn set_case(&mut self, case: Option<TextCase>) {
        self.case = case;
    }

    /// Clear the case of the next text.
    fn clear_case(&mut self) {
        self.case = None;
    }

    fn len(&self) -> usize {
        self.buf.len() + self.elem_stack.iter().map(|e| e.str_len()).sum::<usize>()
    }
}

impl Write for Context<'_> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.push_str(s);
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct Elem {
    children: Vec<ElemChild>,
    display: Option<Display>,
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

#[derive(Debug, Clone)]
enum ElemChild {
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

#[derive(Debug, Default, Clone, Copy)]
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
        ctx.set_case(self.text_case);

        ctx.push_str(
            &match &self.target {
                TextTarget::Variable { var, form } => match var {
                    Variable::Standard(var) => {
                        resolve_standard_variable(ctx.entry, *form, *var)
                            .map(Cow::Borrowed)
                    }
                    Variable::Number(var) => {
                        match resolve_number_variable(ctx.entry, *var) {
                            Some(MaybeTyped::String(s)) => Some(Cow::Owned(s)),
                            Some(MaybeTyped::Typed(n)) => Some(Cow::Owned(n.to_string())),
                            None => None,
                        }
                    }
                    _ => None,
                },
                TextTarget::Macro { name } => {
                    let mac = ctx.get_macro(name);
                    todo!()
                }
                TextTarget::Term { term, form, plural } => {
                    ctx.term(*term, *form, *plural).map(|t| t.to_owned().into())
                }
                TextTarget::Value { val } => Some(Cow::Owned(val.clone())),
            }
            .unwrap_or_default(),
        );

        ctx.clear_case();
        ctx.stop_stripping_periods();

        if self.quotes {
            ctx.pop_quotes();
            ctx.may_pull_punctuation();
        }

        if let Some(suffix) = &self.affixes.suffix {
            ctx.push_str(suffix);
        }

        ctx.pop_elem(depth, self.formatting);
    }
}

impl RenderCsl for citationberg::Number {
    fn render(&self, ctx: &mut Context) {
        let depth = ctx.push_elem(self.display, self.formatting);

        if let Some(prefix) = &self.affixes.prefix {
            ctx.push_str(prefix);
        }

        ctx.set_case(self.text_case);

        let value = resolve_number_variable(ctx.entry, self.variable);
        match value {
            Some(MaybeTyped::Typed(num)) if num.will_transform() => {
                num.with_form(ctx, self.form, ctx.ordinal_lookup()).unwrap();
            }
            Some(MaybeTyped::Typed(num)) => write!(ctx, "{}", num).unwrap(),
            Some(MaybeTyped::String(s)) => ctx.push_str(&s),
            None => {}
        }

        ctx.clear_case();

        if let Some(suffix) = &self.affixes.suffix {
            ctx.push_str(suffix);
        }

        ctx.pop_elem(depth, self.formatting);
    }
}

impl RenderCsl for citationberg::Label {
    fn render(&self, ctx: &mut Context) {
        let Some(variable) = resolve_number_variable(ctx.entry, self.variable) else {
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
    ctx.push_format(label.formatting);

    let affixes = &label.affixes;
    if let Some(prefix) = &affixes.prefix {
        ctx.push_str(prefix);
    }

    ctx.may_strip_periods(label.strip_periods);
    ctx.set_case(label.text_case);

    ctx.push_str(content);

    ctx.clear_case();
    ctx.stop_stripping_periods();

    if let Some(suffix) = &affixes.suffix {
        ctx.push_str(suffix);
    }

    ctx.pop_format(label.formatting);
}

impl RenderCsl for citationberg::Date {
    fn render(&self, ctx: &mut Context) {
        let Some(variable) = self.variable else { return };
        let Some(date) = resolve_date_variable(ctx.entry, variable) else { return };

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

        ctx.set_case(self.text_case.or(base.and_then(|b| b.text_case)));

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

        ctx.clear_case();

        if let Some(suffix) = &self.affixes.suffix {
            ctx.push_str(suffix);
        }

        ctx.pop_elem(depth, formatting);
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

    ctx.push_format(formatting);

    let affixes = &date_part.affixes;

    if let Some(prefix) = &affixes.prefix {
        ctx.push_str(prefix);
    }

    ctx.set_case(over_ride.and_then(|o| o.text_case).or(date_part.text_case));

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

    ctx.clear_case();
    ctx.pop_format(formatting);
}

impl RenderCsl for Names {
    fn render(&self, ctx: &mut Context) {
        let people: Vec<(Vec<&Person>, Term)> = if self.variable.len() == 2
            && self.variable.contains(&NameVariable::Editor)
            && self.variable.contains(&NameVariable::Translator)
        {
            let editors = resolve_name_variable(ctx.entry, NameVariable::Editor);
            let translators = resolve_name_variable(ctx.entry, NameVariable::Translator);

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
                    (
                        resolve_name_variable(ctx.entry, *v).unwrap_or_default(),
                        Term::from(*v),
                    )
                })
                .collect()
        };

        let is_empty = people.iter().all(|(p, _)| p.is_empty());
        if is_empty {
            todo!("implement cs:substitute and don't do what happens below.")
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

        ctx.pop_elem(idx, self.formatting);
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

            ctx.push_format(names.et_al.formatting);
            ctx.push_str(term);
            ctx.pop_format(names.et_al.formatting);
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
        ctx.push_format(family_format);
        ctx.set_case(family_case);
        if let Some(prefix) = family_affixes[0] {
            ctx.push_str(prefix);
        }
        ctx.push_str(&name.name);
        ctx.clear_case();
        ctx.pop_format(family_format);
        if let Some(suffix) = family_affixes[1] {
            ctx.push_str(suffix);
        }
    };

    match (long, reverse, demote_non_dropping) {
        _ if name.is_institutional() => simple(ctx),
        (true, _, _) if name.is_cjk() => {
            ctx.push_format(family_format);
            if let Some(prefix) = family_affixes[0] {
                ctx.push_str(prefix);
            }
            ctx.push_str(&name.name);
            ctx.pop_format(family_format);
            if let Some(suffix) = family_affixes[1] {
                ctx.push_str(suffix);
            }

            if let Some(given) = &name.given_name {
                ctx.push_format(first_format);
                if let Some(prefix) = first_affixes[0] {
                    ctx.push_str(prefix);
                }

                ctx.push_str(given);
                ctx.pop_format(first_format);

                if let Some(suffix) = first_affixes[1] {
                    ctx.push_str(suffix);
                }
            }
        }
        (true, false, _) => {
            ctx.push_format(first_format);
            ctx.set_case(first_case);

            if let Some(prefix) = first_affixes[0] {
                ctx.push_str(prefix);
            }

            first_name(ctx);
            ctx.ensure_space();
            if let Some(prefix) = &name.prefix {
                ctx.push_str(prefix);
            }

            ctx.pop_format(first_format);
            ctx.clear_case();

            if let Some(suffix) = first_affixes[1] {
                ctx.push_str(suffix);
            }

            ctx.ensure_space();
            ctx.push_format(family_format);
            ctx.set_case(family_case);

            if let Some(prefix) = family_affixes[0] {
                ctx.push_str(prefix);
            }

            ctx.push_str(&name.name);

            ctx.clear_case();
            ctx.pop_format(family_format);

            if let Some(suffix) = &name.suffix {
                ctx.ensure_space();
                ctx.push_str(suffix);
            }

            if let Some(suffix) = family_affixes[1] {
                ctx.push_str(suffix);
            }
        }
        (true, true, false) => {
            ctx.push_format(family_format);
            ctx.set_case(family_case);

            if let Some(prefix) = family_affixes[0] {
                ctx.push_str(prefix);
            }

            ctx.push_str(&name.name);

            ctx.clear_case();
            ctx.pop_format(family_format);

            if let Some(suffix) = family_affixes[1] {
                ctx.push_str(suffix);
            }

            if name.given_name.is_some() {
                ctx.push_str(sort_sep);
                ctx.ensure_space();

                ctx.push_format(first_format);
                ctx.set_case(first_case);

                if let Some(prefix) = first_affixes[0] {
                    ctx.push_str(prefix);
                }

                first_name(ctx);

                if let Some(prefix) = &name.prefix {
                    ctx.ensure_space();
                    ctx.push_str(prefix);
                }

                ctx.clear_case();
                ctx.pop_format(first_format);

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
            ctx.push_format(family_format);
            ctx.set_case(family_case);

            if let Some(prefix) = family_affixes[0] {
                ctx.push_str(prefix);
            }

            ctx.push_str(name.name_without_particle());

            ctx.clear_case();
            ctx.pop_format(family_format);

            if let Some(suffix) = family_affixes[1] {
                ctx.push_str(suffix);
            }

            if name.given_name.is_some() {
                ctx.push_str(sort_sep);
                ctx.ensure_space();

                ctx.push_format(first_format);
                ctx.set_case(first_case);

                if let Some(prefix) = first_affixes[0] {
                    ctx.push_str(prefix);
                }

                first_name(ctx);

                if let Some(prefix) = &name.prefix {
                    ctx.ensure_space();
                    ctx.push_str(prefix);
                }

                ctx.clear_case();
                ctx.pop_format(first_format);

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
