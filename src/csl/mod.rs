use std::borrow::Cow;
use std::mem;
use std::{fmt::Write, ops::Deref};

use crate::lang::{Case, CaseFolder, SentenceCaseConf, TitleCaseConf};
use crate::Entry;
use citationberg::TextCase;
use citationberg::{
    taxonomy::{OtherTerm, Term, Variable},
    CslMacro, Display, FontStyle, FontVariant, FontWeight, Locale, LocaleCode, TermForm,
    TextDecoration, TextTarget, ToAffixes, VerticalAlign,
};

mod taxonomy;
use taxonomy::{resolve_number_variable, resolve_standard_variable};

use self::taxonomy::MaybeTyped;

struct Context<'a> {
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
    format_stack: Vec<Formatting>,
    /// Finished elements. The last element may be unfinished.
    elem_stack: Vec<Elem<String>>,
    /// Whether to watch out for punctuation that should be pulled inside the
    /// preceeding quoted content.
    pull_punctuation: bool,
    /// The depth of the quotation marks.
    quote_depth: u8,
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
    ) -> Self {
        Self {
            entry,
            buf: CaseFolder::new(),
            locale,
            macros,
            style_locales,
            locale_file,
            format_stack: vec![Formatting::default().apply(root_format)],
            elem_stack: vec![Elem::new(None)],
            pull_punctuation: false,
            quote_depth: 0,
            strip_periods: false,
            case: None,
        }
    }

    fn formatting(&self) -> &Formatting {
        self.format_stack.last().unwrap()
    }

    /// Saves the current buffer to the last element in the finished list. This
    /// must happen if [`Display`] or formatting changes, as well as when we're
    /// done with an element.
    fn save_to_block(&mut self) -> &mut Self {
        if self.buf.is_empty() {
            return self;
        }

        let formatted = (*self.formatting()).add_text(mem::take(&mut self.buf).finish());
        match self.elem_stack.last_mut() {
            Some(elem) => elem.children.push(ElemChild::Text(formatted)),
            None => panic!("elem stack is empty"),
        }

        self
    }

    /// Push a format on top of the stack if it is not empty.
    fn push_format(&mut self, format: citationberg::Formatting) -> &mut Self {
        if format.is_empty() {
            return self;
        }

        self.save_to_block();
        self.format_stack.push(self.formatting().apply(format));
        self
    }

    /// Pop a format from the stack if it is not empty.
    fn pop_format(&mut self, format: citationberg::Formatting) -> &mut Self {
        if format.is_empty() {
            return self;
        }

        self.save_to_block();
        self.format_stack.pop();
        if self.format_stack.is_empty() {
            panic!("formatting stack is empty");
        }

        self
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
                Some(self.elem_stack.len() - 1)
            }
            None => None,
        };
        self.push_format(format);
        idx
    }

    /// Pop from the element stack if the current element has some [`Display`].
    /// Also pop formatting.
    fn pop_elem(
        &mut self,
        idx: Option<usize>,
        format: citationberg::Formatting,
    ) -> &mut Self {
        self.pop_format(format);

        if let Some(idx) = idx {
            self.save_to_block();
            let mut children = self
                .elem_stack
                .drain(idx + 1..self.elem_stack.len())
                .map(ElemChild::Elem)
                .collect::<Vec<_>>();
            let elem = self.elem_stack.last_mut().unwrap();
            elem.children.append(&mut children);
        }

        self
    }

    /// Add the appropriate opening quotation marks.
    fn push_quotes(&mut self) {
        let mark = self.term(
            if self.quote_depth % 2 == 0 {
                OtherTerm::OpenQuote
            } else {
                OtherTerm::OpenInnerQuote
            }
            .into(),
            TermForm::default(),
            false,
        );

        if let Some(mark) = mark {
            self.write_str(mark).unwrap();
        }

        self.quote_depth = self.quote_depth.wrapping_add(1);
    }

    /// Add the appropriate closing quotation marks.
    fn pop_quotes(&mut self) {
        self.quote_depth = self.quote_depth.wrapping_sub(1);

        let mark = self.term(
            if self.quote_depth % 2 == 0 {
                OtherTerm::CloseQuote
            } else {
                OtherTerm::CloseInnerQuote
            }
            .into(),
            TermForm::default(),
            false,
        );

        if let Some(mark) = mark {
            self.write_str(&mark).unwrap();
        }
    }

    /// Folds all remaining elements into the first element and returns it.
    fn flush(mut self) -> Elem<String> {
        self.save_to_block();

        assert_eq!(
            self.format_stack.len(),
            1,
            "formatting stack is not empty but {}",
            self.format_stack.len()
        );

        let mut first = self.elem_stack.remove(0);
        for elem in self.elem_stack {
            first.children.push(ElemChild::Elem(elem));
        }

        first
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
                Some(if plural { term.single() } else { term.multiple() })
            }) {
                return localization;
            }

            form = current_form.fallback();
        }

        None
    }

    /// Check whether to do punctuation in quotes.
    fn punctuation_in_quotes(&self) -> bool {
        self.lookup_locale(|f| f.style_options?.punctuation_in_quote)
            .map(|b| *b.deref())
            .unwrap_or_default()
    }

    /// Pull the next punctuation character into the preceeding quoted content
    /// if appropriate for the locale.
    fn may_pull_punctuation(&mut self) -> &mut Self {
        self.pull_punctuation |= self.punctuation_in_quotes();
        self
    }

    /// Set whether to strip periods.
    fn may_strip_periods(&mut self, strip: bool) -> &mut Self {
        self.strip_periods = strip;
        self
    }

    /// Stop stripping periods.
    fn stop_stripping_periods(&mut self) -> &mut Self {
        self.strip_periods = false;
        self
    }

    /// Set the case of the next text.
    fn set_case(&mut self, case: Option<TextCase>) -> &mut Self {
        self.case = case;
        self
    }

    /// Clear the case of the next text.
    fn clear_case(&mut self) -> &mut Self {
        self.case = None;
        self
    }
}

impl Write for Context<'_> {
    fn write_str(&mut self, mut s: &str) -> std::fmt::Result {
        if self.pull_punctuation && s.starts_with(['.', ',']) {
            let close_quote =
                self.term(OtherTerm::CloseQuote.into(), TermForm::default(), false);
            let close_inner_quote =
                self.term(OtherTerm::CloseInnerQuote.into(), TermForm::default(), false);

            let buf = match self.elem_stack.last_mut() {
                Some(elem) if self.buf.is_empty() => match elem.children.last_mut() {
                    Some(ElemChild::Text(f)) => &mut f.text,
                    _ => self.buf.as_str_mut(),
                },
                _ => self.buf.as_str_mut(),
            };

            for quote in [close_quote, close_inner_quote].iter().flatten() {
                // Check if buf ends with a close quote.
                // If it does, replace it with the punctuation.
                if let Some(head) = buf.strip_suffix(quote) {
                    buf.truncate(head.len());

                    let punctuation = s.chars().next().unwrap();
                    s = &s[punctuation.len_utf8()..];
                    buf.write_char(punctuation)?;
                    buf.write_str(quote)?;
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
                    self.buf.write_char(c)?;
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
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct Elem<T>
where
    T: AsRef<str> + Clone,
{
    children: Vec<ElemChild<T>>,
    display: Option<Display>,
}

impl<T> Elem<T>
where
    T: AsRef<str> + Clone,
{
    fn new(display: Option<Display>) -> Self {
        Self { children: Vec::new(), display }
    }
}

#[derive(Debug, Clone)]
enum ElemChild<T>
where
    T: AsRef<str> + Clone,
{
    Text(Formatted<T>),
    Elem(Elem<T>),
}

#[derive(Debug, Clone)]
pub struct Formatted<T>
where
    T: AsRef<str> + Clone,
{
    pub text: T,
    pub formatting: Formatting,
}

impl<T> Formatted<T>
where
    T: AsRef<str> + Clone,
{
    fn new(text: T) -> Self {
        Self { text, formatting: Formatting::new() }
    }

    fn elem(self, display: Option<Display>) -> Elem<T> {
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

    fn add_text<T>(self, text: T) -> Formatted<T>
    where
        T: AsRef<str> + Clone,
    {
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

        let affixes = self.to_affixes();
        if let Some(prefix) = affixes.prefix {
            ctx.write_str(&prefix).unwrap();
        }

        if *self.quotes {
            ctx.push_quotes();
        }

        ctx.may_strip_periods(*self.strip_periods);
        ctx.set_case(self.text_case);

        ctx.write_str(
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
                    ctx.term(*term, *form, **plural).map(|t| t.to_owned().into())
                }
                TextTarget::Value { val } => Some(Cow::Owned(val.clone())),
            }
            .unwrap_or_default(),
        )
        .unwrap();

        ctx.clear_case();
        ctx.stop_stripping_periods();

        if *self.quotes {
            ctx.pop_quotes();
            ctx.may_pull_punctuation();
        }

        if let Some(suffix) = affixes.suffix {
            ctx.write_str(&suffix).unwrap();
        }

        ctx.pop_elem(depth, self.formatting);
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
