use core::fmt;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::hash_map::Entry as HmEntry;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::num::{NonZeroI16, NonZeroUsize};
use std::{mem, vec};

use citationberg::taxonomy::{Locator, OtherTerm, Term, Variable};
use citationberg::{
    taxonomy as csl_taxonomy, Affixes, Citation, CslMacro, Display, IndependentStyle,
    InheritableNameOptions, Locale, LocaleCode, RendersYearSuffix, SecondFieldAlign,
    StyleClass, TermForm, ToFormatting,
};
use citationberg::{DateForm, LongShortForm, OrdinalLookup, TextCase};
use indexmap::IndexSet;

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
use self::rendering::names::NameDisambiguationProperties;

/// This struct formats a set of citations according to a style.
#[derive(Debug, Default)]
pub struct BibliographyDriver<'a> {
    /// The citations we have seen so far.
    citations: Vec<CitationRequest<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct SpeculativeItemRender<'a> {
    rendered: ElemChildren,
    entry: &'a Entry,
    cite_props: CiteProperties<'a>,
    checked_disambiguate: bool,
    first_name: Option<NameDisambiguationProperties>,
}

#[derive(Debug, Hash, PartialEq, Eq)]
struct SpeculativeCiteRender<'a, 'b> {
    items: Vec<SpeculativeItemRender<'a>>,
    request: &'b CitationRequest<'a>,
}

impl<'a> BibliographyDriver<'a> {
    /// Create a new bibliography driver.
    pub fn new() -> Option<Self> {
        Some(Self::default())
    }

    /// Create a new citation with the given items. Bibliography-wide
    /// disambiguation will not be applied.
    pub fn instant_citation(&self, mut req: CitationRequest<'_>) -> ElemChildren {
        let style = req.style();
        style.sort(&mut req.items, style.csl.citation.sort.as_ref());
        let mut res = vec![];
        for item in req.items {
            res.push(
                style.citation(item.entry, CiteProperties::for_sorting(item.locator, 0)),
            );
        }

        let non_empty: Vec<_> = res.into_iter().filter(|c| c.has_content()).collect();
        // TODO in-cite disambiguation.

        let formatting =
            Formatting::default().apply(style.csl.citation.layout.to_formatting());

        if !non_empty.is_empty() {
            let mut res = if let Some(prefix) = style.csl.citation.layout.prefix.as_ref()
            {
                ElemChildren(vec![Formatted { text: prefix.clone(), formatting }.into()])
            } else {
                ElemChildren::new()
            };

            for (i, elem_children) in non_empty.into_iter().enumerate() {
                let first = i == 0;
                if !first {
                    res.0.push(
                        Formatted {
                            text: style
                                .csl
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

                res.0.extend(elem_children.0)
            }

            if let Some(suffix) = style.csl.citation.layout.suffix.as_ref() {
                res.0.push(Formatted { text: suffix.clone(), formatting }.into());
            }

            simplify_children(res)
        } else {
            ElemChildren::new()
        }
    }

    /// Create a new citation with the given items.
    pub fn citation(&mut self, mut req: CitationRequest<'a>) {
        let style = req.style();
        style.sort(&mut req.items, style.csl.citation.sort.as_ref());
        self.citations.push(req);
    }
}

/// Implementations for finishing the bibliography.
impl<'a> BibliographyDriver<'a> {
    /// Render the bibliography.
    pub fn finish(self, request: BibliographyRequest<'_>) -> Rendered {
        // 1.  Assign citation numbers by bibliography ordering or by citation
        //     order and render them a first time without their locators.
        let bib_style = request.style();

        // Only remember each entry once, even if it is cited multiple times.
        let mut entry_set = IndexSet::new();
        for req in self.citations.iter() {
            for item in req.items.iter() {
                entry_set.insert(item.entry);
            }
        }

        let mut entries: Vec<_> =
            entry_set.into_iter().map(CitationItem::from_entry).collect();
        bib_style.sort(
            &mut entries,
            bib_style.csl.bibliography.as_ref().and_then(|b| b.sort.as_ref()),
        );
        let citation_number = |item: &Entry| {
            entries.iter().position(|e| e.entry == item).expect("entry not found")
        };

        let mut seen: HashSet<&Entry> = HashSet::new();
        let mut res: Vec<SpeculativeCiteRender> = Vec::new();
        let mut last_cite: Option<&CitationItem> = None;

        for citation in &self.citations {
            let items = &citation.items;
            let style = citation.style();

            let mut renders: Vec<SpeculativeItemRender<'_>> = Vec::new();

            for item in items.iter() {
                let entry = &item.entry;

                let is_near_note = citation.note_number.map_or(false, |_| {
                    res.iter()
                        .rev()
                        .take(style.csl.citation.near_note_distance as usize)
                        .any(|cite| {
                            cite.request.note_number.is_some()
                                && cite.items.iter().any(|item| &item.entry == entry)
                        })
                });

                let first_note_number = citation.note_number.map(|n| {
                    res.iter()
                        .find_map(|cite| {
                            cite.request.note_number.filter(|_| {
                                cite.items.iter().any(|item| &item.entry == entry)
                            })
                        })
                        .unwrap_or(n)
                });

                let cite_props = CiteProperties {
                    certain: CertainCiteProperties {
                        note_number: citation.note_number,
                        first_note_number,
                        is_near_note,
                        is_first: seen.insert(entry),
                    },
                    speculative: SpeculativeCiteProperties::speculate(
                        None,
                        citation_number(entry),
                        IbidState::with_last(item, last_cite),
                    ),
                };

                let mut ctx = style.ctx(entry, cite_props.clone());
                ctx.writing.push_name_options(&style.csl.citation.name_options);
                style.csl.citation.layout.render(&mut ctx);
                renders.push(SpeculativeItemRender {
                    entry,
                    cite_props,
                    checked_disambiguate: ctx.writing.checked_disambiguate,
                    first_name: ctx.writing.first_name.clone(),
                    rendered: ctx.flush(),
                });

                last_cite = Some(item);
            }

            res.push(SpeculativeCiteRender { items: renders, request: citation });
        }

        // 2.  Disambiguate the citations.
        //
        // If we have set the disambiguation state for an item, we need to set
        // the same state for all entries referencing that item.
        for _ in 0..5 {
            let ambiguous = find_ambiguous_sets(&res);
            if ambiguous.is_empty() {
                break;
            }

            let mut rerender: HashMap<*const Entry, DisambiguateState> = HashMap::new();
            let mark = |map: &mut HashMap<*const Entry, DisambiguateState>,
                        entry: &Entry,
                        state: DisambiguateState| {
                map.entry(entry)
                    .and_modify(|e| *e = e.clone().max(state.clone()))
                    .or_insert(state);
            };

            for group in ambiguous.iter() {
                // 2a. Name Disambiguation loop
                if bib_style.csl.citation.disambiguate_add_givenname {
                    disambiguate_names(&res, group, |entry, state| {
                        mark(&mut rerender, entry, state)
                    });
                }

                // Do not try other methods if the previous method succeeded.
                if !rerender.is_empty() {
                    continue;
                }

                // 2b. Disambiguate by allowing `cs:choose` disambiguation.
                disambiguate_with_choose(&res, group, |entry, state| {
                    mark(&mut rerender, entry, state)
                });

                if !rerender.is_empty() {
                    continue;
                }

                // 2c. Disambiguate by year-suffix.
                disambiguate_year_suffix(&res, group, |entry, state| {
                    mark(&mut rerender, entry, state)
                });
            }

            if rerender.is_empty() {
                break;
            }

            for cite in res.iter_mut() {
                let style_ctx = cite.request.style();
                for item in cite.items.iter_mut() {
                    if let Some(state) = rerender.get(&(item.entry as _)) {
                        item.cite_props.speculative.disambiguation = state.clone();
                        item.rendered =
                            style_ctx.citation(item.entry, item.cite_props.clone());
                    }
                }
            }
        }

        for render in res.iter() {
            print!("{}", render.request.prefix().unwrap_or_default());
            let mut first = true;
            for item in render.items.iter() {
                if !first {
                    if let Some(delim) = &render.request.style.citation.layout.delimiter {
                        print!("{}", delim);
                    }
                }

                let mut buf = String::new();
                item.rendered.write_buf(&mut buf, BufWriteFormat::Plain).unwrap();
                print!("{}", buf);
                first = false;
            }
            println!("{}", render.request.suffix().unwrap_or_default());
        }

        // 3.  Group adjacent citations.
        // 3a. Collapse grouped citations.
        // 4.  Add affixes.

        todo!()
    }
}

type AmbiguousGroup = Vec<(usize, usize)>;

/// Progressively transform names to disambiguate them.
fn disambiguate_names<F>(
    renders: &[SpeculativeCiteRender<'_, '_>],
    group: &AmbiguousGroup,
    mut mark: F,
) where
    F: FnMut(&Entry, DisambiguateState),
{
    for &(cite_idx, item_idx) in group.iter() {
        let style = renders[cite_idx].request.style;
        let item = &renders[cite_idx].items[item_idx];

        if !item.cite_props.speculative.disambiguation.may_disambiguate_names() {
            continue;
        }

        if let Some(name_props) = &item.first_name {
            let mut name_props = name_props.clone();
            name_props.disambiguate(
                style.citation.givenname_disambiguation_rule,
                style.citation.disambiguate_add_names,
            );
            mark(item.entry, DisambiguateState::NameDisambiguation(name_props))
        }
    }
}

/// Mark qualifying entries for disambiguation with `cs:choose`.
fn disambiguate_with_choose<F>(
    renders: &[SpeculativeCiteRender<'_, '_>],
    group: &AmbiguousGroup,
    mut mark: F,
) where
    F: FnMut(&Entry, DisambiguateState),
{
    if group.iter().any(|&(cite_idx, item_idx)| {
        renders[cite_idx].items[item_idx].checked_disambiguate
            && renders[cite_idx].items[item_idx]
                .cite_props
                .speculative
                .disambiguation
                .may_disambiguate_with_choose()
    }) {
        // Do not set this for the first qualifying entry.
        let mut armed = false;

        for &(cite_idx, item_idx) in group.iter() {
            let item = &renders[cite_idx].items[item_idx];
            if item.checked_disambiguate {
                if armed {
                    mark(item.entry, DisambiguateState::Choose);
                } else {
                    armed = true;
                }
            }
        }
    }
}

/// Mark qualifying entries for disambiguation with year suffixes.
fn disambiguate_year_suffix<F>(
    renders: &[SpeculativeCiteRender<'_, '_>],
    group: &AmbiguousGroup,
    mut mark: F,
) where
    F: FnMut(&Entry, DisambiguateState),
{
    if group.iter().any(|&(cite_idx, item_idx)| {
        renders[cite_idx].request.style.citation.disambiguate_add_year_suffix
            && renders[cite_idx].items[item_idx]
                .cite_props
                .speculative
                .disambiguation
                .may_disambiguate_with_year_suffix()
    }) {
        let mut entries = Vec::new();
        for &(cite_idx, item_idx) in group.iter() {
            let item = &renders[cite_idx].items[item_idx];
            if item
                .cite_props
                .speculative
                .disambiguation
                .may_disambiguate_with_year_suffix()
                && !entries.contains(&item.entry)
            {
                entries.push(item.entry);
            }
        }

        // Assign year suffixes.
        for (i, entry) in entries.into_iter().enumerate() {
            mark(entry, DisambiguateState::YearSuffix(i as u8));
        }
    }
}

/// Return a vector of that contains every group of mutually ambiguous items
/// with cite and item index.
fn find_ambiguous_sets(cites: &[SpeculativeCiteRender]) -> Vec<AmbiguousGroup> {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum PotentialDisambiguation {
        /// Two usizes for an item that produced this string.
        Single((usize, usize)),
        /// There were multiple matches. This is the index in the result vector.
        Match(usize),
    }

    let mut map: HashMap<String, PotentialDisambiguation> = HashMap::new();
    let mut res: Vec<Vec<(usize, usize)>> = Vec::new();

    for (i, cite) in cites.iter().enumerate() {
        for (j, item) in cite.items.iter().enumerate() {
            if !item
                .cite_props
                .speculative
                .disambiguation
                .may_disambiguate_with_year_suffix()
            {
                // There is nothing we can do.
                continue;
            }

            let mut buf = String::new();
            item.rendered.write_buf(&mut buf, BufWriteFormat::Plain).unwrap();
            match map.entry(buf) {
                HmEntry::Occupied(entry) => match *entry.get() {
                    PotentialDisambiguation::Single(pos) => {
                        *entry.into_mut() = PotentialDisambiguation::Match(res.len());
                        res.push(vec![pos, (i, j)]);
                    }
                    PotentialDisambiguation::Match(idx) => {
                        res[idx].push((i, j));
                    }
                },
                HmEntry::Vacant(vacant) => {
                    vacant.insert(PotentialDisambiguation::Single((i, j)));
                }
            }
        }
    }

    // Only return items if not every item points to the same entry.
    res.into_iter()
        .filter(|e| {
            let Some(first_entry) =
                e.first().map(|(cite, item)| cites[*cite].items[*item].entry)
            else {
                return false;
            };

            e.iter()
                .any(|(cite, item)| cites[*cite].items[*item].entry != first_entry)
        })
        .collect()
}

#[derive(Debug, Clone)]
pub struct Rendered {
    /// The bibliography items.
    pub bibliography: RenderedBibliography,
    /// The citation items.
    pub citations: RenderedCitation,
}

#[derive(Debug, Clone)]
pub struct RenderedBibliography {
    /// Render the bibliography in a hanging indent.
    pub hanging_indent: bool,
    /// When set, the second field is aligned.
    pub second_field_align: Option<SecondFieldAlign>,
    /// The line spacing within the bibliography as a multiple of regular line spacing.
    pub line_spacing: NonZeroI16,
    /// Extra space between entries as a multiple of line height.
    pub entry_spacing: i16,
    /// The bibliography items.
    pub items: Vec<ElemChildren>,
}

impl RenderedBibliography {
    fn new(items: Vec<ElemChildren>, bibliography: &citationberg::Bibliography) -> Self {
        Self {
            hanging_indent: bibliography.hanging_indent,
            second_field_align: bibliography.second_field_align,
            line_spacing: bibliography.line_spacing,
            entry_spacing: bibliography.entry_spacing,
            items,
        }
    }
}

#[derive(Debug, Clone)]
pub struct RenderedCitation {
    pub note_number: Option<usize>,
    pub citation: ElemChildren,
}

/// A context that contains all information related to rendering a single entry.
#[derive(Debug, Clone, PartialEq)]
struct InstanceContext<'a> {
    // Entry-dependent data.
    /// The current entry.
    pub entry: &'a Entry,
    /// The position of this citation in the list of citations.
    pub cite_props: CiteProperties<'a>,
    /// Whether we are sorting or formatting right now.
    pub sorting: bool,
}

impl<'a> InstanceContext<'a> {
    fn new(entry: &'a Entry, cite_props: CiteProperties<'a>, sorting: bool) -> Self {
        Self { entry, cite_props, sorting }
    }

    fn sort_instance(item: &CitationItem<'a>, idx: usize) -> Self {
        Self::new(item.entry, CiteProperties::for_sorting(item.locator, idx), true)
    }
}

/// A context that contains information about the style we are using to render.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct StyleContext<'a> {
    // Settings from the style.
    /// The settings of the style.
    pub csl: &'a IndependentStyle,
    /// A list of locales defined in their respective locale file.
    locale_files: &'a [Locale],
    /// Which locale we're using.
    locale_override: Option<LocaleCode>,
}

impl<'a> StyleContext<'a> {
    fn new(
        style: &'a IndependentStyle,
        locale: Option<LocaleCode>,
        locale_files: &'a [Locale],
    ) -> Self {
        Self { csl: style, locale_files, locale_override: locale }
    }

    fn ctx<'b>(
        &'b self,
        entry: &'b Entry,
        cite_props: CiteProperties<'a>,
    ) -> Context<'b> {
        Context {
            instance: InstanceContext::new(entry, cite_props, false),
            style: self,
            writing: WritingContext::new(),
        }
    }

    fn sorting_ctx<'b>(&'b self, item: &'b CitationItem<'b>, idx: usize) -> Context<'b> {
        Context {
            instance: InstanceContext::new(
                item.entry,
                CiteProperties::for_sorting(item.locator, idx),
                true,
            ),
            style: self,
            writing: WritingContext::new(),
        }
    }

    /// Render the given item within a citation.
    fn citation(&self, entry: &Entry, props: CiteProperties<'a>) -> ElemChildren {
        let mut ctx = self.ctx(entry, props);
        ctx.writing.push_name_options(&self.csl.citation.name_options);
        self.csl.citation.layout.render(&mut ctx);
        ctx.flush()
    }

    /// Return the locale to use for this style.
    fn locale(&self) -> LocaleCode {
        self.locale_override
            .clone()
            .or_else(|| self.csl.default_locale.clone())
            .unwrap_or_else(LocaleCode::en_us)
    }

    /// Checks that neither the bibliography nor the citation layout render the
    /// year suffix.
    fn renders_year_suffix_implicitly(&self) -> bool {
        self.csl.citation.layout.renders_year_suffix(&self.csl.macros)
            == RendersYearSuffix::No
            && self
                .csl
                .bibliography
                .as_ref()
                .map(|b| b.layout.renders_year_suffix(&self.csl.macros))
                .unwrap_or_default()
                == RendersYearSuffix::No
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct CitationRequest<'a> {
    pub items: Vec<CitationItem<'a>>,
    style: &'a IndependentStyle,
    /// The requested locale for the style's terms.
    ///
    /// This is also the place to store a locale override from a dependent
    /// style.
    pub locale: Option<LocaleCode>,
    /// The files used to retrieve locale settings and terms if the style does
    /// not define all neccessary items.
    pub locale_files: &'a [Locale],
    /// The number to use for `first-reference-note-number`.
    ///
    /// `near-note` will always test false if this is none for the referenced
    /// note.
    note_number: Option<usize>,
    /// Whether to override the default affixes of the style with some brackets.
    pub affix_override: Option<Brackets>,
}

impl<'a> CitationRequest<'a> {
    pub fn new(
        items: Vec<CitationItem<'a>>,
        style: &'a IndependentStyle,
        locale: Option<LocaleCode>,
        locale_files: &'a [Locale],
        note_number: Option<usize>,
        affix_override: Option<Brackets>,
    ) -> Self {
        Self {
            items,
            style,
            locale,
            locale_files,
            note_number: note_number.filter(|_| style.settings.class == StyleClass::Note),
            affix_override,
        }
    }

    fn style(&self) -> StyleContext<'a> {
        StyleContext::new(self.style, self.locale.clone(), self.locale_files)
    }

    fn prefix(&self) -> Option<CharOrSlice<'a>> {
        if let Some(o) = self.affix_override {
            o.left().map(CharOrSlice::Char)
        } else {
            self.style.citation.layout.prefix.as_deref().map(CharOrSlice::Slice)
        }
    }

    fn suffix(&self) -> Option<CharOrSlice<'a>> {
        if let Some(o) = self.affix_override {
            o.right().map(CharOrSlice::Char)
        } else {
            self.style.citation.layout.suffix.as_deref().map(CharOrSlice::Slice)
        }
    }
}

enum CharOrSlice<'a> {
    Char(char),
    Slice(&'a str),
}

impl Default for CharOrSlice<'_> {
    fn default() -> Self {
        Self::Slice("")
    }
}

impl fmt::Display for CharOrSlice<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CharOrSlice::Char(c) => c.fmt(f),
            CharOrSlice::Slice(s) => s.fmt(f),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct BibliographyRequest<'a> {
    pub style: &'a IndependentStyle,
    /// The requested locale for the style's terms.
    ///
    /// This is also the place to store a locale override from a dependent
    /// style.
    pub locale: Option<LocaleCode>,
    /// The files used to retrieve locale settings and terms if the style does
    /// not define all neccessary items.
    pub locale_files: &'a [Locale],
}

impl<'a> BibliographyRequest<'a> {
    pub fn new(
        style: &'a IndependentStyle,
        locale: Option<LocaleCode>,
        locale_files: &'a [Locale],
    ) -> Self {
        Self { style, locale, locale_files }
    }

    fn style(&self) -> StyleContext<'a> {
        StyleContext::new(self.style, self.locale.clone(), self.locale_files)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CitationItem<'a> {
    /// The entry to format.
    pub entry: &'a Entry,
    /// The locator that specifies where in the entry the item is found.
    pub locator: Option<SpecificLocator<'a>>,
    /// A locale code that overrides the assumed locale of the entry. If this is
    /// none, this will default to [`CitationRequest::locale`] and then
    /// [`citationberg::Style::default_locale`].
    pub locale: Option<LocaleCode>,
    /// Whether this item will be included in the output.
    pub hide: bool,
    /// Format the item in a special way.
    pub kind: Option<SpecialForm>,
}

impl<'a> CitationItem<'a> {
    fn from_entry(entry: &'a Entry) -> Self {
        Self {
            entry,
            locator: None,
            locale: None,
            hide: false,
            kind: None,
        }
    }
}

impl<'a> StyleContext<'a> {
    /// Retrieve a macro.
    fn get_macro(&self, name: &str) -> Option<&'a CslMacro> {
        self.csl.macros.iter().find(|m| m.name == name)
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

        let locale = self.locale();
        let fallback = locale.fallback();
        let en_us = LocaleCode::en_us();

        for (i, resource) in [self.csl.locale.as_slice(), self.locale_files]
            .into_iter()
            .enumerate()
        {
            if let Some(output) = lookup(resource, Some(&locale)) {
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
    /// Whether this render has checked for `disambiguate` in `cs:choose`.
    checked_disambiguate: bool,
    /// The disambiguation-relevant properties of the first `cs:name` element.
    /// This is `None` if no `cs:name` elements were rendered.
    first_name: Option<NameDisambiguationProperties>,

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
        if (!self.buf.is_empty() || self.elem_stack.iter().any(|e| !e.is_empty()))
            && !self.buf.ends_with(' ')
        {
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

    /// Write the [`NameDisambiguationProperties`] that the first `cs:name`
    /// element used. Do not change if this is not the first `cs:name` element.
    /// Returns whether this was the first `cs:name` element.
    fn first_name_properties<F>(&mut self, f: F) -> bool
    where
        F: FnOnce() -> NameDisambiguationProperties,
    {
        if self.first_name.is_none() {
            self.first_name = Some(f());
            true
        } else {
            false
        }
    }
}

pub(crate) struct Context<'a> {
    instance: InstanceContext<'a>,
    style: &'a StyleContext<'a>,
    writing: WritingContext,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct CiteProperties<'a> {
    pub certain: CertainCiteProperties,
    pub speculative: SpeculativeCiteProperties<'a>,
}

impl<'a> CiteProperties<'a> {
    fn for_sorting(locator: Option<SpecificLocator<'a>>, citation_number: usize) -> Self {
        Self {
            certain: CertainCiteProperties::new(),
            speculative: SpeculativeCiteProperties::speculate(
                locator,
                citation_number,
                IbidState::Different,
            ),
        }
    }
}

/// Item properties that can be determined before the first render of a
/// citation or bibliography entry.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
struct CertainCiteProperties {
    /// The number of the footnote this citation appears in.
    ///
    /// We can determine this because it depends on citation order only. May be
    /// none if the current style is in-text.
    pub note_number: Option<usize>,
    /// The number of the first footnote this citation appears in.
    pub first_note_number: Option<usize>,
    /// Whether the item is within `near-note-distance` to the previous citation
    /// of the same item.
    pub is_near_note: bool,
    /// Whether this citation is the first citation of the entry.
    ///
    /// Only depends on insertion order. The entry cannot be switched with
    /// itself during cite grouping.
    pub is_first: bool,
}

impl CertainCiteProperties {
    fn new() -> Self {
        Self::default()
    }
}

/// Item properies that can only be determined after one or multiple renders.
/// These require validation.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct SpeculativeCiteProperties<'a> {
    /// Locator with its type.
    ///
    /// This will be none during the first render because it would interfere
    /// with disambiguation otherwise.
    pub locator: Option<SpecificLocator<'a>>,
    /// The position the item appears at in the bibliography.
    ///
    /// We can determine this using bibliography sort and cite order once we
    /// have seen all cites.
    pub citation_number: usize,
    /// Whether this citation is exactly the same as the previous citation.
    ///
    /// Although cite ordering is well-defined, grouping occurs only after the
    /// first render and can change the order.
    pub ibid: IbidState,
    /// Whether this citation is a disambiguation.
    ///
    /// Is always false for the first set of renders. Only the rendered entries
    /// allow us to determine if we need to disambiguate.
    pub disambiguation: DisambiguateState,
}

impl<'a> SpeculativeCiteProperties<'a> {
    /// We can guess about the [`IbidState`] under the assumption that neither
    /// this item nor its predecessors will be subject to cite grouping.
    fn speculate(
        locator: Option<SpecificLocator<'a>>,
        citation_number: usize,
        ibid: IbidState,
    ) -> Self {
        Self {
            locator,
            citation_number,
            ibid,
            disambiguation: DisambiguateState::None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum DisambiguateState {
    None,
    NameDisambiguation(NameDisambiguationProperties),
    Choose,
    YearSuffix(u8),
}

impl DisambiguateState {
    /// Name disambiguation can be iterated upon, so we can stay here.
    fn may_disambiguate_names(&self) -> bool {
        matches!(self, Self::None | Self::NameDisambiguation(_))
    }

    /// We can only disambiguate with choose once.
    fn may_disambiguate_with_choose(&self) -> bool {
        matches!(self, Self::None | Self::NameDisambiguation(_))
    }

    /// We can only disambiguate with year suffix once.
    fn may_disambiguate_with_year_suffix(&self) -> bool {
        matches!(self, Self::None | Self::NameDisambiguation(_) | Self::Choose)
    }

    /// Return the more advanced disambiguation state between the two.
    fn max(self, other: Self) -> Self {
        match (self, other) {
            (Self::None, other) => other,
            (self_, Self::None) => self_,
            (Self::NameDisambiguation(a), Self::NameDisambiguation(b)) => {
                Self::NameDisambiguation(a.max(b))
            }
            (Self::NameDisambiguation(_), other) => other,
            (self_, Self::NameDisambiguation(_)) => self_,
            (Self::Choose, other) => other,
            (self_, Self::Choose) => self_,
            (Self::YearSuffix(a), Self::YearSuffix(b)) => Self::YearSuffix(a.max(b)),
            (Self::YearSuffix(_), other) => other,
            (self_, Self::YearSuffix(_)) => self_,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IbidState {
    /// The previous cite referenced another entry.
    Different,
    /// The previous cite referenced the same entry, but with a different
    /// locator.
    IbidWithLocator,
    /// The previous cite referenced the same entry with the same locator.
    Ibid,
}

impl IbidState {
    fn is_ibid_with_locator(self) -> bool {
        matches!(self, Self::IbidWithLocator | Self::Ibid)
    }

    fn with_last(this: &CitationItem, last: Option<&CitationItem>) -> Self {
        if let Some(last) = last {
            if last.entry == this.entry {
                if last.locator == this.locator {
                    IbidState::Ibid
                } else {
                    IbidState::IbidWithLocator
                }
            } else {
                IbidState::Different
            }
        } else {
            IbidState::Different
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
        self.writing.usage_info.borrow_mut().last_mut().has_vars = true;
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
        self.writing.usage_info.borrow_mut().last_mut().has_vars = true;
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
        self.writing.usage_info.borrow_mut().last_mut().has_vars = true;
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
        self.writing.usage_info.borrow_mut().last_mut().has_vars = true;
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

    /// Test whether this entry should disambiguate via `cs:choose`.
    fn should_disambiguate(&mut self) -> bool {
        self.writing.checked_disambiguate = true;
        self.instance.cite_props.speculative.disambiguation == DisambiguateState::Choose
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

/// Describes the bracket preference of a citation style.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Brackets {
    /// "(...)" Parentheses.
    Round,
    /// "[...]" Brackets.
    Square,
    /// No brackets.
    None,
}

impl Brackets {
    /// Get the according left bracket.
    pub fn left(&self) -> Option<char> {
        match self {
            Brackets::Round => Some('('),
            Brackets::Square => Some('['),
            Brackets::None => None,
        }
    }

    /// Get the according right bracket.
    pub fn right(&self) -> Option<char> {
        match self {
            Brackets::Round => Some(')'),
            Brackets::Square => Some(']'),
            Brackets::None => None,
        }
    }
}

/// A special citation form to use for the [`CitationItem`].
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SpecialForm {
    AuthorOnly,
    SuppressAuthor,
    Composite(Option<ChunkedString>),
}

#[cfg(test)]
mod tests {
    use citationberg::LocaleFile;

    use crate::csl::elem::BufWriteFormat;
    use crate::io::from_yaml_str;

    use super::*;
    use std::fs;

    #[test]
    fn test_csl() {
        let style = fs::read_to_string("tests/art-history.csl").unwrap();
        let style = IndependentStyle::from_xml(&style).unwrap();

        let en_locale = fs::read_to_string("tests/locales-en-US.xml").unwrap();
        let en_locale = LocaleFile::from_xml(&en_locale).unwrap();

        let yaml = fs::read_to_string("tests/basic.yml").unwrap();
        let bib = from_yaml_str(&yaml).unwrap();
        let en_locale = [en_locale.into()];

        let mut driver = BibliographyDriver::new().unwrap();

        for n in (0..bib.len()).step_by(2) {
            let mut items = vec![
                CitationItem::from_entry(bib.nth(n).unwrap()),
                CitationItem::from_entry(bib.nth(n + 1).unwrap()),
            ];
            let citation = driver.citation(CitationRequest::new(
                items, &style, None, &en_locale, None, None,
            ));

            // driver.finish(BibliographyRequest {
            //     style: &style,
            //     locale: None,
            //     locale_files: &en_locale,
            // });
            // let mut buf = String::new();

            // for e in citation.0 {
            //     e.write_buf(&mut buf, BufWriteFormat::VT100).unwrap();
            // }

            // eprintln!("{}", buf);
        }

        driver.finish(BibliographyRequest {
            style: &style,
            locale: None,
            locale_files: &en_locale,
        });
    }
}
