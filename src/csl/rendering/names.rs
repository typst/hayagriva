use std::borrow::Cow;
use std::cmp::Ordering;
use std::fmt::Write;

use citationberg::taxonomy::{NameVariable, OtherTerm, Term};
use citationberg::{
    DelimiterBehavior, DemoteNonDroppingParticle, LayoutRenderingElement, NameAnd,
    NameAsSortOrder, NameForm, NameLabelPosition, NameOptions, Names, ToAffixes,
    ToFormatting,
};
use citationberg::{DisambiguationRule, TermForm};

use crate::csl::taxonomy::EntryLike;
use crate::csl::{Context, DisambiguateState, ElemMeta, SpecialForm};
use crate::types::Person;

use super::{render_label_with_var, RenderCsl};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DisambiguatedNameForm {
    /// Count the names.
    Count,
    /// Print only the given name. Initialization is available.
    ShortInitialized,
    /// Print only the given name. Initialization is unavailable.
    ShortFull,
    /// Print the name with a first name initial.
    LongInitialized,
    /// Print the full name.
    LongFull,
}

impl DisambiguatedNameForm {
    fn from<T: EntryLike>(option: &citationberg::NameOptions, ctx: &Context<T>) -> Self {
        if ctx.instance.sorting {
            return Self::LongFull;
        }

        let form = option.form;

        if option.initialize_with.is_some() && option.initialize {
            if form == NameForm::Short {
                Self::ShortInitialized
            } else {
                Self::LongInitialized
            }
        } else if form == NameForm::Short {
            Self::ShortFull
        } else if form == NameForm::Count {
            Self::Count
        } else {
            Self::LongFull
        }
    }

    pub fn disambiguate(self, allow_full_first_name: bool) -> Option<Self> {
        match self {
            Self::ShortInitialized => Some(Self::LongInitialized),
            Self::LongInitialized if allow_full_first_name => Some(Self::LongFull),
            Self::LongInitialized => None,
            Self::ShortFull if allow_full_first_name => Some(Self::LongFull),
            Self::ShortFull => None,
            Self::LongFull => None,
            Self::Count => None,
        }
    }

    pub fn is_long(self) -> bool {
        matches!(self, Self::LongFull | Self::LongInitialized)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NameDisambiguationProperties {
    /// For each variable that this term renders, there is a list of persons.
    /// This list contains the current form of the name for each person or
    /// `None` if it was suppressed.
    name_forms: Vec<Vec<Option<DisambiguatedNameForm>>>,
    default_name_form: DisambiguatedNameForm,
    variables: Vec<NameVariable>,
}

impl NameDisambiguationProperties {
    /// Disambiguate the name further. Return none if the name cannot be
    /// disambiguated further.
    pub fn disambiguate(
        &mut self,
        may_upgrade: bool,
        rule: DisambiguationRule,
        add_names: bool,
    ) -> bool {
        let allow_full_first_name = rule.allows_full_first_names();

        for list in self.name_forms.iter_mut() {
            let mut idx = 0;

            if may_upgrade {
                // First try to step an item that is `Some`.
                for (i, form) in list.iter_mut().enumerate() {
                    if let Some(form) = form {
                        if let Some(new_form) = form.disambiguate(allow_full_first_name) {
                            *form = new_form;
                            return true;
                        }

                        if !rule.allows_multiple_names() {
                            return false;
                        }

                        idx = i;
                    }
                }
            }

            if add_names {
                // Process the remaining items by setting the default.
                for form in list[idx..].iter_mut() {
                    if form.is_none() {
                        *form = Some(self.default_name_form);
                        return true;
                    }
                }
            }
        }

        false
    }

    /// Disambuiguate a list of identical names.
    ///
    /// TODO: Use for disambiguation of identical names.
    #[allow(dead_code)]
    pub fn disambiguate_list(&mut self, variable: NameVariable, items: &[usize]) -> bool {
        let mut change = false;
        for &idx in items {
            let Some(outer) = self.variables.iter().position(|v| v == &variable) else {
                continue;
            };

            let form = &mut self.name_forms[outer][idx];
            if let Some(form) = form {
                if let Some(new_form) = form.disambiguate(true) {
                    *form = new_form;
                    change = true;
                }
            } else {
                *form = Some(self.default_name_form);
                change = true;
            }
        }

        change
    }

    /// Return the more disambiguated form of the name.
    pub fn max(self, other: Self) -> Self {
        let count_some = |x: &Self| {
            x.name_forms.iter().flatten().filter(|curr| curr.is_some()).count()
        };

        match count_some(&self).cmp(&count_some(&other)).then_with(|| {
            self.name_forms
                .iter()
                .flatten()
                .cmp(other.name_forms.iter().flatten())
        }) {
            Ordering::Greater => self,
            _ => other,
        }
    }
}

impl RenderCsl for Names {
    fn render<T: EntryLike>(&self, ctx: &mut Context<T>) {
        // Suppress this variable if we are in a special form.
        match &ctx.instance.kind {
            Some(SpecialForm::AuthorOnly) => {
                if self.variable.iter().all(|v| &NameVariable::Author != v) {
                    return;
                }
            }
            Some(SpecialForm::SuppressAuthor) => {
                if self.variable.iter().any(|v| &NameVariable::Author == v) {
                    return;
                }
            }
            None => {}
        }

        // The editor and translator variables need to be merged if they are
        // both present and identical.
        let people: Vec<(Vec<Cow<'_, Person>>, NameVariable)> = if self.variable.len()
            == 2
            && self.variable.contains(&NameVariable::Editor)
            && self.variable.contains(&NameVariable::Translator)
        {
            let editors = ctx.resolve_name_variable(NameVariable::Editor);
            let translators = ctx.resolve_name_variable(NameVariable::Translator);

            let mut res = Vec::new();
            if !editors.is_empty() && editors == translators {
                res.push((editors, NameVariable::EditorTranslator))
            } else {
                if !editors.is_empty() {
                    res.push((editors, NameVariable::Editor));
                }

                if !translators.is_empty() {
                    res.push((translators, NameVariable::Translator));
                }
            }

            res
        } else {
            self.variable
                .iter()
                .map(|v| (ctx.resolve_name_variable(*v), *v))
                .collect()
        };

        // Push to the name options stack.
        ctx.writing.push_name_options(&self.options());

        // Write the substitute if all variables are empty.
        let is_empty = people.iter().all(|(p, _)| p.is_empty());
        if is_empty {
            if let Some(substitute) = &self.substitute() {
                ctx.writing.start_suppressing_queried_variables();

                for child in &substitute.children {
                    let len = ctx.writing.len();
                    if let LayoutRenderingElement::Names(names_child) = child {
                        self.from_names_substitue(names_child).render(ctx)
                    } else {
                        child.render(ctx);
                    }
                    if len < ctx.writing.len() {
                        break;
                    }
                }

                ctx.writing.stop_suppressing_queried_variables();
            }

            ctx.writing.pop_name_options();
            return;
        }

        let depth = ctx.push_elem(self.to_formatting());
        let affix_loc = ctx.apply_prefix(&self.to_affixes());
        let cs_name = self.name().cloned().unwrap_or_default();
        let options = cs_name.options(ctx.writing.name_options.last());

        let default_form = DisambiguatedNameForm::from(&options, ctx);

        // Return here if we should only count the names.
        if default_form == DisambiguatedNameForm::Count {
            write!(ctx, "{}", people.into_iter().fold(0, |acc, curr| acc + curr.0.len()))
                .unwrap();
            ctx.apply_suffix(&self.to_affixes(), affix_loc);
            ctx.commit_elem(depth, self.display, Some(ElemMeta::Names));
            ctx.writing.pop_name_options();
            return;
        }

        // If we disambiguate, we need to copy the name forms and otherwise
        // compute them using the options.
        let props = if let DisambiguateState::NameDisambiguation(props) =
            &ctx.instance.cite_props.speculative.disambiguation
        {
            if props.variables.iter().eq(people.iter().map(|(_, v)| v)) {
                Some(props.clone())
            } else {
                None
            }
        } else {
            None
        }
        .unwrap_or_else(|| NameDisambiguationProperties {
            default_name_form: default_form,
            variables: self.variable.clone(),
            name_forms: people
                .iter()
                .map(|(p, _)| {
                    p.iter()
                        .enumerate()
                        .map(|(i, _)| {
                            if options.is_suppressed(i, p.len()) {
                                None
                            } else {
                                Some(default_form)
                            }
                        })
                        .collect()
                })
                .collect(),
        });

        for (i, ((persons, variable), forms)) in people
            .into_iter()
            .zip(props.name_forms.iter())
            .filter(|(p, _)| !p.0.is_empty())
            .enumerate()
        {
            let plural = persons.len() != 1;
            let label = self.label();
            let do_label = |requested_pos: NameLabelPosition,
                            ctx: &mut Context<'_, T>| {
                if !ctx.instance.sorting {
                    if let Some((label, pos)) = label {
                        if pos == requested_pos {
                            // if pos == NameLabelPosition::AfterName
                            //     && !label
                            //         .affixes
                            //         .prefix
                            //         .as_ref()
                            //         .map_or(false, |p| p.starts_with(' '))
                            // {
                            //     ctx.ensure_space();
                            // }

                            render_label_with_var(
                                label,
                                ctx,
                                ctx.term(variable.into(), label.form, plural)
                                    .unwrap_or_default(),
                            )
                        }
                    }
                }
            };

            do_label(NameLabelPosition::BeforeName, ctx);
            add_names(self, ctx, persons, &cs_name, forms, variable);
            do_label(NameLabelPosition::AfterName, ctx);

            if i > 0 {
                let delim = self.delimiter(ctx.writing.name_options.last());
                if !delim.is_empty() {
                    let delim = delim.to_string();
                    ctx.push_str(&delim);
                }
            }
        }

        // TODO Compare each elem with a name meta and run
        // [`NameDisambiguationProperties::disambiguate_list`] on the identical
        // pairs. Rerender if necessary.

        ctx.apply_suffix(&self.to_affixes(), affix_loc);
        ctx.commit_elem(depth, self.display, Some(ElemMeta::Names));
        ctx.writing.pop_name_options();
        ctx.writing.first_name_properties(|| props);
    }
}

#[derive(Debug, Clone, Copy)]
enum EndDelim {
    Delim,
    And(NameAnd),
    DelimAnd(NameAnd),
}

fn add_names<T: EntryLike>(
    names: &citationberg::Names,
    ctx: &mut Context<T>,
    persons: Vec<Cow<'_, Person>>,
    cs_name: &citationberg::Name,
    forms: &[Option<DisambiguatedNameForm>],
    variable: NameVariable,
) {
    let has_et_al = forms.iter().any(|f| f.is_none());
    let take = forms.iter().position(|f| f.is_none()).unwrap_or(persons.len());
    let names_opts = ctx.writing.name_options.last().clone();
    let name_opts = cs_name.options(&names_opts);
    let et_al_use_last = has_et_al.then(|| forms.last().copied().flatten()).flatten();
    let mut last_inverted = false;

    let demote_non_dropping = match ctx.style.csl.settings.demote_non_dropping_particle {
        DemoteNonDroppingParticle::Never => false,
        DemoteNonDroppingParticle::SortOnly => ctx.instance.sorting,
        DemoteNonDroppingParticle::DisplayAndSort => true,
    };

    let mut first = true;

    for (i, (name, form)) in persons.iter().take(take).zip(forms).enumerate() {
        let &Some(form) = form else { unreachable!("form is none") };
        let last = i + 1 == take;

        if !first {
            let mut delim = EndDelim::Delim;
            if last && !has_et_al {
                if let Some(d) = name_opts.and {
                    delim = match name_opts.delimiter_precedes_last {
                        DelimiterBehavior::Contextual if i >= 2 => EndDelim::DelimAnd(d),
                        DelimiterBehavior::AfterInvertedName if last_inverted => {
                            EndDelim::DelimAnd(d)
                        }
                        DelimiterBehavior::Always => EndDelim::DelimAnd(d),
                        _ => EndDelim::And(d),
                    }
                }
            }

            match delim {
                EndDelim::Delim => ctx.push_str(name_opts.delimiter),
                EndDelim::And(and) => {
                    ctx.push_str(" ");
                    ctx.push_str(match and {
                        NameAnd::Text => ctx
                            .term(Term::Other(OtherTerm::And), TermForm::default(), false)
                            .unwrap_or_default(),
                        NameAnd::Symbol => "&",
                    });
                    ctx.ensure_space();
                }
                EndDelim::DelimAnd(and) => {
                    ctx.push_str(name_opts.delimiter);
                    ctx.push_str(match and {
                        NameAnd::Text => ctx
                            .term(Term::Other(OtherTerm::And), TermForm::default(), false)
                            .unwrap_or_default(),
                        NameAnd::Symbol => "&",
                    });
                    ctx.ensure_space();
                }
            }
        }

        let reverse = match name_opts.name_as_sort_order {
            Some(NameAsSortOrder::First) if i == 0 => true,
            Some(NameAsSortOrder::All) => true,
            _ => false,
        };

        write_name(
            name,
            ctx,
            form,
            reverse,
            demote_non_dropping,
            cs_name,
            &name_opts,
            variable,
            i,
        );

        last_inverted = reverse;
        first = false;
    }

    if let Some(form) = et_al_use_last {
        if let Some(name) = persons.last() {
            ctx.push_str(name_opts.delimiter);
            ctx.push_str("… ");
            write_name(
                name,
                ctx,
                form,
                matches!(name_opts.name_as_sort_order, Some(NameAsSortOrder::All)),
                demote_non_dropping,
                cs_name,
                &name_opts,
                variable,
                persons.len() - 1,
            );
        }
    } else if has_et_al {
        let cs_et_al = names.et_al().cloned().unwrap_or_default();
        if let Some(term) = ctx.term(cs_et_al.term.into(), TermForm::default(), false) {
            let delim = match name_opts.delimiter_precedes_et_al {
                DelimiterBehavior::Always => true,
                DelimiterBehavior::Contextual if take >= 2 => true,
                DelimiterBehavior::AfterInvertedName if last_inverted => true,
                _ => false,
            };

            if delim {
                ctx.push_str(name_opts.delimiter);
            }

            let idx = ctx.push_format(cs_et_al.formatting);
            ctx.ensure_space();
            ctx.push_str(term);
            ctx.pop_format(idx);
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn write_name<T: EntryLike>(
    name: &Person,
    ctx: &mut Context<T>,
    form: DisambiguatedNameForm,
    reverse: bool,
    demote_non_dropping: bool,
    cs_name: &citationberg::Name,
    name_opts: &NameOptions,
    variable: NameVariable,
    name_idx: usize,
) {
    let hyphen_init = ctx.style.csl.settings.initialize_with_hyphen;
    let sort_sep = name_opts.sort_separator;

    let first_part = cs_name.name_part_given();
    let family_part = cs_name.name_part_family();
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

    let first_name = |ctx: &mut Context<T>| {
        if let Some(first) = &name.given_name {
            if let Some(initialize_with) = name_opts.initialize_with {
                if form == DisambiguatedNameForm::LongInitialized {
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

    let simple = |ctx: &mut Context<T>| {
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

    let reverse_keep_particle = |ctx: &mut Context<T>| {
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
    };

    let reverse_demote_particle = |ctx: &mut Context<T>| {
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
    };

    let elem_idx = ctx.push_elem(citationberg::Formatting::default());
    match (form.is_long(), reverse, demote_non_dropping) {
        _ if name.is_institutional() && ctx.instance.sorting => {
            let idx = ctx.push_format(family_format);
            let cidx = ctx.push_case(family_case);
            // TODO make locale aware
            ctx.push_str(name.name_without_article());
            ctx.pop_case(cidx);
            ctx.pop_format(idx);
        }
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
        // Always reverse when sorting.
        (true, _, false) if ctx.instance.sorting => reverse_keep_particle(ctx),
        (true, _, true) if ctx.instance.sorting => reverse_demote_particle(ctx),
        (true, true, false) => reverse_keep_particle(ctx),
        (true, true, true) => reverse_demote_particle(ctx),
        (true, false, _) => {
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
        (false, _, _) => {
            simple(ctx);
        }
    }
    ctx.commit_elem(elem_idx, None, Some(ElemMeta::Name(variable, name_idx)))
}
