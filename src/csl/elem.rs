use std::fmt;
use std::mem;
use std::num::NonZeroUsize;

use citationberg::taxonomy::NameVariable;
use citationberg::{
    Display, FontStyle, FontVariant, FontWeight, TextDecoration, VerticalAlign,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Elem {
    pub children: ElemChildren,
    pub display: Option<Display>,
    pub meta: Option<ElemMeta>,
}

impl Elem {
    pub(super) fn new(display: Option<Display>, meta: Option<ElemMeta>) -> Self {
        Self { children: ElemChildren::new(), display, meta }
    }

    pub(super) fn str_len(&self) -> usize {
        self.children
            .0
            .iter()
            .map(|c| match c {
                ElemChild::Text(t) => t.text.len(),
                ElemChild::Elem(e) => e.str_len(),
            })
            .sum()
    }

    pub(super) fn write_buf(
        &self,
        w: &mut impl fmt::Write,
        format: BufWriteFormat,
    ) -> Result<(), fmt::Error> {
        match (format, self.display) {
            (BufWriteFormat::HTML, Some(Display::Block)) => w.write_str("<div>")?,
            (BufWriteFormat::HTML, Some(Display::Indent)) => {
                w.write_str("<div style=\"padding-left: 4em;\">")?
            }
            (BufWriteFormat::HTML, Some(Display::LeftMargin)) => {
                w.write_str("<div style=\"float: left;\">")?
            }
            (BufWriteFormat::HTML, Some(Display::RightInline)) => {
                w.write_str("<div style=\"float: right; clear: both;\">")?
            }
            (_, Some(Display::Block)) => w.write_char('\n')?,
            (_, _) => {}
        }

        for child in &self.children.0 {
            child.write_buf(w, format)?;
        }

        match (format, self.display) {
            (BufWriteFormat::HTML, Some(_)) => w.write_str("</div>")?,
            (_, Some(Display::Block)) => w.write_char('\n')?,
            (_, _) => {}
        }

        Ok(())
    }

    pub(super) fn is_empty(&self) -> bool {
        if self.children.is_empty() {
            true
        } else {
            self.children.is_empty()
        }
    }

    pub(super) fn has_content(&self) -> bool {
        self.children.has_content()
    }

    pub(super) fn simplify(self) -> Self {
        Self { children: simplify_children(self.children), ..self }
    }

    pub(super) fn may_inline(&self) -> bool {
        self.display.is_none() && self.meta.is_none()
    }
}

/// Merge adjacent text nodes with the same formatting.
pub(super) fn simplify_children(children: ElemChildren) -> ElemChildren {
    ElemChildren(children.0.into_iter().fold(Vec::new(), |mut acc, child| {
        match (child, acc.last_mut()) {
            (ElemChild::Text(t), Some(ElemChild::Text(last)))
                if last.formatting == t.formatting =>
            {
                last.text.push_str(&t.text);
                return acc;
            }
            (ElemChild::Elem(e), _) => {
                acc.push(ElemChild::Elem(e.simplify()));
            }
            (child, _) => {
                acc.push(child);
            }
        }
        acc
    }))
}

/// Which CSL construct created an element.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ElemMeta {
    /// The element is the output of `cs:names`.
    Names,
    /// The element is the output of `cs:date`.
    Date,
    /// The element is the output of `cs:number (variable="citation-number")`.
    CitationNumber,
    /// The element is the output of a single name.
    /// It notes which name variable was used and the index within the variable.
    Name(NameVariable, usize),
}

/// A container for element children with useful methods.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct ElemChildren(pub(super) Vec<ElemChild>);

impl ElemChildren {
    /// Create an empty `ElemChildren`.
    pub(super) fn new() -> Self {
        Self(Vec::new())
    }

    /// Whether this container is empty.
    pub(super) fn is_empty(&self) -> bool {
        self.0.iter().all(|e| e.is_empty())
    }

    /// Whether this container has any content other than whitespace.
    pub(super) fn has_content(&self) -> bool {
        self.0.iter().any(|e| e.has_content())
    }

    /// Retrieve a reference to the first child with a matching meta by
    /// DFS.
    pub(super) fn get_meta(&self, meta: ElemMeta) -> Option<&Elem> {
        for child in &self.0 {
            match child {
                ElemChild::Elem(e) if e.meta == Some(meta) => return Some(e),
                ElemChild::Elem(e) => {
                    if let Some(e) = e.children.get_meta(meta) {
                        return Some(e);
                    }
                }
                _ => {}
            }
        }

        None
    }

    /// Retrieve a mutable reference to the first child with a matching meta by
    /// DFS.
    pub(super) fn get_mut_meta(&mut self, meta: ElemMeta) -> Option<&mut Elem> {
        for child in &mut self.0 {
            if let ElemChild::Elem(e) = child {
                if e.meta == Some(meta) {
                    return Some(e);
                }

                if let Some(e) = e.children.get_mut_meta(meta) {
                    return Some(e);
                }
            }
        }

        None
    }

    /// Write the children to the given buffer.
    pub fn write_buf(
        &self,
        w: &mut impl fmt::Write,
        format: BufWriteFormat,
    ) -> Result<(), fmt::Error> {
        for child in &self.0 {
            child.write_buf(w, format)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ElemChild {
    Text(Formatted),
    Elem(Elem),
}

impl ElemChild {
    pub(super) fn write_buf(
        &self,
        w: &mut impl fmt::Write,
        format: BufWriteFormat,
    ) -> Result<(), fmt::Error> {
        match self {
            ElemChild::Text(t) if format == BufWriteFormat::HTML => {
                let is_default = t.formatting == Formatting::default();
                if !is_default {
                    w.write_str("<span style=\"")?;
                    t.formatting.write_css(w)?;
                    w.write_str("\">")?;
                }
                w.write_str(&t.text)?;
                if !is_default {
                    w.write_str("</span>")?;
                }
                Ok(())
            }
            ElemChild::Text(t) if format == BufWriteFormat::VT100 => {
                t.formatting.write_vt100(w)?;
                w.write_str(&t.text)?;
                w.write_str("\x1b[0m")
            }
            ElemChild::Text(t) => w.write_str(&t.text),
            ElemChild::Elem(e) => e.write_buf(w, format),
        }
    }

    pub(super) fn str_len(&self) -> usize {
        match self {
            ElemChild::Text(t) => t.text.len(),
            ElemChild::Elem(e) => e.str_len(),
        }
    }

    pub(super) fn has_content(&self) -> bool {
        match self {
            ElemChild::Text(t) => t.text.chars().any(|c| !c.is_whitespace()),
            ElemChild::Elem(e) => e.has_content(),
        }
    }

    pub(super) fn is_empty(&self) -> bool {
        match self {
            ElemChild::Text(t) => t.text.is_empty(),
            ElemChild::Elem(e) => e.is_empty(),
        }
    }
}

impl From<Elem> for ElemChild {
    fn from(e: Elem) -> Self {
        ElemChild::Elem(e)
    }
}

impl From<Formatted> for ElemChild {
    fn from(f: Formatted) -> Self {
        ElemChild::Text(f)
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum BufWriteFormat {
    /// Just write text.
    #[default]
    Plain,
    /// Write with terminal colors.
    VT100,
    /// Write HTML.
    HTML,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Formatted {
    pub text: String,
    pub formatting: Formatting,
}

impl Formatted {
    fn new(text: String) -> Self {
        Self { text, formatting: Formatting::new() }
    }

    fn elem(self, display: Option<Display>, meta: Option<ElemMeta>) -> Elem {
        Elem {
            children: ElemChildren(vec![ElemChild::Text(self)]),
            display,
            meta,
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Formatting {
    pub font_style: FontStyle,
    pub font_variant: FontVariant,
    pub font_weight: FontWeight,
    pub text_decoration: TextDecoration,
    pub vertical_align: VerticalAlign,
}

impl Formatting {
    pub(super) fn new() -> Self {
        Self::default()
    }

    pub(super) fn add_text(self, text: String) -> Formatted {
        Formatted { text, formatting: self }
    }

    /// Apply a partial formatting on top of this formatting.
    pub(super) fn apply(mut self, other: citationberg::Formatting) -> Self {
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
    pub(super) fn will_change(&self, other: citationberg::Formatting) -> bool {
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

    pub(super) fn write_vt100(
        &self,
        buf: &mut impl fmt::Write,
    ) -> Result<(), fmt::Error> {
        if self.font_style == FontStyle::Italic {
            buf.write_str("\x1b[3m")?;
        }

        if self.font_weight == FontWeight::Bold {
            buf.write_str("\x1b[1m")?;
        } else if self.font_weight == FontWeight::Light {
            buf.write_str("\x1b[2m")?;
        }

        if self.text_decoration == TextDecoration::Underline {
            buf.write_str("\x1b[4m")?;
        }

        Ok(())
    }

    pub(super) fn write_css(&self, buf: &mut impl fmt::Write) -> Result<(), fmt::Error> {
        if self.font_style == FontStyle::Italic {
            buf.write_str("font-style: italic;")?;
        }

        match self.font_weight {
            FontWeight::Bold => buf.write_str("font-weight: bold;")?,
            FontWeight::Light => buf.write_str("font-weight: lighter;")?,
            _ => {}
        }

        if self.text_decoration == TextDecoration::Underline {
            buf.write_str("text-decoration: underline;")?;
        }

        if self.font_variant == FontVariant::SmallCaps {
            buf.write_str("font-variant: small-caps;")?;
        }

        match self.vertical_align {
            VerticalAlign::Sub => buf.write_str("vertical-align: sub;")?,
            VerticalAlign::Sup => buf.write_str("vertical-align: super;")?,
            _ => {}
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct NonEmptyStack<T> {
    head: Vec<T>,
    last: T,
}

impl<T> NonEmptyStack<T> {
    pub fn new(last: T) -> Self {
        Self { head: Vec::new(), last }
    }

    pub fn last(&self) -> &T {
        &self.last
    }

    pub fn last_mut(&mut self) -> &mut T {
        &mut self.last
    }

    pub fn len(&self) -> NonZeroUsize {
        NonZeroUsize::new(self.head.len() + 1).unwrap()
    }

    pub fn push(&mut self, elem: T) {
        self.head.push(mem::replace(&mut self.last, elem));
    }

    pub fn pop(&mut self) -> Option<T> {
        let new_last = self.head.pop()?;
        Some(mem::replace(&mut self.last, new_last))
    }

    pub fn get(&self, idx: usize) -> Option<&T> {
        if idx == self.head.len() {
            Some(&self.last)
        } else {
            self.head.get(idx)
        }
    }

    /// Drains all elements including and after the given index.
    pub fn drain(&mut self, idx: NonZeroUsize) -> impl Iterator<Item = T> + '_ {
        let idx = idx.get();
        mem::swap(&mut self.head[idx - 1], &mut self.last);
        let mut drain = self.head.drain(idx - 1..);
        let first = drain.next();
        drain.chain(first)
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.head.iter().chain(std::iter::once(&self.last))
    }

    pub fn finish(self) -> T {
        if !self.head.is_empty() {
            panic!("NonEmptyStack::finish called with non-empty stack")
        }

        self.last
    }
}

impl<T: Default> Default for NonEmptyStack<T> {
    fn default() -> Self {
        Self::new(T::default())
    }
}
