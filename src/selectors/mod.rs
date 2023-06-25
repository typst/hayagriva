//! Filter bibliographies by structural selection.

/// Construct a [`Selector`].
///
/// Selectors can be used to filter bibliographies or to differentiate between
/// [`entries`](Entry) when implementing a custom [`CitationStyle`][CitStyle] or
/// [`BibliographyStyle`][BibStyle]. An [introduction to selectors][intro] is
/// available in the Git repository. This macro accepts expressions very similar
/// to the user-facing macros as parsed by [`Selector::parse`].
///
/// There are three main differences:
/// - Binding names and attributes have to be strings and thus wrapped in double
///   quotes
/// - Type names are case sensitive and have to start with a capital letter
///   since they are just variants of [`EntryType`][EType]
/// - Any non-atomic selector (`*` or an [`EntryType`][EType] variant are
///   atomic) in other constructions like ancestrage selectors or bindings must
///   be in parentheses since they have to parse as a single token tree for
///   correct expansion. If you get an error message along the lines of having
///   too much recursion, you likely forgot some parentheses.
///
/// This example illustrates some of the differences:
///
/// ```rust
/// use hayagriva::select;
/// use hayagriva::Selector;
///
/// // finds an article that is parented by a conference proceedings volume
/// assert_eq!(Selector::parse("article > proceedings").unwrap(), select!(Article > Proceedings));
///
/// // matches either a video or audio item or an artwork
/// assert_eq!(Selector::parse("video | audio | artwork").unwrap(), select!(Video | Audio | Artwork));
///
/// // matches anything that is parented by both a blog and a newspaper
/// assert_eq!(Selector::parse("* > (blog | newspaper)").unwrap(), select!(* > (Blog | Newspaper)));
///
/// // matches anything with a URL or a parent with a URL and binds the entry with the attribute to the variable `i`.
/// // Note that expressions like i:*[url] do not need parentheses in the parsed selector, but they do in the macro!
/// assert_eq!(Selector::parse("i:*[url] | (* > i:*[url])").unwrap(), select!(("i":(*["url"])) | (* > ("i":(*["url"])))));
/// ```
///
/// [intro]: https://github.com/typst/hayagriva/blob/main/docs/selectors.md
/// [CitStyle]: crate::style::CitationStyle
/// [BibStyle]: crate::style::BibliographyStyle
/// [EType]: crate::types::EntryType
#[macro_export]
macro_rules! select {
    (($($tts:tt)*)) => {
        select!($($tts)*)
    };

    (*) => {
        $crate::Selector::Wildcard
    };

    ($entry_type:ident) => {
        $crate::Selector::Entry($crate::types::EntryType::$entry_type)
    };

    ($binding:literal:$expr:tt) => {
        $crate::Selector::Binding(
            $binding.to_string(),
            Box::new(select!($expr)),
        )
    };

    ($expr:tt[$($attr:literal),* $(,)?]) => {
        $crate::Selector::Attr(
            Box::new(select!($expr)),
            vec![$($attr.to_string()),*],
        )
    };

    (!$expr:tt) => {
        $crate::Selector::Neg(Box::new(select!($expr)))
    };

    ($lhs:tt > $rhs:tt) => {
        $crate::Selector::Ancestrage(
            Box::new(select!($lhs)),
            Box::new(select!($rhs)),
        )
    };

    ($($expr:tt)|+) => {
        $crate::Selector::Alt(vec![$(select!($expr)),*])
    };

    ($($expr:tt)&+) => {
        $crate::Selector::Multi(vec![$(select!($expr)),*])
    };
}

mod parser;

use std::collections::HashMap;

use thiserror::Error;

use crate::types::EntryType;
use crate::Entry;

/// A selector used to filter bibliographies and match on entries.
#[derive(Debug, Clone, Eq, PartialEq)]
#[non_exhaustive]
pub enum Selector {
    /// A wildcard selector: `*`.
    Wildcard,
    /// An entry type literal: `report`.
    Entry(EntryType),
    /// A negation: `!x`.
    Neg(Box<Self>),
    /// A binding: `x:misc`.
    Binding(String, Box<Self>),
    /// An attribute filtering: `abc[att1, attr2]`.
    Attr(Box<Self>, Vec<String>),
    /// An alternate selector: `a | b`.
    Alt(Vec<Self>),
    /// A multi-parent selector: `a & b`.
    Multi(Vec<Self>),
    /// An ancestrage selector: `a > b`.
    Ancestrage(Box<Self>, Box<Self>),
}

impl Selector {
    /// Parse a selector from a string.
    pub fn parse(src: &str) -> SelectorResult<Self> {
        parser::parse(src)
    }

    /// Checks if the selector matches the provided [`Entry`].
    pub fn matches(&self, entry: &Entry) -> bool {
        self.apply(entry).is_some()
    }

    /// Applies the selector and returns the bound element if there was a match.
    ///
    /// This can panic if there are resolving entries which do not bind the
    /// argument.
    pub(crate) fn bound<'s>(&self, entry: &'s Entry, bound: &str) -> Option<&'s Entry> {
        self.apply(entry).map(|mut hm| hm.remove(bound).unwrap())
    }

    /// Applies the selector to an [`Entry`] and returns the bound variables
    /// in a hash map if there was a match.
    pub fn apply<'s>(&self, entry: &'s Entry) -> Option<HashMap<String, &'s Entry>> {
        match self {
            Self::Wildcard => Some(HashMap::new()),

            Self::Entry(entry_type) => {
                if &entry.entry_type == entry_type {
                    Some(HashMap::new())
                } else {
                    None
                }
            }

            Self::Neg(expr) => {
                if expr.apply(entry).is_some() {
                    None
                } else {
                    Some(HashMap::new())
                }
            }

            Self::Binding(binding, expr) => expr.apply(entry).map(|mut bound| {
                bound.insert(binding.to_string(), entry);
                bound
            }),

            Self::Attr(expr, attributes) => expr.apply(entry).and_then(|bound| {
                if attributes.iter().all(|arg| entry.get(arg.as_ref()).is_some()) {
                    Some(bound)
                } else {
                    None
                }
            }),

            Self::Alt(exprs) => {
                for expr in exprs {
                    let applied = expr.apply(entry);
                    if applied.is_some() {
                        return applied;
                    }
                }
                None
            }

            Self::Multi(_) => None,

            Self::Ancestrage(lhs, rhs) => lhs.apply(entry).and_then(|mut bound| {
                let parents = entry.parents().unwrap_or_default();
                if let Some((other, _)) = rhs.apply_any(parents) {
                    bound.extend(other);
                    Some(bound)
                } else {
                    None
                }
            }),
        }
    }

    fn apply_any<'s>(
        &self,
        entries: &'s [Entry],
    ) -> Option<(HashMap<String, &'s Entry>, Vec<&'s Entry>)> {
        match self {
            Self::Wildcard => {
                if !entries.is_empty() {
                    Some((HashMap::new(), entries.iter().collect()))
                } else {
                    None
                }
            }

            Self::Entry(_) => entries
                .iter()
                .filter_map(|e| self.apply(e).map(|r| (r, vec![e])))
                .next(),

            Self::Neg(expr) => {
                if expr.apply_any(entries).is_some() {
                    None
                } else {
                    Some((HashMap::new(), vec![]))
                }
            }

            Self::Binding(binding, expr) => {
                expr.apply_any(entries).map(|(mut bound, es)| {
                    if !es.is_empty() {
                        bound.insert(binding.to_string(), es.get(0).unwrap());
                    }
                    (bound, vec![])
                })
            }

            Self::Attr(expr, attributes) => {
                expr.apply_any(entries).and_then(|(bound, es)| {
                    if !es.is_empty() {
                        if es.iter().any(|e| {
                            attributes.iter().all(|arg| e.get(arg.as_ref()).is_some())
                        }) {
                            Some((bound, es))
                        } else {
                            None
                        }
                    } else {
                        Some((bound, es))
                    }
                })
            }

            Self::Alt(exprs) => {
                for expr in exprs {
                    let applied = expr.apply_any(entries);
                    if applied.is_some() {
                        return applied;
                    }
                }
                None
            }

            Self::Multi(exprs) => {
                let mut consumed = vec![];
                let mut res = HashMap::new();

                for spec in exprs {
                    let mut item = None;
                    for (i, e) in entries.iter().enumerate() {
                        if consumed.contains(&i) {
                            continue;
                        }

                        item = spec.apply(e).map(|v| (i, v));
                        if item.is_some() {
                            break;
                        }
                    }

                    if let Some((index, bound)) = item {
                        res.extend(bound);
                        consumed.push(index);
                    } else {
                        return None;
                    }
                }

                let mut es = vec![];
                for i in consumed.into_iter() {
                    es.push(entries.get(i).unwrap());
                }

                Some((res, es))
            }

            Self::Ancestrage(_, _) => entries
                .iter()
                .filter_map(|e| self.apply(e).map(|r| (r, vec![e])))
                .next(),
        }
    }
}

/// A specialized result type with a selector error.
type SelectorResult<T> = Result<T, SelectorError>;

/// The error when parsing a selector expression fails.
#[derive(Debug, Clone, Eq, PartialEq, Error)]
pub enum SelectorError {
    /// A value (entry type or subexpression) was expected.
    #[error("missing value")]
    MissingValue,
    /// An attribute list contained something expected.
    #[error("malformed attribute")]
    MalformedAttribute,
    /// A comma was expected in an attribute expression.
    #[error("missing comma")]
    MissingComma,
    /// The parentheses are unbalanced.
    #[error("unbalanced parentheses")]
    UnbalancedParens,
    /// The entry type is not known.
    #[error("unknown entry type: `{0}`")]
    UnknownEntryType(String),
}
