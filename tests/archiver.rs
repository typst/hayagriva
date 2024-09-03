use citationberg::{IndependentStyle, LocaleCode, Style};
use citationberg::{Locale, LocaleFile, XmlError};
use serde::Serialize;
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Write};
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::{fmt, iter};

mod common;
use common::{ensure_repo, iter_files, iter_files_with_name};

use crate::common::CACHE_PATH;

const STYLES_REPO_NAME: &str = "styles";
const CSL_REPO: &str = "https://github.com/citation-style-language/styles";
const LOCALES_REPO: &str = "https://github.com/citation-style-language/locales";
const LOCALES_REPO_NAME: &str = "locales";
const OWN_STYLES: &str = "styles";
const ARCHIVER_SRC_PATH: &str = "src/csl/archive.rs";

/// Always archive.
#[test]
fn always_archive() {
    ensure_repos().unwrap_or_else(|err| panic!("Downloading repos failed: {}", err));
    create_archive().unwrap_or_else(|err| panic!("{}", err));
}

#[test]
fn no_dupe_id() {
    let set: HashSet<&str> = STYLE_IDS.iter().map(|s| strip_id(s)).collect();
    assert_eq!(set.len(), STYLE_IDS.len());
}

/// Download the CSL styles and locales repos.
fn ensure_repos() -> Result<(), ArchivalError> {
    ensure_repo(CSL_REPO, STYLES_REPO_NAME, "master")?;
    Ok(ensure_repo(LOCALES_REPO, LOCALES_REPO_NAME, "master")?)
}

fn ensure_archive_up_to_date(
    path: impl AsRef<Path>,
    item: impl Display,
    expected: &[u8],
) -> Result<(), ArchivalError> {
    let mut file = match fs::File::open(path) {
        Ok(file) => file,
        Err(err) if err.kind() == io::ErrorKind::NotFound => {
            // The archive file simply wasn't created yet.
            return Err(ArchivalError::NeedsUpdate(item.to_string()));
        }
        Err(err) => return Err(ArchivalError::Io(err)),
    };

    let mut position = 0;
    let mut buf = vec![0u8; 4096];
    while position < expected.len() {
        let read_bytes = file.read(&mut buf)?;
        if read_bytes == 0
            || buf[..read_bytes] != expected[position..position + read_bytes]
        {
            return Err(ArchivalError::NeedsUpdate(item.to_string()));
        }
        position += read_bytes;
    }

    Ok(())
}

/// Create an archive of CSL and its locales as CBOR.
fn create_archive() -> Result<(), ArchivalError> {
    let style_path = PathBuf::from(CACHE_PATH).join(STYLES_REPO_NAME);
    let own_style_path = PathBuf::from(OWN_STYLES);

    // Without "HAYAGRIVA_ARCHIVE_UPDATE=1", we only check if the archive files
    // are up-to-date.
    let should_write =
        std::env::var_os("HAYAGRIVA_ARCHIVER_UPDATE").is_some_and(|var| var == "1");

    let mut w = String::new();
    let mut styles: Vec<_> = iter_files(&style_path, "csl")
        .chain(iter_files(&own_style_path, "csl"))
        .filter_map(|path| {
            {
                let style: Style =
                    Style::from_xml(&fs::read_to_string(path).unwrap()).unwrap();
                let Style::Independent(indep) = &style else { return None };

                if STYLE_IDS.binary_search(&indep.info.id.as_str()).is_err() {
                    return None;
                }

                let bytes = to_cbor_vec(&style).unwrap();
                Some((bytes, indep.clone()))
            }
            .map(|(bytes, indep)| {
                let stripped_id = strip_id(indep.info.id.as_str());

                let over = OVERRIDES.iter().find(|o| o.id == stripped_id);
                let names = get_names(stripped_id, over);
                let variant_name = heck::AsUpperCamelCase(&names[0]).to_string();

                (bytes, indep, names, variant_name)
            })
        })
        .collect();

    styles.sort_by_key(|(_, _, names, _)| names[0].clone());

    write_styles_section(&mut w, styles.as_slice())
        .map_err(|e| ArchivalError::ValidationError(e.to_string()))?;

    let locales_path = PathBuf::from(CACHE_PATH).join(LOCALES_REPO_NAME);
    let mut locales =
        iter_files_with_name(&locales_path, "xml", |n| n.starts_with("locales-"))
            .map(|path| {
                let locale: Locale =
                    LocaleFile::from_xml(&fs::read_to_string(path).unwrap())
                        .unwrap()
                        .into();
                let bytes = to_cbor_vec(&locale).unwrap();
                (bytes, locale)
            })
            .collect::<Vec<_>>();

    locales.sort_by_cached_key(|(_, locale)| {
        locale.lang.clone().unwrap_or(LocaleCode("und-ZZ".to_string())).0
    });

    write_locales_section(&mut w, locales.as_slice())
        .map_err(|e| ArchivalError::LocaleValidationError(e.to_string()))?;

    for (bytes, indep, _, _) in styles {
        let stripped_id = strip_id(indep.info.id.as_str());
        let path = PathBuf::from("archive/styles/").join(format!("{}.cbor", stripped_id));

        if should_write {
            fs::write(path, bytes)?;
        } else {
            let item = format!("style '{}.cbor'", stripped_id);
            ensure_archive_up_to_date(&path, item, &bytes)?;
        }
    }

    for (bytes, locale) in locales {
        let lang = locale.lang.unwrap();
        let path = PathBuf::from("archive/locales/").join(format!("{}.cbor", lang));

        if should_write {
            fs::write(path, bytes)?;
        } else {
            let item = format!("locale '{}.cbor'", lang);
            ensure_archive_up_to_date(&path, item, &bytes)?;
        }
    }

    if should_write {
        fs::write(ARCHIVER_SRC_PATH, w)?;
    } else {
        let item = format!("file '{}'", ARCHIVER_SRC_PATH);
        ensure_archive_up_to_date(ARCHIVER_SRC_PATH, item, w.as_bytes())?;
    }

    Ok(())
}

fn write_styles_section(
    w: &mut String,
    items: &[(Vec<u8>, IndependentStyle, Vec<String>, String)],
) -> fmt::Result {
    writeln!(w, "//! Optional archive of included CSL styles.")?;
    writeln!(w, "// This file is generated by tests/generate.rs")?;
    writeln!(w, "// Do not edit by hand!")?;
    writeln!(w)?;
    writeln!(w, "use citationberg::{{Locale, Style}};")?;
    writeln!(w, "use serde::de::DeserializeOwned;")?;
    writeln!(w)?;

    writeln!(w, "/// An embedded CSL style.")?;
    writeln!(w, "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]")?;
    writeln!(w, "#[non_exhaustive]")?;
    writeln!(w, "pub enum ArchivedStyle {{")?;
    for (_, style, _, variant) in items {
        writeln!(w, "    /// {}.", style.info.title.value)?;
        if !style.info.authors.is_empty() {
            writeln!(w, "    ///")?;
            write!(w, "    /// Authors: ")?;
            for (i, author) in style.info.authors.iter().enumerate() {
                if i != 0 {
                    write!(w, ", ")?;
                }
                write!(w, "{}", author.name)?;
            }
            writeln!(w, ".")?;
        }
        writeln!(w, "    {},", variant)?;
    }
    writeln!(w, "}}")?;
    writeln!(w)?;

    writeln!(w, "#[rustfmt::skip]")?;
    writeln!(w, "impl ArchivedStyle {{")?;
    writeln!(w, "    /// Retrieve this style by name.")?;
    writeln!(w, "    pub fn by_name(name: &str) -> Option<Self> {{")?;
    writeln!(w, "        match name {{")?;
    for (_, _, names, variant) in items {
        for name in names {
            writeln!(w, "            {:?} => Some(Self::{}),", name, variant)?;
        }
    }
    writeln!(w, "            _ => None,")?;
    writeln!(w, "        }}")?;
    writeln!(w, "    }}")?;
    writeln!(w)?;

    writeln!(w, "    /// Retrieve this style by CSL ID.")?;
    writeln!(w, "    pub fn by_id(id: &str) -> Option<Self> {{")?;
    writeln!(w, "        match id {{")?;
    for (_, style, _, variant) in items {
        writeln!(w, "            {:?} => Some(Self::{}),", style.info.id, variant)?;
    }
    writeln!(w, "            _ => None")?;
    writeln!(w, "        }}")?;
    writeln!(w, "    }}")?;
    writeln!(w)?;

    writeln!(w, "    /// Returns all variants.")?;
    writeln!(w, "    pub fn all() -> &'static [Self] {{")?;
    writeln!(w, "        &[")?;
    for (_, _, _, variant) in items {
        writeln!(w, "            Self::{},", variant)?;
    }
    writeln!(w, "        ]")?;
    writeln!(w, "    }}")?;
    writeln!(w)?;

    writeln!(w, "    /// Get the CBOR representation of this style.")?;
    writeln!(w, "    fn bytes(self) -> &'static [u8] {{")?;
    writeln!(w, "        match self {{")?;
    for (_, style, _, variant) in items {
        let stripped_id = strip_id(style.info.id.as_str());

        writeln!(
            w,
            "            Self::{} => include_bytes!(\"../../archive/styles/{}.cbor\"),",
            variant, stripped_id
        )?;
    }
    writeln!(w, "        }}")?;
    writeln!(w, "    }}")?;
    writeln!(w)?;

    writeln!(w, "    /// Retrieve the style.")?;
    writeln!(w, "    pub fn get(self) -> Style {{")?;
    writeln!(w, "        from_cbor(self.bytes()).unwrap()")?;
    writeln!(w, "    }}")?;
    writeln!(w)?;

    writeln!(w, "    /// Get the style's names in Hayagriva.")?;
    writeln!(w, "    pub fn names(self) -> &'static [&'static str] {{")?;
    writeln!(w, "        match self {{")?;
    for (_, _, names, variant) in items {
        writeln!(w, "            Self::{} => &[", variant)?;
        for name in names {
            writeln!(w, "                {:?},", name)?;
        }
        writeln!(w, "            ],")?;
    }
    writeln!(w, "        }}")?;
    writeln!(w, "    }}")?;
    writeln!(w)?;

    writeln!(w, "    /// Get the style's full name.")?;
    writeln!(w, "    pub fn display_name(self) -> &'static str {{")?;
    writeln!(w, "        match self {{")?;
    for (_, style, _, variant) in items {
        writeln!(w, "            Self::{} => {:?},", variant, style.info.title.value)?;
    }
    writeln!(w, "        }}")?;
    writeln!(w, "    }}")?;
    writeln!(w)?;

    writeln!(w, "    /// Get the style's CSL ID.")?;
    writeln!(w, "    pub fn csl_id(self) -> &'static str {{")?;
    writeln!(w, "        match self {{")?;
    for (_, style, _, variant) in items {
        writeln!(w, "            Self::{} => {:?},", variant, style.info.id)?;
    }
    writeln!(w, "        }}")?;
    writeln!(w, "    }}")?;
    writeln!(w, "}}")?;

    writeln!(w, "fn from_cbor<T: DeserializeOwned>(")?;
    writeln!(w, "    reader: &[u8],")?;
    writeln!(w, ") -> Result<T, ciborium::de::Error<std::io::Error>> {{")?;
    writeln!(w, "    ciborium::de::from_reader(reader)")?;
    writeln!(w, "}}")?;

    Ok(())
}

fn write_locales_section(w: &mut String, items: &[(Vec<u8>, Locale)]) -> fmt::Result {
    writeln!(w)?;

    writeln!(w, "/// CBOR-encoded CSL locales.")?;
    writeln!(w, "pub const LOCALES: &[&[u8]] = &[")?;
    for (_, locale) in items {
        writeln!(
            w,
            "    include_bytes!(\"../../archive/locales/{}.cbor\"),",
            locale.lang.as_ref().unwrap()
        )?;
    }
    writeln!(w, "];")?;
    writeln!(w)?;

    writeln!(w, "/// Get all CSL locales.")?;
    writeln!(w, "pub fn locales() -> Vec<Locale> {{")?;
    writeln!(w, "    LOCALES")?;
    writeln!(w, "        .iter()")?;
    writeln!(w, "        .map(|bytes| from_cbor::<Locale>(bytes).unwrap())")?;
    writeln!(w, "        .collect()")?;
    writeln!(w, "}}")?;

    Ok(())
}

fn get_names<'a>(id: &'a str, over: Option<&'a Override>) -> Vec<String> {
    let main = if let Some(name) = over.and_then(|o| o.main) {
        name
    } else {
        id.trim_end_matches("-journals")
            .trim_end_matches("-publications")
            .trim_end_matches("-brackets")
            .trim_end_matches("-group")
            .trim_end_matches("-bibliography")
    }
    .to_string();

    let other = over.and_then(|o| o.alias).unwrap_or_default();
    iter::once(main)
        .chain(other.iter().map(ToString::to_string))
        .collect()
}

/// Remove the common URL trunk from CSL ids.
fn strip_id(full_id: &str) -> &str {
    full_id
        .trim_start_matches("http://www.zotero.org/styles/")
        .trim_start_matches("http://typst.org/csl/")
}

/// Map which styles are referenced by which dependent styles.
#[allow(dead_code)]
fn retrieve_dependent_aliasses() -> Result<HashMap<String, Vec<String>>, ArchivalError> {
    let mut dependent_alias: HashMap<_, Vec<_>> = HashMap::new();
    let style_path = PathBuf::from(CACHE_PATH).join(STYLES_REPO_NAME);
    for path in iter_files(&style_path.join("dependent"), "csl") {
        let style: Style = Style::from_xml(&fs::read_to_string(path)?)?;
        if let Style::Dependent(d) = style {
            let id = strip_id(&d.info.id).to_string();
            let parent_id = strip_id(&d.parent_link.href).to_string();
            dependent_alias
                .entry(parent_id)
                .and_modify(|e| e.push(id.clone()))
                .or_insert_with(|| vec![id]);
        }
    }

    Ok(dependent_alias)
}

/// An error while creating or checking an archive.
#[derive(Debug)]
pub enum ArchivalError {
    Io(io::Error),
    Deserialize(XmlError),
    Serialize(ciborium::ser::Error<std::io::Error>),
    CborDeserialize(ciborium::de::Error<std::io::Error>),
    ValidationError(String),
    LocaleValidationError(String),
    NeedsUpdate(String),
}

impl From<io::Error> for ArchivalError {
    fn from(value: io::Error) -> Self {
        Self::Io(value)
    }
}

impl From<XmlError> for ArchivalError {
    fn from(value: XmlError) -> Self {
        Self::Deserialize(value)
    }
}

impl From<ciborium::ser::Error<std::io::Error>> for ArchivalError {
    fn from(value: ciborium::ser::Error<std::io::Error>) -> Self {
        Self::Serialize(value)
    }
}
impl From<ciborium::de::Error<std::io::Error>> for ArchivalError {
    fn from(value: ciborium::de::Error<std::io::Error>) -> Self {
        Self::CborDeserialize(value)
    }
}

impl fmt::Display for ArchivalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(io) => write!(f, "io error: {}", io),
            Self::Deserialize(deser) => write!(f, "deserialization error: {}", deser),
            Self::Serialize(ser) => write!(f, "serialization error: {}", ser),
            Self::CborDeserialize(deser) => {
                write!(f, "cbor deserialization error: {}", deser)
            }
            Self::ValidationError(id) => write!(f, "error when validating style {}", id),
            Self::LocaleValidationError(id) => {
                write!(f, "error when validating locale {}", id)
            }
            Self::NeedsUpdate(item) => {
                write!(
                    f,
                    "{} is outdated, run archiver tests with env var 'HAYAGRIVA_ARCHIVER_UPDATE=1' to update",
                    item
                )
            }
        }
    }
}

/// IDs of CSL styles requested for archive inclusion.
const STYLE_IDS: [&str; 81] = [
    "http://typst.org/csl/alphanumeric",
    "http://www.zotero.org/styles/american-anthropological-association",
    "http://www.zotero.org/styles/american-chemical-society",
    "http://www.zotero.org/styles/american-geophysical-union",
    "http://www.zotero.org/styles/american-institute-of-aeronautics-and-astronautics",
    "http://www.zotero.org/styles/american-institute-of-physics",
    "http://www.zotero.org/styles/american-medical-association",
    "http://www.zotero.org/styles/american-meteorological-society",
    "http://www.zotero.org/styles/american-physics-society",
    "http://www.zotero.org/styles/american-physiological-society",
    "http://www.zotero.org/styles/american-political-science-association",
    "http://www.zotero.org/styles/american-society-for-microbiology",
    "http://www.zotero.org/styles/american-society-of-civil-engineers",
    "http://www.zotero.org/styles/american-society-of-mechanical-engineers",
    "http://www.zotero.org/styles/american-sociological-association",
    "http://www.zotero.org/styles/angewandte-chemie",
    "http://www.zotero.org/styles/annual-reviews",
    "http://www.zotero.org/styles/annual-reviews-author-date",
    "http://www.zotero.org/styles/apa",
    "http://www.zotero.org/styles/associacao-brasileira-de-normas-tecnicas",
    "http://www.zotero.org/styles/association-for-computing-machinery",
    "http://www.zotero.org/styles/biomed-central",
    "http://www.zotero.org/styles/bmj",
    "http://www.zotero.org/styles/bristol-university-press",
    "http://www.zotero.org/styles/cell",
    "http://www.zotero.org/styles/chicago-author-date",
    "http://www.zotero.org/styles/chicago-fullnote-bibliography",
    "http://www.zotero.org/styles/chicago-note-bibliography",
    "http://www.zotero.org/styles/china-national-standard-gb-t-7714-2015-author-date",
    "http://www.zotero.org/styles/china-national-standard-gb-t-7714-2015-note",
    "http://www.zotero.org/styles/china-national-standard-gb-t-7714-2015-numeric",
    "http://www.zotero.org/styles/chinese-gb7714-2005-numeric",
    "http://www.zotero.org/styles/copernicus-publications",
    "http://www.zotero.org/styles/council-of-science-editors-author-date",
    "http://www.zotero.org/styles/council-of-science-editors-brackets",
    "http://www.zotero.org/styles/current-opinion",
    "http://www.zotero.org/styles/deutsche-gesellschaft-fur-psychologie",
    "http://www.zotero.org/styles/deutsche-sprache",
    "http://www.zotero.org/styles/elsevier-harvard",
    "http://www.zotero.org/styles/elsevier-vancouver",
    "http://www.zotero.org/styles/elsevier-with-titles",
    "http://www.zotero.org/styles/frontiers",
    "http://www.zotero.org/styles/future-medicine",
    "http://www.zotero.org/styles/future-science-group",
    "http://www.zotero.org/styles/gost-r-7-0-5-2008-numeric",
    "http://www.zotero.org/styles/harvard-cite-them-right",
    "http://www.zotero.org/styles/ieee",
    "http://www.zotero.org/styles/institute-of-physics-numeric",
    "http://www.zotero.org/styles/iso690-author-date-en",
    "http://www.zotero.org/styles/iso690-numeric-en",
    "http://www.zotero.org/styles/karger-journals",
    "http://www.zotero.org/styles/mary-ann-liebert-vancouver",
    "http://www.zotero.org/styles/modern-humanities-research-association",
    "http://www.zotero.org/styles/modern-language-association",
    "http://www.zotero.org/styles/modern-language-association-8th-edition",
    "http://www.zotero.org/styles/multidisciplinary-digital-publishing-institute",
    "http://www.zotero.org/styles/nature",
    "http://www.zotero.org/styles/pensoft-journals",
    "http://www.zotero.org/styles/plos",
    "http://www.zotero.org/styles/royal-society-of-chemistry",
    "http://www.zotero.org/styles/sage-vancouver",
    "http://www.zotero.org/styles/sist02",
    "http://www.zotero.org/styles/spie-journals",
    "http://www.zotero.org/styles/springer-basic-author-date",
    "http://www.zotero.org/styles/springer-basic-brackets",
    "http://www.zotero.org/styles/springer-fachzeitschriften-medizin-psychologie",
    "http://www.zotero.org/styles/springer-humanities-author-date",
    "http://www.zotero.org/styles/springer-lecture-notes-in-computer-science",
    "http://www.zotero.org/styles/springer-mathphys-brackets",
    "http://www.zotero.org/styles/springer-socpsych-author-date",
    "http://www.zotero.org/styles/springer-vancouver-brackets",
    "http://www.zotero.org/styles/taylor-and-francis-chicago-author-date",
    "http://www.zotero.org/styles/taylor-and-francis-national-library-of-medicine",
    "http://www.zotero.org/styles/the-institution-of-engineering-and-technology",
    "http://www.zotero.org/styles/the-lancet",
    "http://www.zotero.org/styles/thieme-german",
    "http://www.zotero.org/styles/trends-journals",
    "http://www.zotero.org/styles/turabian-author-date",
    "http://www.zotero.org/styles/turabian-fullnote-bibliography-8th-edition",
    "http://www.zotero.org/styles/vancouver",
    "http://www.zotero.org/styles/vancouver-superscript",
];

/// Override for the name of a style.
struct Override {
    /// ID without the common trunk.
    id: &'static str,
    /// Main name.
    main: Option<&'static str>,
    /// Alternative names.
    alias: Option<&'static [&'static str]>,
}

impl Override {
    /// Create a new override for the main name.
    const fn first(id: &'static str, name: &'static str) -> Self {
        Self { id, main: Some(name), alias: None }
    }

    const fn alias(
        id: &'static str,
        name: &'static str,
        alias: &'static [&'static str],
    ) -> Self {
        Self { id, main: Some(name), alias: Some(alias) }
    }
}

const OVERRIDES: [Override; 19] = [
    Override::alias("apa", "american-psychological-association", &["apa"]),
    Override::alias("bmj", "british-medical-journal", &["bmj"]),
    Override::first(
        "china-national-standard-gb-t-7714-2015-author-date",
        "gb-7714-2015-author-date",
    ),
    Override::first("chicago-fullnote-bibliography", "chicago-fullnotes"),
    Override::first("chicago-note-bibliography", "chicago-notes"),
    Override::first("china-national-standard-gb-t-7714-2015-note", "gb-7714-2015-note"),
    Override::first(
        "china-national-standard-gb-t-7714-2015-numeric",
        "gb-7714-2015-numeric",
    ),
    Override::first("chinese-gb7714-2005-numeric", "gb-7714-2005-numeric"),
    Override::first(
        "deutsche-gesellschaft-fur-psychologie",
        "deutsche-gesellschaft-für-psychologie",
    ),
    Override::first("gost-r-7-0-5-2008-numeric", "gost-r-705-2008-numeric"),
    Override::alias(
        "ieee",
        "institute-of-electrical-and-electronics-engineers",
        &["ieee"],
    ),
    Override::first("iso690-author-date-en", "iso-690-author-date"),
    Override::first("iso690-numeric-en", "iso-690-numeric"),
    Override::alias(
        "modern-language-association",
        "modern-language-association",
        &["mla"],
    ),
    Override::alias(
        "modern-language-association-8th-edition",
        "modern-language-association-8",
        &["mla-8"],
    ),
    Override::alias(
        "spie",
        "society-of-photo-optical-instrumentation-engineers",
        &["spie"],
    ),
    Override::alias("plos", "public-library-of-science", &["plos"]),
    Override::first("thieme-german", "thieme"),
    Override::first("turabian-fullnote-bibliography-8th-edition", "turabian-fullnote-8"),
];

fn to_cbor<T: Serialize>(
    writer: &mut Vec<u8>,
    value: &T,
) -> Result<(), ciborium::ser::Error<std::io::Error>> {
    ciborium::ser::into_writer(value, writer)
}

fn to_cbor_vec<T: Serialize>(
    value: &T,
) -> Result<Vec<u8>, ciborium::ser::Error<std::io::Error>> {
    let mut writer = Vec::new();
    to_cbor(&mut writer, value)?;
    Ok(writer)
}
