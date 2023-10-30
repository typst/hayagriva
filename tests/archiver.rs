#![cfg(feature = "rkyv")]
use citationberg::Style;
use citationberg::{Locale, LocaleFile, XmlError};
use rkyv::Archive;
use serde::de::DeserializeOwned;
use serde::Serialize;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::fs;
use std::io;
use std::path::PathBuf;

use hayagriva::archive::{Lookup, StyleMatch};

mod common;
use common::{ensure_repo, iter_files, iter_files_with_name};

use crate::common::CACHE_PATH;

const STYLES_REPO_NAME: &str = "styles";
const CSL_REPO: &str = "https://github.com/citation-style-language/styles";
const LOCALES_REPO: &str = "https://github.com/citation-style-language/locales";
const LOCALES_REPO_NAME: &str = "locales";
const ARCHIVE_NAME: &str = "styles.cbor.rkyv";
const OWN_STYLES: &str = "styles";

/// Ensure the CSL repos are available, create an archive, and validate it.
#[test]
fn try_archive() {
    ensure_repos().unwrap();

    // Create the archive if it does not exist
    let created = if !PathBuf::from(ARCHIVE_NAME).exists() {
        create_archive().unwrap();
        true
    } else {
        false
    };

    match (created, validate_archive()) {
        (true, Err(e)) => panic!("{:?}", e),
        (false, Err(_)) => create_archive().unwrap(),
        (_, Ok(())) => {}
    }
}

/// Always archive.
#[test]
#[ignore]
fn always_archive() {
    ensure_repos().unwrap();
    create_archive().unwrap();
    validate_archive().unwrap();
}

/// Download the CSL styles and locales repos.
fn ensure_repos() -> Result<(), ArchivalError> {
    ensure_repo(CSL_REPO, STYLES_REPO_NAME, "master")?;
    Ok(ensure_repo(LOCALES_REPO, LOCALES_REPO_NAME, "master")?)
}

/// Create an archive of CSL and its locales as CBOR.
fn create_archive() -> Result<(), ArchivalError> {
    let style_path = PathBuf::from(CACHE_PATH).join(STYLES_REPO_NAME);
    let own_style_path = PathBuf::from(OWN_STYLES);
    let mut res = Lookup {
        map: HashMap::new(),
        id_map: HashMap::new(),
        styles: Vec::new(),
        locales: retrieve_locales()?,
    };

    for path in iter_files(&style_path, "csl").chain(iter_files(&own_style_path, "csl")) {
        let style: Style = Style::from_xml(&fs::read_to_string(path)?)?;
        let Style::Independent(indep) = &style else {
            continue;
        };

        if STYLE_IDS.binary_search(&indep.info.id.as_str()).is_err() {
            continue;
        }

        let bytes = to_cbor_vec(&style)?;
        let idx = res.styles.len();
        let id = strip_id(indep.info.id.as_str());

        let rides = OVERRIDES;
        let over = rides.iter().find(|o| o.id == id);
        let name = clean_name(id, over);

        println!("\n{};{}", &id, &name);

        let mut insert = |name: &str, alias: bool| {
            if res
                .map
                .insert(
                    name.to_string(),
                    StyleMatch::new(
                        indep.info.title.value.to_string(),
                        alias,
                        indep.bibliography.is_some(),
                        idx,
                    ),
                )
                .is_some()
            {
                panic!("duplicate name {} ({})", name, idx);
            }

            if !alias {
                res.id_map.insert(indep.info.id.clone(), idx);
            }
        };

        insert(&name, false);

        for alias in over.and_then(|o| o.alias.as_ref()).iter().flat_map(|a| a.iter()) {
            insert(alias, true);
        }

        res.styles.push(bytes);
    }

    assert_eq!(res.styles.len(), STYLE_IDS.len());

    let bytes = rkyv::to_bytes::<_, 1024>(&res).expect("failed to serialize vec");
    fs::write("styles.cbor.rkyv", bytes)?;

    Ok(())
}

fn clean_name(id: &str, over: Option<&Override>) -> String {
    if let Some(name) = over.and_then(|o| o.main) {
        name.to_string()
    } else {
        id.trim_end_matches("-journals")
            .trim_end_matches("-publications")
            .trim_end_matches("-brackets")
            .trim_end_matches("-group")
            .trim_end_matches("-bibliography")
            .to_string()
    }
}

/// Retrieve all available CSL locales.
fn retrieve_locales() -> Result<Vec<Vec<u8>>, ArchivalError> {
    let mut res = Vec::new();
    let locales_path = PathBuf::from(CACHE_PATH).join(LOCALES_REPO_NAME);
    for path in iter_files_with_name(&locales_path, "xml", |n| n.starts_with("locales-"))
    {
        let xml = fs::read_to_string(path)?;
        let locale: Locale = LocaleFile::from_xml(&xml)?.into();
        let bytes = to_cbor_vec(&locale)?;
        res.push(bytes);
    }

    Ok(res)
}

/// Check whether all desired styles are available and correctly encoded.
fn validate_archive() -> Result<(), ArchivalError> {
    let archive_file = fs::read("styles.cbor.rkyv")?;
    if archive_file.is_empty() {
        return Err(ArchivalError::ValidationError("empty file".to_string()));
    }
    let archive = unsafe { read(&archive_file) };

    // Check that every archive entry maps to a style.
    for (k, idx) in archive.map.iter().filter(|(_, v)| !v.alias) {
        let bytes = archive
            .styles
            .get(idx.index as usize)
            .ok_or_else(|| ArchivalError::ValidationError(k.to_string()))?;
        let style = from_cbor::<Style>(bytes)
            .map_err(|_| ArchivalError::ValidationError(k.to_string()))?;
        let id = &style.info().id;

        // Check that this style is requested.
        if STYLE_IDS.binary_search(&id.as_str()).is_err() {
            return Err(ArchivalError::ValidationError(id.to_string()));
        }

        // Check that the archive is well-formed.
        let path = if id.contains("typst.org") {
            format!("styles/{}.csl", strip_id(id))
        } else {
            format!("{}/{}/{}.csl", CACHE_PATH, STYLES_REPO_NAME, strip_id(id))
        };
        eprintln!("{}", path);
        let original_xml = fs::read_to_string(path)?;
        let original_style = Style::from_xml(&original_xml)?;
        assert_eq!(style, original_style);
    }

    // Check that all requested styles are encoded and referenced.
    assert_eq!(archive.styles.len(), STYLE_IDS.len());
    assert_eq!(
        archive.map.values().map(|s| s.index).collect::<HashSet<_>>().len(),
        STYLE_IDS.len()
    );

    // Check that all locales are well-formed.
    for l in archive.locales.iter() {
        let locale = from_cbor::<Locale>(l)?;
        let locale_name = format!(
            "locales-{}.xml",
            locale.lang.as_ref().ok_or_else(|| {
                ArchivalError::LocaleValidationError("missing lang".to_string())
            })?
        );
        let path = PathBuf::from("target/haya-cache/")
            .join(LOCALES_REPO_NAME)
            .join(locale_name);
        let original_xml = fs::read_to_string(path)?;
        let original_locale: Locale = LocaleFile::from_xml(&original_xml)?.into();
        assert_eq!(locale, original_locale);
    }

    Ok(())
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

/// Read an archive
unsafe fn read(buf: &[u8]) -> &<Lookup as Archive>::Archived {
    rkyv::archived_root::<Lookup>(buf)
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
            Self::Io(io) => fmt::Display::fmt(io, f),
            Self::Deserialize(deser) => fmt::Display::fmt(deser, f),
            Self::Serialize(ser) => fmt::Display::fmt(ser, f),
            Self::CborDeserialize(deser) => fmt::Display::fmt(deser, f),
            Self::ValidationError(id) => write!(f, "error when validating style {}", id),
            Self::LocaleValidationError(id) => {
                write!(f, "error when validating locale {}", id)
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

const OVERRIDES: [Override; 17] = [
    Override::alias("apa", "american-psychological-association", &["apa"]),
    Override::alias("bmj", "british-medical-journal", &["bmj"]),
    Override::first(
        "china-national-standard-gb-t-7714-2015-author-date",
        "gb-7114-2015-author-date",
    ),
    Override::first("china-national-standard-gb-t-7714-2015-note", "gb-7114-2015-note"),
    Override::first(
        "china-national-standard-gb-t-7714-2015-numeric",
        "gb-7114-2015-numeric",
    ),
    Override::first("chinese-gb7714-2005-numeric", "gb-7114-2005-numeric"),
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

fn from_cbor<T: DeserializeOwned>(
    reader: &[u8],
) -> Result<T, ciborium::de::Error<std::io::Error>> {
    ciborium::de::from_reader(reader)
}

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
