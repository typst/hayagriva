#![cfg(feature = "rkyv")]
use citationberg::{CborDeserializeError, CborSerializeError, Style, XmlSerdeError};
use citationberg::{Locale, LocaleFile};
use rkyv::Archive;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::fs;
use std::io;
use std::path::PathBuf;
use std::process::Command;

use hayagriva::archive::Lookup;

const CACHE_PATH: &str = "target/haya-cache";
const STYLES_REPO_NAME: &str = "styles";
const CSL_REPO: &str = "https://github.com/citation-style-language/styles";
const LOCALES_REPO: &str = "https://github.com/citation-style-language/locales";
const LOCALES_REPO_NAME: &str = "locales";
const ARCHIVE_NAME: &str = "styles.cbor.rkyv";

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

/// Download the CSL styles and locales repos.
fn ensure_repos() -> Result<(), ArchivalError> {
    ensure_repo(CSL_REPO, STYLES_REPO_NAME, "master")?;
    ensure_repo(LOCALES_REPO, LOCALES_REPO_NAME, "master")
}

/// Clone a repo if it does not exist, pull it otherwise.
///
/// This function requires a `git` installation to be available in PATH.
fn ensure_repo(
    repo_url: &str,
    repo_name: &str,
    branch_name: &str,
) -> Result<(), ArchivalError> {
    let cache_path = PathBuf::from(CACHE_PATH);
    fs::create_dir_all(CACHE_PATH)?;

    // Check if styles directory exists. If it does, try to git pull origin
    // main. If that fails or the directory does not exist, clone the repo.
    let style_path = cache_path.join(repo_name);
    let clone = if style_path.exists() {
        let status = Command::new("git")
            .args(["pull", "origin", branch_name])
            .current_dir(&style_path)
            .status()
            .expect("Please ensure git is installed");

        if !status.success() {
            fs::remove_dir_all(&style_path)?;
            true
        } else {
            false
        }
    } else {
        true
    };

    if clone {
        let status = Command::new("git")
            .args(["clone", repo_url, repo_name, "--depth", "1"])
            .current_dir(&cache_path)
            .status()
            .expect("Please ensure git is installed");

        if !status.success() {
            return Err(ArchivalError::Io(io::Error::new(
                io::ErrorKind::Other,
                "Failed to clone repo. Is git installed correnctly and is the internet working?",
            )));
        }
    }

    Ok(())
}

/// Create an archive of CSL and its locales as CBOR.
fn create_archive() -> Result<(), ArchivalError> {
    let style_path = PathBuf::from(CACHE_PATH).join(STYLES_REPO_NAME);
    let mut res = Lookup {
        map: HashMap::new(),
        styles: Vec::new(),
        locales: retrieve_locales()?,
    };

    for style_thing in fs::read_dir(&style_path).unwrap() {
        let thing = style_thing.unwrap();
        if !thing.file_type()?.is_file() {
            continue;
        }

        let path = thing.path();
        let extension = path.extension();
        if let Some(extension) = extension {
            if extension.to_str() != Some("csl") {
                continue;
            }
        } else {
            continue;
        }

        let style: Style = Style::from_xml(&fs::read_to_string(path)?)?;
        if let Style::Dependent(_) = style {
            continue;
        }

        if STYLE_IDS.binary_search(&style.info().id.as_str()).is_err() {
            continue;
        }

        let bytes = style.to_cbor()?;
        let idx = res.styles.len();
        let id = strip_id(style.info().id.as_str());

        let rides = OVERRIDES;
        let over = rides.iter().find(|o| o.id == id);
        let name = if let Some(name) = over.and_then(|o| o.main) {
            name.to_string()
        } else {
            id.trim_end_matches("-journals")
                .trim_end_matches("-publications")
                .trim_end_matches("-brackets")
                .trim_end_matches("-group")
                .trim_end_matches("-bibliography")
                .to_string()
        };

        println!("\n{};{}", &id, &name);

        let mut insert = |name: &str| {
            if res.map.insert(name.to_string(), idx).is_some() {
                panic!("duplicate name {} ({})", name, idx);
            }
        };

        insert(&name);

        for alias in over.and_then(|o| o.alias.as_ref()).iter().flat_map(|a| a.iter()) {
            insert(alias);
        }

        res.styles.push(bytes);
    }

    assert_eq!(res.styles.len(), STYLE_IDS.len());

    let bytes = rkyv::to_bytes::<_, 1024>(&res).expect("failed to serialize vec");
    fs::write("styles.cbor.rkyv", bytes)?;

    Ok(())
}

/// Retrieve all available CSL locales.
fn retrieve_locales() -> Result<Vec<Vec<u8>>, ArchivalError> {
    let mut res = Vec::new();
    let locales_path = PathBuf::from(CACHE_PATH).join(LOCALES_REPO_NAME);
    for style_thing in fs::read_dir(&locales_path).unwrap() {
        let thing = style_thing.unwrap();
        if !thing.file_type()?.is_file() {
            continue;
        }

        let path = thing.path();
        let extension = path.extension();
        if let Some(extension) = extension {
            if extension.to_str() != Some("xml") {
                continue;
            }
        } else {
            continue;
        }

        let file_name = path.file_name();
        if let Some(name) = file_name {
            if !name.to_string_lossy().starts_with("locales-") {
                continue;
            }
        } else {
            continue;
        }

        let xml = fs::read_to_string(path)?;
        let locale: Locale = LocaleFile::from_xml(&xml)?.into();
        let bytes = locale.to_cbor()?;
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
    for (k, idx) in archive.map.iter() {
        let bytes = archive
            .styles
            .get(*idx as usize)
            .ok_or_else(|| ArchivalError::ValidationError(k.to_string()))?;
        let style = Style::from_cbor(bytes)
            .map_err(|_| ArchivalError::ValidationError(k.to_string()))?;
        let id = &style.info().id;

        // Check that this style is requested.
        if STYLE_IDS.binary_search(&id.as_str()).is_err() {
            return Err(ArchivalError::ValidationError(id.to_string()));
        }

        // Check that the archive is well-formed.
        let path = format!("target/haya-cache/styles/{}.csl", strip_id(id));
        eprintln!("{}", path);
        let original_xml = fs::read_to_string(path)?;
        let original_style = Style::from_xml(&original_xml)?;
        assert_eq!(style, original_style);
    }

    // Check that all requested styles are encoded and referenced.
    assert_eq!(archive.styles.len(), STYLE_IDS.len());
    assert_eq!(archive.map.values().collect::<HashSet<_>>().len(), STYLE_IDS.len());

    // Check that all locales are well-formed.
    for l in archive.locales.iter() {
        let locale = Locale::from_cbor(l)?;
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
    full_id.trim_start_matches("http://www.zotero.org/styles/")
}

/// Map which styles are referenced by which dependent styles.
#[allow(dead_code)]
fn retrieve_dependent_aliasses() -> Result<HashMap<String, Vec<String>>, ArchivalError> {
    let mut dependent_alias: HashMap<_, Vec<_>> = HashMap::new();
    let style_path = PathBuf::from(CACHE_PATH).join(STYLES_REPO_NAME);
    for style_thing in fs::read_dir(style_path.join("dependent")).unwrap() {
        let thing = style_thing.unwrap();
        if !thing.file_type()?.is_file() {
            continue;
        }

        let path = thing.path();
        let extension = path.extension();
        if let Some(extension) = extension {
            if extension.to_str() != Some("csl") {
                continue;
            }
        } else {
            continue;
        }

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
enum ArchivalError {
    Io(io::Error),
    Deserialize(XmlSerdeError),
    Serialize(CborSerializeError),
    CborDeserialize(CborDeserializeError),
    ValidationError(String),
    LocaleValidationError(String),
}

impl From<io::Error> for ArchivalError {
    fn from(value: io::Error) -> Self {
        Self::Io(value)
    }
}

impl From<XmlSerdeError> for ArchivalError {
    fn from(value: XmlSerdeError) -> Self {
        Self::Deserialize(value)
    }
}

impl From<CborSerializeError> for ArchivalError {
    fn from(value: CborSerializeError) -> Self {
        Self::Serialize(value)
    }
}

impl From<CborDeserializeError> for ArchivalError {
    fn from(value: CborDeserializeError) -> Self {
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
const STYLE_IDS: [&str; 80] = [
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
    alias: Option<Vec<&'static str>>,
}

impl Override {
    /// Create a new override for the main name.
    const fn first(id: &'static str, name: &'static str) -> Self {
        Self { id, main: Some(name), alias: None }
    }
}

const OVERRIDES: [Override; 10] = [
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
    Override::first("gost-r-7-0-5-2008-numeric", "gost-r-705-2008-numeric"),
    Override::first("iso690-author-date-en", "iso-690-author-date"),
    Override::first("iso690-numeric-en", "iso-690-numeric"),
    Override::first(
        "modern-language-association-8th-edition",
        "modern-language-association-8",
    ),
    Override::first("thieme-german", "thieme"),
    Override::first("turabian-fullnote-bibliography-8th-edition", "turabian-fullnote"),
];
