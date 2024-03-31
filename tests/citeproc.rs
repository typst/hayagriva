//! Parse and execute the citeproc test suite.

use std::io::{BufWriter, Write};
use std::path::PathBuf;
use std::str::FromStr;
use std::{fmt, fs};

mod common;
use citationberg::taxonomy::Locator;
use citationberg::{Locale, LocaleCode, Style, XmlError};
use common::{ensure_repo, iter_files_with_name, CACHE_PATH};

use citationberg::json as csl_json;
use hayagriva::archive::{locales, ArchivedStyle};
use hayagriva::io::from_biblatex_str;
use hayagriva::{
    BibliographyDriver, BibliographyRequest, CitationItem, CitationRequest, CitePurpose,
    Entry, LocatorPayload, SpecificLocator,
};
use unscanny::Scanner;

const TEST_REPO_NAME: &str = "test-suite";
const TEST_REPO: &str = "https://github.com/citation-style-language/test-suite";

#[derive(Debug, Clone, Copy)]
enum SectionTag {
    /// Which output mode to test.
    Mode,
    /// The expected output.
    Result,
    /// The CSL file to use.
    Csl,
    /// The CSL-JSON input.
    Input,
    /// A 2D vector containing keys of items in the input.
    BibEntries,
    /// Selects what to include in the bibliography output. A JSON object.
    BibSection,
    /// List of citations with citation items.
    CitationItems,
    /// List of arguments to pass to the citation processor.
    Citations,
    /// Version of the test suite.
    Version,
    /// Other sections are ignored.
    Unknown,
}

impl FromStr for SectionTag {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "MODE" => Ok(SectionTag::Mode),
            "RESULT" => Ok(SectionTag::Result),
            "CSL" => Ok(SectionTag::Csl),
            "INPUT" => Ok(SectionTag::Input),
            "BIBENTRIES" => Ok(SectionTag::BibEntries),
            "BIBSECTION" => Ok(SectionTag::BibSection),
            "CITATION-ITEMS" => Ok(SectionTag::CitationItems),
            "CITATIONS" => Ok(SectionTag::Citations),
            "VERSION" => Ok(SectionTag::Version),
            _ => Ok(Self::Unknown),
        }
    }
}

impl fmt::Display for SectionTag {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SectionTag::Mode => write!(f, "MODE"),
            SectionTag::Result => write!(f, "RESULT"),
            SectionTag::Csl => write!(f, "CSL"),
            SectionTag::Input => write!(f, "INPUT"),
            SectionTag::BibEntries => write!(f, "BIBENTRIES"),
            SectionTag::BibSection => write!(f, "BIBSECTION"),
            SectionTag::CitationItems => write!(f, "CITATION-ITEMS"),
            SectionTag::Citations => write!(f, "CITATIONS"),
            SectionTag::Version => write!(f, "VERSION"),
            SectionTag::Unknown => write!(f, "UNKNOWN"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TestMode {
    Citation,
    Bibliography,
}

impl FromStr for TestMode {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "citation" => Ok(TestMode::Citation),
            "bibliography" => Ok(TestMode::Bibliography),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
enum TestParseError {
    UnknownSection(String),
    SyntaxError,
    WrongClosingTag,
    MissingRequiredSection(SectionTag),
    CslError(XmlError),
    JsonError(serde_json::Error),
}

impl fmt::Display for TestParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TestParseError::UnknownSection(s) => write!(f, "unknown section {}", s),
            TestParseError::SyntaxError => write!(f, "syntax error"),
            TestParseError::WrongClosingTag => write!(f, "wrong closing tag"),
            TestParseError::MissingRequiredSection(s) => {
                write!(f, "missing required section {}", s)
            }
            TestParseError::CslError(e) => write!(f, "csl error: {}", e),
            TestParseError::JsonError(e) => write!(f, "json error: {}", e),
        }
    }
}

fn eat_void(s: &mut Scanner) {
    s.eat_until(">>==");
}

fn eat_equals(s: &mut Scanner) -> usize {
    s.eat_while(|c| c == '=').len()
}

fn eat_n_equals(s: &mut Scanner, count: usize) {
    for _ in 0..count {
        s.eat_if('=');
    }
}

fn expect_header(s: &mut Scanner) -> Result<SectionTag, TestParseError> {
    let start = ">>==";
    s.eat_until(start);
    if s.done() {
        return Err(TestParseError::SyntaxError);
    }

    s.jump(s.cursor() + start.len());
    let eq = eat_equals(s);
    s.eat_whitespace();

    let tag = s.eat_while(is_section_tag);
    let tag = SectionTag::from_str(tag)
        .map_err(|_| TestParseError::UnknownSection(tag.to_string()))?;
    if s.done() {
        return Err(TestParseError::SyntaxError);
    }

    s.eat_whitespace();
    eat_n_equals(s, eq);
    if !s.eat_if("==>>") {
        return Err(TestParseError::SyntaxError);
    }
    s.eat_whitespace();

    Ok(tag)
}

fn eat_section<'s>(
    s: &mut Scanner<'s>,
    tag: SectionTag,
) -> Result<&'s str, TestParseError> {
    let end = "<<==";
    let content = s.eat_until(end);
    if s.done() {
        return Err(TestParseError::SyntaxError);
    }

    s.jump(s.cursor() + end.len());
    let eq = eat_equals(s);
    s.eat_whitespace();
    if s.done() {
        return Err(TestParseError::SyntaxError);
    }

    if matches!(tag, SectionTag::Unknown) {
        s.eat_while(is_section_tag);
    } else if !s.eat_if(tag.to_string().as_str()) {
        return Err(TestParseError::WrongClosingTag);
    }

    s.eat_whitespace();

    eat_n_equals(s, eq);
    if !s.eat_if("==<<") {
        return Err(TestParseError::SyntaxError);
    }

    Ok(content)
}

fn section<'s>(s: &mut Scanner<'s>) -> Result<(SectionTag, &'s str), TestParseError> {
    let header = expect_header(s)?;
    let content = eat_section(s, header)?;
    Ok((header, content))
}

fn is_section_tag(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '-'
}

#[derive(Debug)]
struct TestCase {
    mode: TestMode,
    result: String,
    csl: Style,
    input: Vec<csl_json::Item>,
    bib_entries: Option<Vec<Vec<String>>>,
    bib_section: Option<String>,
    citation_items: Option<Vec<Vec<csl_json::CitationItem>>>,
    citations: Option<String>,
}

#[derive(Debug, Default)]
struct TestCaseBuilder<'s> {
    mode: Option<TestMode>,
    result: Option<&'s str>,
    csl: Option<&'s str>,
    input: Option<&'s str>,
    bib_entries: Option<&'s str>,
    bib_section: Option<&'s str>,
    citation_items: Option<&'s str>,
    citations: Option<&'s str>,
}

impl<'s> TestCaseBuilder<'s> {
    fn new() -> Self {
        Self::default()
    }

    fn process(&mut self, tag: SectionTag, content: &'s str) {
        match tag {
            SectionTag::Mode => {
                self.mode = Some(TestMode::from_str(content.trim()).unwrap());
            }
            SectionTag::Result => {
                self.result = Some(content);
            }
            SectionTag::Csl => {
                self.csl = Some(content);
            }
            SectionTag::Input => {
                self.input = Some(content);
            }
            SectionTag::BibEntries => {
                self.bib_entries = Some(content);
            }
            SectionTag::BibSection => {
                self.bib_section = Some(content);
            }
            SectionTag::CitationItems => {
                self.citation_items = Some(content);
            }
            SectionTag::Citations => {
                self.citations = Some(content);
            }
            SectionTag::Version | SectionTag::Unknown => {}
        }
    }

    fn finish(self) -> Result<TestCase, TestParseError> {
        Ok(TestCase {
            mode: self
                .mode
                .ok_or(TestParseError::MissingRequiredSection(SectionTag::Mode))?,
            result: self
                .result
                .ok_or(TestParseError::MissingRequiredSection(SectionTag::Result))?
                .replace("&#38;", "&"),
            csl: Style::from_xml(
                self.csl
                    .ok_or(TestParseError::MissingRequiredSection(SectionTag::Csl))?,
            )
            .map_err(TestParseError::CslError)?,
            input: serde_json::from_str(
                self.input
                    .ok_or(TestParseError::MissingRequiredSection(SectionTag::Input))?,
            )
            .map_err(TestParseError::JsonError)?,
            bib_entries: self
                .bib_entries
                .map(|e| serde_json::from_str(e).map_err(TestParseError::JsonError))
                .transpose()?,

            bib_section: self.bib_section.map(ToString::to_string),
            citation_items: self
                .citation_items
                .map(|e| serde_json::from_str(e).map_err(TestParseError::JsonError))
                .transpose()?,
            citations: self.citations.map(ToString::to_string),
        })
    }
}

#[test]
fn test_parse_tests() {
    let mut results = TestSuiteResults::obtain();

    eprintln!(
        "Total: {}, skipped: {}, passed: {}",
        results.total,
        results.skipped,
        results.passed.len()
    );
    let percentage =
        results.passed.len() as f64 / (results.total - results.skipped) as f64 * 100.0;
    eprintln!("{:.2}% passed", percentage);

    let should_pass = load_passing_tests();
    // Enable binary search.
    results.passed.sort_unstable();
    results.failed.sort_unstable();

    let mut fail = false;

    // Check that all tests that should pass actually passed.
    for test in &should_pass {
        if results.passed.binary_search(test).is_err() {
            if results.failed.binary_search(test).is_ok() {
                eprintln!("❌ Test {} should pass but failed", test);
            } else {
                eprintln!("🤨 Test {} should pass but was not found", test);
            }

            fail = true;
        }
    }

    if fail {
        panic!("Some tests that should pass failed or were not found");
    }

    // Only retain the tests that were not marked as passing.
    results.passed.retain(|t| !should_pass.contains(t));

    // Check that the `passing` file contains all passed tests.
    for test in &results.passed {
        eprintln!("👁️ Test {} passed but is not in the `passing` file", test);
        fail = true;
    }

    if fail {
        panic!("⚠️ Some test passed but were not found in the `passing` file.\nRebuild the file by running `cargo test --test citeproc --features csl-json -- write_passing --ignored`")
    }

    eprintln!("✅ All tests expected to pass passed");
}

#[test]
#[ignore]
fn write_passing() {
    let results = TestSuiteResults::obtain();
    let mut passing = results.passed;
    passing.sort_unstable();
    write_passing_test(passing.as_ref());
}

struct TestSuiteResults {
    pub total: usize,
    pub skipped: usize,
    pub passed: Vec<String>,
    pub failed: Vec<String>,
}

impl TestSuiteResults {
    fn obtain() -> Self {
        let locales = locales();
        ensure_repo(TEST_REPO, TEST_REPO_NAME, "master").unwrap();
        let test_path = PathBuf::from(CACHE_PATH)
            .join(TEST_REPO_NAME)
            .join("processor-tests/humans/");

        let mut total = 0;
        let mut skipped = 0;
        let mut passed = vec![];
        let mut failed = vec![];

        for path in iter_files_with_name(&test_path, "txt", |name| {
            ![
                "bugreports_EnvAndUrb",
                "affix_PrefixFullCitationTextOnly",
                "affix_PrefixWithDecorations",
                "flipflop_ApostropheInsideTag",
                "date_OtherAlone",
                // Upstream bug: https://github.com/tafia/quick-xml/issues/674
                "bugreports_SelfLink",
                "bugreports_SingleQuoteXml",
                "number_SpacesMakeIsNumericFalse",
                "textcase_LocaleUnicode",
                "date_YearSuffixImplicitWithNoDateOneOnly",
                "position_NearNoteWithPlugin",
            ]
            .contains(&name)
                && !name.starts_with("magic_")
        }) {
            let str = std::fs::read_to_string(&path).unwrap();
            let case = build_case(&str);
            total += 1;

            if !can_test(&case, || path.display(), false) {
                skipped += 1;
                continue;
            }

            let file_stem = path.file_stem().unwrap().to_string_lossy().to_string();
            if test_file(case, &locales, || path.display()) {
                passed.push(file_stem);
            } else {
                failed.push(file_stem);
            }
        }

        Self { total, skipped, passed, failed }
    }
}

#[test]
#[ignore]
fn test_single_file() {
    let locales = locales();
    let name = "nameattr_DelimiterPrecedesLastOnCitationInCitation.txt";
    let test_path = PathBuf::from(CACHE_PATH)
        .join(TEST_REPO_NAME)
        .join("processor-tests/humans/");
    let path = test_path.join(name);
    let case = build_case(&std::fs::read_to_string(&path).unwrap());
    assert!(can_test(&case, || path.display(), true));
    assert!(test_file(case, &locales, || path.display()));
}

fn build_case(s: &str) -> TestCase {
    let mut s = Scanner::new(s);
    let mut builder = TestCaseBuilder::new();
    while !s.done() {
        let (tag, section) = section(&mut s).unwrap();
        builder.process(tag, section);
        eat_void(&mut s);
    }
    builder.finish().unwrap()
}

fn can_test<F, D>(case: &TestCase, mut display: F, print: bool) -> bool
where
    F: FnMut() -> D,
    D: fmt::Display,
{
    let can_test = case.bib_entries.is_none()
        && case.bib_section.is_none()
        && case.citations.is_none()
        && case.citation_items.as_ref().map_or(true, |cites| {
            cites.iter().flatten().all(|i| {
                i.prefix.is_none()
                    && i.suffix.is_none()
                    && i.near_note.is_none()
                    && i.position.is_none()
            })
        });

    let contains_date_ranges = case
        .input
        .iter()
        .flat_map(|i| i.0.values())
        .filter_map(|v| if let csl_json::Value::Date(d) = v { Some(d) } else { None })
        .any(|d| {
            csl_json::FixedDateRange::try_from(d.clone())
                .map_or(false, |d| d.end.is_some())
        });

    if case.mode == TestMode::Bibliography {
        if print {
            eprintln!("Skipping test {}\t(cause: Bibliography mode)", display());
        }
        false
    } else if !can_test {
        if print {
            eprintln!("Skipping test {}\t(cause: unsupported test feature)", display());
        }
        false
    } else if case.result.contains('<') {
        if print {
            eprintln!("Skipping test {}\t(cause: HTML suspected)", display());
        }
        false
    } else if contains_date_ranges {
        if print {
            eprintln!("Skipping test {}\t(cause: date ranges)", display());
        }
        false
    } else if case.input.iter().any(|i| i.has_html() || i.may_have_hack()) {
        if print {
            eprintln!("Skipping test {}\t(cause: HTML suspected in input)", display());
        }
        false
    } else {
        true
    }
}

fn test_file<F, D>(case: TestCase, locales: &[Locale], mut display: F) -> bool
where
    F: FnMut() -> D,
    D: fmt::Display,
{
    let Style::Independent(style) = case.csl else {
        panic!("test {} has dependent style", display());
    };

    let mut driver: BibliographyDriver<'_, csl_json::Item> = BibliographyDriver::new();
    let mut output = String::new();
    if let Some(cites) = &case.citation_items {
        for cite in cites {
            driver.citation(CitationRequest::new(
                cite.iter()
                    .map(|i| {
                        CitationItem::new(
                            case.input
                                .iter()
                                .find(|e| e.id().unwrap_or_default() == i.id.as_str())
                                .unwrap(),
                            i.locator.as_deref().map(|lo| {
                                SpecificLocator(
                                    i.label
                                        .as_deref()
                                        .map(|l| Locator::from_str(l).unwrap())
                                        .unwrap_or(Locator::Page),
                                    LocatorPayload::Str(lo),
                                )
                            }),
                            None,
                            false,
                            if i.suppress_author {
                                Some(hayagriva::CitePurpose::Author)
                            } else {
                                None
                            },
                        )
                    })
                    .collect(),
                &style,
                None,
                locales,
                Some(1),
            ));
        }
    } else {
        driver.citation(CitationRequest::new(
            case.input.iter().map(CitationItem::with_entry).collect(),
            &style,
            None,
            locales,
            Some(1),
        ));
    }

    let rendered = driver.finish(BibliographyRequest::new(&style, None, locales));

    for citation in rendered.citations {
        citation
            .citation
            .write_buf(&mut output, hayagriva::BufWriteFormat::Plain)
            .unwrap();
        output.push('\n');
    }

    if output.trim() == case.result.trim() {
        true
    } else {
        eprintln!("Test {} failed", display());
        eprintln!("Expected:\n{}", case.result);
        eprintln!("Got:\n{}", output);
        false
    }
}

#[test]
fn purposes() {
    let style = ArchivedStyle::by_name("apa").unwrap().get();
    let Style::Independent(style) = style else {
        panic!("test has dependent style");
    };

    let item: csl_json::Item = serde_json::from_str(
        r#"{
        "id": "ITEM-1",
        "title": "Book A",
        "author": [
            {
                "family": "Doe",
                "given": "John"
            }
        ],
        "issued": {
           "date-parts": [
             [
               "2000"
             ]
           ]
        },
        "type": "book"
    }"#,
    )
    .unwrap();

    for (purpose, res) in [
        (CitePurpose::Author, "Doe"),
        (CitePurpose::Prose, "Doe (2000)"),
        (CitePurpose::Year, "2000"),
        (CitePurpose::Full, "Doe, J. (2000). Book A."),
    ] {
        let mut driver: BibliographyDriver<'_, csl_json::Item> =
            BibliographyDriver::new();
        driver.citation(CitationRequest::new(
            vec![CitationItem::new(&item, None, None, false, Some(purpose))],
            &style,
            None,
            &[],
            Some(1),
        ));

        let rendered = driver.finish(BibliographyRequest::new(&style, None, &[]));
        let mut buf = String::new();
        rendered.citations[0]
            .citation
            .write_buf(&mut buf, hayagriva::BufWriteFormat::Plain)
            .unwrap();
        assert_eq!(buf, res);
    }
}

/// Load the file with the names of the test cases that should pass.
/// The file should be located at `tests/citeproc-pass.txt`.
/// It contains one test case name per line.
fn load_passing_tests() -> Vec<String> {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("citeproc-pass.txt");
    std::fs::read_to_string(path)
        .unwrap()
        .lines()
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(ToString::to_string)
        .collect()
}

/// Write the names of the test cases that should pass to the file
/// `tests/citeproc-pass.txt`.
fn write_passing_test(tests: &[String]) {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("citeproc-pass.txt");
    let comment = "# This file contains all test cases that should pass.\n\
        # The test fails if a test case in this file fails or if a test case\n\
        # that is not in this file passes.\n#\n\
        # This file is automatically generated by running `cargo test --test citeproc --features csl-json -- write_passing --ignored`.\n\n";

    let mut w = BufWriter::new(fs::File::create(path).unwrap());
    w.write_all(comment.as_bytes()).unwrap();
    for test in tests {
        w.write_all(test.as_bytes()).unwrap();
        w.write_all(b"\n").unwrap();
    }
    w.flush().unwrap();
}

#[test]
fn case_folding() {
    let style = ArchivedStyle::by_name("chicago-author-date").unwrap().get();
    let Style::Independent(style) = style else {
        panic!("test has dependent style");
    };

    let item: csl_json::Item = serde_json::from_str(
        r#"{
        "id": "ITEM-1",
        "container-title": "my lowercase container title",
        "type": "paper-conference"
    }"#,
    )
    .unwrap();

    let mut driver: BibliographyDriver<'_, csl_json::Item> = BibliographyDriver::new();
    driver.citation(CitationRequest::new(
        vec![CitationItem::new(&item, None, None, false, None)],
        &style,
        Some(LocaleCode("de-DE".to_string())),
        &[],
        Some(1),
    ));

    let rendered = driver.finish(BibliographyRequest::new(
        &style,
        Some(LocaleCode("de-DE".to_string())),
        &[],
    ));
    let mut buf = String::new();
    rendered.bibliography.unwrap().items[0]
        .content
        .write_buf(&mut buf, hayagriva::BufWriteFormat::Plain)
        .unwrap();
    assert_eq!(buf, ". my lowercase container title");
}

#[test]
fn access_date() {
    let style = ArchivedStyle::by_name("apa").unwrap().get();
    let locales = locales();
    let Style::Independent(style) = style else {
        panic!("test has dependent style");
    };

    let lib = from_biblatex_str(
        r#"@test{test,
        url={https://example.com},
        urldate={2021}
      }"#,
    )
    .unwrap();
    let entry = lib.get("test").unwrap();
    assert_eq!(entry.url().unwrap().visit_date.unwrap().year, 2021);

    let mut driver: BibliographyDriver<'_, Entry> = BibliographyDriver::new();
    driver.citation(CitationRequest::new(
        vec![CitationItem::new(entry, None, None, false, None)],
        &style,
        None,
        &locales,
        Some(1),
    ));

    let rendered = driver.finish(BibliographyRequest::new(&style, None, &locales));
    let mut buf = String::new();
    rendered.bibliography.unwrap().items[0]
        .content
        .write_buf(&mut buf, hayagriva::BufWriteFormat::Plain)
        .unwrap();
    assert_eq!(buf, "Retrieved 2021, from https://example.com/");
}
