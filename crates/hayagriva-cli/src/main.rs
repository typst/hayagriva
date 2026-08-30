/*!
Hayagriva provides a YAML-backed format and data model for various
bibliography items as well as a CSL processor formatting both in-text citations and
reference lists based on these literature databases.

The crate is intended to assist scholarly writing and reference management
and can be used both through a CLI and an API.

Below, there is an example of how to parse a YAML database and get a Modern
Language Association-style citation.

# Supported styles

Hayagriva supports all styles provided in the
[official Citation Style Language repository](https://github.com/citation-style-language/styles),
currently over 2,600. You can provide your own style files or use the ones
bundled with this library in the [`hayagriva_archive`] crate.

```ignore
use hayagriva_format::Entry;
let converted: Entry = your_biblatex_entry.into();
```

If you do not need BibLaTeX compatibility, you can use Hayagriva without the
default features by writing this in your `Cargo.toml`:

```toml
[dependencies]
hayagriva = { version = "0.9", default-features = false }
```
*/

use std::borrow::Cow;
use std::fs::{self, read_to_string};
use std::io::ErrorKind as IoErrorKind;
use std::path::Path;
use std::process::exit;

use citationberg::taxonomy::Locator;
use citationberg::{
    IndependentStyle, Locale, LocaleCode, LocaleFile, LongShortForm, Style,
};
use clap::builder::PossibleValue;
use clap::{Arg, ArgAction, Command, ValueEnum, crate_version};
use strum::VariantNames;

use hayagriva_archive::{ArchivedStyle, locales};
use hayagriva_csl::{
    BibliographyDriver, BibliographyRequest, CitationItem, CitationRequest,
    LocatorPayload, SpecificLocator,
};
#[cfg(feature = "biblatex")]
use hayagriva_format::io::from_biblatex_str;
use hayagriva_format::{Selector, io};

#[derive(Debug, Copy, Clone, PartialEq, VariantNames)]
#[strum(serialize_all = "kebab_case")]
/// Which input format is expected.
pub enum Format {
    #[cfg(feature = "biblatex")]
    /// The bibtex format.
    Bibtex,
    #[cfg(feature = "biblatex")]
    /// BibLaTeX format.
    Biblatex,
    /// Hayagriva YAML.
    Yaml,
}

impl ValueEnum for Format {
    fn value_variants<'a>() -> &'a [Self] {
        if cfg!(feature = "biblatex") {
            &[Self::Bibtex, Self::Biblatex, Self::Yaml]
        } else {
            &[Self::Yaml]
        }
    }

    fn to_possible_value(&self) -> Option<PossibleValue> {
        let value = match self {
            #[cfg(feature = "biblatex")]
            Format::Bibtex => "bibtex",
            #[cfg(feature = "biblatex")]
            Format::Biblatex => "biblatex",
            Format::Yaml => "yaml",
        };

        Some(PossibleValue::new(value))
    }
}

#[derive(ValueEnum, Clone)]
/// CSL locator types. This enum is used to specify the type of locator given
/// via the `--locators` argument of the `cite` command.
/// See the `--locator-type` argument.
enum LocatorTypes {
    Act,
    Appendix,
    ArticleLocator,
    Book,
    Canon,
    Chapter,
    Column,
    #[value(name = "e-location")]
    Elocation,
    Equation,
    Figure,
    Folio,
    Issue,
    Line,
    Note,
    Opus,
    Page,
    Paragraph,
    Part,
    Rule,
    Scene,
    Section,
    SubVerbo,
    Supplement,
    Table,
    Timestamp,
    Title,
    TitleLocator,
    Verse,
    Volume,
}

impl From<&LocatorTypes> for Locator {
    fn from(value: &LocatorTypes) -> Self {
        match value {
            LocatorTypes::Act => Locator::Act,
            LocatorTypes::Appendix => Locator::Appendix,
            LocatorTypes::ArticleLocator => Locator::ArticleLocator,
            LocatorTypes::Book => Locator::Book,
            LocatorTypes::Canon => Locator::Canon,
            LocatorTypes::Chapter => Locator::Chapter,
            LocatorTypes::Column => Locator::Column,
            LocatorTypes::Elocation => Locator::Elocation,
            LocatorTypes::Equation => Locator::Equation,
            LocatorTypes::Figure => Locator::Figure,
            LocatorTypes::Folio => Locator::Folio,
            LocatorTypes::Issue => Locator::Issue,
            LocatorTypes::Line => Locator::Line,
            LocatorTypes::Note => Locator::Note,
            LocatorTypes::Opus => Locator::Opus,
            LocatorTypes::Page => Locator::Page,
            LocatorTypes::Paragraph => Locator::Paragraph,
            LocatorTypes::Part => Locator::Part,
            LocatorTypes::Rule => Locator::Rule,
            LocatorTypes::Scene => Locator::Scene,
            LocatorTypes::Section => Locator::Section,
            LocatorTypes::SubVerbo => Locator::SubVerbo,
            LocatorTypes::Supplement => Locator::Supplement,
            LocatorTypes::Table => Locator::Table,
            LocatorTypes::Timestamp => Locator::Timestamp,
            LocatorTypes::Title => Locator::Title,
            LocatorTypes::TitleLocator => Locator::TitleLocator,
            LocatorTypes::Verse => Locator::Verse,
            LocatorTypes::Volume => Locator::Volume,
        }
    }
}

/// Main function of the Hayagriva CLI.
fn main() {
    let matches = Command::new("Hayagriva CLI")
            .version(crate_version!())
            .author("The Typst Project Developers <hi@typst.app>")
            .about("Format references and citations for your YAML-encoded or BibLaTeX bibliography files and query bibliographies using selectors.")
            .arg(
                Arg::new("INPUT")
                    .help("Sets the bibliography file to use")
                    .required(true)
                    .index(1)
            ).arg(
                Arg::new("format")
                    .long("format")
                    .help("What input file format to expect")
                    .value_parser(clap::value_parser!(Format))
                    .ignore_case(true)
                    .num_args(1)
                    .global(true),
            ).arg(
                Arg::new("selector")
                    .long("select")
                    .help("Filter the bibliography using selectors")
                    .num_args(1)
                    .global(true)
            )
            .arg(
                Arg::new("key")
                    .long("key")
                    .short('k')
                    .help("Filter the bibliography using a comma-separated list of keys")
                    .num_args(1)
                    .global(true)
            )
            .arg(
                Arg::new("show-keys")
                    .long("show-keys")
                    .help("Show the keys of all filtered entries")
                    .action(ArgAction::SetTrue)
                    .global(true)
            )
            .arg(
                Arg::new("show-bound")
                    .long("show-bound")
                    .help("Show the bound entries of your selector for each key")
                    .action(ArgAction::SetTrue)
                    .global(true)
            )
            .arg(
                Arg::new("no-fmt")
                    .long("no-fmt")
                    .short('n')
                    .help("Suppress the formatting of output with ANSI / VT100 character sequences")
                    .action(ArgAction::SetTrue)
                    .global(true)
            )
            .subcommand(
                Command::new("cite")
                    .about("Format citations for all filtered entries")
                    .arg(
                        Arg::new("style")
                            .long("style")
                            .short('s')
                            .help("Set the citation style")
                            .ignore_case(true)
                            .num_args(1)
                            .required_unless_present("csl")
                    )
                    .arg(
                        Arg::new("csl")
                            .long("csl")
                            .help("Set a CSL file to use the style therein")
                            .num_args(1)
                    )
                    .arg(
                        Arg::new("locale")
                            .long("locale")
                            .help("Which locale to force for the citation (e.g. `en-US`)")
                            .num_args(1)
                    )
                    .arg(
                        Arg::new("locales")
                            .long("locales")
                            .help("Set a comma-separated list of CSL locales")
                            .num_args(1)
                    )
                    .arg(
                        Arg::new("locators")
                            .long("locators")
                            .help("Specify additional information for the citations, e.g. \"p. 6,p. 4\", in a comma-separated list.")
                            .num_args(1)
                    )
                    .arg(
                        Arg::new("locator_type")
                            .long("type")
                            .help("Specify the type of the locator, e.g. page number, line number, timestamp.")
                            .value_parser(clap::value_parser!(LocatorTypes))
                            .hide_possible_values(true)
                            .long_help(r#"Specify the type of the locator, e.g. page number, line number, timestamp.

Possible values:
act         appendix        article-locator
book        canon           chapter
column      e-location      equation
figure      folio           issue
line        note            opus
page        paragraph       part
rule        scene           section
sub-verbo   supplement      table
timestamp   title           title-locator
verse       volume"#)
                            .ignore_case(true)
                            .num_args(1..)
                    )
                    .arg(
                        Arg::new("combined")
                            .long("combined")
                            .short('c')
                            .action(ArgAction::SetTrue)
                            .help("Combine all keys into one citation (ignored for Chicago Notes)")
                    )
            )
            .subcommand(
                Command::new("reference")
                    .about("Format a bibliography of all filtered entries")
                    .arg(
                        Arg::new("style")
                            .long("style")
                            .short('s')
                            .help("Set the referencing style")
                            .ignore_case(true)
                            .num_args(1)
                            .required_unless_present("csl")
                    )
                    .arg(
                        Arg::new("csl")
                            .long("csl")
                            .help("Set a CSL file to use the style therein")
                            .num_args(1)
                    )
                    .arg(
                        Arg::new("locale")
                            .long("locale")
                            .help("Which locale to force for the citation (e.g. `en-US`)")
                            .num_args(1)
                    )
                    .arg(
                        Arg::new("locales")
                            .long("locales")
                            .help("Set a space-separated list of CSL locales")
                            .num_args(1)
                    )
            )
            .subcommand(
                Command::new("styles")
                    .about("List all available citation styles")
            )
            .get_matches();

    let input = Path::new(matches.get_one::<String>("INPUT").unwrap());

    let format = matches.get_one("format").cloned().unwrap_or_else(|| {
        #[allow(unused_mut)]
        let mut format = Format::Yaml;

        #[cfg(feature = "biblatex")]
        if input
            .extension()
            .and_then(|ext| ext.to_str())
            .is_some_and(|ext| ext.to_lowercase() == "bib")
        {
            format = Format::Bibtex;
        }

        format
    });

    let bibliography = {
        let input = match read_to_string(input) {
            Ok(s) => s,
            Err(e) => {
                if e.kind() == IoErrorKind::NotFound {
                    eprintln!("Bibliography file \"{}\" not found.", input.display());
                    exit(5);
                } else if let Some(os) = e.raw_os_error() {
                    eprintln!(
                        "Error while reading the bibliography file \"{}\": {}",
                        input.display(),
                        os
                    );
                    exit(6);
                } else {
                    eprintln!(
                        "Error while reading the bibliography file \"{}\".",
                        input.display()
                    );
                    exit(6);
                }
            }
        };

        match format {
            Format::Yaml => io::from_yaml_str(&input).unwrap(),
            #[cfg(feature = "biblatex")]
            Format::Biblatex | Format::Bibtex => from_biblatex_str(&input).unwrap(),
        }
    };

    let bib_len = bibliography.len();

    let selector = matches.get_one::<String>("selector").cloned().map(|src| {
        match Selector::parse(&src) {
            Ok(selector) => selector,
            Err(err) => {
                eprintln!("Error while parsing selector: {err}");
                exit(7);
            }
        }
    });

    let bibliography = if let Some(keys) = matches.get_one::<String>("key") {
        let mut res = vec![];
        for key in keys.split(',') {
            if let Some(entry) = bibliography.iter().find(|e| e.key() == key) {
                res.push(entry.clone());
            }
        }
        res.into_iter().collect()
    } else if let Some(selector) = &selector {
        bibliography.into_iter().filter(|e| selector.matches(e)).collect()
    } else {
        bibliography
    };

    if matches.get_flag("show-keys") || matches.get_flag("show-bound") {
        println!(
            "Selected {} of {} entries in the bibliography\n",
            bibliography.len(),
            bib_len
        );

        for entry in &bibliography {
            println!("{}", entry.key());
            if matches.get_flag("show-bound")
                && let Some(selector) = &selector
            {
                for (k, v) in selector.apply(entry).unwrap() {
                    println!(
                        "\t{} => [{:?}] {}, {}",
                        k,
                        v.entry_type(),
                        if let Some(authors) = entry.authors().or_else(|| entry.editors())
                        {
                            authors.iter().map(|a| a.name.as_str()).fold(
                                String::new(),
                                |mut prev, curr| {
                                    if !prev.is_empty() {
                                        prev.push_str(", ");
                                    }
                                    prev.push_str(curr);
                                    prev
                                },
                            )
                        } else {
                            "no authors".to_string()
                        },
                        entry
                            .title()
                            .map(|s| s.select(LongShortForm::default()).to_str())
                            .unwrap_or_else(|| Cow::Borrowed("no title"))
                    );
                }
            }
        }
        exit(0);
    }

    match matches.subcommand() {
        Some(("reference", sub_matches)) => {
            let style: Option<&String> = sub_matches.get_one("style");
            let csl: Option<&String> = sub_matches.get_one("csl");
            let locale_path =
                sub_matches.get_one::<String>("locales").map(|s| s.split(','));
            let locale_str: Option<&String> = sub_matches.get_one("locale");

            let (style, locales, locale) =
                retrieve_assets(style, csl, locale_path, locale_str);

            if style.bibliography.is_none() {
                eprintln!("style has no bibliography");
                exit(4);
            }

            let mut driver = BibliographyDriver::new();
            for entry in &bibliography {
                driver.citation(CitationRequest::new(
                    vec![CitationItem::with_entry(entry)],
                    &style,
                    locale.clone(),
                    &locales,
                    None,
                ))
            }

            for row in driver
                .finish(BibliographyRequest::new(&style, locale, &locales))
                .bibliography
                .map(|b| b.items)
                .unwrap_or_default()
            {
                let alternate = matches.get_flag("no-fmt");

                if let Some(prefix) = row.first_field {
                    if alternate { println!("{prefix:#}") } else { println!("{prefix}") }
                }

                if alternate {
                    println!("{:#}", row.content)
                } else {
                    println!("{}", row.content)
                }
            }
        }
        Some(("cite", sub_matches)) => {
            let style: Option<&String> = sub_matches.get_one("style");
            let csl: Option<&String> = sub_matches.get_one("csl");
            let locale_path =
                sub_matches.get_one::<String>("locales").map(|s| s.split(','));
            let locale_str: Option<&String> = sub_matches.get_one("locale");
            let collapse = sub_matches.get_flag("combined");
            let locators: Vec<_> = sub_matches
                .get_one::<String>("locators")
                .into_iter()
                .flat_map(|s| s.split(','))
                .collect();

            let (style, locales, locale) =
                retrieve_assets(style, csl, locale_path, locale_str);

            let indicated_locator = sub_matches
                .get_one::<LocatorTypes>("locator_type")
                .map(Into::into)
                .unwrap_or(Locator::Custom);

            let assign_locator = |(i, e)| {
                let mut item = CitationItem::with_entry(e);
                if let Some(&locator) = locators.get(i) {
                    item.locator = Some(SpecificLocator(
                        indicated_locator,
                        LocatorPayload::Str(locator),
                    ));
                }
                item
            };

            let mut driver = BibliographyDriver::new();
            if collapse {
                driver.citation(CitationRequest::new(
                    bibliography.iter().enumerate().map(assign_locator).collect(),
                    &style,
                    locale.clone(),
                    &locales,
                    None,
                ));
            } else {
                for (i, entry) in bibliography.iter().enumerate() {
                    driver.citation(CitationRequest::new(
                        vec![assign_locator((i, entry))],
                        &style,
                        locale.clone(),
                        &locales,
                        None,
                    ))
                }
            }

            for row in driver
                .finish(BibliographyRequest::new(&style, locale, &locales))
                .citations
            {
                let alternate = matches.get_flag("no-fmt");

                if let Some(note_number) = row.note_number {
                    if alternate {
                        println!("{note_number:#}")
                    } else {
                        println!("{note_number}.")
                    }
                }

                if alternate {
                    println!("{:#}", row.citation)
                } else {
                    println!("{}", row.citation)
                }
            }
        }
        Some(("styles", _)) => {
            for key in ArchivedStyle::all() {
                let style = key.get();
                println!("- {}", key.names()[0]);
                println!("  Full name: {}", style.info().title.value);
                println!("  Authors:");
                for author in style.info().authors.iter() {
                    print!("  - {}", author.name);
                    if let Some(email) = &author.email {
                        println!(" <{email}>");
                    } else {
                        println!();
                    }
                }
                if let Some(desc) = &style.info().summary {
                    println!("  Summary: {}", desc.value);
                }
            }
        }
        _ => {
            let bib = io::to_yaml_str(&bibliography).unwrap();
            println!("{bib}");
        }
    }
}

fn retrieve_assets<'a>(
    style: Option<&String>,
    csl: Option<&String>,
    locale_paths: Option<impl Iterator<Item = &'a str>>,
    locale_str: Option<&String>,
) -> (IndependentStyle, Vec<Locale>, Option<LocaleCode>) {
    let locale: Option<_> = locale_str.map(|l: &String| LocaleCode(l.into()));

    let style = match (style, csl) {
        (_, Some(csl)) => {
            let file_str = fs::read_to_string(csl).expect("could not read CSL file");
            IndependentStyle::from_xml(&file_str).expect("CSL file malformed")
        }
        (Some(style), _) => {
            let Style::Independent(indep) =
                ArchivedStyle::by_name(style.as_str()).expect("no style found").get()
            else {
                panic!("dependent style in archive")
            };
            indep
        }
        (None, None) => panic!("must specify style or CSL file"),
    };

    let locales: Vec<Locale> = match locale_paths {
        Some(locale_paths) => locale_paths
            .into_iter()
            .map(|locale_path| {
                let file_str =
                    fs::read_to_string(locale_path).expect("could not read locale file");
                LocaleFile::from_xml(&file_str).expect("locale file malformed").into()
            })
            .collect(),
        None => locales(),
    };

    (style, locales, locale)
}

#[cfg(test)]
mod tests {
    use std::{borrow::Cow, fs, path::Path, sync::Arc};

    use citationberg::{
        IndependentStyle, LocaleCode, LocaleFile,
        taxonomy::{Locator, NumberVariable},
    };
    use hayagriva_core::{EntryLike, MaybeTyped, Numeric, NumericValue};
    use hayagriva_csl::{
        BibliographyDriver, BibliographyRequest, BufWriteFormat, CitationItem,
        CitationRequest, CitePurpose, LocatorPayload, SpecificLocator,
        TransparentLocator,
    };
    use hayagriva_format::io::from_yaml_str;

    #[test]
    fn test_csl() {
        let workspace = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../");
        let en_locale =
            fs::read_to_string(workspace.join("tests/data/locales-en-US.xml")).unwrap();
        let en_locale = LocaleFile::from_xml(&en_locale).unwrap();

        let yaml = fs::read_to_string(workspace.join("tests/data/basic.yml")).unwrap();
        let bib = from_yaml_str(&yaml).unwrap();
        let en_locale = [en_locale.into()];

        for style_thing in fs::read_dir(workspace.join("styles/")).unwrap().take(100) {
            let thing = style_thing.unwrap();
            if thing.file_type().unwrap().is_dir() {
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

            println!("testing {path:?}");
            let style = fs::read_to_string(path).unwrap();
            let style = IndependentStyle::from_xml(&style).unwrap();
            let mut driver = BibliographyDriver::new();

            for n in (0..bib.len()).step_by(2) {
                let items = vec![
                    CitationItem::with_entry(bib.nth(n).unwrap()),
                    CitationItem::with_entry(bib.nth(n + 1).unwrap()),
                ];
                driver.citation(CitationRequest::new(
                    items, &style, None, &en_locale, None,
                ));
            }

            driver.finish(BibliographyRequest {
                style: &style,
                locale: None,
                locale_files: &en_locale,
            });

            // for cite in finished.citations {
            //     println!("{}", cite.citation.to_string(BufWriteFormat::Plain))
            // }

            // if let Some(bib) = finished.bibliography {
            //     for (prefix, item) in bib.items {
            //         if let Some(prefix) = prefix {
            //             print!("{} ", prefix.to_string(BufWriteFormat::Plain))
            //         }
            //         println!("{}", item.to_string(BufWriteFormat::Html))
            //     }
            // }
        }
    }

    #[test]
    fn test_transparent_locator_ibid() {
        let test_data = r#"
            katalog:
                type: Book
                title: "Book"
                author: "Surname, Name"
                location: Warsaw
                date: 1971
            katalog2:
                type: Book
                title: "Book2"
                author: "Surname, Name"
                location: Warsaw
                date: 1971
        "#;
        let test_style = r#"<?xml version="1.0" encoding="utf-8"?>
        <style xmlns="http://purl.org/net/xbiblio/csl" class="note" version="1.0" demote-non-dropping-particle="sort-only">
           <info>
              <title>Citing style</title>
              <id>http://www.example.com/</id>
           </info>

           <citation>
              <layout prefix="" suffix="." delimiter="; ">
                 <choose>
                    <if position="first">
                      <text value="first" />
                    </if>
                    <else-if position="ibid-with-locator">
                      <text value="ibid-with-locator" />
                    </else-if>
                    <else-if position="ibid">
                       <text value="ibid" />
                    </else-if>
                    <else-if position="subsequent">
                      <text value="subsequent" />
                    </else-if>
                    <else>
                      <text value="other" />
                    </else>
                 </choose>
              </layout>
           </citation>
          <bibliography>
            <sort>
              <key macro="contributors-biblio"/>
            </sort>
            <layout suffix=".">
              <group delimiter=", ">
                <group delimiter=", ">
                  <text macro="locators-chapter"/>
                </group>
              </group>
            </layout>
          </bibliography>
        </style>"#;

        let library = from_yaml_str(test_data).unwrap();
        let style = citationberg::Style::from_xml(test_style).unwrap();
        let citationberg::Style::Independent(style) = style else { unreachable!() };

        let mut driver = BibliographyDriver::new();
        let citations = [
            ("katalog", Some([2, 3]), "first."),
            ("katalog", Some([2, 3]), "ibid."),
            ("katalog", Some([2, 3]), "ibid."),
            ("katalog", Some([2, 4]), "ibid-with-locator."),
            ("katalog", None, "subsequent."),
            ("katalog", Some([2, 3]), "ibid-with-locator."),
            ("katalog", Some([2, 3]), "ibid."),
            ("katalog2", Some([2, 3]), "first."),
        ];

        for (entry, supplement, _) in citations {
            let entry = library.get(entry).unwrap();
            driver.citation(CitationRequest::new(
                vec![CitationItem::with_locator(
                    entry,
                    supplement.map(|s| {
                        SpecificLocator(
                            Locator::Supplement,
                            LocatorPayload::Transparent(TransparentLocator(Arc::new(s))),
                        )
                    }),
                )],
                &style,
                None,
                &[],
                None,
            ));
        }

        let finished = driver.finish(BibliographyRequest {
            style: &style,
            locale: None,
            locale_files: &[],
        });

        for (citation, (_, _, expected_value)) in finished.citations.iter().zip(citations)
        {
            let mut s = String::new();
            citation.citation.write_buf(&mut s, BufWriteFormat::Plain).unwrap();
            assert_eq!(s, expected_value);
        }
    }

    #[test]
    fn test_chapter_field() {
        let workspace = Path::new(env!("CARGO_MANIFEST_DIR"));
        let yaml =
            fs::read_to_string(workspace.join("../../tests/data/basic.yml")).unwrap();
        let bib = from_yaml_str(&yaml).unwrap();
        let book = bib.get("lamb").unwrap();
        assert_eq!(book.chapter().unwrap(), &MaybeTyped::Typed(Numeric::new(20)));
        assert_eq!(book.volume().unwrap(), &MaybeTyped::Typed(Numeric::new(10)));
        assert_eq!(book.keyed_serial_number("serial"), None);
        assert_eq!(book.resolve_number_variable(NumberVariable::Number), None);

        let book = bib.get("snail").unwrap();
        assert_eq!(
            book.chapter().unwrap(),
            &MaybeTyped::Typed(Numeric {
                value: NumericValue::Number(2),
                prefix: None,
                suffix: Some(Box::new("A".into()))
            })
        );
        assert_eq!(
            book.volume().unwrap(),
            &MaybeTyped::Typed(Numeric {
                value: NumericValue::Number(4),
                prefix: None,
                suffix: Some(Box::new("B".into()))
            })
        );
        assert_eq!(book.keyed_serial_number("serial"), None);
        assert_eq!(book.resolve_number_variable(NumberVariable::Number), None);

        let chapter = bib.get("lamb-chapter").unwrap();
        assert_eq!(chapter.chapter().unwrap(), &MaybeTyped::Typed(Numeric::new(3)));
        assert_eq!(chapter.keyed_serial_number("serial"), None);
        assert_eq!(
            chapter.resolve_number_variable(NumberVariable::Number),
            Some(MaybeTyped::Typed(Cow::Borrowed(&Numeric::new(3))))
        );
        assert_eq!(chapter.volume(), None);
        assert_eq!(
            chapter.parents().first().unwrap().volume().unwrap(),
            &MaybeTyped::Typed(Numeric::new(10))
        );

        let chapter = bib.get("snail-chapter").unwrap();
        assert_eq!(chapter.chapter(), None);
        assert_eq!(chapter.keyed_serial_number("serial").unwrap(), "3");

        assert_eq!(chapter.volume(), None);
        assert_eq!(
            chapter.parents().first().unwrap().volume().unwrap(),
            &MaybeTyped::Typed(Numeric {
                value: NumericValue::Number(4),
                prefix: None,
                suffix: Some(Box::new("B".into()))
            })
        );
    }

    #[test]
    fn test_alphanumeric_disambiguation() {
        let bibtex = r#"@article{chenTransMorphTransformerUnsupervised2021,
        title = {{{TransMorph}}: {{Transformer}} for Unsupervised Medical Image Registration},
        author = {Chen, Junyu and Frey, Eric C. and He, Yufan and Segars, William P. and Li, Ye and Du, Yong},
        date = {2021},
        }

        @article{chenViTVNetVisionTransformer2021,
        title = {{{ViT-V-Net}}: {{Vision Transformer}} for {{Unsupervised Volumetric Medical Image Registration}}},
        author = {Chen, Junyu and He, Yufan and Frey, Eric C. and Li, Ye and Du, Yong},
        date = {2021},
        }"#;

        let library = crate::io::from_biblatex_str(bibtex).unwrap();
        let alphanumeric = hayagriva_archive::ArchivedStyle::Alphanumeric.get();
        let citationberg::Style::Independent(alphanumeric) = alphanumeric else {
            unreachable!()
        };

        let mut driver = BibliographyDriver::new();
        for entry in library.iter() {
            driver.citation(CitationRequest::new(
                vec![CitationItem::with_entry(entry)],
                &alphanumeric,
                None,
                &[],
                None,
            ));
        }

        let finished = driver.finish(BibliographyRequest {
            style: &alphanumeric,
            locale: None,
            locale_files: &[],
        });

        let mut c1 = String::new();
        let mut c2 = String::new();

        finished.citations[0]
            .citation
            .write_buf(&mut c1, BufWriteFormat::Plain)
            .unwrap();
        finished.citations[1]
            .citation
            .write_buf(&mut c2, BufWriteFormat::Plain)
            .unwrap();

        assert_eq!(c1, "[Che+21a]");
        assert_eq!(c2, "[Che+21b]");
    }

    #[test]
    /// See https://github.com/typst/hayagriva/issues/243
    fn issue_243() {
        let bibtex = r#"@book{downs57,
            title = {An Economic Theory of Democracy},
            author = {Downs, Anthony},
            date = {1957},
            edition = {1},
            publisher = {Harper \& Row},
            location = {New York},
            langid = {english}
            }

            @book{brady_collier10,
            title = {Rethinking Social Inquiry. Diverse Tools, Shared Standards},
            editor = {Brady, Henry E. and Collier, David},
            date = {2010},
            edition = {2},
            publisher = {Rowman \& Littlefield Publishers},
            location = {Maryland}
            }"#;

        let library = crate::io::from_biblatex_str(bibtex).unwrap();
        let apa =
            hayagriva_archive::ArchivedStyle::AmericanPsychologicalAssociation.get();
        let citationberg::Style::Independent(apa) = apa else { unreachable!() };

        let locales = hayagriva_archive::locales();

        let mut driver = BibliographyDriver::new();
        for entry in library.iter() {
            driver.citation(CitationRequest::new(
                vec![CitationItem::new(
                    entry,
                    None,
                    Some(LocaleCode::en_us()),
                    false,
                    Some(CitePurpose::Prose),
                )],
                &apa,
                Some(LocaleCode::en_us()),
                &locales,
                None,
            ));
        }

        let finished = driver.finish(BibliographyRequest {
            style: &apa,
            locale: Some(LocaleCode::en_us()),
            locale_files: &locales,
        });

        let mut c1 = String::new();
        let mut c2 = String::new();

        finished.citations[0]
            .citation
            .write_buf(&mut c1, BufWriteFormat::Plain)
            .unwrap();
        finished.citations[1]
            .citation
            .write_buf(&mut c2, BufWriteFormat::Plain)
            .unwrap();

        assert_eq!(c1, "Downs (1957)");
        assert_eq!(c2, "Brady & Collier (2010)");
    }

    #[test]
    /// See https://github.com/typst/hayagriva/issues/48
    fn issue_48() {
        let bibtex = r#"@article{chenTransMorphTransformerUnsupervised2021,
        title = {{{TransMorph}}: {{Transformer}} for Unsupervised Medical Image Registration},
        author = {Chen, Junyu and Frey, Eric C. and He, Yufan and Segars, William P. and Li, Ye and Du, Yong},
        date = {2021},
        }

        @article{chenViTVNetVisionTransformer2021,
        title = {{{ViT-V-Net}}: {{Vision Transformer}} for {{Unsupervised Volumetric Medical Image Registration}}},
        author = {Chen, Junyu and He, Yufan and Frey, Eric C. and Li, Ye and Du, Yong},
        date = {2021},
        }"#;

        let library = crate::io::from_biblatex_str(bibtex).unwrap();
        let alphanumeric = hayagriva_archive::ArchivedStyle::Alphanumeric.get();
        let citationberg::Style::Independent(alphanumeric) = alphanumeric else {
            unreachable!()
        };

        let mut driver = BibliographyDriver::new();

        let locator = SpecificLocator(Locator::Custom, LocatorPayload::Str("12"));
        for entry in library.iter() {
            driver.citation(CitationRequest::new(
                vec![CitationItem::with_locator(entry, Some(locator.clone()))],
                &alphanumeric,
                None,
                &[],
                None,
            ));
        }

        driver.citation(CitationRequest::new(
            vec![CitationItem::with_locator(library.iter().next().unwrap(), None)],
            &alphanumeric,
            None,
            &[],
            None,
        ));

        let finished = driver.finish(BibliographyRequest {
            style: &alphanumeric,
            locale: None,
            locale_files: &[],
        });

        let mut c1 = String::new();
        let mut c2 = String::new();
        let mut c3 = String::new();

        finished.citations[0]
            .citation
            .write_buf(&mut c1, BufWriteFormat::Plain)
            .unwrap();
        finished.citations[1]
            .citation
            .write_buf(&mut c2, BufWriteFormat::Plain)
            .unwrap();
        finished.citations[2]
            .citation
            .write_buf(&mut c3, BufWriteFormat::Plain)
            .unwrap();

        assert_eq!(c1, "[Che+21a, 12]");
        assert_eq!(c2, "[Che+21b, 12]");
        assert_eq!(c3, "[Che+21a]");
    }

    #[test]
    fn issue_347() {
        let bibtex = r#"@book{pratchett96,
            title = {Eric},
            author = {Pratchett, T.},
            year = {1996},
            publisher = {Vista}
        }"#;

        let library = crate::io::from_biblatex_str(bibtex).unwrap();
        let mla = hayagriva_archive::ArchivedStyle::ModernLanguageAssociation.get();
        let citationberg::Style::Independent(mla) = mla else { unreachable!() };
        let entry = library.iter().next().unwrap();
        let locales = hayagriva_archive::locales();

        let mut driver = BibliographyDriver::new();
        driver.citation(CitationRequest::new(
            vec![CitationItem::with_entry(entry).kind(CitePurpose::Prose)],
            &mla,
            None,
            &locales,
            None,
        ));
        let rendered = driver.finish(BibliographyRequest::new(&mla, None, &locales));
        let mut output = String::new();
        rendered.citations[0]
            .citation
            .write_buf(&mut output, BufWriteFormat::Plain)
            .unwrap();

        assert_eq!(output, "Pratchett");
    }

    #[test]
    /// A webpage with only a year (no month/day) as its issued date must not render a dangling delimiter before the (empty) month/day part.
    ///
    /// See https://github.com/typst/hayagriva/issues/246
    fn issue_year_only_date_apa() {
        let yaml = r#"
        nistCVE:
            type: Web
            author: "NIST"
            title: "CVE-2021-44228"
            date: "2021"
            url:
                value: "https://nvd.nist.gov/vuln/detail/CVE-2021-44228"
                date: 2024-10-28
        "#;

        let library = from_yaml_str(yaml).unwrap();
        let apa =
            hayagriva_archive::ArchivedStyle::AmericanPsychologicalAssociation.get();
        let citationberg::Style::Independent(apa) = apa else { unreachable!() };
        let locales = hayagriva_archive::locales();

        let mut driver = BibliographyDriver::new();
        driver.citation(CitationRequest::new(
            vec![CitationItem::with_entry(library.iter().next().unwrap())],
            &apa,
            None,
            &locales,
            None,
        ));

        let finished = driver.finish(BibliographyRequest {
            style: &apa,
            locale: None,
            locale_files: &locales,
        });

        let mut bib_entry = String::new();
        finished
            .bibliography
            .unwrap()
            .items
            .remove(0)
            .content
            .write_buf(&mut bib_entry, BufWriteFormat::Plain)
            .unwrap();

        assert_eq!(
            bib_entry,
            "NIST. (2021). CVE-2021-44228. https://nvd.nist.gov/vuln/detail/CVE-2021-44228"
        );
    }

    #[test]
    fn ibid_handling_with_deutsche_sprache_csl() {
        let bibtex = r#"@book{ITEM,
            title = {A},
            type = {book},
            }"#;

        let library = crate::io::from_biblatex_str(bibtex).unwrap();
        let style = hayagriva_archive::ArchivedStyle::DeutscheSprache.get();
        let citationberg::Style::Independent(style) = style else { unreachable!() };

        let mut driver = BibliographyDriver::new();
        let entry = library.iter().next().unwrap();

        for locator in ["33", "33", "34"] {
            driver.citation(CitationRequest::new(
                vec![CitationItem::new(
                    entry,
                    Some(SpecificLocator(Locator::Page, LocatorPayload::Str(locator))),
                    None,
                    false,
                    Some(CitePurpose::Prose),
                )],
                &style,
                None,
                &[],
                None,
            ));
        }

        let finished = driver.finish(BibliographyRequest {
            style: &style,
            locale: None,
            locale_files: &[],
        });

        let actual = finished
            .citations
            .iter()
            .map(|c| {
                let mut s = String::new();
                c.citation.write_buf(&mut s, BufWriteFormat::Plain).unwrap();
                s
            })
            .collect::<Vec<_>>();

        assert_eq!(actual, ["(33)", "()", "(34)"]);
    }

    #[test]
    #[cfg(feature = "biblatex")]
    fn test_issue_227() {
        let yaml = r#"
AAAnonymous_AventureMortevielle_1987:
  type: Book
  page-range: 100"#;

        let library = crate::io::from_yaml_str(yaml).unwrap();
        let entry = library.get("AAAnonymous_AventureMortevielle_1987").unwrap();
        assert_eq!(
            entry
                .page_range()
                .as_ref()
                .unwrap()
                .as_typed()
                .unwrap()
                .first()
                .unwrap(),
            &Numeric::new(100)
        );
    }
}
