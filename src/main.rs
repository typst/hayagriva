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

use hayagriva::archive::{ArchivedStyle, locales};
use hayagriva::{
    BibliographyDriver, CitationItem, CitationRequest, LocatorPayload, SpecificLocator,
    io,
};
use hayagriva::{BibliographyRequest, Selector};

#[derive(Debug, Copy, Clone, PartialEq, VariantNames)]
#[strum(serialize_all = "kebab_case")]
pub enum Format {
    #[cfg(feature = "biblatex")]
    Bibtex,
    #[cfg(feature = "biblatex")]
    Biblatex,
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
            Format::Biblatex | Format::Bibtex => io::from_biblatex_str(&input).unwrap(),
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

            let indicated_locator = if let Some(locator_type) =
                sub_matches.get_one::<LocatorTypes>("locator_type")
            {
                locator_type.into()
            } else {
                Locator::Custom
            };

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
