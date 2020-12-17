use biblatex::Bibliography as TexBibliography;
use clap::{value_t, crate_version, App, AppSettings, Arg, SubCommand};
use hayagriva::input::load_yaml_structure;
use hayagriva::output::chicago::bibliography::BibliographyFormatter as ChicagoBib;
use hayagriva::output::{apa, chicago, ieee, mla, BibliographyFormatter};
use hayagriva::selectors::parse;
use std::fs::read_to_string;
use std::str::FromStr;
use strum::{EnumVariantNames, VariantNames};

#[derive(Debug, Copy, Clone, PartialEq, EnumVariantNames)]
#[strum(serialize_all = "kebab_case")]
pub enum Mode {
    Keys,
    Citation,
    Reference,
    Biblatex,
    Bibtex,
    Yaml,
}

impl FromStr for Mode {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, &'static str> {
        match s.to_ascii_lowercase().as_ref() {
            "keys" => Ok(Mode::Keys),
            "citation" => Ok(Mode::Citation),
            "reference" => Ok(Mode::Reference),
            "bibtex" => Ok(Mode::Bibtex),
            "biblatex" => Ok(Mode::Biblatex),
            "yaml" => Ok(Mode::Yaml),
            _ => Err("unknown mode"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, EnumVariantNames)]
#[strum(serialize_all = "kebab_case")]
pub enum Format {
    Bibtex,
    Biblatex,
    Yaml,
}

impl FromStr for Format {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, &'static str> {
        match s.to_ascii_lowercase().as_ref() {
            "bibtex" => Ok(Format::Bibtex),
            "biblatex" => Ok(Format::Biblatex),
            "yaml" => Ok(Format::Yaml),
            _ => Err("unknown format"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, EnumVariantNames)]
#[strum(serialize_all = "kebab_case")]
pub enum Style {
    Chicago,
    Mla,
    Apa,
    Ieee,
}

impl FromStr for Style {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, &'static str> {
        match s.to_ascii_lowercase().as_ref() {
            "chicago" => Ok(Style::Chicago),
            "mla" => Ok(Style::Mla),
            "apa" => Ok(Style::Apa),
            "ieee" => Ok(Style::Ieee),
            _ => Err("unknown style"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, EnumVariantNames)]
#[strum(serialize_all = "kebab_case")]
pub enum CitationStyle {
    AuthorDate,
    ChicagoNote,
    Numeric,
    Alphabetic,
    AuthorTitle,
}

impl FromStr for CitationStyle {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, &'static str> {
        match s.to_ascii_lowercase().as_ref() {
            "author-date" | "author-year" => Ok(CitationStyle::AuthorDate),
            "note" | "chicago-note" | "chicago" => Ok(CitationStyle::ChicagoNote),
            "numeric" | "ieee" => Ok(CitationStyle::Numeric),
            "alphabetic" | "alpha" => Ok(CitationStyle::Alphabetic),
            "author-title" => Ok(CitationStyle::AuthorTitle),
            _ => Err("unknown style"),
        }
    }
}

/// Main function of the Hayagriva CLI.
fn main() {
    let matches = App::new("Hayagriva CLI")
        .version(crate_version!())
        .author("The Typst Project Developers <hi@typst.app>")
        .setting(AppSettings::VersionlessSubcommands)
        .about("Output appropriate references and citations for your YAML-encoded bibliography database. Query the database for various source types with selectors.")
        .arg(
            Arg::with_name("INPUT").help("Sets the bibliograpghy to use").required(true).index(1)
        )
        .arg(
            Arg::with_name("format")
                .long("format")
                .short("f")
                .help("What input file format to expect")
                .possible_values(&Format::VARIANTS)
                .case_insensitive(true)
                .takes_value(true)
        )
        .arg(
            Arg::with_name("selector")
                .long("select")
                .help("Filter your bibliography using selectors")
                .takes_value(true)
        )
        .arg(
            Arg::with_name("key")
                .long("key")
                .short("k")
                .help("Get a specific key from the database")
                .takes_value(true)
        )
        .subcommand(
            SubCommand::with_name("reference")
                .about("Create references for your Bibliography / Works Cited listing")
                .arg(
                    Arg::with_name("style")
                        .long("style")
                        .short("s")
                        .help("Set the referencing style")
                        .possible_values(&Style::VARIANTS)
                        .case_insensitive(true)
                        .takes_value(true)
                )
        )
        .subcommand(
            SubCommand::with_name("keys")
                .about("Show the selected keys and, optionally, the parents bound by your selector")
                .arg(
                    Arg::with_name("bindings")
                        .long("show-bound")
                        .short("b")
                        .help("Show the parents that were bound by your selector")
                )
        )
        .subcommand(
            SubCommand::with_name("citation")
                .about("Get pointers to your sources for use in your text")
                .arg(
                    Arg::with_name("style")
                        .long("style")
                        .short("s")
                        .help("Set the citation style")
                        .possible_values(&CitationStyle::VARIANTS)
                        .case_insensitive(true)
                        .takes_value(true)
                )
        )
        .subcommand(
            SubCommand::with_name("dump")
                .about("Get a bibliography file with your selected entries")
                .arg(
                    Arg::with_name("output-format")
                        .long("output-format")
                        .short("o")
                        .help("Set the desired output format")
                        .possible_values(&Format::VARIANTS)
                        .case_insensitive(true)
                        .takes_value(true)
                )
        )
        .get_matches();

    let input = matches.value_of("INPUT").unwrap();
    let format = value_t!(matches, "format", Format).unwrap_or_else(|_| {
        if let Some(pos) = input.rfind('.') {
            let file_ext = &input[pos + 1 ..].to_lowercase();
            match file_ext.as_ref() {
                "bib" => Format::Bibtex,
                _ => Format::Yaml,
            }
        } else {
            Format::Yaml
        }
    });

    let bibliography = match format {
        Format::Yaml => load_yaml_structure(&read_to_string(input).unwrap()).unwrap(),
        Format::Biblatex | Format::Bibtex => {
            let tex = TexBibliography::parse(&read_to_string(input).unwrap()).unwrap();
            tex.into_iter().map(|e| e.into()).collect()
        }
    };

    let bibliography = if let Some(key) = matches.value_of("key") {
        bibliography.into_iter().filter(|e| e.key() == key).collect()
    } else if let Some(selector) = matches.value_of("selector") {
        let selector = parse(selector).unwrap();
        bibliography.into_iter().filter(|e| selector.matches(e)).collect()
    } else {
        bibliography
    };

    match matches.subcommand() {
        ("keys", _) => {
            for entry in &bibliography {
                println!("{}", entry.key());
            }
        }
        ("reference", Some(sub_matches)) => {
            match value_t!(sub_matches, "style", Style).unwrap_or_else(|_| Style::Chicago) {
                Style::Chicago => {
                    let chicago = ChicagoBib::new(chicago::Mode::AuthorDate);
                    for entry in &bibliography {
                        println!("{}", chicago.format(entry, None).print_ansi_vt100());
                    }
                }
                Style::Apa => {
                    let apa = apa::ApaBibliographyFormatter::new();
                    for entry in &bibliography {
                        println!("{}", apa.format(entry, None).print_ansi_vt100());
                    }
                }
                Style::Ieee => {
                    let ieee = ieee::IeeeBibliographyFormatter::new();
                    for entry in &bibliography {
                        println!("{}", ieee.format(entry, None).print_ansi_vt100());
                    }
                }
                Style::Mla => {
                    let mla = mla::MlaBibliographyFormatter::new();
                    for entry in &bibliography {
                        println!("{}", mla.format(entry, None).print_ansi_vt100());
                    }
                }
            }
        }
        _ => todo!(),
    }
}
