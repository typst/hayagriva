use biblatex::Bibliography as TexBibliography;
use clap::{crate_version, value_t, App, AppSettings, Arg, SubCommand};
use hayagriva::input::load_yaml_structure;
use hayagriva::output::chicago::bibliography::BibliographyFormatter as ChicagoBib;
use hayagriva::output::{
    apa, chicago, ieee, mla, Alphabetical, AtomicCitation, BibliographyFormatter,
    CitationFormatter,
};
use hayagriva::selectors::parse;
use std::fs::read_to_string;
use std::str::FromStr;
use strum::{EnumVariantNames, VariantNames};

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
    ChicagoAuthorDate,
    Mla,
    Apa,
    Ieee,
}

impl FromStr for Style {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, &'static str> {
        match s.to_ascii_lowercase().as_ref() {
            "chicago" | "chicago-bibliography" => Ok(Style::Chicago),
            "chicago-author-date" | "author-date" => Ok(Style::ChicagoAuthorDate),
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
    Alphabetical,
    AuthorTitle,
}

impl FromStr for CitationStyle {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, &'static str> {
        match s.to_ascii_lowercase().as_ref() {
            "author-date" | "author-year" => Ok(CitationStyle::AuthorDate),
            "note" | "chicago-note" | "chicago" => Ok(CitationStyle::ChicagoNote),
            "alphabetic" | "alpha" | "alphabetical" => Ok(CitationStyle::Alphabetical),
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
            Arg::with_name("INPUT").help("Sets the bibliography to use").required(true).index(1)
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
                .help("Filter the database using a comma-seperated list of keys")
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
                .arg(
                    Arg::with_name("supplement")
                        .long("supplement")
                        .help("Specify some information for the citation, e.g. \"p. 6\", in a comma-seperated list.")
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
                .arg(
                    Arg::with_name("combined")
                        .long("combined")
                        .short("c")
                        .help("Combine all keys into one citation (ignored for Chicago Notes)")
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

    let bib_len = bibliography.len();

    let bibliography = if let Some(keys) = matches.value_of("key") {
        let mut res = vec![];
        for key in keys.split(',') {
            if let Some(entry) = bibliography.iter().find(|e| e.key() == key) {
                res.push(entry.clone());
            }
        }
        res
    } else if let Some(selector) = matches.value_of("selector") {
        let selector = parse(selector).expect("Invalid selector");
        bibliography.into_iter().filter(|e| selector.matches(e)).collect()
    } else {
        bibliography
    };

    match matches.subcommand() {
        ("keys", Some(sub_matches)) => {
            println!(
                "Selected {} of {} entries in the bibliography\n",
                bibliography.len(),
                bib_len
            );

            for entry in &bibliography {
                println!("{}", entry.key());
                if sub_matches.is_present("bindings") {
                    if let Some(selector) = matches.value_of("selector") {
                        let selector = parse(selector).unwrap();
                        let formatter = chicago::notes::NoteCitationFormatter::new(
                            bibliography.iter(),
                        );
                        for (k, v) in selector.apply(entry).unwrap() {
                            println!(
                                "\t{} => [{}] {}",
                                k,
                                v.kind(),
                                formatter
                                    .get_entry_note(
                                        v,
                                        None,
                                        chicago::notes::NoteType::Short
                                    )
                                    .value
                            );
                        }
                    }
                }
            }
        }
        ("reference", Some(sub_matches)) => {
            match value_t!(sub_matches, "style", Style)
                .unwrap_or_else(|_| Style::ChicagoAuthorDate)
            {
                Style::Chicago => {
                    let chicago = ChicagoBib::new(chicago::Mode::NotesAndBibliography);
                    for entry in &bibliography {
                        println!("{}", chicago.format(entry, None).print_ansi_vt100());
                    }
                }
                Style::ChicagoAuthorDate => {
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
        ("citation", Some(sub_matches)) => {
            let style = value_t!(sub_matches, "style", CitationStyle)
                .unwrap_or_else(|_| CitationStyle::AuthorDate);
            let supplements = matches
                .value_of("supplement")
                .map(|sup| sup.split(',').collect::<Vec<_>>())
                .unwrap_or_default();
            let mut atomics = bibliography
                .iter()
                .enumerate()
                .map(|(i, e)| {
                    let supp = supplements.get(i).cloned();
                    AtomicCitation::new(e.key(), supp, None)
                })
                .collect::<Vec<_>>();
            let combined = sub_matches.is_present("combined");
            match style {
                CitationStyle::AuthorDate => {
                    let formatter =
                        chicago::author_date::AuthorYear::new(bibliography.iter());
                    if !combined {
                        for atomic in atomics {
                            println!(
                                "{}",
                                formatter
                                    .format(vec![atomic].into_iter())
                                    .unwrap()
                                    .print_ansi_vt100()
                            );
                        }
                    } else {
                        println!(
                            "{}",
                            formatter.format(atomics).unwrap().print_ansi_vt100()
                        );
                    }
                }
                CitationStyle::AuthorTitle => todo!(),
                CitationStyle::Alphabetical => {
                    let formatter = Alphabetical::new(bibliography.iter());
                    for atomic in atomics.iter_mut() {
                        let item =
                            bibliography.iter().find(|i| i.key() == atomic.key).unwrap();
                        let creators = chicago::get_creators(item).0;
                        let mut same = bibliography.iter().filter(|e| {
                            item.any_date() == e.any_date()
                                && chicago::get_creators(e).0 == creators
                                && creators.len() >= 1
                        });
                        if same.clone().count() > 1 {
                            atomic.number.replace(
                                same.position(|e| e.key() == item.key()).unwrap(),
                            );
                        }
                    }
                    if !combined {
                        for atomic in atomics {
                            println!(
                                "{}",
                                formatter
                                    .format(vec![atomic].into_iter())
                                    .unwrap()
                                    .print_ansi_vt100()
                            );
                        }
                    } else {
                        println!(
                            "{}",
                            formatter.format(atomics).unwrap().print_ansi_vt100()
                        );
                    }
                }
                CitationStyle::ChicagoNote => {
                    let formatter =
                        chicago::notes::NoteCitationFormatter::new(bibliography.iter());
                    for atomic in atomics {
                        println!(
                            "{}",
                            formatter
                                .get_note(atomic, chicago::notes::NoteType::Full)
                                .unwrap()
                                .print_ansi_vt100()
                        );
                    }
                }
            }
        }
        _ => todo!(),
    }
}
