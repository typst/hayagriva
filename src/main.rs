use std::fs::{read_to_string, OpenOptions};
use std::io::{ErrorKind as IoErrorKind, Write};
use std::path::Path;
use std::process::exit;
use std::str::FromStr;

use clap::{crate_version, value_t, App, AppSettings, Arg, SubCommand};
use strum::{EnumVariantNames, VariantNames};

use hayagriva::style::{
    Apa, AuthorTitle, BibliographyStyle, ChicagoAuthorDate, ChicagoNotes, Citation,
    CitationStyle as UsableCitationStyle, Database, Ieee, Mla,
};
use hayagriva::Selector;
use hayagriva::{
    io,
    style::{Alphanumerical, Numerical},
};

#[derive(Debug, Copy, Clone, PartialEq, EnumVariantNames)]
#[strum(serialize_all = "kebab_case")]
pub enum Format {
    #[cfg(feature = "biblatex")]
    Bibtex,
    #[cfg(feature = "biblatex")]
    Biblatex,
    Yaml,
}

impl FromStr for Format {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, &'static str> {
        match s.to_ascii_lowercase().as_ref() {
            #[cfg(feature = "biblatex")]
            "bibtex" => Ok(Format::Bibtex),
            #[cfg(feature = "biblatex")]
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
    Alphanumerical,
    AuthorTitle,
    Numerical,
}

impl FromStr for CitationStyle {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, &'static str> {
        match s.to_ascii_lowercase().as_ref() {
            "author-date" | "author-year" => Ok(CitationStyle::AuthorDate),
            "note" | "chicago-note" | "chicago" => Ok(CitationStyle::ChicagoNote),
            "alphabetic" | "alpha" | "alphabetical" => Ok(CitationStyle::Alphanumerical),
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
        ).arg(
            Arg::with_name("format")
                .long("format")
                .short("f")
                .help("What input file format to expect")
                .possible_values(&Format::VARIANTS)
                .case_insensitive(true)
                .takes_value(true),
        ).arg(
            Arg::with_name("selector")
                .long("select")
                .help("Filter your bibliography using selectors")
                .takes_value(true)
        )
        .arg(
            Arg::with_name("key")
                .long("key")
                .short("k")
                .help("Filter the database using a comma-separated list of keys")
                .takes_value(true)
        )
        .arg(
            Arg::with_name("no-fmt")
                .long("no-fmt")
                .help("Suppress the formatting of the output with ANSI / VT100 character sequences")
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
                .arg(
                    Arg::with_name("supplement")
                        .long("supplement")
                        .help("Specify some information for the citation, e.g. \"p. 6\", in a comma-seperated list.")
                        .takes_value(true)
                )
                .arg(
                    Arg::with_name("combined")
                        .long("combined")
                        .short("c")
                        .help("Combine all keys into one citation (ignored for Chicago Notes)")
                )
                .arg(
                    Arg::with_name("no-prepopulate")
                        .long("no-prepopulate")
                        .help("Whether the database should be prepared with all selected entries or be built as we go")
                )
        )
        .subcommand(
            SubCommand::with_name("dump")
                .about("Get a bibliography file with your selected entries. This can also be used to convert a BibTeX file to the YAML database format.")
                .arg(
                    Arg::with_name("output")
                        .long("output")
                        .short("o")
                        .help("Save the file at a path instead of writing to the terminal")
                        .takes_value(true)
                )
                .arg(
                    Arg::with_name("force")
                        .long("force")
                        .help("Override the output file if it already exists")
                )
        )
        .get_matches();

    let input = Path::new(matches.value_of("INPUT").unwrap());

    let format = value_t!(matches, "format", Format).unwrap_or_else(|_| {
        #[allow(unused_mut)]
        let mut format = Format::Yaml;

        #[cfg(feature = "biblatex")]
        if input
            .extension()
            .and_then(|ext| ext.to_str())
            .map_or(false, |ext| ext.to_lowercase() == "bib")
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
                        "Error when reading the bibliography file \"{}\": {}",
                        input.display(),
                        os
                    );
                    exit(6);
                } else {
                    eprintln!(
                        "Error when reading the bibliography file \"{}\".",
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

    let bibliography = if let Some(keys) = matches.value_of("key") {
        let mut res = vec![];
        for key in keys.split(',') {
            if let Some(entry) = bibliography.iter().find(|e| e.key() == key) {
                res.push(entry.clone());
            }
        }
        res
    } else if let Some(selector) = matches.value_of("selector") {
        let selector = Selector::parse(selector).expect("Invalid selector");
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
                        let selector =
                            Selector::parse(selector).expect("Invalid selector");

                        let mut style = ChicagoNotes::default();
                        let mut database = Database::new();

                        for (k, v) in selector.apply(entry).unwrap() {
                            println!(
                                "\t{} => [{}] {}",
                                k,
                                v.kind(),
                                database
                                    .citation(&mut style, &[Citation::new(entry, None)])
                                    .display
                            );
                        }
                    }
                }
            }
        }
        ("reference", Some(sub_matches)) => {
            let style: Box<dyn BibliographyStyle> =
                match value_t!(sub_matches, "style", Style)
                    .unwrap_or_else(|_| Style::ChicagoAuthorDate)
                {
                    Style::Chicago => Box::new(ChicagoNotes::default()),
                    Style::ChicagoAuthorDate => Box::new(ChicagoAuthorDate::default()),
                    Style::Apa => Box::new(Apa::new()),
                    Style::Ieee => Box::new(Ieee::new()),
                    Style::Mla => Box::new(Mla::new()),
                };

            let mut database = Database::new();
            for entry in &bibliography {
                database.push(entry);
            }

            let rows = database.bibliography(&*style, None);
            for row in rows {
                if matches.is_present("no-fmt") {
                    println!("{:#}", row.display);
                } else {
                    println!("{}", row.display)
                }
            }
        }
        ("citation", Some(sub_matches)) => {
            let style = value_t!(sub_matches, "style", CitationStyle)
                .unwrap_or_else(|_| CitationStyle::AuthorDate);
            let supplements = sub_matches
                .value_of("supplement")
                .map(|sup| sup.split(',').collect::<Vec<_>>())
                .unwrap_or_default();
            let citations = bibliography
                .iter()
                .enumerate()
                .map(|(i, e)| {
                    let supp = supplements.get(i).cloned();
                    Citation::new(e, supp)
                })
                .collect::<Vec<_>>();
            let combined = sub_matches.is_present("combined");

            let mut database = Database::new();

            if !sub_matches.is_present("no-prepopulate") {
                for entry in &bibliography {
                    database.push(entry);
                }
            }

            let mut style: Box<dyn UsableCitationStyle> = match style {
                CitationStyle::Alphanumerical => Box::new(Alphanumerical::new()),
                CitationStyle::AuthorDate => Box::new(ChicagoAuthorDate::default()),
                CitationStyle::AuthorTitle => Box::new(AuthorTitle::new()),
                CitationStyle::ChicagoNote => Box::new(ChicagoNotes::default()),
                CitationStyle::Numerical => Box::new(Numerical::new()),
            };

            let lines = if combined {
                vec![database.citation(style.as_mut(), &citations)]
            } else {
                citations
                    .into_iter()
                    .map(|c| database.citation(&mut *style, &[c]))
                    .collect()
            };

            for r in &lines {
                if matches.is_present("no-fmt") {
                    println!("{:#}", r.display);
                } else {
                    println!("{}", r.display)
                }
            }
        }
        ("dump", Some(sub_matches)) => {
            let bib = io::to_yaml_str(&bibliography).unwrap();
            if let Some(path) = sub_matches.value_of("output") {
                let options = if sub_matches.is_present("force") {
                    OpenOptions::new().write(true).create(true).truncate(true).open(path)
                } else {
                    OpenOptions::new().write(true).create_new(true).open(path)
                };

                if let Err(e) = options {
                    if e.kind() == IoErrorKind::AlreadyExists {
                        eprintln!(
                            "The file already exists. If you want to override its contents, add the `--force` flag."
                        );
                        exit(2);
                    } else if e.kind() == IoErrorKind::PermissionDenied {
                        eprintln!(
                            "The permission to create the file \"{}\" was denied.",
                            path
                        );
                        exit(3);
                    } else if let Some(os) = e.raw_os_error() {
                        eprintln!("Error when creating the file \"{}\": {}", path, os);
                        exit(4);
                    } else {
                        eprintln!("Error when creating the file \"{}\".", path);
                        exit(4);
                    }
                } else if let Ok(mut file) = options {
                    file.write_all(bib.as_bytes()).unwrap();
                }
            } else {
                println!("{}", bib);
            }
        }
        _ => {
            eprintln!(
                "Unknown subcommand. Try the switch `--help` to see a listing of all possible sub-commands."
            );
            exit(1);
        }
    }
}
