use std::fs::read_to_string;
use std::io::ErrorKind as IoErrorKind;
use std::path::Path;
use std::process::exit;
use std::str::FromStr;

use clap::{crate_version, value_t, App, AppSettings, Arg, SubCommand};
use strum::{EnumVariantNames, VariantNames};

use hayagriva::style::{
    Apa, AuthorTitle, BibliographyStyle as UsableBibliographyStyle, ChicagoAuthorDate,
    ChicagoNotes, Citation, CitationStyle as UsableCitationStyle, Database, Ieee, Mla,
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
pub enum BibliographyStyle {
    Chicago,
    ChicagoAuthorDate,
    Mla,
    Apa,
    Ieee,
}

impl FromStr for BibliographyStyle {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, &'static str> {
        match s.to_ascii_lowercase().as_ref() {
            "chicago" | "chicago-bibliography" => Ok(BibliographyStyle::Chicago),
            "chicago-author-date" | "author-date" => {
                Ok(BibliographyStyle::ChicagoAuthorDate)
            }
            "mla" => Ok(BibliographyStyle::Mla),
            "apa" => Ok(BibliographyStyle::Apa),
            "ieee" => Ok(BibliographyStyle::Ieee),
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
            "chicago-note" | "note" | "chicago" => Ok(CitationStyle::ChicagoNote),
            "alphanumerical" | "alphanumeric" | "alphabetical" | "alphabetic"
            | "alpha" => Ok(CitationStyle::Alphanumerical),
            "author-title" => Ok(CitationStyle::AuthorTitle),
            "numerical" | "numeric" => Ok(CitationStyle::Numerical),
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
        .about("Format references and citations for your YAML-encoded or BibLaTeX bibliography files and query bibliographies using selectors.")
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the bibliography file to use")
                .required(true)
                .index(1)
        ).arg(
            Arg::with_name("format")
                .long("format")
                .help("What input file format to expect")
                .possible_values(&Format::VARIANTS)
                .case_insensitive(true)
                .takes_value(true)
                .global(true),
        ).arg(
            Arg::with_name("selector")
                .long("select")
                .help("Filter the bibliography using selectors")
                .takes_value(true)
                .global(true)
        )
        .arg(
            Arg::with_name("key")
                .long("key")
                .short("k")
                .help("Filter the bibliography using a comma-separated list of keys")
                .takes_value(true)
                .global(true)
        )
        .arg(
            Arg::with_name("show-keys")
                .long("show-keys")
                .help("Show the keys of all filtered entries")
                .global(true)
        )
        .arg(
            Arg::with_name("show-bound")
                .long("show-bound")
                .help("Show the bound entries of your selector for each key")
                .global(true)
        )
        .arg(
            Arg::with_name("no-fmt")
                .long("no-fmt")
                .short("n")
                .help("Suppress the formatting of output with ANSI / VT100 character sequences")
                .global(true)
        )
        .subcommand(
            SubCommand::with_name("cite")
                .about("Format citations for all filtered entries")
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
                    Arg::with_name("supplements")
                        .long("supplements")
                        .help("Specify additional information for the citations, e.g. \"p. 6,p. 4\", in a comma-seperated list.")
                        .takes_value(true)
                )
                .arg(
                    Arg::with_name("combined")
                        .long("combined")
                        .short("c")
                        .help("Combine all keys into one citation (ignored for Chicago Notes)")
                )
                .arg(
                    Arg::with_name("unpredictive")
                        .long("unpredictive")
                        .help("Do not prepare the database with all selected entries, but build it as we go")
                )
                .arg(
                    Arg::with_name("no-brackets")
                        .long("no-brackets")
                        .help("Print the citation without brackets.")
                )
                .arg(
                    Arg::with_name("force-brackets")
                        .long("force-brackets")
                        .help("Print the citation with brackets, no matter the style.")
                )
        )
        .subcommand(
            SubCommand::with_name("reference")
                .about("Format a bibliography of all filtered entries")
                .arg(
                    Arg::with_name("style")
                        .long("style")
                        .short("s")
                        .help("Set the referencing style")
                        .possible_values(&BibliographyStyle::VARIANTS)
                        .case_insensitive(true)
                        .takes_value(true)
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

    let selector = matches.value_of("selector").map(|src| match Selector::parse(src) {
        Ok(selector) => selector,
        Err(err) => {
            eprintln!("Error while parsing selector: {}", err);
            exit(7);
        }
    });

    let bibliography = if let Some(keys) = matches.value_of("key") {
        let mut res = vec![];
        for key in keys.split(',') {
            if let Some(entry) = bibliography.iter().find(|e| e.key() == key) {
                res.push(entry.clone());
            }
        }
        res
    } else if let Some(selector) = &selector {
        bibliography.into_iter().filter(|e| selector.matches(e)).collect()
    } else {
        bibliography
    };

    if matches.is_present("show-keys") || matches.is_present("show-bound") {
        println!(
            "Selected {} of {} entries in the bibliography\n",
            bibliography.len(),
            bib_len
        );

        for entry in &bibliography {
            println!("{}", entry.key());
            if matches.is_present("show-bound") {
                if let Some(selector) = &selector {
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
        exit(0);
    }

    match matches.subcommand() {
        ("reference", Some(sub_matches)) => {
            let style: Box<dyn UsableBibliographyStyle> =
                match value_t!(sub_matches, "style", BibliographyStyle)
                    .unwrap_or_else(|_| BibliographyStyle::ChicagoAuthorDate)
                {
                    BibliographyStyle::Chicago => Box::new(ChicagoNotes::default()),
                    BibliographyStyle::ChicagoAuthorDate => {
                        Box::new(ChicagoAuthorDate::default())
                    }
                    BibliographyStyle::Apa => Box::new(Apa::new()),
                    BibliographyStyle::Ieee => Box::new(Ieee::new()),
                    BibliographyStyle::Mla => Box::new(Mla::new()),
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
        ("cite", Some(sub_matches)) => {
            let style = value_t!(sub_matches, "style", CitationStyle)
                .unwrap_or_else(|_| CitationStyle::AuthorDate);

            let supplements = sub_matches
                .value_of("supplements")
                .into_iter()
                .flat_map(|sup| sup.split(','))
                .map(Some)
                .chain(std::iter::repeat(None));

            let citations = bibliography
                .iter()
                .zip(supplements)
                .map(|(e, s)| Citation::new(e, s))
                .collect::<Vec<_>>();

            let mut database = Database::new();

            if !sub_matches.is_present("unpredictive") {
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

            let lines = if sub_matches.is_present("combined") {
                vec![database.citation(style.as_mut(), &citations)]
            } else {
                citations
                    .into_iter()
                    .map(|c| database.citation(&mut *style, &[c]))
                    .collect()
            };

            for r in lines {
                let ds = if sub_matches.is_present("no-brackets") {
                    r.display
                } else if sub_matches.is_present("force-brackets") {
                    r.display.with_forced_brackets(&*style)
                } else {
                    r.display.with_default_brackets(&*style)
                };

                if matches.is_present("no-fmt") {
                    println!("{:#}", ds);
                } else {
                    println!("{}", ds)
                }
            }
        }
        _ => {
            let bib = io::to_yaml_str(&bibliography).unwrap();
            println!("{}", bib);
        }
    }
}
