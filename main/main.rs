use clap::{arg_enum, value_t, App, Arg};
use hayagriva::input::load_yaml_structure;
use hayagriva::output::chicago::bibliography::BibliographyFormatter as ChicagoBib;
use hayagriva::output::{apa, chicago, ieee, mla, BibliographyFormatter};
use hayagriva::selectors::parse;
use biblatex::Bibliography as TexBibliography;
use std::fs::read_to_string;

arg_enum! {
    #[derive(Debug, Copy, Clone, PartialEq)]
    pub enum Mode {
        Keys,
        Citation,
        Reference,
        Biblatex,
        Bibtex,
        Yaml,
    }
}

arg_enum! {
    #[derive(Debug, Copy, Clone, PartialEq)]
    pub enum Format {
        Bibtex,
        Biblatex,
        Yaml,
    }
}

arg_enum! {
    #[derive(Debug, Copy, Clone, PartialEq)]
    pub enum Style {
        Chicago,
        Mla,
        Apa,
        Ieee,
    }
}

/// Main function of the Hayagriva CLI.
fn main() {
    let matches = App::new("Hayagriva CLI")
        .version("0.1")
        .author("The Typst Project Developers <hi@typst.app>")
        .about("Output appropriate references and citations for your YAML-encoded bibliography database. Query the database for various source types with selectors.")
        .arg(Arg::with_name("INPUT").help("Sets the YAML bibliograpghy to use").required(true).index(1))
        .arg(Arg::with_name("format").long("format").short("f").help("What input file format to expect").possible_values(&Format::variants()).takes_value(true))
        .arg(Arg::with_name("selector").long("select").help("Filter your bibliography using selectors").takes_value(true))
        .arg(Arg::with_name("key").long("key").short("k").help("Get a specific key from the database").takes_value(true))
        .arg(Arg::with_name("mode").long("mode").short("m").help("Sets Hayagriva's mode").possible_values(&Mode::variants()).takes_value(true))
        .arg(Arg::with_name("style").long("style").short("s").help("Set the referencing/citation style").possible_values(&Style::variants()).takes_value(true))
        .get_matches();

    let format = value_t!(matches, "format", Format).unwrap_or_else(|_| Format::Yaml);

    let bibliography = match format {
        Format::Yaml => load_yaml_structure(
            &matches
                .value_of("INPUT")
                .and_then(|s| read_to_string(s).ok())
                .unwrap(),
        )
        .unwrap(),
        Format::Biblatex | Format::Bibtex => {
            let tex = TexBibliography::parse(
                &matches
                .value_of("INPUT")
                .and_then(|s| read_to_string(s).ok())
                .unwrap(),
            );
            tex.into_iter().map(|e| e.into()).collect()
        }
    };

    let mode = value_t!(matches, "mode", Mode).unwrap_or_else(|_| Mode::Keys);

    let bibliography = if let Some(key) = matches.value_of("key") {
        bibliography.into_iter().filter(|e| e.key() == key).collect()
    } else if let Some(selector) = matches.value_of("selector") {
        let selector = parse(selector).unwrap();
        bibliography.into_iter().filter(|e| selector.matches(e)).collect()
    } else {
        bibliography
    };

    match mode {
        Mode::Keys => {
            for entry in &bibliography {
                println!("{}", entry.key());
            }
        }
        Mode::Reference => {
            match value_t!(matches, "style", Style).unwrap_or_else(|_| Style::Chicago) {
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
