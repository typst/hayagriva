# Hayagriva

[![Build status](https://github.com/typst/hayagriva/workflows/Continuous%20integration/badge.svg)](https://github.com/typst/hayagriva/actions)
[![Current crates.io release](https://img.shields.io/crates/v/hayagriva)](https://crates.io/crates/hayagriva)
[![Documentation](https://img.shields.io/badge/docs.rs-hayagriva-66c2a5?labelColor=555555&logoColor=white&logo=data:image/svg+xml;base64,PHN2ZyByb2xlPSJpbWciIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgdmlld0JveD0iMCAwIDUxMiA1MTIiPjxwYXRoIGZpbGw9IiNmNWY1ZjUiIGQ9Ik00ODguNiAyNTAuMkwzOTIgMjE0VjEwNS41YzAtMTUtOS4zLTI4LjQtMjMuNC0zMy43bC0xMDAtMzcuNWMtOC4xLTMuMS0xNy4xLTMuMS0yNS4zIDBsLTEwMCAzNy41Yy0xNC4xIDUuMy0yMy40IDE4LjctMjMuNCAzMy43VjIxNGwtOTYuNiAzNi4yQzkuMyAyNTUuNSAwIDI2OC45IDAgMjgzLjlWMzk0YzAgMTMuNiA3LjcgMjYuMSAxOS45IDMyLjJsMTAwIDUwYzEwLjEgNS4xIDIyLjEgNS4xIDMyLjIgMGwxMDMuOS01MiAxMDMuOSA1MmMxMC4xIDUuMSAyMi4xIDUuMSAzMi4yIDBsMTAwLTUwYzEyLjItNi4xIDE5LjktMTguNiAxOS45LTMyLjJWMjgzLjljMC0xNS05LjMtMjguNC0yMy40LTMzLjd6TTM1OCAyMTQuOGwtODUgMzEuOXYtNjguMmw4NS0zN3Y3My4zek0xNTQgMTA0LjFsMTAyLTM4LjIgMTAyIDM4LjJ2LjZsLTEwMiA0MS40LTEwMi00MS40di0uNnptODQgMjkxLjFsLTg1IDQyLjV2LTc5LjFsODUtMzguOHY3NS40em0wLTExMmwtMTAyIDQxLjQtMTAyLTQxLjR2LS42bDEwMi0zOC4yIDEwMiAzOC4ydi42em0yNDAgMTEybC04NSA0Mi41di03OS4xbDg1LTM4Ljh2NzUuNHptMC0xMTJsLTEwMiA0MS40LTEwMi00MS40di0uNmwxMDItMzguMiAxMDIgMzguMnYuNnoiPjwvcGF0aD48L3N2Zz4K)](https://docs.rs/hayagriva/)

Rusty bibliography management.

Hayagriva is a tool that can help you or your apps deal with literature and
other media. Its features include:

- Data structures for literature collections
- Reading and writing said collections from YAML files
- Formatting literature into reference list entries and in-text citations as
  defined by popular style guides
- Interoperability with BibTeX
- Querying your literature items by type and available metadata

Hayagriva can be used both as a library and as a Command Line Interface (CLI).
Skip to the [section "Usage"](#usage) for more information about usage in your
application or to the [section "Installation"](#installation) to learn about how
to install and use Hayagriva on your terminal.

## Supported styles

Hayagriva supports all styles provided in the
[official Citation Style Language repository](https://github.com/citation-style-language/styles),
currently over 2,600.

# Usage

```rust
use hayagriva::io::from_yaml_str;

let yaml = r#"
crazy-rich:
    type: Book
    title: Crazy Rich Asians
    author: Kwan, Kevin
    date: 2014
    publisher: Anchor Books
    location: New York, NY, US
"#;

// Parse a bibliography
let bib = from_yaml_str(yaml).unwrap();
assert_eq!(bib.get("crazy-rich").unwrap().date().unwrap().year, 2014);

// Format the reference
use std::fs;
use hayagriva::{
    BibliographyDriver, BibliographyRequest, BufWriteFormat,
    CitationItem, CitationRequest,
};
use hayagriva::citationberg::{LocaleFile, IndependentStyle};

let en_locale = fs::read_to_string("tests/data/locales-en-US.xml").unwrap();
let locales = [LocaleFile::from_xml(&en_locale).unwrap().into()];

let style = fs::read_to_string("tests/data/art-history.csl").unwrap();
let style = IndependentStyle::from_xml(&style).unwrap();

let mut driver = BibliographyDriver::new();

for entry in bib.iter() {
    let items = vec![CitationItem::with_entry(entry)];
    driver.citation(CitationRequest::from_items(items, &style, &locales));
}

let result = driver.finish(BibliographyRequest {
    style: &style,
    locale: None,
    locale_files: &locales,
});

for cite in result.citations {
    println!("{}", cite.citation.to_string())
}
```

To format entries, you need to wrap them in a `CitationRequest`. Each of these
can reference multiple entries in their respective `CitationItem`s.
Use these with a `BibliographyDriver` to obtain formatted citations and bibliographies.

You can either supply your own CSL files or choose from about 100 bundled
citation styles using the `archive` feature.

If the default features are enabled, Hayagriva supports BibTeX and BibLaTeX
bibliographies. You can use `io::from_biblatex_str` to parse such
bibliographies.

Should you need more manual control, the library's native `Entry` struct also
offers an implementation of the `From<&biblatex::Entry>`-Trait. You will need to
depend on the [biblatex](https://docs.rs/biblatex/latest/biblatex/) crate to
obtain its `Entry`. Therefore, you could also use your BibLaTeX content like
this:

```rust
use hayagriva::Entry;
let converted: Entry = your_biblatex_entry.into();
```

If you do not need BibLaTeX compatibility, you can use Hayagriva without the
default features by writing this in your `Cargo.toml`:

```toml
[dependencies]
hayagriva = { version = "0.2", default-features = false }
```

### Selectors

Hayagriva uses a custom selector language that enables you to filter
bibliographies by type of media. For more information about selectors, refer to
the [selectors.md
file](https://github.com/typst/hayagriva/blob/main/docs/selectors.md). While you
can parse user-defined selectors using the function `Selector::parse`, you may
instead want to use the selector macro to avoid the run time cost of parsing a
selector when working with constant selectors.

```rust
use hayagriva::select;
use hayagriva::io::from_yaml_str;

let yaml = r#"
quantized-vortex:
    type: Article
    author: Gross, E. P.
    title: Structure of a Quantized Vortex in Boson Systems
    date: 1961-05
    page-range: 454-477
    doi: 10.1007/BF02731494
    parent:
        issue: 3
        volume: 20
        title: Il Nuovo Cimento
"#;

let entries = from_yaml_str(yaml).unwrap();
let journal = select!((Article["date"]) > ("journal":Periodical));
assert!(journal.matches(entries.nth(0).unwrap()));
```

There are two ways to check if a selector matches an entry.
You should use [`Selector::matches`] if you just want to know if an item
matches a selector and [`Selector::apply`] to continue to work with the data from
parents of a matching entry. Keep in mind that the latter function will
return `Some` even if no sub-entry was bound / if the hash map is empty.

## Installation

Run this in your terminal:
```bash
cargo install hayagriva --features cli
```

Cargo will install the Hayagriva Command Line Interface for you. Now, you just
need a Hayagriva YAML literature file or a Bib(La)TeX file to get started. The
Hayagriva YAML file is intuitive to write and can represent a wealth of media
types, [learn how to write one in its dedicated
documentation.](https://github.com/typst/hayagriva/blob/main/docs/file-format.md)

Suppose you have this file saved as `literature.yml` in your current working
directory:

```yaml
dependence:
    type: Article
    title: The program dependence graph and its use in optimization
    author: ["Ferrante, Jeanne", "Ottenstein, Karl J.", "Warren, Joe D."]
    date: 1987-07
    serial-number:
        doi: "10.1145/24039.24041"
    parent:
        type: Periodical
        title: ACM Transactions on Programming Languages and Systems
        volume: 9
        issue: 3

feminism:
    type: Article
    title: She swoons to conquer
    author: Ungard-Sargon, Batya
    editor: Weintraub, Pam
    date: 2015-09-25
    url: https://aeon.co/essays/can-you-enjoy-romance-fiction-and-be-a-feminist
    parent:
        type: Blog
        title: Aeon
```

You can then issue the following command to get reference list entries for both
of these articles.
```bash
hayagriva literature.yml reference
```

Hayagriva defaults to the Author-Date style of the Chicago Manual of Style (17th
edition). If you prefer to use another style, you can, for example, do the
following to use the style of the American Psychological Association instead:
```bash
hayagriva literature.yml reference --style apa
```

Available values for the `--style` argument can be viewed by calling
`hayagriva help reference`.

If you now need an in-text citation to the second article in the above file, you
can call:
```bash
hayagriva literature.yml cite --key feminism
```

The `--key` takes a comma-separated list of keys (or a single one). The
sub-command will then only work on the specified keys. Just like the `reference`
sub-command, the `cite` command also allows the `--style` argument. Its possible
values can be viewed with `hayagriva help cite`. It will default to the _Author
Date_ style.

Instead of the `--key` argument, you can also use `--select` to provide a custom
[Hayagriva selector.](https://github.com/typst/hayagriva/blob/main/docs/selectors.md)
For example, you could run the following to only reference entries that have a
URL or DOI at the top level:
```bash
hayagriva literature.yml --select "*[url] | *[doi]" reference
```

This expression would match both entries in our example and therefore the
command would return the same result as the first reference command.

Hayagriva also allows you to explore which values were bound to which
sub-entries if the selector matches. This is especially useful if you intend to
consume Hayagriva as a dependency in your application and need to debug an
expression. Consider this selector which always binds the sub-entry with the
volume field to `a`, regardless of if it occurred at the top level or in the
first parent: `a:*[volume] | * > a:[volume]`. You can then use the command below
to show which sub-entry the selector bound as `a` for each match:
```bash
hayagriva literature.yml --select "a:*[volume] | * > a:[volume]" --show-bound
```

The `--show-bound` flag shows all keys matching the selector or key filter and
details which sub-entries of each entry were bound by the selector. If, instead,
you only want to obtain a list of matching keys, use the `--keys` flag.

If you are working with BibTeX, you can use your `.bib` file with Hayagriva just
like you would use a `.yml` file. If you want to convert your `.bib` file to a
`.yml` file, you can simply pass the `.bib` file to the CLI without any additional
arguments. It will then show the YAML-formatted bibliography with key and selector
filters applied on standard output. If you therefore want to convert your `.bib`
file and save the result somewhere, you can just use `>`:
```bash
hayagriva literature.bib > converted.yml
```

## Contributing

We are looking forward to receiving your bugs and feature requests in the Issues
tab. We would also be very happy to accept PRs for bug fixes, minor
refactorings, features that were requested in the issues and greenlit by us, as
well as the planned features listed below:

- Implementing the YAML-to-BibLaTeX conversion
- Documentation improvements
- CSL bugfixes
- CSL-M Support

We wish to thank each and every prospective contributor for the effort you (plan
to) invest in this project and for adopting it!

## License

Hayagriva is licensed under a MIT / Apache 2.0 dual license.

Users and consumers of the library may choose which of those licenses they want
to apply whereas contributors have to accept that their code is in compliance
and distributed under the terms of both of these licenses.

Hayagriva includes CSL styles that are licensed as CC-BY-SA 3.0 Deed if the
`archive` feature is enabled. The file `styles.cbor.rkyv` is a collection of
these works and falls under this license. Retrieve attribution information by
deserializing it using the `styles` function and reading the `StyleInfo`
structs.
