# Hayagriva

[![Build status](https://github.com/typst/hayagriva/workflows/Continuous%20integration/badge.svg)](https://github.com/typst/hayagriva/actions)

Rusty bibliography management.

Hayagriva is a tool that can help your or your apps deal with literature and other media. Its features include:

- Data structures for literature databases
- Reading and writing said databases from YAML files
- Formatting literature into reference list entries and in-text citations as defined by popular style guides
- Interoperability with BibTeX
- Querying your literature items by type and available meta data

Hayagriva can be used both as a library and as a Command Line Interface (CLI). Skip to the [section "Usage"](#usage) for more information about usage in your application or to the [section "Installation"](#installation) to learn about how to install and use Hayagriva on your terminal.

## Usage
Add this to your `Cargo.toml`:
```toml
[dependencies]
hayagriva = "0.1"
```

Below, there is an example about how to parse a YAML database and get an Modern Language Association-style citation.

```rust
// Parse a bibliography
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

let bib = from_yaml_str(yaml).unwrap();
assert_eq!(bib[0].date().unwrap().year, 2014);

// Format the citation
use hayagriva::style::BibliographyFormatter;
use hayagriva::style::mla::Mla;

let foratter = Mla::new();
let citation = Mla::format(&bib[0], None);
assert_eq!(citation.value, "Kwan, Kevin. Crazy Rich Asians. Anchor Books, 2014.");
// Information about the italicisation is available in `citation.formatting`
```

If the default features are enabled, the [`Entry`](https://docs.rs/biblatex/latest/biblatex/struct.Entry.html)-struct of [biblatex](https://crates.io/crates/biblatex) will receive an implementation of the `Into<hayagriva::Entry>`-Trait. Therefore you can use your Biblatex content like this:

```rust
use hayagriva::Entry;
let converted: Entry = your_biblatex_entry.into();
```

If you do not need BibLaTeX compatibility, you can use Hayagriva without the default features by writing this in your `Cargo.toml`:

```toml
[dependencies]
hayagriva = { version = "0.1", default-features = false }
```

