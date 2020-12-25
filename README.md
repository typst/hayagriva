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

Formatting for in-text citations is available through implementors of the `hayagriva::style::CitationFormatter` trait.

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

Hayagriva uses a custom selector language that enables you to filter bibliographies by type of media. For more information about selectors, refer to the [selectors.md file](https://github.com/typst/hayagriva/blob/main/selectors.md). While you can parse user-defined selectors using the function `hayagriva::selectors::parse`, you may instead want to use the selector functions and macros to avoid the run time cost of parsing a selector when working with constant selectors.

The functions `Id` (identifier of the entry type) and `Wc` (wildcard) allow you to create atomic selectors while the functions `Neg` (negate an expression) and `Bind` (obtain a reference to a matching parent) as well as the `attrs!` macro (specify attributes like `volume` that have to be set on matching entries) allow to modify selectors. The macro `sel` allows to compose selectors:

- `sel!(Id("article") => Id("proceedings"))` finds an article that is parented by a conference proceedings volume
- `sel!(alt Id("video"), Id("audio"), Id("artwork"))` will match either a video or audio item or an artwork.
- `sel!(Wc() => sel!(mul Id("blog"), Id("newspapaer")))` will match anything that is parented by both a blog and a newspaper.

There are two ways to check if a selector matches an entry:

```rust
let journal = sel!(Id("article") => Bind("journal", Id("periodical")));
journal.matches(&your_entry); // Returns a `bool`
journal.apply(&your_entry); // Returns an `Option<HashMap<String, &Entry>>` that is `Some(...)` if the selector matched
```

Therefore you should use `Expr::matches` if you just want to know if an item matches a selector and `Expr::apply` to continue to work with the data from parents of a matching entry. Keep in mind that the latter function will return `Some` even if no sub-entry was bound / if the hash map is empty.

## Installation

Run this in your terminal:
```bash
cargo install hayagriva --features cli
```

Cargo will install the Hayagriva Command Line Interface for you. Now, you just need a Hayagriva YAML literature file or a Bib(La)TeX file to get started. The Hayagriva YAML file is intuitive to write and can represent a wealth of media types, [learn how to write one in its dedicated documentation.](https://github.com/typst/hayagriva/blob/main/file-format.md)

Suppose you have this file saved as `literature.yml` in your current working directory:

```yaml
dependance:
    type: Article
    title: The program dependence graph and its use in optimization
    author: ["Ferrante, Jeanne", "Ottenstein, Karl J.", "Warren, Joe D."]
    date: 1987-07
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

You can then issue the following command to get reference list entries for both of these articles.
```bash
hayagriva literature.yml reference
```

Hayagriva defaults to the Author Date style of the Chicago Manual of Style (17th edition). If you prefer to use another style, you can, for example, do the following to use the style of ther American Psychological Association instead:
```bash
hayagriva literature.yml reference -s apa
```

Available values for the `-s` flag can be viewed by calling `hayagriva help reference`.

If you now need an in-text citation to the second article in the above file, you can call:
```bash
hayagriva literature.yml -k feminism citation
```

The `-k` flag preceeds the sub-command (e.g. `citation`, `reference`) and takes a comma-separated list of keys (or a single one). The sub-command will then only work on the specified keys. The `citation` sub-command also allows for a `-s` flag. Its possible values can be viewed with `hayagriva help citation`, it will default to an author-date style.

Instead of the `-k` flag, you can also use the `--select` flag to provide a custom [Hayagriva selector.](https://github.com/typst/hayagriva/blob/main/selectors.md) For example, you could run the following to only reference entries that have a URL or DOI at the top level:
```bash
hayagriva literature.yml --select "*[url] | *[doi]" reference
```

This expression would match both entries in our example and therefore the command would return the same result as the first reference command.

Hayagriva also allows you to explore which values where bound to which sub-entries if the selector matches. This is especially useful if you intend to consume Hayagriva as a dependancy in your application and need to debug an expression. Suppose this selector that always binds the sub-entry with the volume field to `a`, regardless of if it occurred at the top level or in the first parent: `a:*[volume] | * > a:[volume]`. You can then use the command below to show which sub-entry was bound as `a` for each match:
```bash
hayagriva literature.yml --select "a:*[volume] | * > a:[volume]" keys --show-bound
```

The `keys` sub-command enumerates all keys that match the selector / key conditions that you specified or all keys in the bibliography if there are none. The `--show-bound` flag (or `-b` for short) then shows the selector bindings for each match.

If you are working with BibTeX, you can use your `.bib` file with Hayagriva just like you would use a `.yml` file. If you want to convert your `.bib` file to a `.yml` file, you can run this, which will save the conversion result in the current working directory as `converted.yml`:

```bash
hayagriva literature.bib dump -o converted.yml
```

The converted file will instead be printed to the terminal if you omit the `-o` flag.

## Supported styles
- Institute of Electrical and Electronics Engineers (IEEE)
    - References
    - Numerical citations (not available in CLI)
- Modern Language Association (MLA) "Works Cited" references from the 8th edition of the MLA Handbook
- Chicago Manual of Style (CMoS), 17th edition
    - Notes and Bibliography
    - Author Date references and citations
- American Psychological Association (APA) style from the 7th edition of the APA Publication Manual
- Other in-text citation styles:
    - Alphabetical (e.g. "Rass97")
    - Author Title

## Contributing
We are very happy to receive your bugs and feature requests in the Issues tab. We would be very happy to accept PRs for bug fixes, minor refactorings, features that were requested in the issues and greenlit by us, as well as the planned features listed below:

- More citation and reference styles (especially styles used in the 'hard' sciences would be incredibly appreciated)
- Implementing the YAML-to-BibLaTeX conversion
- Improvements to the sentence and title formatter
- Work for non-english bibliographies
- Documentation improvements

We wish to thank each and every prospective contributor for the effort you (plan to) invest in this project and for adopting it!

## License

As of December 2020, Hayagriva is unlicensed and unpublished.
