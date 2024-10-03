# 0.7.0

- **Breaking change:** `Entry::page_range` now returns
  `Option<&MaybeTyped<PageRanges>>` instead of `Option<&PageRanges>`. This fixes
  a panic that occurred when a page range had an unknown format
- `MaybeTyped` now has an `as_typed` method

# 0.6.0

- **Breaking change:** Fix that the page range labels were not pluralized,
  `NumericValue::Range` now returns an inclusive range (#142)
- **Breaking change:** The field `publisher` can now accept a dictionary with a
  `location`. The top-level `location` key is now primarily for event and item
  locations.
- **Breaking change:** The field `annote` has been removed
- Allow multiple page ranges with prefixes and suffixes
- Fixes with sorting bibliography entries
- Fix sorting citations by their number (#115)
- Fix how citation number ranges collapse (#154)
- `BibliographyItem` is now exported (#157)
- Fix when the short form of a title was used (#173)
- Bumped the `biblatex` crate to 0.10.0 to fix a BibLaTeX parsing bug
  (https://github.com/typst/biblatex/issues/53) and allow the Unknown and
  Director editor types (https://github.com/typst/biblatex/issues/52).

We also updated our collection of Citation Styles.

# 0.5.3

- Fixed a bug with initials (#150)
- Fixed suppression of title when no author was provided (#144)
- Fixed et al handling on subsequent citations by [bumping citationberg](https://github.com/typst/citationberg/releases/tag/v0.3.1)

# 0.5.2

- Allow the `abstract`, `annote`, and `genre` fields to Hayagriva files and process them from BibTeX files. 
- Fix retrieval of an item's editor (#94)
- Fixed issue with pulling punctuation into quotation marks (#85)
- Allow non-range values in the `pages` field (#103)
- Fix multiple subsequent prose citations to the same item (#122)
- Interpret the `eprint` BibTeX key as `serial-number.arxiv` if the `eprinttype` is set to `arxiv`
- Fixed issue with multiple subsequent citations (#122)
- Improved handling of empty CSL objects

# 0.5.1

- Fixed spacing around math blocks
- Fixed title case formatting next to `{verbatim}` blocks and apostrophes

# 0.5.0

- **Breaking change:** The API for archived styles has changed.
- **Breaking change:** The name of the GB/T 7714 family of styles have been
  corrected to `gb-7714-...` from `gb-7114-...`.
- **Breaking change:** The reexported `TypeErrorKind` and `ParseErrorKind` enums
  in `biblatex` have added variants and become non-exhaustive.
- Date parsing will not panic anymore
  (https://github.com/typst/typst/issues/2553).
- Anthos entries now properly recognize their parent (#72,
  https://github.com/typst/typst/issues/2572). Thanks, @zepinglee!
- Proceedings titles will now be printed correctly (#78). Thanks, @vtta!
- Citation numbers will now collapse if the style requests it.
- Escaping in format and chunked strings now works
  (https://github.com/typst/typst/issues/2669).
- The old behavior of the alphanumeric style has been restored.
- Bibliographies now forcibly print the alphanumeric `citation-label` instead of
  the `citation-number` if the cite only printed the former (and vice-versa;
  https://github.com/typst/typst/issues/2707).
- We dropped the dependency on `rkyv` in favor of code generation in a test.
  This should resolve build problems on some platforms.
- The retrieval of the volume variable is now more robust (#82). Thanks,
  @mpmdean!
- Fixed delimiter order for contributors (#73). Thanks, @zepinglee!
- Page ranges can now be strings (#83).
- Page ranges will now use the correct delimiter, even if printed with `cs:text`
- Fixed a bug with the suppression of empty groups
  (https://github.com/typst/typst/issues/2548).
- Bumped `citationberg` to solve a CSL locale fallback issue that affected
  https://github.com/typst/typst/issues/2548
- Bumped the `biblatex` crate to 0.9.0 to fix BibLaTeX parsing bugs (e.g.
  https://github.com/typst/biblatex/issues/41,
  https://github.com/typst/biblatex/issues/33,
  https://github.com/typst/biblatex/issues/40,
  https://github.com/typst/typst/issues/2751, #81)

# 0.4.0

## Breaking changes:

Hayagriva now uses the [Citation Style Language](https://citationstyles.org) to
encode formatting styles. This means that Hayagriva's own formatting styles have
been deprecated.

### For users:
- The YAML input format has changed.
    - Titles and formattable strings have been merged into one type. All
      formattable strings can have a shorthand now.
    - Formattable Strings do not have `title-case` and `sentence-case` keys
      anymore. `shorthand` has been renamed to `short`. To prevent changes of
      the text case of formattable strings, you can use braces. Enclose a part
      of a formattable string (or `short`) in `{braces}` to print it as-is.
    - The fields `doi`, `isbn`, and `issn` have been moved to `serial-number`
      which can now be a dictionary containing these and arbitrary other serial
      numbers like a `pmid` (PubMed ID) and `arxiv` (ArXiv Identifier).
    - The `tweet` entry type has been renamed to `post`.
    - All numeric variables can now also contains strings. Numbers can have
      string affixes.

Refer to the updated
[file format](https://github.com/typst/hayagriva/blob/main/docs/file-format.md)
docs for examples.

### For developers:
- To use a CSL style, you can either supply a CSL file or use an archive of
  provided styles with the `archive` feature.
- The `from_yaml_str` function will now return the new `Library` struct, with the
  entries within.
- The `Database` struct has been replaced by the easier to handle
  `BibliographyDriver`.
- We switched from `yaml_rust` to `serde_yaml`. The `Entry` now implements
  `serde`'s `Serialize` and `Deserialize` traits. Hence, the `from_yaml` and
  `to_yaml` functions have been deleted.
- Brackets are no longer individually overridable. Instead, use the new
  `CitePurpose`.
- `Entry::kind` has been renamed to `Entry::entry_type`.
    - The citation styles `AuthorTitle` and `Keys` have been removed but can be
      realized with CSL.

This release fixes many bugs and makes Hayagriva a serious contender for
reference management.

## Other changes

- We added the entry types `Performance` and `Original`.
- We added the field `call-number`.


# 0.3.2

Fixes a title case formatting bug introduced in the previous release.

# 0.3.1

_Bug Fixes:_
- Added an option to turn off abbreviation of journals (thanks to @CMDJojo)
- Fixed bugs with title case formatting (thanks to @jmskov)
- Fixed off-by-one error with dates in APA style (thanks to @bluebear94)
- Fixed supplements in the Alphanumeric and AuthorTitle styles (thanks to @lynn)
- Fixed bugs with sentence case formatting
- Fixed `verbatim` option
- Fixed terminal formatting
- Fixed some typos (thanks to @kianmeng and @bluebear94)

# 0.3.0

*Breaking:*
- Updated to `biblatex` 0.8.0

*Bug Fixes:*
- Fixed string indexing for titles, removed panic
- More permissive BibLaTeX parsing

# 0.2.1

*Bug Fixes:*
- Fixed APA bibliography ordering

# 0.2.0

*Breaking:*
- Replaced `NoHyphenation` formatting with `Link` formatting
- Switched to newest BibLaTeX (which is part of the public API)

*Bug Fixes:*
- Fixed IEEE bibliography ordering
- Fixed A, B, C, ... suffixes for Author Date citations
- Removed `println` calls

# 0.1.1

🐞 This release fixes the documentation of the CLI in the `README.md` file.
✨ There are new options for bracketed citations in the CLI.
✅ No breaking changes.

# 0.1.0

🎉 This is the initial release!
