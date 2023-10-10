# Selectors

As you have seen in the Readme and the [file format documentation](https://github.com/typst/hayagriva/blob/main/docs/file-format.md), a Hayagriva entry not only has a type but also parents that can have their own type. With this architecture, differentiation between articles from blogs, newspapers, and conference proceedings is easy without having a dedicated type for each of them!

However, this also means that you cannot just match on the top-level type of an entry to make all the distinctions that you or your citation and reference styles may require.

Enter selectors: They provide a convenient way to query entries by structure and retrieve parents that hold crucial information for your use case.

Two ways of macro usage are offered: Either as a string (used on the command line) or, if you are depending on Hayagriva as a library, with the `select!` macro. Library users can parse string selectors using `Selector::parse`. The Readme explains the fundamental differences between the two formats. If there are divergences between both forms, we will provide both variants as examples.

## Entry type selector

The most basic selectors are entry types; they will match any entry that has the same top-level type:

| Variant     | Example                                              |
|-------------|------------------------------------------------------|
| **String:** | `thesis`                                             |
| **Macro:**  | `Thesis`                                             |

This works with any of the [entry types](https://github.com/typst/hayagriva/blob/main/docs/file-format.md#entry-type). Be aware that you have to capitalize exactly like in the EntryType struct variants when using the macro.
The string selectors are case-insensitive.

## Wildcard

There is also a wildcard selector that applies to every entry:

```
*
```

This might seem pointless on its own but is quite useful in conjunction with other selector constructs.

## Required fields

Sometimes you want to filter for entries that have certain fields set. This can be accomplished with the fields selector. Attach square brackets to a selector and put the fields you want to be set inside, separated by commas. All specified fields have to contain some value for the selector to match.

| Variant     | Example                                              |
|-------------|------------------------------------------------------|
| **String:** | `artwork[archive, archive-location]`                 |
| **Macro:**  | `Artwork["archive", "archive-location"]`             |

This example finds all artworks with a known archive (including its location). The macro needs the attributes to be strings.

## Negation

The exclamation mark allows you to select everything that does not match the following selector.

| Variant     | Example                                              |
|-------------|------------------------------------------------------|
| **String:** | `!article`                                           |
| **Macro:**  | `!Article`                                           |

## Disjunction

The disjunction operator `|` allows you to offer multiple alternative selectors, only one of which has to be matched for the whole construction to match. Think about it as an 'or.'

| Variant     | Example 1                                            |
|-------------|------------------------------------------------------|
| **String:** | `anthology \| *[volume]`                             |
| **Macro:**  | `Anthology \| (*["volume"])`                         |

<!-- The pipe character (|) is escaped with a backslash because it would otherwise break GitHub's Markdown tables. When writing real selectors, of course, omit the backslash. -->

Either an anthology or anything with a volume field.

| Variant     | Example 2                                            |
|-------------|------------------------------------------------------|
| **String:** | `(video \| audio \| web[runtime])[affiliated]`       |
| **Macro:**  | `(Video \| Audio \| (Web["runtime"]))["affiliated"]` |

Matches every video, audio, and web (with runtime field) entry, given that it has the affiliated field set.

## Ancestrage

Require that a parent has to match some selector with the `>` operator. The conditions to its left have to apply for the top-level, the selector on the right side must match any of the parents. The operator can also be chained to examine nested parents.

| Variant     | Example 1                                            |
|-------------|------------------------------------------------------|
| **String:** | `article > proceedings`                              |
| **Macro:**  | `Article > Proceedings`                              |

This selector finds published conference articles.

| Variant     | Example 2                                            |
|-------------|------------------------------------------------------|
| **String:** | `chapter > (book \| anthology) > (book \| anthology)`|
| **Macro:**  | `Chapter > (Book \| Anthology) > (Book \| Anthology)`|

This selects a chapter in a monograph (a long-form text on a subject published in another book).

## Bindings

_Less interesting for CLI users._

Once you know that a matching entry has some parents, you might need to access their fields. You can be sure you got the right parent with bindings.
Create a binding using the `:` operator. To the left of the selector, you provide a name which you can freely choose. The parent (or the top-level entry) that matches the selector to the right will then appear in the resulting map under the key that you have chosen.

| Variant     | Example                                              |
|-------------|------------------------------------------------------|
| **String:** | `article > parent:(blog \| newspaper)`               |
| **Macro:**  | `Article > ("parent":(Blog \| Newspaper))`           |

The binding name has to be a string for macro use.
This binds the blog or newspaper parent of an article to 'parent' if the selector matches.
It is possible to write selectors that only sometimes bind a variable if it matches.
You could reformulate the right part of the above selector as `(parent:blog | newspaper)`, then it would not bind 'parent' if the selector matches with a newspaper-type parent.

Note that all bindings within a negation are discarded.

## Require multiple parents

Sometimes, a single parent does not provide the full picture. For example, a dataset (type repository) could be both published on the web and presented in a paper, so it would have a `web` and an `article` parent (with the latter possibly having an `periodical` or `proceedings` parent). To capture such entries with selectors, we need to define multiple conditions for parents, that all must be satisfied. This can be done using the `&`-operator. The operator may only be used to the right of an [ancestrage operator](#ancestrage).

| Variant     | Example                                              |
|-------------|------------------------------------------------------|
| **String:** | `article > (conference & video)`                     |
| **Macro:**  | `Article > (Conference & Video)`                     |

This selector matches conference talks published as a video.
