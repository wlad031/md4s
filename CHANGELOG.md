# Changelog
All notable changes to this project will be documented in this file.

## [Unreleased]

### Added

- Obsidian flavor support with parser, formatter, models, and integration tests.
- Java/Clojure interop API:
  - `dev.vgerasimov.md4s.interop.ParsersInterop`
  - `dev.vgerasimov.md4s.interop.Parsers` (Java static facade)
  - `ParseResult` and `ParseException`
- Java/Clojure-friendly `parse*AsData` conversion to plain `Map`/`List` tree.

### Changed

- Extended Obsidian parse/format round-trip coverage with large document including frontmatter, nested headings/lists, callouts, tables, footnotes, comments, links, tags, and code blocks.
- Fixed Logseq updater integration test to use `Updater.Change.Replacement`.

## [0.1.0] - 2021-02-24

### Added

- first
- second

### Changed

- first
- second

### Removed

- first
- second

[0.1.0]: https://github.com/wlad031/md4s/releases/tag/v0.1.0
[Unreleased]: https://github.com/wlad031/md4s/compare/v0.1.0...HEAD
