# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),

Versions prior to v0.1.0 are considered experimental, their API may change.

## [Unreleased]

### Fixed
- :fn with-condition in the `describe` function.
  Updated the minimap model to reflect that with-condition is valid for the :fn nodes.
- Throw an error when a reference cannot be resolved in the model, instead of just let
  minimallist crash somewhere else with an unexpected `nil` model.

## [0.0.6] - 2020-08-09
## [0.0.5] - 2020-08-09

### Fixed
- A Cljdoc problem.

## [0.0.4] - 2020-08-09

### Added
- this changelog file.
- the models `gen/fn-simple-symbol`, `gen/fn-qualified-symbol`,
  `gen/fn-simple-keyword` and `gen/fn-qualified-keyword`.
- the `describe` function, for parsing data.

### Changed
- the models `gen/fn-symbol`, `gen/fn-keyword` now generate qualified symbols and keywords,

## [0.0.3] - 2020-07-26

### Added
- the setup for a nice cljdoc documentation.

## [0.0.2] - 2020-07-26

### Added
- the generator function in a separate namespace.
- a documentation in `doc/`.

### Changed
- some functions in the helpers.

## [0.0.1] - 2020-05-13

### Added
- the `valid?` function, to validate data against a model.
- the helper functions, to build the models.
