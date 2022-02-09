# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added

- Basic implementation of lua macros ([#3](https://todo.sr.ht/~mikelma/oktac/3)).
- Add `@len` built-in function ([#6](https://todo.sr.ht/~mikelma/oktac/6)).

### Changed

- Default type for floating point numbers changed from `f32` to `f64`.

### Fixed

- Cannot match identifiers starting with keyword bug ([#2](https://todo.sr.ht/~mikelma/oktac/2)).
- Wrong compiler error message in builtin functions calls with few arguments.
- Constant value importing bugs.
- Missing codegen implementation for comparison operations between booleans.

## [0.1.0] - 2022-01-17

This is the first release of *oktac*. All changes after this release will 
be documented here.
