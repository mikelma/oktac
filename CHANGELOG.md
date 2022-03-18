# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added

- Basic implementation of lua macros ([#3](https://todo.sr.ht/~mikelma/oktac/3)).
- Add `@len` built-in function ([#6](https://todo.sr.ht/~mikelma/oktac/6)).
- Verbose and quiet cli options added.
- Add support for extra flags to pass to clang ([#7](https://todo.sr.ht/~mikelma/oktac/7)). 
- New escape sequence for strings: unicode character code.
- Support for path resolving and absolute paths when importing modules with `use`.
- Language reference (`docs/`).
- Compilation options from code ([#4](https://todo.sr.ht/~mikelma/oktac/4)).
- `@inttoptr` and `@ptrtoint` built-in functions.

### Changed

- Default type for floating point numbers changed from `f32` to `f64`.
- CLI flag for C files to include changed from `-c` to `-I`.

### Fixed

- Cannot match identifiers starting with keyword bug ([#2](https://todo.sr.ht/~mikelma/oktac/2)).
- Wrong compiler error message in builtin functions calls with few arguments.
- Constant value importing bugs.
- Missing codegen implementation for comparison operations between booleans.
- Avoid importing same prototype definitions multiple times in a single compilation unit.
- Import multiple modules under the same prefix fixed
- Compilation error when applying binary operator to non literal types.
- `valueOrType` rule parsing struct and enums as identifiers fixed.
- Allow member access on references to structs ([#3](https://todo.sr.ht/~mikelma/oktac/13))

## [0.1.0] - 2022-01-17

This is the first release of *oktac*. All changes after this release will 
be documented here.
