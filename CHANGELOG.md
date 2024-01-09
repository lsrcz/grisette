# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- Added wrappers for state transformers. ([#132](https://github.com/lsrcz/grisette/pull/132))
- Added `toGuardList` function. ([#137](https://github.com/lsrcz/grisette/pull/137))
- Exported some previously hidden API (`BVSignConversion`, `runFreshTFromIndex`) that we found useful or forgot to export. ([#138](https://github.com/lsrcz/grisette/pull/138), [#139](https://github.com/lsrcz/grisette/pull/139))
- Provided `mrgRunFreshT` to run `FreshT` with merging. ([#140](https://github.com/lsrcz/grisette/pull/140))
- Added `Grisette.Data.Class.SignConversion.SignConversion` for types from `Data.Int` and `Data.Word`. ([#142](https://github.com/lsrcz/grisette/pull/142))
- Added shift functions by symbolic shift amounts. ([#151](https://github.com/lsrcz/grisette/pull/151))
- Added `apply` for uninterpreted functions. ([#155](https://github.com/lsrcz/grisette/pull/155))
- Added `liftFresh` to lift a `Fresh` into `MonadFresh`. ([#156](https://github.com/lsrcz/grisette/pull/156))
- Added a handle types for SBV solvers. This allows users to use SBV solvers without the need to wrap everything in the SBV monads. ([#159](https://github.com/lsrcz/grisette/pull/159))
- Added a new generic CEGIS interface. This allows any verifier/fuzzer to be used in the CEGIS loop. ([#159](https://github.com/lsrcz/grisette/pull/159))

### Removed

- [Breaking] Removed the `Grisette.Lib.Mtl` module. ([#132](https://github.com/lsrcz/grisette/pull/132))
- [Breaking] Removed `SymBoolOp` and `SymIntegerOp`. ([#146](https://github.com/lsrcz/grisette/pull/146))
- [Breaking] Removed `ExtractSymbolics` instance for `SymbolSet`. ([#146](https://github.com/lsrcz/grisette/pull/146))

### Fixed

- Removed the quotation marks around the pretty printed results for string-like data types. ([#127](https://github.com/lsrcz/grisette/pull/127))
- Fixed the `SOrd` instance for `VerificationConditions`. ([#131](https://github.com/lsrcz/grisette/pull/131))
- Fixed the missing `SubstituteSym` instance for `UnionM`. ([#131](https://github.com/lsrcz/grisette/pull/131))
- Fixed the symbolic generation order for `Maybe`. ([#131](https://github.com/lsrcz/grisette/pull/131))
- Fixed the `toInteger` function for `IntN 1`. ([#143](https://github.com/lsrcz/grisette/pull/143))
- Fixed the `abs` function for `WordN`. ([#144](https://github.com/lsrcz/grisette/pull/143))
- Fixed the QuickCheck shrink function for `WordN 1` and `IntN 1`. ([#149](https://github.com/lsrcz/grisette/pull/149))
- Fixed the heap overflow bug for `shiftL` for `WordN` and `IntN` by large numbers. ([#150](https://github.com/lsrcz/grisette/pull/150))

### Changed

- Reorganized the files for `MonadTrans`. ([#132](https://github.com/lsrcz/grisette/pull/132))
- [Breaking] Changed the name of `Union` constructors and patterns. ([#133](https://github.com/lsrcz/grisette/pull/133))
- The `Union` patterns, when used as constructors, now merges the result. ([#133](https://github.com/lsrcz/grisette/pull/133))
- Changed the symbolic identifier type from `String` to `Data.Text.Text`. ([#141](https://github.com/lsrcz/grisette/pull/141))
- [Breaking] `Grisette.Data.Class.BitVector.BVSignConversion` is now `Grisette.Data.Class.SignConversion.SignConversion`. ([#142](https://github.com/lsrcz/grisette/pull/142))
- [Breaking] Moved the `ITEOp`, `LogicalOp`, and `SEq` type classes to dedicated modules. ([#146](https://github.com/lsrcz/grisette/pull/146))
- [Breaking] Moved `Grisette.Data.Class.Evaluate` to `Grisette.Data.Class.EvaluateSym`. ([#146](https://github.com/lsrcz/grisette/pull/146))
- [Breaking] Moved `Grisette.Data.Class.Substitute` to `Grisette.Data.Class.SubstituteSym`. ([#146](https://github.com/lsrcz/grisette/pull/146))
- [Breaking] Split the `Grisette.Data.Class.SafeArith` module to `Grisette.Data.Class.SafeDivision` and `Grisette.Data.Class.SafeLinearArith`. ([#146](https://github.com/lsrcz/grisette/pull/146))
- [Breaking] Changed the API to `MonadFresh`. ([#156](https://github.com/lsrcz/grisette/pull/156))
- [Breaking] Renamed multiple symbolic operators. ([#158](https://github.com/lsrcz/grisette/pull/158))
- [Breaking] Changed the solver interface. ([#159](https://github.com/lsrcz/grisette/pull/159))
- [Breaking] Changed the CEGIS solver interface. ([#159](https://github.com/lsrcz/grisette/pull/159))

## [0.3.1.1] -- 2023-09-29

No user-facing changes.

## [0.3.1.0] -- 2023-07-19

### Added

- Added support to `Data.Text`. ([#95](https://github.com/lsrcz/grisette/pull/95))
- Added `Arbitrary` instances for bit vectors. ([#97](https://github.com/lsrcz/grisette/pull/97))
- Added pretty printers for Grisette data types. ([#101](https://github.com/lsrcz/grisette/pull/101))
- Added `ExtractSymbolics` instances for tuples longer than 2. ([#103](https://github.com/lsrcz/grisette/pull/103))

### Fixed

- Fixed the `Read` instance for bit vectors. ([#99](https://github.com/lsrcz/grisette/pull/99), [#100](https://github.com/lsrcz/grisette/pull/100))

## [0.3.0.0] -- 2023-07-07

### Added

- Added the conversion between signed and unsigned bit vectors. ([#69](https://github.com/lsrcz/grisette/pull/69))
- Added the generation of `SomeSymIntN` and `SomeSymWordN` from a single `Int` for bit width. ([#73](https://github.com/lsrcz/grisette/pull/73))
- Added the `FiniteBits` instance for `SomeSymIntN` and `SomeSymWordN`. ([#83](https://github.com/lsrcz/grisette/pull/83))
- Added more flexible instances for symbolic generation for `Either`, `Maybe` and list types. ([#84](https://github.com/lsrcz/grisette/pull/84))
- Added an experimental `GenSymConstrained` type class. ([#89](https://github.com/lsrcz/grisette/pull/89))

### Changed

- Changed the operations for `SomeIntN` and `SomeWordN` to accepting dynamic runtime integers rather than compile-time integers. ([#71](https://github.com/lsrcz/grisette/pull/71))
- Comparing the equality of `SomeIntN`/`SomeWordN`/`SomeSymIntN`/`SomeSymWordN` with different bit widths returns false rather than crash now. ([#74](https://github.com/lsrcz/grisette/pull/74))

### Fixed

- Fixed the compatibility issue with sbv 10+. ([#66](https://github.com/lsrcz/grisette/pull/66))
- Fixed build error with newer GHC. ([#70](https://github.com/lsrcz/grisette/pull/70))
- Fixed the merging for `SomeSymIntN` and `SomeSymWordN`. ([#72](https://github.com/lsrcz/grisette/pull/72))

## [0.2.0.0] - 2023-04-13

### Added

- Add term size count API. ([#48](https://github.com/lsrcz/grisette/pull/48), [#53](https://github.com/lsrcz/grisette/pull/53))
- Add timeout to solver interface. ([#49](https://github.com/lsrcz/grisette/pull/49), [#50](https://github.com/lsrcz/grisette/pull/50))
- Add parallel do-notation for parallel symbolic compilation. ([#51](https://github.com/lsrcz/grisette/pull/51))
- Added some missing instances for symbolic values and bit vectors. ([#46](https://github.com/lsrcz/grisette/pull/46), [#61](https://github.com/lsrcz/grisette/pull/61))
- Add missing instances for `MonadFresh` and `FreshT`. ([#59](https://github.com/lsrcz/grisette/pull/59))

### Changed

- New safe operator interfaces. ([#56](https://github.com/lsrcz/grisette/pull/56))
- Redesigned symbolic value interface.
  - `Sym Bool`/`Sym Integer`, etc., are no longer available and are replaced with `SymBool` and `SymInteger`. ([#41](https://github.com/lsrcz/grisette/pull/41))
  - New symbolic bit vector interface. Added unsized bit vector. ([#41](https://github.com/lsrcz/grisette/pull/41))

### Removed

- Dropped merging cache for `UnionM`. This fixed some segmentation fault errors. ([#43](https://github.com/lsrcz/grisette/pull/43))

### Fixed

- Fix CEGIS when no symbolic input is present. ([#52](https://github.com/lsrcz/grisette/pull/52))
- Fix overlapping `ToSym` and `ToCon` instances. ([#54](https://github.com/lsrcz/grisette/pull/54))
- Fix uninterpreted function lowering. ([#57](https://github.com/lsrcz/grisette/pull/57), [#58](https://github.com/lsrcz/grisette/pull/58))
- Fix CEGIS crash when subsequent solver calls introduces new symbolic constant. ([#60](https://github.com/lsrcz/grisette/pull/60))

## [0.1.0.0] - 2023-01-20

### Added

- Initial release for Grisette.

[Unreleased]: https://github.com/lsrcz/grisette/compare/v0.3.1.0...HEAD
[0.3.1.1]: https://github.com/lsrcz/grisette/compare/v0.3.1.0...v0.3.1.1
[0.3.1.0]: https://github.com/lsrcz/grisette/compare/v0.3.0.0...v0.3.1.0
[0.3.0.0]: https://github.com/lsrcz/grisette/compare/v0.2.0.0...v0.3.0.0
[0.2.0.0]: https://github.com/lsrcz/grisette/compare/v0.1.0.0...v0.2.0.0
[0.1.0.0]: https://github.com/lsrcz/grisette/tree/v0.1.0.0
