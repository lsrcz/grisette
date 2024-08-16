# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic
Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Added missing instances for concrete general and tabular functions.
  ([#249](https://github.com/lsrcz/grisette/pull/249))
- Added eval mode constraint on demand.
  ([#250](https://github.com/lsrcz/grisette/pull/250))
- Added support for uninterpreted functions in unified interfaces.
  ([#250](https://github.com/lsrcz/grisette/pull/250))

### Changed
- [Breaking] Moved the constraints for the general and tabular functions and
  simplified their instances declaration.
  ([#249](https://github.com/lsrcz/grisette/pull/249))
- [Breaking] Renamed `EvalMode` to `EvalModeAll`, renamed `MonadWithMode` to 
  `MonadEvalModeAll`.
  ([#250](https://github.com/lsrcz/grisette/pull/250))

## [0.8.0.0] - 2024-08-13

### Added
- Added pretty printer for models.
  ([#225](https://github.com/lsrcz/grisette/pull/225))
- Added support for algebraic reals (`AlgReal` and `SymAlgReal`).
  ([#228](https://github.com/lsrcz/grisette/pull/228),
   [#229](https://github.com/lsrcz/grisette/pull/229))
- Added support for quantifiers.
  ([#230](https://github.com/lsrcz/grisette/pull/230))
- Added `SafeFdiv`, `SafeLogBase`, `DivOr`, `FdivOr`, and `LogBaseOr`.
  ([#228](https://github.com/lsrcz/grisette/pull/228),
   [#231](https://github.com/lsrcz/grisette/pull/231))
- Added bitcast from and to `Bool`, `IntN`, `WordN`, `FP` and their symbolic
  counterparts when appropriate.
  ([#232](https://github.com/lsrcz/grisette/pull/232))
- Add `SymFromIntegral`. ([#233](https://github.com/lsrcz/grisette/pull/233))
- Add operations for concrete floating point numbers. Add IEEE754-2019
  `fpMinimum`, `fpMinimumNumber`, `fpMaximum`, and `fpMaximumNumber` operations.
  ([#235](https://github.com/lsrcz/grisette/pull/235))
- Add conversion from and to floating points.
  ([#236](https://github.com/lsrcz/grisette/pull/236))
- Add `SymFiniteBits`. ([#237](https://github.com/lsrcz/grisette/pull/237))
- Add unified instances for all provided operations, including `FP` and
  `AlgReal`.
  ([#239](https://github.com/lsrcz/grisette/pull/239),
   [#240](https://github.com/lsrcz/grisette/pull/240),
   [#243](https://github.com/lsrcz/grisette/pull/243))
- Allow the use of number literals for `SomeBV`.
  ([#245](https://github.com/lsrcz/grisette/pull/245))
- Add `symDistinct`.
  ([#246](https://github.com/lsrcz/grisette/pull/246),
   [#247](https://github.com/lsrcz/grisette/pull/247))

### Fixed
- Fixed model parsing for floating points.
  ([#227](https://github.com/lsrcz/grisette/pull/227))
- Allowed `mkUnifiedConstructor` to be used with types without modes or args.
  ([#242](https://github.com/lsrcz/grisette/pull/242))

### Changed
- [Breaking] Changed the operand order for `liftPFormatPrec2` and
  `liftPFormatList2`.
  ([#225](https://github.com/lsrcz/grisette/pull/225))
- [Breaking] Changed the term representation with a compile-time tag for its
  kind (`AnyKind` for all symbols and `ConstantKind` for symbols other than
  uninterpreted functions). This also affects the 'ExtractSym'. A new
  `extractSymMaybe` will regard this tag if not all symbols can be casted to
  that tag. `extractSym` will always succeed, returning a set with `AnyKind`.
  ([#230](https://github.com/lsrcz/grisette/pull/230))
- [Breaking] `SafeDivision` renamed to `SafeDiv`.
  ([#231](https://github.com/lsrcz/grisette/pull/231))
- Refined the template-haskell-based derivation mechanism.
  ([#238](https://github.com/lsrcz/grisette/pull/238))
- [Breaking] `GetData` is made injective by giving `Identity` wrapped type for
  concrete evaluation instead of the type itself.
  ([#242](https://github.com/lsrcz/grisette/pull/242))
- Changed pprint for `Identity` to not to print the constructor.
  ([#242](https://github.com/lsrcz/grisette/pull/242))
- Make `ToSym` requires the target type to be `Mergeable`. This enable us to
  merge the results for converting from `Union a` to `Union b` again.
  ([#244](https://github.com/lsrcz/grisette/pull/244))

### Removed
- Removed `fpMin` and `fpMax`, which is removed in IEEE754-2019.
  ([#235](https://github.com/lsrcz/grisette/pull/235))
- Dropped support for post-evaluation approximation.
  ([#241](https://github.com/lsrcz/grisette/pull/241))
  

## [0.7.0.0] - 2024-07-02

### Added
- Added `true` and `false` in `LogicalOp`.
  ([#211](https://github.com/lsrcz/grisette/pull/211))
- Exported the `FP` constructs in the `Grisette` module.
  ([#209](https://github.com/lsrcz/grisette/pull/209))
- Added missing `AllSyms` instance for `WordN`, `IntN`, and `FP`.
  ([#209](https://github.com/lsrcz/grisette/pull/209))
- Added unified interfaces.
  ([#210](https://github.com/lsrcz/grisette/pull/210),
   [#212](https://github.com/lsrcz/grisette/pull/212),
   [#213](https://github.com/lsrcz/grisette/pull/213),
   [#214](https://github.com/lsrcz/grisette/pull/214),
   [#215](https://github.com/lsrcz/grisette/pull/215),
   [#217](https://github.com/lsrcz/grisette/pull/217))
- Added `IEEEFPRoundingMode`.
  ([#219](https://github.com/lsrcz/grisette/pull/219))
- Added `Show` instance for `SomeSym`.
  ([#219](https://github.com/lsrcz/grisette/pull/219))
- Added incoherent conversions (`ToSym`, `ToCon`) from/to `Identity a` and `a`.
  ([#221](https://github.com/lsrcz/grisette/pull/221))

### Fixed
- Fixed the printing of FP terms.
  ([#219](https://github.com/lsrcz/grisette/pull/219))

### Changed
- [Breaking] Relaxed constraints for type classes, according to
  https://github.com/haskell/core-libraries-committee/issues/10. One problem
  this causes is that the instances for `Union` will no longer be able to always
  merge the results. This is unfortunate, but should not be critical.
  ([#213](https://github.com/lsrcz/grisette/pull/213),
   [#214](https://github.com/lsrcz/grisette/pull/214),
   [#221](https://github.com/lsrcz/grisette/pull/221))
- [Breaking] Rewritten the generic derivation mechanism.
  ([#213](https://github.com/lsrcz/grisette/pull/213),
   [#214](https://github.com/lsrcz/grisette/pull/214),
   [#216](https://github.com/lsrcz/grisette/pull/216))
- [Breaking] Changed the type class hierarchy for operations for functors, e.g.
  `SEq1`, as described in
  https://github.com/haskell/core-libraries-committee/issues/10.
  ([#216](https://github.com/lsrcz/grisette/pull/216))
- [Breaking] Renamed `UnionMergeable1` to `SymBranching`. Renamed `Union` to
  `UnionBase`, and `UnionM` to `Union`.
  ([#214](https://github.com/lsrcz/grisette/pull/214),
   [#217](https://github.com/lsrcz/grisette/pull/217))
- [Breaking] Renamed `EvaluateSym` to `EvalSym`. Renamed `SubstituteSym` to
  `SubstSym`. Renamed `ExtractSymbolics` to `ExtractSym`.
  ([#217](https://github.com/lsrcz/grisette/pull/217))
- [Breaking] Renamed `SEq` to `SymEq`. Renamed `SOrd` to `SymOrd`.
  ([#217](https://github.com/lsrcz/grisette/pull/217))
- [Breaking] Renamed `GPretty` to `PPrint`.
  ([#217](https://github.com/lsrcz/grisette/pull/217),
   [#224](https://github.com/lsrcz/grisette/pull/224))
- [Breaking] Discourage the use of approximation with `approx`. `precise` is now
  the default and we do not require `precise` to be used everytime we call a
  solver.
  ([#218](https://github.com/lsrcz/grisette/pull/218))

## [0.6.0.0] -- 2024-06-07

### Added

- Added solving procedures with solver handles.
  ([#198](https://github.com/lsrcz/grisette/pull/198))
- Added `overestimateUnionValues`.
  ([#203](https://github.com/lsrcz/grisette/pull/203))
- Added pretty printing for hashset and hashmaps.
  ([#205](https://github.com/lsrcz/grisette/pull/205))
- Added support for refinement of solutions in CEGIS algorithm.
  ([#206](https://github.com/lsrcz/grisette/pull/206))
- Added generation of globally unique identifier with `uniqueIdentifier`.
  ([#206](https://github.com/lsrcz/grisette/pull/206))
- Added support for arbitrary precision floating point theory.
  ([#207](https://github.com/lsrcz/grisette/pull/207))

### Fixed

- `withSolver` now forcifully terminate the solver when exiting the scope.
  ([#199](https://github.com/lsrcz/grisette/pull/199))
- Fixed pretty printing for monad transformers.
  ([#205](https://github.com/lsrcz/grisette/pull/205))

### Changed

- [Breaking] Equality test for `SomeBV` with different bit widths will now
  return false rather than crash.
  ([#200](https://github.com/lsrcz/grisette/pull/200))
- [Breaking] More intuitive CEGIS interface.
  ([#201](https://github.com/lsrcz/grisette/pull/201))
- [Breaking] Changed the low-level solver interface.
  ([#206](https://github.com/lsrcz/grisette/pull/206))
- [Breaking] Changed the CEGIS interface.
  ([#206](https://github.com/lsrcz/grisette/pull/206))
- Bumped the minimum supported sbv version to 8.17.
  ([#207](https://github.com/lsrcz/grisette/pull/207))

## [0.5.0.1] -- 2024-04-18

### Fixed

- Fix an building error due to a GHC 9.8.1 regression
  (https://gitlab.haskell.org/ghc/ghc/-/issues/24084).
  ([#195](https://github.com/lsrcz/grisette/pull/195))

## [0.5.0.0] -- 2024-04-18

### Added

- Added the creation of unparameterized bit vectors from run-time bit-widths.
  ([#168](https://github.com/lsrcz/grisette/pull/168),
  [#177](https://github.com/lsrcz/grisette/pull/177))
- Added all the functions available for the exception transformer in
  `transformers` and `mtl` packages.
  ([#171](https://github.com/lsrcz/grisette/pull/171))
- Improved the partial evaluation for bit vectors.
  ([#176](https://github.com/lsrcz/grisette/pull/176))
- Added `symRotateNegated` and `symShiftNegated`.
  ([#181](https://github.com/lsrcz/grisette/pull/181))
- Added `mrg` and `sym` variants for all reasonable operations from
  `Control.Monad`, `Control.Applicative`, `Data.Foldable`, `Data.List`, and
  `Data.Traversable`. ([#182](https://github.com/lsrcz/grisette/pull/182))
- Added `mrgIfPropagatedStrategy`.
  ([#184](https://github.com/lsrcz/grisette/pull/184))
- Added `freshString`. ([#188](https://github.com/lsrcz/grisette/pull/188))
- Added `localFreshIdent`. ([#190](https://github.com/lsrcz/grisette/pull/190))
- Added deriving for void types for builtin type classes.
  ([#191](https://github.com/lsrcz/grisette/pull/191))

### Fixed

- Fixed the merging for safe division.
  ([#173](https://github.com/lsrcz/grisette/pull/173))
- Fixed the behavior for safe `mod` and `rem` for signed, bounded concrete
  types. ([#173](https://github.com/lsrcz/grisette/pull/173))
- Fixed merging in `mrg*` operations for monad transformers to ensure that they
  merge the results. ([#187](https://github.com/lsrcz/grisette/pull/187))

### Changed

- [Breaking] Removed the `UnionLike` and `UnionPrjOp` interface, added the
  `TryMerge` and `PlainUnion` interface. This allows `mrg*` operations to be
  used with non-union programs.
  ([#170](https://github.com/lsrcz/grisette/pull/170))
- [Breaking] Refined the safe operations interface using `TryMerge`.
  ([#172](https://github.com/lsrcz/grisette/pull/172))
- [Breaking] Renamed `safeMinus` to `safeSub` to be more consistent.
  ([#172](https://github.com/lsrcz/grisette/pull/172))
- [Breaking] Unifies the implementation for all symbolic non-indexed
  bit-vectors. The legacy types are now type and pattern synonyms.
  ([#174](https://github.com/lsrcz/grisette/pull/174),
  [#179](https://github.com/lsrcz/grisette/pull/179),
  [#180](https://github.com/lsrcz/grisette/pull/180))
- [Breaking] Use functional dependency instead of type family for the `Function`
  class. ([#178](https://github.com/lsrcz/grisette/pull/178))
- [Breaking] Added `Mergeable` constraints to some `mrg*` list operators
  ([#182](https://github.com/lsrcz/grisette/pull/182))
- [Breaking] Refactored the `mrg*` constructor related template haskell code.
  ([#185](https://github.com/lsrcz/grisette/pull/185))
- [Breaking] Dropped symbols with extra information.
  ([#188](https://github.com/lsrcz/grisette/pull/188))
- [Breaking] The `FreshIdent` is removed. It is now changed to `Identifier` and
  `Symbol` types. ([#192](https://github.com/lsrcz/grisette/pull/192))
- Changed the internal representation of the terms.
  ([#193](https://github.com/lsrcz/grisette/pull/193))
- [Breaking] Refactored the project structures.
  ([#194](https://github.com/lsrcz/grisette/pull/194))

## [0.4.1.0] -- 2024-01-10

### Added

- Added `cegisForAll` interfaces.
  ([#165](https://github.com/lsrcz/grisette/pull/165))

## [0.4.0.0] -- 2024-01-08

### Added

- Added wrappers for state transformers.
  ([#132](https://github.com/lsrcz/grisette/pull/132))
- Added `toGuardList` function.
  ([#137](https://github.com/lsrcz/grisette/pull/137))
- Exported some previously hidden API (`BVSignConversion`, `runFreshTFromIndex`)
  that we found useful or forgot to export.
  ([#138](https://github.com/lsrcz/grisette/pull/138),
  [#139](https://github.com/lsrcz/grisette/pull/139))
- Provided `mrgRunFreshT` to run `FreshT` with merging.
  ([#140](https://github.com/lsrcz/grisette/pull/140))
- Added `Grisette.Data.Class.SignConversion.SignConversion` for types from
  `Data.Int` and `Data.Word`.
  ([#142](https://github.com/lsrcz/grisette/pull/142))
- Added shift functions by symbolic shift amounts.
  ([#151](https://github.com/lsrcz/grisette/pull/151))
- Added `apply` for uninterpreted functions.
  ([#155](https://github.com/lsrcz/grisette/pull/155))
- Added `liftFresh` to lift a `Fresh` into `MonadFresh`.
  ([#156](https://github.com/lsrcz/grisette/pull/156))
- Added a handle types for SBV solvers. This allows users to use SBV solvers
  without the need to wrap everything in the SBV monads.
  ([#159](https://github.com/lsrcz/grisette/pull/159))
- Added a new generic CEGIS interface. This allows any verifier/fuzzer to be
  used in the CEGIS loop. ([#159](https://github.com/lsrcz/grisette/pull/159))

### Removed

- [Breaking] Removed the `Grisette.Lib.Mtl` module.
  ([#132](https://github.com/lsrcz/grisette/pull/132))
- [Breaking] Removed `SymBoolOp` and `SymIntegerOp`.
  ([#146](https://github.com/lsrcz/grisette/pull/146))
- [Breaking] Removed `ExtractSymbolics` instance for `SymbolSet`.
  ([#146](https://github.com/lsrcz/grisette/pull/146))

### Fixed

- Removed the quotation marks around the pretty printed results for string-like
  data types. ([#127](https://github.com/lsrcz/grisette/pull/127))
- Fixed the `SOrd` instance for `VerificationConditions`.
  ([#131](https://github.com/lsrcz/grisette/pull/131))
- Fixed the missing `SubstituteSym` instance for `UnionM`.
  ([#131](https://github.com/lsrcz/grisette/pull/131))
- Fixed the symbolic generation order for `Maybe`.
  ([#131](https://github.com/lsrcz/grisette/pull/131))
- Fixed the `toInteger` function for `IntN 1`.
  ([#143](https://github.com/lsrcz/grisette/pull/143))
- Fixed the `abs` function for `WordN`.
  ([#144](https://github.com/lsrcz/grisette/pull/143))
- Fixed the QuickCheck shrink function for `WordN 1` and `IntN 1`.
  ([#149](https://github.com/lsrcz/grisette/pull/149))
- Fixed the heap overflow bug for `shiftL` for `WordN` and `IntN` by large
  numbers. ([#150](https://github.com/lsrcz/grisette/pull/150))

### Changed

- Reorganized the files for `MonadTrans`.
  ([#132](https://github.com/lsrcz/grisette/pull/132))
- [Breaking] Changed the name of `Union` constructors and patterns.
  ([#133](https://github.com/lsrcz/grisette/pull/133))
- The `Union` patterns, when used as constructors, now merges the result.
  ([#133](https://github.com/lsrcz/grisette/pull/133))
- Changed the symbolic identifier type from `String` to `Data.Text.Text`.
  ([#141](https://github.com/lsrcz/grisette/pull/141))
- [Breaking] `Grisette.Data.Class.BitVector.BVSignConversion` is now
  `Grisette.Data.Class.SignConversion.SignConversion`.
  ([#142](https://github.com/lsrcz/grisette/pull/142))
- [Breaking] Moved the `ITEOp`, `LogicalOp`, and `SEq` type classes to dedicated
  modules. ([#146](https://github.com/lsrcz/grisette/pull/146))
- [Breaking] Moved `Grisette.Data.Class.Evaluate` to
  `Grisette.Data.Class.EvaluateSym`.
  ([#146](https://github.com/lsrcz/grisette/pull/146))
- [Breaking] Moved `Grisette.Data.Class.Substitute` to
  `Grisette.Data.Class.SubstituteSym`.
  ([#146](https://github.com/lsrcz/grisette/pull/146))
- [Breaking] Split the `Grisette.Data.Class.SafeArith` module to
  `Grisette.Data.Class.SafeDivision` and `Grisette.Data.Class.SafeLinearArith`.
  ([#146](https://github.com/lsrcz/grisette/pull/146))
- [Breaking] Changed the API to `MonadFresh`.
  ([#156](https://github.com/lsrcz/grisette/pull/156))
- [Breaking] Renamed multiple symbolic operators.
  ([#158](https://github.com/lsrcz/grisette/pull/158))
- [Breaking] Changed the solver interface.
  ([#159](https://github.com/lsrcz/grisette/pull/159))
- [Breaking] Changed the CEGIS solver interface.
  ([#159](https://github.com/lsrcz/grisette/pull/159))

## [0.3.1.1] -- 2023-09-29

No user-facing changes.

## [0.3.1.0] -- 2023-07-19

### Added

- Added support to `Data.Text`.
  ([#95](https://github.com/lsrcz/grisette/pull/95))
- Added `Arbitrary` instances for bit vectors.
  ([#97](https://github.com/lsrcz/grisette/pull/97))
- Added pretty printers for Grisette data types.
  ([#101](https://github.com/lsrcz/grisette/pull/101))
- Added `ExtractSymbolics` instances for tuples longer than 2.
  ([#103](https://github.com/lsrcz/grisette/pull/103))

### Fixed

- Fixed the `Read` instance for bit vectors.
  ([#99](https://github.com/lsrcz/grisette/pull/99),
  [#100](https://github.com/lsrcz/grisette/pull/100))

## [0.3.0.0] -- 2023-07-07

### Added

- Added the conversion between signed and unsigned bit vectors.
  ([#69](https://github.com/lsrcz/grisette/pull/69))
- Added the generation of `SomeSymIntN` and `SomeSymWordN` from a single `Int`
  for bit width. ([#73](https://github.com/lsrcz/grisette/pull/73))
- Added the `FiniteBits` instance for `SomeSymIntN` and `SomeSymWordN`.
  ([#83](https://github.com/lsrcz/grisette/pull/83))
- Added more flexible instances for symbolic generation for `Either`, `Maybe`
  and list types. ([#84](https://github.com/lsrcz/grisette/pull/84))
- Added an experimental `GenSymConstrained` type class.
  ([#89](https://github.com/lsrcz/grisette/pull/89))

### Changed

- Changed the operations for `SomeIntN` and `SomeWordN` to accepting dynamic
  runtime integers rather than compile-time integers.
  ([#71](https://github.com/lsrcz/grisette/pull/71))
- Comparing the equality of `SomeIntN`/`SomeWordN`/`SomeSymIntN`/`SomeSymWordN`
  with different bit widths returns false rather than crash now.
  ([#74](https://github.com/lsrcz/grisette/pull/74))

### Fixed

- Fixed the compatibility issue with sbv 10+.
  ([#66](https://github.com/lsrcz/grisette/pull/66))
- Fixed build error with newer GHC.
  ([#70](https://github.com/lsrcz/grisette/pull/70))
- Fixed the merging for `SomeSymIntN` and `SomeSymWordN`.
  ([#72](https://github.com/lsrcz/grisette/pull/72))

## [0.2.0.0] - 2023-04-13

### Added

- Add term size count API. ([#48](https://github.com/lsrcz/grisette/pull/48),
  [#53](https://github.com/lsrcz/grisette/pull/53))
- Add timeout to solver interface.
  ([#49](https://github.com/lsrcz/grisette/pull/49),
  [#50](https://github.com/lsrcz/grisette/pull/50))
- Add parallel do-notation for parallel symbolic compilation.
  ([#51](https://github.com/lsrcz/grisette/pull/51))
- Added some missing instances for symbolic values and bit vectors.
  ([#46](https://github.com/lsrcz/grisette/pull/46),
  [#61](https://github.com/lsrcz/grisette/pull/61))
- Add missing instances for `MonadFresh` and `FreshT`.
  ([#59](https://github.com/lsrcz/grisette/pull/59))

### Changed

- New safe operator interfaces.
  ([#56](https://github.com/lsrcz/grisette/pull/56))
- Redesigned symbolic value interface.
  - `Sym Bool`/`Sym Integer`, etc., are no longer available and are replaced
    with `SymBool` and `SymInteger`.
    ([#41](https://github.com/lsrcz/grisette/pull/41))
  - New symbolic bit vector interface. Added unsized bit vector.
    ([#41](https://github.com/lsrcz/grisette/pull/41))

### Removed

- Dropped merging cache for `UnionM`. This fixed some segmentation fault errors.
  ([#43](https://github.com/lsrcz/grisette/pull/43))

### Fixed

- Fix CEGIS when no symbolic input is present.
  ([#52](https://github.com/lsrcz/grisette/pull/52))
- Fix overlapping `ToSym` and `ToCon` instances.
  ([#54](https://github.com/lsrcz/grisette/pull/54))
- Fix uninterpreted function lowering.
  ([#57](https://github.com/lsrcz/grisette/pull/57),
  [#58](https://github.com/lsrcz/grisette/pull/58))
- Fix CEGIS crash when subsequent solver calls introduces new symbolic constant.
  ([#60](https://github.com/lsrcz/grisette/pull/60))

## [0.1.0.0] - 2023-01-20

### Added

- Initial release for Grisette.

[Unreleased]: https://github.com/lsrcz/grisette/compare/v0.8.0.0...HEAD
[0.8.0.0]: https://github.com/lsrcz/grisette/compare/v0.7.0.0...v0.8.0.0
[0.7.0.0]: https://github.com/lsrcz/grisette/compare/v0.6.0.0...v0.7.0.0
[0.6.0.0]: https://github.com/lsrcz/grisette/compare/v0.5.0.1...v0.6.0.0
[0.5.0.1]: https://github.com/lsrcz/grisette/compare/v0.5.0.0...v0.5.0.1
[0.5.0.0]: https://github.com/lsrcz/grisette/compare/v0.4.1.0...v0.5.0.0
[0.4.1.0]: https://github.com/lsrcz/grisette/compare/v0.4.0.0...v0.4.1.0
[0.4.0.0]: https://github.com/lsrcz/grisette/compare/v0.3.1.1...v0.4.0.0
[0.3.1.1]: https://github.com/lsrcz/grisette/compare/v0.3.1.0...v0.3.1.1
[0.3.1.0]: https://github.com/lsrcz/grisette/compare/v0.3.0.0...v0.3.1.0
[0.3.0.0]: https://github.com/lsrcz/grisette/compare/v0.2.0.0...v0.3.0.0
[0.2.0.0]: https://github.com/lsrcz/grisette/compare/v0.1.0.0...v0.2.0.0
[0.1.0.0]: https://github.com/lsrcz/grisette/tree/v0.1.0.0
