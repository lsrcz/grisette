# Changes

## Version 0.2.0.0

### Feature
- Redesigned symbolic value interface.
  - `Sym Bool`/`Sym Integer`, etc., are no longer available and are replaced with `SymBool` and `SymInteger`. ([#41](https://github.com/lsrcz/grisette/pull/41))
  - New symbolic bit vector interface. Added unsized bit vector. ([#41](https://github.com/lsrcz/grisette/pull/41))
- Add term size count API. ([#48](https://github.com/lsrcz/grisette/pull/48), [#53](https://github.com/lsrcz/grisette/pull/53))
- Add timeout to solver interface. ([#49](https://github.com/lsrcz/grisette/pull/49), [#50](https://github.com/lsrcz/grisette/pull/50))
- Add parallel do-notation for parallel symbolic compilation. ([#51](https://github.com/lsrcz/grisette/pull/51))
- New safe operator interfaces. ([#56](https://github.com/lsrcz/grisette/pull/56))


### Bugfix
- Dropped merging cache for `UnionM`. This fixed some segmentation fault errors. ([#43](https://github.com/lsrcz/grisette/pull/43))
- Added some missing instances for symbolic values and bit vectors. ([#46](https://github.com/lsrcz/grisette/pull/46), [#61](https://github.com/lsrcz/grisette/pull/61))
- Fix CEGIS when no symbolic input is present. ([#52](https://github.com/lsrcz/grisette/pull/52))
- Fix overlapping `ToSym` and `ToCon` instances. ([#54](https://github.com/lsrcz/grisette/pull/54))
- Fix uninterpreted function lowering. ([#57](https://github.com/lsrcz/grisette/pull/57), [#58](https://github.com/lsrcz/grisette/pull/58))
- Add missing instances for `MonadFresh` and `FreshT`. ([#59](https://github.com/lsrcz/grisette/pull/59))
- Fix CEGIS crash when subsequent solver calls introduces new symbolic constant. ([#60](https://github.com/lsrcz/grisette/pull/60))



## Version 0.1.0.0
Initial release.