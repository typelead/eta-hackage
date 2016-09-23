# ghcvm-hackage

A set of patches to make Hackage compatible with GHCVM.

## Out-of-the-box Packages
This the list of packages that can be installed with CabalVM without modification:
- [transformers-0.5.2.0](https://hackage.haskell.org/package/transformers-0.5.2.0)

## Patched Packages
These packages needed to be patched (and hence should have a patch file uploaded in this repository):
- None for now


## Built-in Packages
These packages have special meaning in the GHCVM compile and hence are provided upon installation:
- [base-4.8.2.0](https://hackage.haskell.org/package/base-4.8.2.0)
- [integer-0.5.1.0](https://hackage.haskell.org/package/integer-gmp-0.5.1.0)
  - *NOTE*: Due to the drastic differences between the `Integer` implementations,
            the public API between the corresponding GHC package is slightly different.
- [ghc-prim-0.4.0.0](https://hackage.haskell.org/package/ghc-prim-0.4.0.0)
  - *NOTE*: Due to the introduction of new primitives for GHCVM,
            the public API between the corresponding GHC package is slightly different.
