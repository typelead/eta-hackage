# GHCVM Hackage

This repository contains a set of patches for particular packages from Hackage that cannot be built out-of-the-box with GHCVM/CabalVM.

## Package Categorization

### Overview

Below are the packages from Hackage listed according to what had to be done to in order to get them to work. If a package from Hackage has not been listed here, it means no one has tried to build it and categorize it.

If you would like to have a given package/version made compatible with GHCVM, you can:

- Submit an [issue](issues)
- Do it yourself and submit a PR to update this README
- Ask us on [Gitter](https://gitter.im/rahulmutt/ghcvm)

### Out-of-the-box Packages
This the list of packages that can be installed with CabalVM without modification:
- [transformers-0.5.2.0](https://hackage.haskell.org/package/transformers-0.5.2.0)
- [mtl-2.2.1](https://hackage.haskell.org/package/mtl-2.2.1)
- [gdiff-1.1](https://hackage.haskell.org/package/gdiff-1.1)

### Patched Packages
These packages needed to be patched (and hence should have a patch file uploaded in this repository).
- None for now

### Unsupported Packages
These packages are heavy on FFI dependencies and don't make sense in the context of the JVM, hence no effort will be made to port them.
- None for now


### Built-in Packages
These packages have special meaning in the GHCVM compiler and hence are provided upon installation.
- [base-4.8.2.0](https://hackage.haskell.org/package/base-4.8.2.0)
- [integer-0.5.1.0](https://hackage.haskell.org/package/integer-gmp-0.5.1.0)
  - *NOTE*: Due to the drastic differences between the `Integer` implementations,
            the public API between the corresponding GHC package is slightly different.
- [ghc-prim-0.4.0.0](https://hackage.haskell.org/package/ghc-prim-0.4.0.0)
  - *NOTE*: Due to the introduction of new primitives for GHCVM,
            the public API between the corresponding GHC package is slightly different.
