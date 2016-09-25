# GHCVM Hackage

This repository contains a set of patches for particular packages from Hackage that cannot be built out-of-the-box with GHCVM/CabalVM.

**Table of Contents**

- [Package Categorization](#package-categorization)
  - [Supported Packages](#supported-packages)
  - [Unsupported Packages](#unsupported-packages)
  - [Built-in Packages](#built-in-packages)
- [Contributing](#contributing)
  - [Patching a Library](#patching-a-library)  
    - [Prerequisites](#prerequisites)
    - [Patching](#patching)

## Package Categorization

### Overview

We have categorized the packages from Hackage below in relation to GHCVM support. If a package from Hackage has not been listed here, it means no one has tried to build it and categorize it.

If you would like to have a given package/version made compatible with GHCVM, you can:

- Submit an [issue](https://github.com/rahulmutt/ghcvm-hackage/issues)
- Ask us on [Gitter](https://gitter.im/rahulmutt/ghcvm)

### Supported Packages
These packages are supported by GHCVM.
- [Adaptive-0.1](https://hackage.haskell.org/package/Adaptive-0.1)
- [containers--0.5.8.1](https://hackage.haskell.org/package/containers-0.5.8.1)
- [gdiff-1.1](https://hackage.haskell.org/package/gdiff-1.1)
- [gray-code-0.3.1](https://hackage.haskell.org/package/gray-code-0.3.1)
- [heaps-0.3.3](https://hackage.haskell.org/package/heaps-0.3.3)
- [deepseq-1.4.2.0](https://hackage.haskell.org/package/deepseq-1.4.2.0)
- [transformers-0.5.2.0](https://hackage.haskell.org/package/transformers-0.5.2.0)
- [transformers-compat-0.5.1.4](http://hackage.haskell.org/package/transformers-compat-0.5.1.4)
- [list-extras-0.4.1.4](https://hackage.haskell.org/package/list-extras-0.4.1.4)
- [microlens-0.4.7.0](https://hackage.haskell.org/package/microlens-0.4.7.0)
- [microlens-mtl-0.1.10.0](http://hackage.haskell.org/package/microlens-mtl-0.1.10.0)
- [mtl-2.2.1](https://hackage.haskell.org/package/mtl-2.2.1)
- [patch-combinators-0.2.2](https://hackage.haskell.org/package/patch-combinators-0.2.2)

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

## Contributing

### Patching a Library

Suppose that you find a library you want to patch, say `somepackage-0.1.2.3`.

#### Prerequisites
If you have already forked this repository, the skip to step 3.

1. Fork this repository.

2. Clone the forked repository & update the `master` branch.
  ```
  $ git clone http://github.org/your-name-here/ghcvm-hackage
  ```

3. Checkout the `master` branch and pull any changes.
  ```
  $ git checkout master
  $ git pull
  ```

4. Create a new branch with the same name as the package.
  ```
  $ git checkout -b somepackage
  ```

#### Patching

1. Download the `somepackage-0.1.2.3.tar.gz` tarball from Hackage.

2. Initialize a git repository and make an initial commit.
  ```
  $ git init
  $ git add .
  $ git commit -m "First"
  ```

3. Build the package.
  ```
  $ cabalvm build
  ```

4. Apply the desired changes and go back to step 3 as long as the build fails.

5. Once the build succeeds, make a commit.
  ```
  $ git add .
  $ git commit -m "Patched"
  ```

6. Create a patch.
   ```
   $ git format-patch HEAD~ --stdout > somepackage-0.1.2.3.patch
   ```

7. If you have changed the `.cabal` file of the package in your patch , make a copy and rename it from `somepackage.cabal` to `somepackage-0.1.2.3.cabal`.

8. Copy the patch file and the cabal file (if changed) to the `patches` directory in your local clone of your fork of the `ghcvm-hackage` repository.

9. In your `ghcvm-hackage` repository,
   ```
   $ git commit -m "Patched somepackage-0.1.2.3"
   $ git push origin
   ```
10. Submit a pull request to this repository for review.
