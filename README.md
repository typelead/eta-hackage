# Eta Hackage

[![Build Status](https://circleci.com/gh/typelead/eta-hackage.svg?style=shield&circle-token=d70d4858668d80ca74fbaf0172c7f270a6b0a4f6)](https://circleci.com/gh/typelead/eta-hackage)

This repository contains a set of patches for particular packages from
Hackage that cannot be built out-of-the-box with `etlas`. To install a package, you
have to run the following command:

```
etlas install <package-name>
```

`etlas` will take care of downloading the tar file, optionally patching it, and installing it. If a patch is in the repo, but your local `etlas` in unable to find a patch
for it, run

```
etlas update
```

and try the installation again.

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

We have categorized the packages from Hackage below in relation to `etlas` support. If a package from Hackage has not been listed here, it means no one has tried to build it and categorize it.

If you would like to have a given package/version made compatible with `etlas`, you can:

- Submit an [issue](https://github.com/typelead/eta-hackage/issues/new)
- Ask us on [Gitter](https://gitter.im/typelead/eta)

### Supported Packages

These packages are supported by `etlas`.

- [abstract-deque-0.3](https://hackage.haskell.org/package/abstract-deque-0.3)
- [abstract-par-0.3.3](https://hackage.haskell.org/package/abstract-par-0.3.3)
- [Adaptive-0.1](https://hackage.haskell.org/package/Adaptive-0.1)
- [adjunctions-4.3](https://hackage.haskell.org/package/adjunctions-4.3)
- [aeson >= 1.1.0.0 && <= 1.2.3.0](https://hackage.haskell.org/package/aeson)
- [aeson-pretty >= 0.8.2 && aeson-pretty <= 0.8.5](https://hackage.haskell.org/package/aeson-pretty-0.8.5)
- [agum-2.6](https://hackage.haskell.org/package/agum-2.6)
- [ansi-terminal >= 0.6.2.3 && <= 0.6.3.1](https://hackage.haskell.org/package/ansi-terminal)
- [ansi-wl-pprint-0.6.7.3](https://hackage.haskell.org/package/ansi-wl-pprint-0.6.7.3)
- [appar-0.1.4](https://hackage.haskell.org/package/appar-0.1.4)
- [array >= 0.5.1.1 && <= 0.5.2.0](https://hackage.haskell.org/package/array)
- [array-utils-0.3](https://hackage.haskell.org/package/array-utils-0.3)
- [async-2.1.1.1](https://hackage.haskell.org/package/async-2.1.1.1)
- [attoparsec >= 0.13.1.0 && <= 0.13.2.0](https://hackage.haskell.org/package/attoparsec)
- [auto-update-0.1.4](https://hackage.haskell.org/package/auto-update-0.1.4)
- [avro >= 0.1.0.1 && <= 0.2.0.0](https://hackage.haskell.org/package/avro-0.1.0.1)
- [basement-0.0.4](https://hackage.haskell.org/package/basement-0.0.4)
- [base-compat >= 0.9.1 && <= 0.9.3](https://hackage.haskell.org/package/base-compat)
- [base-orphans-0.5.4](https://hackage.haskell.org/package/base-orphans-0.5.4)
- [base-prelude-1.2.0.1](https://hackage.haskell.org/package/base-prelude-1.2.0.1)
- [base16-bytestring-0.1.1.6](https://hackage.haskell.org/package/base16-bytestring-0.1.1.6)
- [base64-bytestring-1.0.0.1](https://hackage.haskell.org/package/base64-bytestring-1.0.0.1)
- [basic-lens-0.0.0](https://hackage.haskell.org/package/basic-lens-0.0.0)
- [basic-prelude-0.6.1](https://hackage.haskell.org/package/basic-prelude-0.6.1)
- [binary-conduit-1.2.4.1](https://hackage.haskell.org/package/binary-conduit-1.2.4.1)
- [binary-search-1.0.0.3](https://hackage.haskell.org/package/binary-search-1.0.0.3)
- [bindings-DSL-1.0.23](https://hackage.haskell.org/package/bindings-DSL-1.0.23)
- [bifunctors-5.4.1](https://hackage.haskell.org/package/bifunctors-5.4.1)
- [bitarray-0.0.1.1](https://hackage.haskell.org/package/bitarray-0.0.1.1)
- [Binpack-0.4.1](https://hackage.haskell.org/package/Binpack-0.4.1)
- [blaze-builder-0.4.0.2](https://hackage.haskell.org/package/blaze-builder-0.4.0.2)
- [blaze-html-0.8.1.3](https://hackage.haskell.org/package/blaze-html-0.8.1.3)
- [blaze-html-truncate-0.3.0.0](https://hackage.haskell.org/package/blaze-html-truncate-0.3.0.0)
- [blaze-markup >= 0.7.1.1 && <= 0.8.0.0](https://hackage.haskell.org/package/blaze-markup)
- [bmp-1.2.6.3](https://hackage.haskell.org/package/bmp-1.2.6.3)
- [Boolean >= 0.2.3 && <= 0.2.4](https://hackage.haskell.org/package/Boolean)
- [byteable-0.1.1](https://hackage.haskell.org/package/byteable-0.1.1)
- [byteorder-1.0.4](https://hackage.haskell.org/package/byteorder-1.0.4)
- [bytes-0.15.3](https://hackage.haskell.org/package/bytes-0.15.3)
- [bytestring >= 0.10.8.1.0 && <= 0.10.8.2.0](https://hackage.haskell.org/package/bytestring)
- [bytestring-builder-0.10.8.1.0](https://hackage.haskell.org/package/bytestring-builder-0.10.8.1.0)
- [bytestring-show-0.3.5.6](https://hackage.haskell.org/package/bytestring-show-0.3.5.6)
- [call-stack-0.1.0](https://hackage.haskell.org/package/call-stack-0.1.0)
- [carray-0.1.6.6](https://hackage.haskell.org/package/carray-0.1.6.6)
- [case-insensitive >= 1.2.0.7 && <= 1.2.0.10](https://hackage.haskell.org/package/case-insensitive)
- [cassava-0.4.5.1](https://hackage.haskell.org/package/cassava-0.4.5.1)
- [cereal-0.5.4.0](https://hackage.haskell.org/package/cereal-0.5.4.0)
- [circle-packing-0.1.0.6](https://hackage.haskell.org/package/circle-packing-0.1.0.6)
- [chunked-data-0.3.0](https://hackage.haskell.org/package/chunked-data-0.3.0)
- [clock-0.7.2](https://hackage.haskell.org/package/clock-0.7.2)
- [colour-2.3.3](https://hackage.haskell.org/package/colour-2.3.3)
- [comonad-5](https://hackage.haskell.org/package/comonad-5)
- [conceit-0.4.0.0](https://hackage.haskell.org/package/conceit-0.4.0.0)
- [concurrent-machines-0.2.3.3](https://hackage.haskell.org/package/concurrent-machines-0.2.3.3)
- [conduit-1.2.8](https://hackage.haskell.org/package/conduit-1.2.8)
- [cookie-0.4.2.1](https://hackage.haskell.org/package/cookie-0.4.2.1)
- [constraints-0.8](https://hackage.haskell.org/package/constraints-0.8)
- [containers >= 0.5.8.1 && <= 0.5.9.1 ](https://hackage.haskell.org/package/containers-0.5.9.1)
- [contravariant-1.4](https://hackage.haskell.org/package/contravariant-1.4)
- [control-monad-omega-0.3.1](https://hackage.haskell.org/package/control-monad-omega-0.3.1)
- [convertible-1.1.1.0](https://hackage.haskell.org/package/convertible-1.1.1.0)
- [data-default-0.7.1.1](https://hackage.haskell.org/package/data-default-0.7.1.1)
- [data-default-class-0.1.2.0](https://hackage.haskell.org/package/data-default-class-0.1.2.0)
- [data-default-instances-containers-0.0.1](https://hackage.haskell.org/package/data-default-instances-containers-0.0.1)
- [data-default-instances-dlist-0.0.1](https://hackage.haskell.org/package/data-default-instances-dlist-0.0.1)
- [data-default-instances-old-locale-0.0.1](https://hackage.haskell.org/package/data-default-instances-old-locale-0.0.1)
- [data-reify-0.6.1](https://hackage.haskell.org/package/data-reify-0.6.1)
- [DecisionTree-0.0](https://hackage.haskell.org/package/DecisionTree-0.0)
- [deepseq-1.4.2.0](https://hackage.haskell.org/package/deepseq-1.4.2.0)
- [deepseq-generics-0.2.0.0](https://hackage.haskell.org/package/deepseq-generics-0.2.0.0)
- [diagrams-solve-0.1.1](https://hackage.haskell.org/package/diagrams-solve-0.1.1)
- [digest 0.0.1.2](https://hackage.haskell.org/package/digest)
- [directory >= 1.3.0.0 && <= 1.3.1.0](https://hackage.haskell.org/package/directory)
- [directory-tree-0.12.1](https://hackage.haskell.org/package/directory-tree-0.12.1)
- [disjoint-sets-st-0.1](https://hackage.haskell.org/package/disjoint-sets-st-0.1)
- [distributive >= 0.5.0.2 && <= 0.5.3](https://hackage.haskell.org/package/distributive)
- [dlist >= 0.8.0.2 && <= 0.8.0.3](https://hackage.haskell.org/package/dlist)
- [dom-lt-0.1.3](https://hackage.haskell.org/package/dom-lt-0.1.3)
- [dual-tree-0.2.1](https://hackage.haskell.org/package/dual-tree-0.2.1)
- [either-4.4.1.1](https://hackage.haskell.org/package/either-4.4.1.1)
- [erf-2.0.0.0](https://hackage.haskell.org/package/erf-2.0.0.0)
- [errors-2.1.3](https://hackage.haskell.org/package/errors-2.1.3)
- [exceptions-0.8.3](https://hackage.haskell.org/package/exceptions-0.8.3)
- [extra-1.5.2](https://hackage.haskell.org/package/extra)
- [fail-4.9.0.0](https://hackage.haskell.org/package/fail-4.9.0.0)
- [fast-math-1.0.2](https://hackage.haskell.org/package/fast-math-1.0.2)
- [fibonacci-0.2.0.1](https://hackage.haskell.org/package/fibonacci-0.2.0.1)
- [fingertree-0.1.1.0](https://hackage.haskell.org/package/fingertree-0.1.1.0)
- [fingertree-psqueue-0.3](https://hackage.haskell.org/package/fingertree-psqueue-0.3)
- [fingertree-tf-0.1.0.0](https://hackage.haskell.org/package/fingertree-tf-0.1.0.0)
- [filepath >= 1.4.1.0 && <= 1.4.1.1](hackage.haskell.org/package/filepath)
- [fgl-5.5.3.0](https://hackage.haskell.org/package/fgl-5.5.3.0)
- [fmlist-0.9](https://hackage.haskell.org/package/fmlist-0.9)
- [foundation-0.0.17](https://hackage.haskell.org/package/foundation-0.0.17)
- [free-4.12.4](https://hackage.haskell.org/package/free-4.12.4)
- [garsia-wachs-1.2](https://hackage.haskell.org/package/garsia-wachs-1.2)
- [generic-deriving >= 1.10.7 && <= 1.11.2](https://hackage.haskell.org/package/generic-deriving-1.11.2)
- [GenericPretty-1.2.1](https://hackage.haskell.org/package/GenericPretty-1.2.1)
- [gdiff-1.1](https://hackage.haskell.org/package/gdiff-1.1)
- [gray-code-0.3.1](https://hackage.haskell.org/package/gray-code-0.3.1)
- [ghc-boot-th-8.0.1](https://hackage.haskell.org/package/ghc-boot-th-8.0.1)
- [Glob-0.7.14](https://hackage.haskell.org/package/Glob-0.7.14)
- [graph-matchings-0.1.0.0](https://hackage.haskell.org/package/graph-matchings-0.1.0.0)
- [graph-wrapper-0.2.5.1](https://hackage.haskell.org/package/graph-wrapper-0.2.5.1)
- [groups-0.4.0.0](https://hackage.haskell.org/package/groups-0.4.0.0)
- [hashable >= 1.2.4.0 && <= 1.2.6.1](https://hackage.haskell.org/package/hashable)
- [hashtables >= 1.2.1.0 && <= 1.2.2.1](https://hackage.haskell.org/package/hashtables)
- [HTTP-4000.3.8](https://hackage.haskell.org/package/HTTP-4000.3.8)
- [HDBC-2.4.0.1](https://hackage.haskell.org/package/HDBC-2.4.0.1)
- [heap-1.0.3](https://hackage.haskell.org/package/heap-1.0.3)
- [heaps-0.3.3](https://hackage.haskell.org/package/heaps-0.3.3)
- [hetero-map-0.21](https://hackage.haskell.org/package/hetero-map-0.21)
- [hex-0.1.2](https://hackage.haskell.org/package/hex-0.1.2)
- [hgal-2.0.0.2](https://hackage.haskell.org/package/hgal-2.0.0.2)
- [HoleyMonoid-0.1.1](https://hackage.haskell.org/package/HoleyMonoid-0.1.1)
- [hostname-1.0](https://hackage.haskell.org/package/hostname-1.0)
- [hspec-2.4.3](https://hackage.haskell.org/package/hspec-2.4.3)
- [hspec-core-2.4.3](https://hackage.haskell.org/package/hspec-core-2.4.3)
- [hspec-discover-2.4.3](https://hackage.haskell.org/package/hspec-discover-2.4.3)
- [hspec-expectations-0.8.2](https://hackage.haskell.org/package/hspec-expectations-0.8.2)
- [html-truncate-0.3.0.0](https://hackage.haskell.org/package/html-truncate-0.3.0.0)
- [http-api-data-0.3.5](https://hackage.haskell.org/package/http-api-data-0.3.5)
- [http-date-0.0.6.1](https://hackage.haskell.org/package/http-date-0.0.6.1)
- [http-media-0.6.4](https://hackage.haskell.org/package/http-media-0.6.4)
- [http-types-0.9.1](https://hackage.haskell.org/package/http-types-0.9.1)
- [HUnit >= 1.2.5.2 && <= 1.6.0.0](https://hackage.haskell.org/package/HUnit)
- [ieee754-0.8.0](https://hackage.haskell.org/package/ieee754-0.8.0)
- [integer-logarithms-1.0.2](https://hackage.haskell.org/package/integer-logarithms-1.0.2)
- [integration-0.2.1](https://hackage.haskell.org/package/integration-0.2.1)
- [interval-1.20160821](https://hackage.haskell.org/package/interval-1.20160821)
- [iproute-1.7.1](https://hackage.haskell.org/package/iproute-1.7.1)
- [ix-shapable-0.1.0](https://hackage.haskell.org/package/ix-shapable-0.1.0)
- [js-flot-0.8.3](https://hackage.haskell.org/package/js-flot-0.8.3)
- [js-jquery-3.1.1](https://hackage.haskell.org/package/js-jquery-3.1.1)
- [kan-extensions-5.0.1](https://hackage.haskell.org/package/kan-extensions-5.0.1)
- [lazy-io-0.1.0](https://hackage.haskell.org/package/lazy-io-0.1.0)
- [labeled-tree-1.0.0.0](https://hackage.haskell.org/package/labeled-tree-1.0.0.0)
- [lens >= 4.15.1 && <= 4.15.4](https://hackage.haskell.org/package/lens)
- [lens-aeson-1.0.2](https://hackage.haskell.org/package/lens-aeson-1.0.2)
- [lens-family-1.2.1](https://hackage.haskell.org/package/lens-family-1.2.1)
- [lens-family-core-1.2.1](https://hackage.haskell.org/package/lens-family-core-1.2.1)
- [lca-0.3](https://hackage.haskell.org/package/lca-0.3)
- [lifted-async-0.9.0](https://hackage.haskell.org/package/lifted-async-0.9.0)
- [lifted-base >= 0.2.3.8 && <= 0.2.3.11](https://hackage.haskell.org/package/lifted-base)
- [linear-1.20.6](https://hackage.haskell.org/package/linear-1.20.6)
- [list-extras-0.4.1.4](https://hackage.haskell.org/package/list-extras-0.4.1.4)
- [logict-0.6.0.2](https://hackage.haskell.org/package/logict-0.6.0.2)
- [loop-0.3.0](https://hackage.haskell.org/package/loop-0.3.0)
- [machines-0.6.1](https://hackage.haskell.org/package/machines-0.6.1)
- [machines-binary-0.3.0.3](https://hackage.haskell.org/package/machines-binary-0.3.0.3)
- [machines-io-0.2.0.13](https://hackage.haskell.org/package/machines-io-0.2.0.13)
- [math-functions-0.2.1.0](https://hackage.haskell.org/package/math-functions-0.2.1.0)
- [matrix-0.3.5.0](https://hackage.haskell.org/package/matrix-0.3.5.0)
- [map-syntax-0.2.0.1](https://hackage.haskell.org/package/map-syntax-0.2.0.1)
- [meldable-heap-2.0.3](https://hackage.haskell.org/package/meldable-heap-2.0.3)
- [memory-0.14.10](https://hackage.haskell.org/package/memory-0.14.10)
- [MemoTrie-0.6.8](https://hackage.haskell.org/package/MemoTrie-0.6.8)
- [mfsolve-0.3.2.0](https://hackage.haskell.org/package/mfsolve-0.3.2.0)
- [microlens >= 0.4.7.0 && <= 0.4.8.0](https://hackage.haskell.org/package/microlens)
- [microlens-mtl >= 0.1.10.0 && <= 0.1.11.0](http://hackage.haskell.org/package/microlens-mtl)
- [mime-types-0.1.0.7](http://hackage.haskell.org/package/mime-types-0.1.0.7)
- [mmorph >= 1.0.6 && <= 1.1.0](https://hackage.haskell.org/package/mmorph)
- [monad-par-0.3.4.8](https://hackage.haskell.org/package/monad-par-0.3.4.8)
- [monad-par-extras-0.3.3](https://hackage.haskell.org/package/monad-par-extras-0.3.3)
- [monoid-absorbing-0.1.0.4](https://hackage.haskell.org/package/monoid-absorbing-0.1.0.4)
- [monad-control >= 1.0.1.0 && <= 1.0.2.1](https://hackage.haskell.org/package/monad-control)
- [monad-loops-0.4.3](https://hackage.haskell.org/package/monad-loops-0.4.3)
- [MonadRandom >= 0.4.2.3 && <= 0.5.1](https://hackage.haskell.org/package/MonadRandom)
- [monads-tf-0.1.0.3](https://hackage.haskell.org/package/monads-tf-0.1.0.3)
- [mono-traversable-1.0.1](https://hackage.haskell.org/package/mono-traversable-1.0.1)
- [mtl-2.2.1](https://hackage.haskell.org/package/mtl-2.2.1)
- [mtl-compat-0.2.1.3](https://hackage.haskell.org/package/mtl-compat-0.2.1.3)
- [multiset-0.3.3](https://hackage.haskell.org/package/multiset-0.3.3)
- [mwc-random >= 0.13.5.0 && <= 0.13.6.0](https://hackage.haskell.org/package/mwc-random)
- [nats-1.1.1](https://hackage.haskell.org/package/nats-1.1.1)
- [nested-sequence-0.2](https://hackage.haskell.org/package/nested-sequence-0.2)
- [nested-sets-0.0.1.1](https://hackage.haskell.org/package/nested-sets-0.0.1.1)
- [network-2.6.3.2](https://hackage.haskell.org/package/network-2.6.3.2)
- [network-uri-2.6.1.0](https://hackage.haskell.org/package/network-uri-2.6.1.0)
- [newtype-0.2](https://hackage.haskell.org/package/newtype-0.2)
- [newtype-generics-0.4.1](https://hackage.haskell.org/package/newtype-generics-0.4.1)
- [NumInstances-1.4](https://hackage.haskell.org/package/NumInstances-1.4)
- [old-locale-1.0.0.7](https://hackage.haskell.org/package/old-locale-1.0.0.7)
- [old-time-1.1.0.3](https://hackage.haskell.org/package/old-time-1.1.0.3)
- [Only-0.1](https://hackage.haskell.org/package/Only-0.1)
- [operational-0.2.3.5](https://hackage.haskell.org/package/operational-0.2.3.5)
- [optparse-applicative >= 0.13.2.0 && <= 0.14.0.0](https://hackage.haskell.org/package/optparse-applicative)
- [parallel-3.2.1.0](https://hackage.haskell.org/package/parallel-3.2.1.0)
- [parsec-3.1.11](https://hackage.haskell.org/package/parsec-3.1.11)
- [patch-combinators-0.2.2](https://hackage.haskell.org/package/patch-combinators-0.2.2)
- [path-pieces-0.2.1](https://hackage.haskell.org/package/path-pieces-0.2.1)
- [pipes >= 4.3.1 && <= 4.3.2](https://hackage.haskell.org/package/pipes-4.3.2)
- [pipes-4.3.1](https://hackage.haskell.org/package/pipes-4.3.1)
- [pipes-bytestring-2.1.4](https://hackage.haskell.org/package/pipes-bytestring-2.1.4)
- [pipes-concurrency-2.0.7](https://hackage.haskell.org/package/pipes-concurrency-2.0.7)
- [pipes-group-1.0.6](https://hackage.haskell.org/package/pipes-group-1.0.6)
- [pipes-parse-3.0.8](https://hackage.haskell.org/package/pipes-parse-3.0.8)
- [pipes-safe-2.2.5](https://hackage.haskell.org/package/pipes-safe-2.2.5)
- [pointed-5](https://hackage.haskell.org/package/pointed-5)
- [polyparse-1.12](https://hackage.haskell.org/package/polyparse-1.12)
- [pqueue-1.3.1.1](https://hackage.haskell.org/package/pqueue-1.3.1.1)
- [psqueues-0.2.2.3](https://hackage.haskell.org/package/psqueues-0.2.2.3)
- [pqueue-1.3.1.1](https://hackage.haskell.org/package/pqueue-1.3.1.1)
- [PSQueue-1.1](https://hackage.haskell.org/package/PSQueue-1.1)
- [prelude-extras-0.4.0.3](https://hackage.haskell.org/package/prelude-extras-0.4.0.3)
- [pretty-1.1.3.4](https://hackage.haskell.org/package/pretty-1.1.3.4)
- [prettyclass-1.0.0.0](https://hackage.haskell.org/package/prettyclass-1.0.0.0)
- [primes-0.2.1.0](https://hackage.haskell.org/package/primes-0.2.1.0)
- [primitive-0.6.2.0](https://hackage.haskell.org/package/primitive-0.6.2.0)
- [process-1.6.2.0](https://hackage.haskell.org/package/process-1.6.2.0)
- [profunctors-5.2](https://hackage.haskell.org/package/profunctors-5.2)
- [protolude-0.1.10](https://hackage.haskell.org/package/protolude-0.1.10)
- [pure-zlib-0.6](https://hackage.haskell.org/package/pure-zlib-0.6)
- [queue-0.1.2](https://hackage.haskell.org/package/queue-0.1.2)
- [QuickCheck >= 2.7.6 && <= 2.10.0.1](https://hackage.haskell.org/package/QuickCheck)
- [quickcheck-io-0.1.4](https://hackage.haskell.org/package/quickcheck-io-0.1.4)
- [ReadArgs-1.2.3](https://hackage.haskell.org/package/ReadArgs-1.2.3)
- [readable-0.3.1](https://hackage.haskell.org/package/readable-0.3.1)
- [reflection-2.1.2](https://hackage.haskell.org/package/reflection-2.1.2)
- [regex-base-0.93.2](https://hackage.haskell.org/package/regex-base-0.93.2)
- [regex-posix-0.95.2](https://hackage.haskell.org/package/regex-posix-0.95.2)
- [regex-tdfa-1.2.2](https://hackage.haskell.org/package/regex-tdfa-1.2.2)
- [relacion-0.1](https://hackage.haskell.org/package/relacion-0.1)
- [repa-3.4.1.2](https://hackage.haskell.org/package/repa-3.4.1.2)
- [repa-algorithms-3.4.1.1](https://hackage.haskell.org/package/repa-algorithms-3.4.1.1)
- [repa-eval-4.0.0.2](https://hackage.haskell.org/package/repa-eval-4.0.0.2)
- [repa-io-3.4.1.1](https://hackage.haskell.org/package/repa-io-3.4.1.1)
- [resourcet >= 1.1.8.1 && <= 1.1.9](https://hackage.haskell.org/package/resourcet-1.1.9)
- [randproc-0.4](https://hackage.haskell.org/package/randproc-0.4)
- [safe >= 0.3.9 && <= 0.3.11](https://hackage.haskell.org/package/safe-0.3.11)
- [selda-0.1.9.0](https://hackage.haskell.org/package/selda-0.1.9.0)
- [servant >= 0.9.1.1 && <= 0.11](https://hackage.haskell.org/package/servant)
- [servant-docs >= 0.9.1.1 && <= 0.11](https://hackage.haskell.org/package/servant-docs)
- [servant-server >= 0.9.1.1 && <= 0.11](https://hackage.haskell.org/package/servant-server)
- [setenv-0.1.1.3](https://hackage.haskell.org/package/setenv-0.1.1.3)
- [scientific >= 0.3.4.9 && <= 0.3.5.1](https://hackage.haskell.org/package/scientific)
- [semigroups >= 0.18.2 && <= 0.18.3](https://hackage.haskell.org/package/semigroups)
- [semigroupoids >= 5.1 && <= 5.2.1](https://hackage.haskell.org/package/semigroupoids)
- [silently-1.2.5](https://hackage.haskell.org/package/silently-1.2.5)
- [singletons-2.0.1](https://hackage.haskell.org/package/singletons-2.0.1)
- [split >= 0.2.3.1 && <= 0.2.3.2](https://hackage.haskell.org/package/split-0.2.3.2)
- [stable-marriage-0.1.1.0](https://hackage.haskell.org/package/stable-marriage-0.1.1.0)
- [StateVar-1.1.0.4](https://hackage.haskell.org/package/StateVar-1.1.0.4)
- [statistics-0.14.0.2](https://hackage.haskell.org/package/statistics-0.14.0.2)
- [stm-2.4.4.1](https://hackage.haskell.org/package/stm-2.4.4.1)
- [stm-chans-3.0.0.4](https://hackage.haskell.org/package/stm-chans-3.0.0.4)
- [storable-complex-0.2.2](https://hackage.haskell.org/package/storable-complex-0.2.2)
- [streaming-commons-0.1.18](https://hackage.haskell.org/package/streaming-commons-0.1.18)
- [string-conversions-0.4.0.1](https://hackage.haskell.org/package/string-conversions-0.4.0.1)
- [stringsearch-0.3.6.6](https://hackage.haskell.org/package/stringsearch-0.3.6.6)
- [svg-builder-0.1.0.2](https://hackage.haskell.org/package/svg-builder-0.1.0.2)
- [syb >= 0.6 && <= 0.7](https://hackage.haskell.org/package/syb)
- [system-filepath-0.4.13.4](https://hackage.haskell.org/package/system-filepath-0.4.13.4)
- [tagged-0.8.5](https://hackage.haskell.org/package/tagged-0.8.5)
- [tagsoup-0.14](https://hackage.haskell.org/package/tagsoup-0.14)
- [tasty >= 0.11.2.1 && <= 0.11.2.5](https://hackage.haskell.org/package/tasty-0.11.2.5)
- [template-haskell-2.11.0.0](https://hackage.haskell.org/package/template-haskell-2.11.0.0)
- [terminal-size-0.3.2.1](https://hackage.haskell.org/package/terminal-size-0.3.2.1)
- [testpack-2.1.3.0](https://hackage.haskell.org/package/testpack-2.1.3.0)
- [text >= 1.2.2.1 && <= 1.2.2.2](https://hackage.haskell.org/package/text)
- [tf-random-0.5](https://hackage.haskell.org/package/tf-random-0.5)
- [time >= 1.6.0.1 <= 1.8.0.3](https://hackage.haskell.org/package/time)
- [time-locale-compat-0.1.1.3](https://hackage.haskell.org/package/time-locale-compat-0.1.1.3)
- [transformers >= 0.5.2.0 && <= 0.5.4.0](https://hackage.haskell.org/package/transformers)
- [transformers-base-0.4.4](http://hackage.haskell.org/package/transformers-base-0.4.4)
- [transformers-compat-0.5.1.4](http://hackage.haskell.org/package/transformers-compat-0.5.1.4)
- [tree-fun-0.8.1.0](https://hackage.haskell.org/package/tree-fun-0.8.1.0)
- [type-level-sets-0.8.0.0](https://hackage.haskell.org/package/type-level-sets-0.8.0.0)
- [unexceptionalio-0.3.0](https://hackage.haskell.org/package/unexceptionalio-0.3.0)
- [unification-fd-0.10.0.1](https://hackage.haskell.org/package/unification-fd-0.10.0.1)
- [union-find-0.2](https://hackage.haskell.org/package/union-find-0.2)
- [union-find-array-0.1.0.2](https://hackage.haskell.org/package/union-find-array-0.1.0.2)
- [unicode-transforms-0.2.0](https://hackage.haskell.org/packages/unicode-transforms-0.2.0)
- [unordered-containers >= 0.2.7.1 && <= 0.2.8.0](https://hackage.haskell.org/package/unordered-containers)
- [uri-bytestring >= 0.2.2.1 && <= 0.3.0.1](https://hackage.haskell.org/package/uri-bytestring)
- [utf8-string-1.0.1.1](https://hackage.haskell.org/package/utf8-string-1.0.1.1)
- [uuid-types-1.0.3](https://hackage.haskell.org/package/uuid-types-1.0.3)
- [vault-0.3.0.6](https://hackage.haskell.org/package/vault-0.3.0.6)
- [vector >= 0.11.0.0 && <= 0.12.0.1](https://hackage.haskell.org/package/vector)
- [vector-algorithms-0.7.0.1](https://hackage.haskell.org/package/vector-algorithms-0.7.0.1)
- [vector-binary-instances >= 0.2.3.3 && <= 0.2.3.5](https://hackage.haskell.org/package/vector-binary-instances)
- [vector-th-unbox-0.2.1.6](https://hackage.haskell.org/package/vector-th-unbox-0.2.1.6)
- [versions-3.0.1.1](https://hackage.haskell.org/package/versions-3.0.1.1)
- [vinyl-0.5.1.1](https://hackage.haskell.org/package/vinyl-0.5.1.1)
- [void-0.7.1](https://hackage.haskell.org/package/void-0.7.1)
- [wai-3.2.1.1](https://hackage.haskell.org/package/wai-3.2.1.1)
- [word8 >= 0.1.2 && <= 0.1.3](https://hackage.haskell.org/package/word8)
- [xhtml-3000.2.1](https://hackage.haskell.org/package/xhtml-3000.2.1)
- [xml-1.3.5](https://hackage.haskell.org/package/xml-1.3.5)
- [xml-types-0.3.6](https://hackage.haskell.org/package/xml-types-0.3.6)
- [xmlhtml-0.2.3.5](https://hackage.haskell.org/package/xmlhtml-0.2.3.5)
- [zlib-0.6.1.2](https://hackage.haskell.org/package/zlib-0.6.1.2)

### Unsupported Packages

These packages are heavy on FFI dependencies and don't make sense in the context
of the JVM, hence no effort will be made to port them.
- None for now

### Built-in Packages
These packages have special meaning in the Eta compiler and hence are provided
upon installation.
- [base-4.8.2.0](https://hackage.haskell.org/package/base-4.8.2.0)
- [integer-0.5.1.0](https://hackage.haskell.org/package/integer-gmp-0.5.1.0)
  - *NOTE*: Due to the drastic differences between the `Integer` implementations,
            the public API between the corresponding GHC package is slightly
            different.
- [ghc-prim-0.4.0.0](https://hackage.haskell.org/package/ghc-prim-0.4.0.0)
  - *NOTE*: Due to the introduction of new primitives for Eta,
            the public API between the corresponding GHC package is slightly
            different.

## Contributing

### Patching a Library

Suppose that you find a library you want to patch or fix an existing patch, say
`somepackage-0.1.2.3`.

#### Prerequisites

If you have already forked this repository, then skip to step 3.

1. [Fork](https://github.com/typelead/eta-hackage/tree/master/patches#fork-destination-box) this repository.

2. Clone the forked repository.

   ```
   git clone http://github.com/your-name-here/eta-hackage
   ```

3. Checkout the `master` branch and pull any changes.

   ```
   git checkout master
   git pull
   ```

4. Create a new branch with the same name as the package.

   ```
   $ git checkout -b somepackage
   ```

#### Patching

1. Fetch the package and navigate to it.

   ```
   etlas get somepackage-0.1.2.3
   cd somepackage-0.1.2.3/
   ```

2. If the message "Found patch in eta-hackage for somepackage-0.1.2.3" appeared
   in step 1, skip to step 3.

   Otherwise, initialize a Git repository and make an initial commit.

   ```
   git init && git add . && git commit -m "First"
   ```

3. Build the package.

   ```
   etlas build
   ```

4. If the previous version of the package has already been patched, try out:

   ```
   git apply --ignore-space-change --ignore-whitespace --reject [path-to-path]
   ```
   
   This will work in most cases. If not, resolve the changes.
   
5. Otherwise, apply the desired changes and go back to step 3 as long as the build fails.

6. Once the build succeeds, make a commit.

   ```
   git add . && git commit -m "Patched"
   ```

7. Create a patch.

   ```
   git format-patch HEAD~ --stdout > somepackage-0.1.2.3.patch
   ```

8. If you have changed the `.cabal` file of the package in your patch, make a copy
   and rename it from `somepackage.cabal` to `somepackage-0.1.2.3.cabal`.

   ```
   cp somepackage.cabal somepackage-0.1.2.3.cabal
   ```

9. Copy the patch file and the cabal file (if changed) to the `patches` directory
   in your local clone of your fork of the `eta-hackage` repository.
   
   NOTE: `$YOUR_FORK_PATH` should be replaced with the path to the local clone
         of your forked version of `eta-hackage`.

   If the cabal file didn't change:

   ```
   cp somepackage-0.1.2.3.patch $YOUR_FORK_PATH/patches/
   ```

   If the cabal file changes:

   ```
   cp somepackage-0.1.2.3.cabal somepackage-0.1.2.3.patch $YOUR_FORK_PATH/patches/
   ```

10. Update this `README.md` with the package name (in alphabetical order) in the
    **Supported Packages** section in the same format as the other packages.
   
    If there already exists an entry for an older or newer version of the package,
    please consolidate the new versions into the existing entry. You can see the
    `directory` entry as an example.

11. In your `eta-hackage` repository,

    ```
    $ git add .
    $ git commit -m "Patched somepackage-0.1.2.3"
    $ git push origin
    ```

12. Submit a pull request to this repository for review.

#### Applying an Existing Patch

If you want to apply the patch to an old version of a package to a new version of a package, you can run

```
git apply --ignore-space-change --ignore-whitespace --reject [location-of-patch]
```
