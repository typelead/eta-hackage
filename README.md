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
- [active-0.2.0.13](https://hackage.haskell.org/package/active-0.2.0.13)
- [ad-4.3.5](https://hackage.haskell.org/package/ad-4.3.5)
- [adjunctions-4.4](https://hackage.haskell.org/package/adjunctions-4.4)
- [amazonka-1.6.0](https://hackage.haskell.org/package/amazonka-1.6.0)
- [amazonka-core-1.6.0](https://hackage.haskell.org/package/amazonka-core-1.6.0)
- [abstract-deque-0.3](https://hackage.haskell.org/package/abstract-deque-0.3)
- [abstract-par-0.3.3](https://hackage.haskell.org/package/abstract-par-0.3.3)
- [Adaptive-0.1](https://hackage.haskell.org/package/Adaptive-0.1)
- [adjunctions-4.3](https://hackage.haskell.org/package/adjunctions-4.3)
- [aeson == 0.11.0.0 || >= 0.11.3.0 && <= 1.2.3.0](https://hackage.haskell.org/package/aeson)
- [aeson-compat >= 0.3.7.1 && <= 0.3.8](https://hackage.haskell.org/package/aeson-diff-1.1.0.5)
- [aeson-diff-1.1.0.5](https://hackage.haskell.org/package/aeson-diff)
- [aeson-extra-0.4.1.1](https://hackage.haskell.org/package/aeson-extra-0.4.1.1)
- [aeson-filthy-0.1.2](https://hackage.haskell.org/package/aeson-filthy-0.1.2)
- [aeson-generic-compat-0.0.1.2](https://hackage.haskell.org/package/aeson-generic-compat-0.0.1.2)
- [aeson-iproute-0.2](https://hackage.haskell.org/package/aeson-iproute-0.2)
- [aeson-options-0.0.0](https://hackage.haskell.org/package/aeson-options)
- [aeson-picker-0.1.0.4](https://hackage.haskell.org/package/aeson-picker)
- [aeson-pretty >= 0.8.2 && <= 0.8.5](https://hackage.haskell.org/package/aeson-pretty)
- [aeson-quick-0.1.2.1](https://hackage.haskell.org/package/aeson-quick-0.1.2.1)
- [affine-0.1.1.0](https://hackage.haskell.org/package/affine-0.1.1.0)
- [agum >= 2.6 && <= 2.7](https://hackage.haskell.org/package/agum)
- [aig-0.2.6](https://hackage.haskell.org/package/aig-0.2.6)
- [aivika-5.8](https://hackage.haskell.org/package/aivika-5.8)
- [aivika-distributed-1.4](https://hackage.haskell.org/package/aivika-distributed-1.4)
- [alarmclock-0.5.0.2](https://hackage.haskell.org/package/alarmclock-0.5.0.2)
- [alex-3.2.4](https://hackage.haskell.org/package/alex-3.2.4)
- [alex-tools-0.4](https://hackage.haskell.org/package/alex-tools-0.4)
- [alg-0.2.5.0](https://hackage.haskell.org/package/alg-0.2.5.0)
- [algebra-4.3.1](https://hackage.haskell.org/package/algebra-4.3.1)
- [algebraic-graphs-0.1.1.1](https://hackage.haskell.org/package/algebraic-graphs-0.1.1.1)
- [alternators-1.0.0.0](https://hackage.haskell.org/package/alternators-1.0.0.0)
- [amqp-0.18.1](https://hackage.haskell.org/package/amqp-0.18.1)
- [ansi-terminal >= 0.6.2.3 && <= 0.6.3.1 || == 0.7.1.1 || == 0.8.0.4](https://hackage.haskell.org/package/ansi-terminal)
- [ansi-wl-pprint-0.6.7.3](https://hackage.haskell.org/package/ansi-wl-pprint-0.6.7.3)
- [appar-0.1.4](https://hackage.haskell.org/package/appar-0.1.4)
- [array >= 0.5.1.1 && <= 0.5.2.0](https://hackage.haskell.org/package/array)
- [array-utils-0.3](https://hackage.haskell.org/package/array-utils-0.3)
- [async-2.1.1.1](https://hackage.haskell.org/package/async-2.1.1.1)
- [asn1-encoding-0.9.5](https://hackage.haskell.org/package/asn1-encoding-0.9.5)
- [asn1-parse-0.9.4](https://hackage.haskell.org/package/asn1-parse-0.9.4)
- [attoparsec >= 0.13.1.0 && <= 0.13.2.0](https://hackage.haskell.org/package/attoparsec)
- [attoparsec-iso8601-1.0.0.0](https://hackage.haskell.org/package/attoparsec-iso8601-1.0.0.0)
- [auto-update-0.1.4](https://hackage.haskell.org/package/auto-update-0.1.4)
- [authenticate-1.3.4](https://hackage.haskell.org/package/authenticate-1.3.4)
- [authenticate-oauth-1.6](https://hackage.haskell.org/package/authenticate-oauth-1.6)
- [avro-0.3.2.0](https://hackage.haskell.org/package/avro-0.3.2.0)
- [basement >= 0.0.4 && <= 0.0.6](https://hackage.haskell.org/package/basement)
- [base-compat >= 0.9.1 && <= 0.10.4](https://hackage.haskell.org/package/base-compat)
- [base-orphans >= 0.5.4 && <= 0.8](https://hackage.haskell.org/package/base-orphans)
- [base-prelude-1.2.0.1](https://hackage.haskell.org/package/base-prelude-1.2.0.1)
- [base16-bytestring-0.1.1.6](https://hackage.haskell.org/package/base16-bytestring-0.1.1.6)
- [base64-bytestring-1.0.0.1](https://hackage.haskell.org/package/base64-bytestring-1.0.0.1)
- [basic-lens-0.0.0](https://hackage.haskell.org/package/basic-lens-0.0.0)
- [basic-prelude-0.6.1](https://hackage.haskell.org/package/basic-prelude-0.6.1)
- [binary-conduit-1.2.4.1](https://hackage.haskell.org/package/binary-conduit-1.2.4.1)
- [binary-orphans-0.1.8.0](https://hackage.haskell.org/package/binary-orphans-0.1.8.0)
- [binary-search-1.0.0.3](https://hackage.haskell.org/package/binary-search-1.0.0.3)
- [bindings-DSL-1.0.23](https://hackage.haskell.org/package/bindings-DSL-1.0.23)
- [bifunctors >= 5.4.1 && <= 5.5.3](https://hackage.haskell.org/package/bifunctors-5.4.1)
- [bitarray-0.0.1.1](https://hackage.haskell.org/package/bitarray-0.0.1.1)
- [Binpack-0.4.1](https://hackage.haskell.org/package/Binpack-0.4.1)
- [blaze-builder >=0.3.3.4 && <= 0.4.0.2](https://hackage.haskell.org/package/blaze-builder)
- [blaze-html >= 0.8.1.3 && <= 0.9.0.1](https://hackage.haskell.org/package/blaze-html)
- [blaze-html-truncate-0.3.0.0](https://hackage.haskell.org/package/blaze-html-truncate-0.3.0.0)
- [blaze-markup >= 0.7.1.1 && <= 0.8.0.0](https://hackage.haskell.org/package/blaze-markup)
- [blaze-textual-0.2.1.0](https://hackage.haskell.org/package/blaze-textual-0.2.1.0)
- [bmp-1.2.6.3](https://hackage.haskell.org/package/bmp-1.2.6.3)
- [Boolean >= 0.2.3 && <= 0.2.4](https://hackage.haskell.org/package/Boolean)
- [byteable-0.1.1](https://hackage.haskell.org/package/byteable-0.1.1)
- [byteorder-1.0.4](https://hackage.haskell.org/package/byteorder-1.0.4)
- [bytes-0.15.3](https://hackage.haskell.org/package/bytes-0.15.3)
- [bytestring >= 0.10.8.1.0 && <= 0.10.8.2.0](https://hackage.haskell.org/package/bytestring)
- [bytestring-builder-0.10.8.1.0](https://hackage.haskell.org/package/bytestring-builder-0.10.8.1.0)
- [bytestring-trie-0.2.4.1](https://hackage.haskell.org/package/bytestring-trie-0.2.4.1)
- [bytestring-show-0.3.5.6](https://hackage.haskell.org/package/bytestring-show-0.3.5.6)
- [call-stack-0.1.0](https://hackage.haskell.org/package/call-stack-0.1.0)
- [carray-0.1.6.6](https://hackage.haskell.org/package/carray-0.1.6.6)
- [case-insensitive >= 1.2.0.7 && <= 1.2.0.10](https://hackage.haskell.org/package/case-insensitive)
- [cassava >= 0.4.5.1 && <= 0.5.1.0](https://hackage.haskell.org/package/cassava)
- [cereal-0.5.4.0](https://hackage.haskell.org/package/cereal-0.5.4.0)
- [circle-packing-0.1.0.6](https://hackage.haskell.org/package/circle-packing-0.1.0.6)
- [chunked-data-0.3.0](https://hackage.haskell.org/package/chunked-data-0.3.0)
- [clock-0.7.2](https://hackage.haskell.org/package/clock-0.7.2)
- [cmdargs >= 0.10.18 && <= 0.10.20](https://hackage.haskell.org/package/cmdargs-0.10.19)
- [code-page-0.1.3](https://hackage.haskell.org/package/code-page-0.1.3)
- [colour-2.3.3](https://hackage.haskell.org/package/colour-2.3.3)
- [comonad-5](https://hackage.haskell.org/package/comonad-5)
- [conceit-0.4.0.0](https://hackage.haskell.org/package/conceit-0.4.0.0)
- [concurrent-machines-0.2.3.3](https://hackage.haskell.org/package/concurrent-machines-0.2.3.3)
- [concurrent-output-1.10.6](https://hackage.haskell.org/package/concurrent-output-1.10.6)
- [conduit >= 1.2.8 && <= 1.3.0.3](https://hackage.haskell.org/package/conduit)
- [conduit-combinators-1.1.2](https://hackage.haskell.org/package/conduit-combinators-1.1.2)
- [conduit-extra-1.2.1](https://hackage.haskell.org/package/conduit-extra-1.2.1)
- [cookie >= 0.4.2.1 && <= 0.4.3](https://hackage.haskell.org/package/cookie)
- [connection-0.2.8](https://hackage.haskell.org/package/connection-0.2.8)
- [constraints-0.8](https://hackage.haskell.org/package/constraints-0.8)
- [containers >= 0.5.8.1 && <= 0.5.9.1 ](https://hackage.haskell.org/package/containers-0.5.9.1)
- [contravariant >= 1.33 && <= 1.4](https://hackage.haskell.org/package/contravariant)
- [control-monad-omega-0.3.1](https://hackage.haskell.org/package/control-monad-omega-0.3.1)
- [convertible-1.1.1.0](https://hackage.haskell.org/package/convertible-1.1.1.0)
- [crackNum-1.9](https://hackage.haskell.org/package/crackNum-1.9)
- [crdt >= 9.3 && <= 10.0](https://hackage.haskell.org/package/crdt)
- [crypto-api-0.13.2](https://hackage.haskell.org/package/crypto-api-0.13.2)
- [crypto-pubkey-types-0.4.3](https://hackage.haskell.org/package/crypto-pubkey-types-0.4.3)
- [cryptonite-0.24](https://hackage.haskell.org/package/cryptonite-0.24)
- [cryptohash-md5-0.11.100.1](http://hackage.haskell.org/package/cryptohash-md5)
- [cryptohash-sha1-0.11.100.1](http://hackage.haskell.org/package/cryptohash-sha1)
- [data-binary-ieee754-0.4.4](https://hackage.haskell.org/package/data-binary-ieee754-0.4.4)
- [data-default-0.7.1.1](https://hackage.haskell.org/package/data-default-0.7.1.1)
- [data-default-class-0.1.2.0](https://hackage.haskell.org/package/data-default-class-0.1.2.0)
- [data-default-instances-containers-0.0.1](https://hackage.haskell.org/package/data-default-instances-containers-0.0.1)
- [data-default-instances-dlist-0.0.1](https://hackage.haskell.org/package/data-default-instances-dlist-0.0.1)
- [data-default-instances-old-locale-0.0.1](https://hackage.haskell.org/package/data-default-instances-old-locale-0.0.1)
- [data-reify-0.6.1](https://hackage.haskell.org/package/data-reify-0.6.1)
- [DecisionTree-0.0](https://hackage.haskell.org/package/DecisionTree-0.0)
- [deepseq >= 1.4.2.0 && <= 1.4.3.0](https://hackage.haskell.org/package/deepseq-1.4.2.0)
- [deepseq-generics-0.2.0.0](https://hackage.haskell.org/package/deepseq-generics-0.2.0.0)
- [dhall >= 1.14.0](https://hackage.haskell.org/package/dhall-1.14.0)
- [dhall-json >= 1.2.1](https://hackage.haskell.org/package/dhall-json-1.2.1)
- [diagrams-solve-0.1.1](https://hackage.haskell.org/package/diagrams-solve-0.1.1)
- [digest 0.0.1.2](https://hackage.haskell.org/package/digest)
- [directory >= 1.3.0.0 && <= 1.3.1.0](https://hackage.haskell.org/package/directory)
- [directory-tree-0.12.1](https://hackage.haskell.org/package/directory-tree-0.12.1)
- [disjoint-sets-st-0.1](https://hackage.haskell.org/package/disjoint-sets-st-0.1)
- [distributive >= 0.5.0.2 && <= 0.5.3](https://hackage.haskell.org/package/distributive)
- [dlist >= 0.8.0.2 && <= 0.8.0.3](https://hackage.haskell.org/package/dlist)
- [dom-lt-0.1.3](https://hackage.haskell.org/package/dom-lt-0.1.3)
- [dual-tree-0.2.1](https://hackage.haskell.org/package/dual-tree-0.2.1)
- [easy-file-0.2.1](https://hackage.haskell.org/package/easy-file-0.2.1)
- [either-4.4.1.1](https://hackage.haskell.org/package/either-4.4.1.1)
- [entropy-0.4](https://hackage.haskell.org/package/entropy)
- [erf-2.0.0.0](https://hackage.haskell.org/package/erf-2.0.0.0)
- [errors >= 2.1.3 && <= 2.2.2](https://hackage.haskell.org/package/errors)
- [exceptions-0.8.3](https://hackage.haskell.org/package/exceptions-0.8.3)
- [extra >= 1.5.2 && <= 1.6.3](https://hackage.haskell.org/package/extra)
- [fail-4.9.0.0](https://hackage.haskell.org/package/fail-4.9.0.0)
- [fast-logger >= 2.4.10 && <= 2.4.11](https://hackage.haskell.org/package/fast-logger)
- [fast-math-1.0.2](https://hackage.haskell.org/package/fast-math-1.0.2)
- [fibonacci-0.2.0.1](https://hackage.haskell.org/package/fibonacci-0.2.0.1)
- [fingertree-0.1.1.0](https://hackage.haskell.org/package/fingertree-0.1.1.0)
- [fingertree-psqueue-0.3](https://hackage.haskell.org/package/fingertree-psqueue-0.3)
- [fingertree-tf-0.1.0.0](https://hackage.haskell.org/package/fingertree-tf-0.1.0.0)
- [filepath >= 1.4.1.0 && <= 1.4.1.1](hackage.haskell.org/package/filepath)
- [file-embed >= 0.0.10.1 && <= 0.0.11](hackage.haskell.org/package/file-embed)
- [fgl-5.5.3.0](https://hackage.haskell.org/package/fgl-5.5.3.0)
- [fmlist-0.9](https://hackage.haskell.org/package/fmlist-0.9)
- [formatting >= 6.3.2 && <= 6.3.4](https://hackage.haskell.org/package/formatting-6.3.4)
- [foldl-1.2.5](https://hackage.haskell.org/package/foldl-1.2.5)
- [foundation >= 0.0.17 && <= 0.0.18](https://hackage.haskell.org/package/foundation)
- [free-4.12.4](https://hackage.haskell.org/package/free-4.12.4)
- [fsnotify-0.3.0.1](https://hackage.haskell.org/package/fsnotify-0.3.0.1)
- [FloatingHex-0.4](https://hackage.haskell.org/package/FloatingHex-0.4)
- [garsia-wachs-1.2](https://hackage.haskell.org/package/garsia-wachs-1.2)
- [generic-deriving >= 1.10.7 && <= 1.11.2](https://hackage.haskell.org/package/generic-deriving-1.11.2)
- [GenericPretty-1.2.1](https://hackage.haskell.org/package/GenericPretty-1.2.1)
- [gdiff-1.1](https://hackage.haskell.org/package/gdiff-1.1)
- [geniplate-mirror-0.7.6](https://hackage.haskell.org/package/geniplate-mirror-0.7.6)
- [gray-code-0.3.1](https://hackage.haskell.org/package/gray-code-0.3.1)
- [ghc-boot-th-8.0.1](https://hackage.haskell.org/package/ghc-boot-th-8.0.1)
- [Glob >= 0.7.14 && <= 0.9.2](https://hackage.haskell.org/package/Glob)
- [graph-matchings-0.1.0.0](https://hackage.haskell.org/package/graph-matchings-0.1.0.0)
- [graph-wrapper-0.2.5.1](https://hackage.haskell.org/package/graph-wrapper-0.2.5.1)
- [groups-0.4.0.0](https://hackage.haskell.org/package/groups-0.4.0.0)
- [half-0.3](https://hackage.haskell.org/package/half-0.3)
- [happy-1.19.9](https://hackage.haskell.org/package/happy-1.19.9)
- [haskeline-0.7.4.3](https://hackage.haskell.org/package/haskeline-0.7.4.3)
- [hashable >= 1.2.4.0 && <= 1.2.7.0](https://hackage.haskell.org/package/hashable)
- [hashtables >= 1.2.1.0 && <= 1.2.3.1](https://hackage.haskell.org/package/hashtables)
- [haxl >= 2.0.0.0 && <= 2.0.1.0](http://hackage.haskell.org/package/haxl)
- [heap-1.0.3](https://hackage.haskell.org/package/heap-1.0.3)
- [heaps-0.3.3](https://hackage.haskell.org/package/heaps-0.3.3)
- [hedgehog >= 0.5.3 && <= 0.6](https://hackage.haskell.org/package/hedgehog-0.6)
- [heist-1.0.1.1](https://hackage.haskell.org/package/heist-1.0.1.1)
- [hetero-map-0.21](https://hackage.haskell.org/package/hetero-map-0.21)
- [hex-0.1.2](https://hackage.haskell.org/package/hex-0.1.2)
- [hgal-2.0.0.2](https://hackage.haskell.org/package/hgal-2.0.0.2)
- [hostname-1.0](https://hackage.haskell.org/package/hostname-1.0)
- [hourglass >= 0.2.10 && <= 0.2.11](https://hackage.haskell.org/package/hourglass)
- [hslogger-1.2.10](https://hackage.haskell.org/package/hslogger-1.2.10)
- [hspec-2.4.3](https://hackage.haskell.org/package/hspec-2.4.3)
- [hspec-core >= 2.4.3 && <= 2.4.6](https://hackage.haskell.org/package/hspec-core-2.4.6)
- [hspec-discover-2.4.3](https://hackage.haskell.org/package/hspec-discover-2.4.3)
- [hspec-expectations-0.8.2](https://hackage.haskell.org/package/hspec-expectations-0.8.2)
- [html-truncate-0.3.0.0](https://hackage.haskell.org/package/html-truncate-0.3.0.0)
- [http2 >= 1.6.2 && <= 1.6.3](https://hackage.haskell.org/package/http2)
- [http-api-data >= 0.3.5 && <= 0.3.7.1](https://hackage.haskell.org/package/http-api-data)
- [http-client >= 0.5.11 && <= 0.5.13.1](https://hackage.haskell.org/package/http-client-0.5.13)
- [http-client-tls-0.3.5.1](https://hackage.haskell.org/package/http-client-tls-0.3.5.1)
- [http-conduit-2.3.2](https://hackage.haskell.org/package/http-conduit-2.3.2)
- [http-date-0.0.6.1](https://hackage.haskell.org/package/http-date-0.0.6.1)
- [http-media-0.6.4](https://hackage.haskell.org/package/http-media-0.6.4)
- [http-types >= 0.9.1 && <= 0.12.1](https://hackage.haskell.org/package/http-types)
- [HTTP-4000.3.8](https://hackage.haskell.org/package/HTTP-4000.3.8)
- [HoleyMonoid-0.1.1](https://hackage.haskell.org/package/HoleyMonoid-0.1.1)
- [HDBC-2.4.0.1](https://hackage.haskell.org/package/HDBC-2.4.0.1)
- [HUnit >= 1.2.5.2 && <= 1.6.0.0](https://hackage.haskell.org/package/HUnit)
- [ieee754-0.8.0](https://hackage.haskell.org/package/ieee754-0.8.0)
- [integer-logarithms-1.0.2](https://hackage.haskell.org/package/integer-logarithms-1.0.2)
- [integration-0.2.1](https://hackage.haskell.org/package/integration-0.2.1)
- [interval-1.20160821](https://hackage.haskell.org/package/interval-1.20160821)
- [invariant-0.5](https://hackage.haskell.org/package/invariant-0.5)
- [iproute-1.7.1](https://hackage.haskell.org/package/iproute-1.7.1)
- [iso8601-time-0.1.4](https://hackage.haskell.org/package/iso8601-time-0.1.4)
- [ix-shapable-0.1.0](https://hackage.haskell.org/package/ix-shapable-0.1.0)
- [JuicyPixels-3.2.9.5](http://hackage.haskell.org/package/JuicyPixels)
- [js-flot-0.8.3](https://hackage.haskell.org/package/js-flot-0.8.3)
- [js-jquery >= 3.1.1 && <= 3.2.1](https://hackage.haskell.org/package/js-jquery)
- [kan-extensions-5.0.1](https://hackage.haskell.org/package/kan-extensions-5.0.1)
- [keys-3.11](https://hackage.haskell.org/package/keys-3.11)
- [lazy-io-0.1.0](https://hackage.haskell.org/package/lazy-io-0.1.0)
- [labeled-tree-1.0.0.0](https://hackage.haskell.org/package/language-glsl-0.2.1)
- [language-glsl-0.2.1](https://hackage.haskell.org/package/lazy-io-0.1.0)
- [lens >= 4.15.1 && <= 4.17](https://hackage.haskell.org/package/lens)
- [lens-aeson-1.0.2](https://hackage.haskell.org/package/lens-aeson-1.0.2)
- [lens-family-1.2.1](https://hackage.haskell.org/package/lens-family-1.2.1)
- [lens-family-core-1.2.1](https://hackage.haskell.org/package/lens-family-core-1.2.1)
- [lens-labels-0.2.0.1](http://hackage.haskell.org/package/lens-labels)
- [lca-0.3](https://hackage.haskell.org/package/lca-0.3)
- [lifted-async-0.9.0](https://hackage.haskell.org/package/lifted-async-0.9.0)
- [lifted-base >= 0.2.3.8 && <= 0.2.3.11](https://hackage.haskell.org/package/lifted-base)
- [linear-1.20.6](https://hackage.haskell.org/package/linear-1.20.6)
- [list-extras-0.4.1.4](https://hackage.haskell.org/package/list-extras-0.4.1.4)
- [log-domain-0.12](https://hackage.haskell.org/package/log-domain-0.12)
- [logict-0.6.0.2](https://hackage.haskell.org/package/logict-0.6.0.2)
- [loop-0.3.0](https://hackage.haskell.org/package/loop-0.3.0)
- [machines-0.6.1](https://hackage.haskell.org/package/machines-0.6.1)
- [machines-binary-0.3.0.3](https://hackage.haskell.org/package/machines-binary-0.3.0.3)
- [machines-io-0.2.0.13](https://hackage.haskell.org/package/machines-io-0.2.0.13)
- [math-functions-0.2.1.0](https://hackage.haskell.org/package/math-functions-0.2.1.0)
- [matrix-0.3.5.0](https://hackage.haskell.org/package/matrix-0.3.5.0)
- [map-syntax >= 0.2.0.1 && <= 0.2.0.2](https://hackage.haskell.org/package/map-syntax)
- [meldable-heap-2.0.3](https://hackage.haskell.org/package/meldable-heap-2.0.3)
- [memory >= 0.14.10 && <= 0.14.14](https://hackage.haskell.org/package/memory)
- [MemoTrie-0.6.8](https://hackage.haskell.org/package/MemoTrie-0.6.8)
- [mfsolve-0.3.2.0](https://hackage.haskell.org/package/mfsolve-0.3.2.0)
- [microlens >= 0.4.7.0 && <= 0.4.8.0](https://hackage.haskell.org/package/microlens)
- [microlens-mtl >= 0.1.10.0 && <= 0.1.11.0](http://hackage.haskell.org/package/microlens-mtl)
- [microstache-1.0.1.1](http://hackage.haskell.org/package/microstache-1.0.1.1)
- [mime-types-0.1.0.7](http://hackage.haskell.org/package/mime-types-0.1.0.7)
- [MissingH-1.4.0.1](https://hackage.haskell.org/package/MissingH)
- [mmorph >= 1.0.6 && <= 1.1.0](https://hackage.haskell.org/package/mmorph)
- [monad-coroutine-0.9.0.3](https://hackage.haskell.org/package/monad-coroutine-0.9.0.3)
- [monad-par-0.3.4.8](https://hackage.haskell.org/package/monad-par-0.3.4.8)
- [monad-par-extras-0.3.3](https://hackage.haskell.org/package/monad-par-extras-0.3.3)
- [monad-parallel-0.7.2.2](https://hackage.haskell.org/package/monad-parallel-0.7.2.2)
- [monoid-absorbing-0.1.0.4](https://hackage.haskell.org/package/monoid-absorbing-0.1.0.4)
- [monad-control >= 1.0.1.0 && <= 1.0.2.2](https://hackage.haskell.org/package/monad-control)
- [monad-logger >= 0.3.26 && <= 0.3.28](https://hackage.haskell.org/package/monad-logger)
- [monad-loops-0.4.3](https://hackage.haskell.org/package/monad-loops-0.4.3)
- [MonadRandom >= 0.4.2.3 && <= 0.5.1](https://hackage.haskell.org/package/MonadRandom)
- [monads-tf-0.1.0.3](https://hackage.haskell.org/package/monads-tf-0.1.0.3)
- [mono-traversable-1.0.1](https://hackage.haskell.org/package/mono-traversable-1.0.1)
- [mtl-2.2.1](https://hackage.haskell.org/package/mtl-2.2.1)
- [mtl-compat-0.2.1.3](https://hackage.haskell.org/package/mtl-compat-0.2.1.3)
- [multiset-0.3.3](https://hackage.haskell.org/package/multiset-0.3.3)
- [mwc-random >= 0.13.5.0 && <= 0.13.6.0](https://hackage.haskell.org/package/mwc-random)
- [nats-1.1.1](https://hackage.haskell.org/package/nats-1.1.1)
- [named-0.2.0.0](https://hackage.haskell.org/package/named-0.2.0.0)
- [nested-sequence-0.2](https://hackage.haskell.org/package/nested-sequence-0.2)
- [nested-sets-0.0.1.1](https://hackage.haskell.org/package/nested-sets-0.0.1.1)
- [network-2.6.3.2](https://hackage.haskell.org/package/network-2.6.3.2)
- [network-info-0.2.0.10](https://hackage.haskell.org/package/network-info)
- [network-transport-0.5.2](https://hackage.haskell.org/package/network-transport-0.5.2)
- [network-uri >= 2.6.0.2 && <= 2.6.1.0](https://hackage.haskell.org/package/network-uri)
- [newtype-0.2](https://hackage.haskell.org/package/newtype-0.2)
- [newtype-generics-0.4.1](https://hackage.haskell.org/package/newtype-generics-0.4.1)
- [NumInstances-1.4](https://hackage.haskell.org/package/NumInstances-1.4)
- [old-locale-1.0.0.7](https://hackage.haskell.org/package/old-locale-1.0.0.7)
- [old-time-1.1.0.3](https://hackage.haskell.org/package/old-time-1.1.0.3)
- [OneTuple-0.2.1](https://hackage.haskell.org/package/OneTuple-0.2.1)
- [Only-0.1](https://hackage.haskell.org/package/Only-0.1)
- [operational-0.2.3.5](https://hackage.haskell.org/package/operational-0.2.3.5)
- [optparse-applicative >= 0.13.2.0 && <= 0.14.0.0 || == 0.14.0.2](https://hackage.haskell.org/package/optparse-applicative)
- [parallel-3.2.1.0](https://hackage.haskell.org/package/parallel-3.2.1.0)
- [parsec-3.1.11](https://hackage.haskell.org/package/parsec-3.1.11)
- [parsec-3.1.13.0](https://hackage.haskell.org/package/parsec-3.1.13.0)
- [patch-combinators-0.2.2](https://hackage.haskell.org/package/patch-combinators-0.2.2)
- [path-pieces-0.2.1](https://hackage.haskell.org/package/path-pieces-0.2.1)
- [pem-0.2.2](https://hackage.haskell.org/package/pipes-0.2.2)
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
- [pretty >= 1.1.3.4 && <= 1.1.3.6](https://hackage.haskell.org/package/pretty-1.1.3.4)
- [pretty-show >= 1.6.16 && <= 1.7](https://hackage.haskell.org/package/pretty-show)
- [prettyclass-1.0.0.0](https://hackage.haskell.org/package/prettyclass-1.0.0.0)
- [primes-0.2.1.0](https://hackage.haskell.org/package/primes-0.2.1.0)
- [primitive >= 0.6.1.0 && <= 0.6.4.0](https://hackage.haskell.org/package/primitive-0.6.4.0)
- [process-1.6.2.0](https://hackage.haskell.org/package/process-1.6.2.0)
- [profunctors-5.2](https://hackage.haskell.org/package/profunctors-5.2)
- [protolude >= 0.1.10 && <= 0.2.2](https://hackage.haskell.org/package/protolude)
- [proto-lens-0.3.1.0](https://hackage.haskell.org/package/proto-lens)
- [pure-zlib-0.6](https://hackage.haskell.org/package/pure-zlib-0.6)
- [pureMD5-2.1.3](https://hackage.haskell.org/package/pureMD5-2.1.3)
- [purescript-0.12.0](https://hackage.haskell.org/package/purescript-0.12.0)
- [queue-0.1.2](https://hackage.haskell.org/package/queue-0.1.2)
- [QuickCheck >= 2.7.6 && <= 2.10.0.1](https://hackage.haskell.org/package/QuickCheck)
- [quickcheck-io-0.1.4](https://hackage.haskell.org/package/quickcheck-io-0.1.4)
- [ReadArgs-1.2.3](https://hackage.haskell.org/package/ReadArgs-1.2.3)
- [readable-0.3.1](https://hackage.haskell.org/package/readable-0.3.1)
- [reflection-2.1.2](https://hackage.haskell.org/package/reflection-2.1.2)
- [regex-base-0.93.2](https://hackage.haskell.org/package/regex-base-0.93.2)
- [regex-posix-0.95.2](https://hackage.haskell.org/package/regex-posix-0.95.2)
- [regex-tdfa >= 1.2.1 && <= 1.2.3.1](https://hackage.haskell.org/package/regex-tdfa)
- [relacion-0.1](https://hackage.haskell.org/package/relacion-0.1)
- [repa-3.4.1.2](https://hackage.haskell.org/package/repa-3.4.1.2)
- [repa-algorithms-3.4.1.1](https://hackage.haskell.org/package/repa-algorithms-3.4.1.1)
- [repa-eval-4.0.0.2](https://hackage.haskell.org/package/repa-eval-4.0.0.2)
- [repa-io-3.4.1.1](https://hackage.haskell.org/package/repa-io-3.4.1.1)
- [resourcet >= 1.1.8.1 && <= 1.1.10](https://hackage.haskell.org/package/resourcet)
- [randproc-0.4](https://hackage.haskell.org/package/randproc-0.4)
- [RSA-2.2.0](https://hackage.haskell.org/package/RSA-2.2.0)
- [safe >= 0.3.9 && <= 0.3.11](https://hackage.haskell.org/package/safe-0.3.11)
- [safe-exceptions-0.1.6.0](https://hackage.haskell.org/package/safe-exceptions-0.1.6.0)
- [sbv-6.1](https://hackage.haskell.org/package/sbv-6.1)
- [selda-0.1.9.0](https://hackage.haskell.org/package/selda-0.1.9.0)
- [servant >= 0.9.1.1 && <= 0.11](https://hackage.haskell.org/package/servant)
- [servant-docs >= 0.9.1.1 && <= 0.11](https://hackage.haskell.org/package/servant-docs)
- [servant-server >= 0.9.1.1 && <= 0.12](https://hackage.haskell.org/package/servant-server)
- [setenv-0.1.1.3](https://hackage.haskell.org/package/setenv-0.1.1.3)
- [scientific >= 0.3.4.9 && <= 0.3.5.1](https://hackage.haskell.org/package/scientific)
- [semigroups >= 0.18.2 && <= 0.18.5](https://hackage.haskell.org/package/semigroups)
- [semigroupoids >= 5.1 && <= 5.3.1](https://hackage.haskell.org/package/semigroupoids)
- [shake >= 0.16.2 && <= 0.16.4](https://hackage.haskell.org/package/shake)
- [silently-1.2.5](https://hackage.haskell.org/package/silently-1.2.5)
- [singletons-2.0.1](https://hackage.haskell.org/package/singletons-2.0.1)
- [socks-0.5.5](https://hackage.haskell.org/package/socks-0.5.5)
- [sorted-list-0.2.0.0](https://hackage.haskell.org/package/sorted-list-0.2.0.0)
- [split >= 0.2.3.1 && <= 0.2.3.2](https://hackage.haskell.org/package/split-0.2.3.2)
- [superbuffer-0.3.1.1](https://hackage.haskell.org/package/superbuffer-0.3.1.1)
- [stable-marriage-0.1.1.0](https://hackage.haskell.org/package/stable-marriage-0.1.1.0)
- [statistics-0.14.0.2](https://hackage.haskell.org/package/statistics-0.14.0.2)
- [StateVar-1.1.1.0](https://hackage.haskell.org/package/StateVar-1.1.1.0)
- [stm-2.4.4.1](https://hackage.haskell.org/package/stm-2.4.4.1)
- [stm-chans-3.0.0.4](https://hackage.haskell.org/package/stm-chans-3.0.0.4)
- [storable-complex-0.2.2](https://hackage.haskell.org/package/storable-complex-0.2.2)
- [streaming-commons >= 0.1.18 && <= 0.1.19](https://hackage.haskell.org/package/streaming-commons)
- [string-conversions-0.4.0.1](https://hackage.haskell.org/package/string-conversions-0.4.0.1)
- [stringsearch-0.3.6.6](https://hackage.haskell.org/package/stringsearch-0.3.6.6)
- [svg-builder-0.1.0.2](https://hackage.haskell.org/package/svg-builder-0.1.0.2)
- [syb >= 0.6 && <= 0.7](https://hackage.haskell.org/package/syb)
- [system-filepath-0.4.13.4](https://hackage.haskell.org/package/system-filepath-0.4.13.4)
- [Scotty](https://hackage.haskell.org/package/Scotty)
- [StateVar-1.1.0.4](https://hackage.haskell.org/package/StateVar-1.1.0.4)
- [Spock-0.13.0.0](https://hackage.haskell.org/package/Spock-0.13.0.0)
- [SHA >=1.6.4.2 && <= 1.6.4.4](https://hackage.haskell.org/package/SHA-1.6.4.2)
- [tagged-0.8.5](https://hackage.haskell.org/package/tagged-0.8.5)
- [tagsoup-0.14](https://hackage.haskell.org/package/tagsoup-0.14)
- [tasty >= 0.11.2.1 && <= 0.11.2.5](https://hackage.haskell.org/package/tasty-0.11.2.5)
- [template-haskell-2.11.1.0](https://hackage.haskell.org/package/template-haskell-2.11.1.0)
- [temporary-1.2.1.1](http://hackage.haskell.org/package/temporary-1.2.1.1)
- [terminal-size-0.3.2.1](https://hackage.haskell.org/package/terminal-size-0.3.2.1)
- [testpack-2.1.3.0](https://hackage.haskell.org/package/testpack-2.1.3.0)
- [text >= 1.2.2.1 && <= 1.2.3.0](https://hackage.haskell.org/package/text)
- [text-binary-0.2.1.1](https://hackage.haskell.org/package/text-binary-0.2.1.1)
- [text-short-0.1.1](https://hackage.haskell.org/package/text-short-0.1.1)
- [th-lift-instances-0.1.11](https://hackage.haskell.org/package/th-lift-instances-0.1.11)
- [tf-random-0.5](https://hackage.haskell.org/package/tf-random-0.5)
- [time >= 1.6.0.1 <= 1.8.0.3](https://hackage.haskell.org/package/time)
- [time-locale-compat-0.1.1.3](https://hackage.haskell.org/package/time-locale-compat-0.1.1.3)
- [tls-1.4.0](https://hackage.haskell.org/package/tls-1.4.0)
- [transformers >= 0.4.2.0 && <= 0.5.5.0](https://hackage.haskell.org/package/transformers)
- [transformers-base-0.4.4](http://hackage.haskell.org/package/transformers-base-0.4.4)
- [transformers-compat-0.5.1.4](http://hackage.haskell.org/package/transformers-compat-0.5.1.4)
- [tree-fun-0.8.1.0](https://hackage.haskell.org/package/tree-fun-0.8.1.0)
- [type-level-sets-0.8.0.0](https://hackage.haskell.org/package/type-level-sets-0.8.0.0)
- [typed-process-0.2.0.0](https://hackage.haskell.org/package/typed-process-0.2.0.0)
- [unexceptionalio-0.3.0](https://hackage.haskell.org/package/unexceptionalio-0.3.0)
- [unicode-transforms-0.2.0](https://hackage.haskell.org/packages/unicode-transforms-0.2.0)
- [unification-fd-0.10.0.1](https://hackage.haskell.org/package/unification-fd-0.10.0.1)
- [union-find-0.2](https://hackage.haskell.org/package/union-find-0.2)
- [union-find-array-0.1.0.2](https://hackage.haskell.org/package/union-find-array-0.1.0.2)
- [unix-time-0.3.7](https://hackage.haskell.org/package/unix-time-0.3.7)
- [unliftio-core-0.1.1.0](https://hackage.haskell.org/package/unliftio-core-0.1.1.0)
- [unordered-containers >= 0.2.7.1 && <= 0.2.9.0](https://hackage.haskell.org/package/unordered-containers)
- [uri-bytestring >= 0.2.2.1 && <= 0.3.0.1](https://hackage.haskell.org/package/uri-bytestring)
- [utf8-string-1.0.1.1](https://hackage.haskell.org/package/utf8-string-1.0.1.1)
- [uuid-types-1.0.3](https://hackage.haskell.org/package/uuid-types-1.0.3)
- [vault >= 0.3.0.6 && <= 0.3.0.7](https://hackage.haskell.org/package/vault)
- [vector >= 0.11.0.0 && <= 0.12.0.1](https://hackage.haskell.org/package/vector)
- [vector-algorithms-0.7.0.1](https://hackage.haskell.org/package/vector-algorithms-0.7.0.1)
- [vector-binary-instances >= 0.2.3.3 && <= 0.2.3.5](https://hackage.haskell.org/package/vector-binary-instances)
- [vector-instances-3.4](https://hackage.haskell.org/package/vector-instances-3.4)
- [vector-th-unbox-0.2.1.6](https://hackage.haskell.org/package/vector-th-unbox-0.2.1.6)
- [versions-3.0.1.1](https://hackage.haskell.org/package/versions-3.0.1.1)
- [vinyl >= 0.5.1.1 && <= 0.8.1.1](https://hackage.haskell.org/package/vinyl)
- [void >= 0.7.1 && <= 0.7.2](https://hackage.haskell.org/package/void)
- [wai-3.2.1.1](https://hackage.haskell.org/package/wai-3.2.1.1)
- [wai-extra-3.0.21.0](https://hackage.haskell.org/package/wai-extra-3.0.21.0)
- [wai-logger-2.3.1](https://hackage.haskell.org/package/wai-logger-2.3.1)
- [wai-app-static-3.1.6.2](https://hackage.haskell.org/package/wai-app-static-3.1.6.2)
- [websockets-0.12.5.1](https://hackage.haskell.org/package/websockets-0.12.5.1)
- [word8 >= 0.1.2 && <= 0.1.3](https://hackage.haskell.org/package/word8)
- [x509-1.7.2](https://hackage.haskell.org/package/x509-1.7.2)
- [x509-store-1.6.5](https://hackage.haskell.org/package/x509-store-1.6.5)
- [x509-system-1.6.6](https://hackage.haskell.org/package/x509-system-1.6.6)
- [x509-validation-1.6.9](https://hackage.haskell.org/package/x509-validation-1.6.9)
- [xhtml-3000.2.1](https://hackage.haskell.org/package/xhtml-3000.2.1)
- [xml-1.3.5](https://hackage.haskell.org/package/xml-1.3.5)
- [xml-types-0.3.6](https://hackage.haskell.org/package/xml-types-0.3.6)
- [xmlhtml >= 0.2.3.5 && <= 0.2.5.2](https://hackage.haskell.org/package/xmlhtml)
- [yi-rope-0.10](https://hackage.haskell.org/package/yi-rope-0.10)
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
   git apply --reject [path-to-patch]
   ```

   This will work in most cases. If not, resolve the changes.

5. Otherwise, apply the desired changes and go back to step 3 as long as the build fails.

6. Once the build succeeds, make a commit.

   ```
   rm -rf dist/ && git add . && git commit -m "Patched"
   ```

7. Create a patch.

   Mac and Linux users can use the following command to patch:

   ```
   git format-patch HEAD~ --stdout > somepackage-0.1.2.3.patch
   ```

   Windows users can use the following command to patch:

   ```
   git format-patch HEAD~ --stdout --ignore-cr-at-eol > somepackage-0.1.2.3.patch
   ```
   It had been detected that in windows [patches created using powershell has not the correct format](https://stackoverflow.com/questions/13675782/git-shell-in-windows-patchs-default-character-encoding-is-ucs-2-little-endian) and can't be applied so it's better to do it in the dos console.

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
