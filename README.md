# cl-prolog-kit

[![CI](https://github.com/nerima-lisp/cl-prolog-kit/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/nerima-lisp/cl-prolog-kit/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Documentation](https://img.shields.io/badge/docs-MkDocs%20Material-0a7a5a)](https://nerima-lisp.github.io/cl-prolog-kit/)

A small, dependency-free Prolog engine for Common Lisp. Rulebases are plain
data, proof search is continuation-passing, and the builtin goal set is
extensible. The public package is `cl-prolog-kit`.

## Quick Start

```lisp
(require :asdf)
(asdf:load-asd (truename "cl-prolog-kit.asd")) ; run from the repository root
(asdf:load-system :cl-prolog-kit)

(in-package #:cl-prolog-kit)

(define-rulebase *family*
  ((parent tom bob))
  ((parent bob alice))
  ((ancestor ?x ?y) (parent ?x ?y))
  ((ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y)))

(query-prolog *family* '(ancestor tom ?who))
;; => (((?WHO . BOB)) ((?WHO . ALICE)))
```

Prolog source text works too:

```lisp
(query-prolog (consult-prolog #p"family.pl") (read-prolog-term "ancestor(tom, Who)"))
```

## Install

cl-prolog-kit is not on Quicklisp. Clone it and load its ASDF definition, or put
the checkout on your [ASDF source registry](https://asdf.common-lisp.dev/asdf.html#Configuring-ASDF):

```sh
git clone https://github.com/nerima-lisp/cl-prolog-kit.git
```

With Nix, `nix run github:nerima-lisp/cl-prolog-kit` runs the regression suite, and
as a flake input:

```nix
# flake.nix
inputs.cl-prolog-kit = {
  url = "github:nerima-lisp/cl-prolog-kit/v1.5.0";
  inputs.nixpkgs.follows = "nixpkgs";
};
```

Note the pinned tag. A bare `github:nerima-lisp/cl-prolog-kit` follows this
repository's default branch, so a push here would break your build without
warning.

## Documentation

Full documentation is published at <https://nerima-lisp.github.io/cl-prolog-kit/>.
The source for that site lives in [docs/src/](docs/src/).

- [Getting Started](https://nerima-lisp.github.io/cl-prolog-kit/getting-started/)
- [Your First Program](https://nerima-lisp.github.io/cl-prolog-kit/guide/first-program/)
- [API Reference](https://nerima-lisp.github.io/cl-prolog-kit/reference/api/)
- [Architecture](https://nerima-lisp.github.io/cl-prolog-kit/reference/architecture/)

## Development

```sh
nix develop          # SBCL with CL_SOURCE_REGISTRY already set
nix run .#test       # run the test suite
nix flake check      # tests + formatting + docs, the same gate CI uses
nix fmt              # format Nix sources (treefmt)
```

Tests live in `t/` and `callgraph/test/` and run under
[cl-weave](https://github.com/nerima-lisp/cl-weave), the org's test framework.
Without Nix, `sbcl --script run-tests.lisp` runs both suites, provided cl-weave
is on `CL_SOURCE_REGISTRY`.

## Contributing

See the org-wide [CONTRIBUTING](https://github.com/nerima-lisp/.github/blob/main/CONTRIBUTING.md)
guide and the [package standard](https://github.com/nerima-lisp/.github/blob/main/PACKAGE_STANDARD.md).
Release history is in the
[releases](https://github.com/nerima-lisp/cl-prolog-kit/releases).

## Support

See [SUPPORT](https://github.com/nerima-lisp/.github/blob/main/SUPPORT.md).

## License

MIT — see [LICENSE](LICENSE).
