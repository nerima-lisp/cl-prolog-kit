# cl-prolog-kit

`cl-prolog-kit` is a small, **dependency-free** Prolog engine for Common Lisp, built
around three ideas:

- **macro-first rule definition** — clauses are data, macros own the syntax
- **CPS proof search** — the engine emits solutions through continuations;
  callers choose streaming or collection
- **data / logic separation** — rulebases are plain structs the engine walks

The public package is `cl-prolog-kit`. It implements a focused Prolog runtime rather
than mirroring every facility of a standalone ISO Prolog system.

## Quick start

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

Facts are one-element clauses; rules are a head followed by body goals. Logic
variables are `?`-prefixed symbols.

!!! tip "New here?"
    Start with [Getting Started](getting-started.md), then follow
    [Your First Program](guide/first-program.md) to build a knowledge base
    step by step.

## Explore the docs

<div class="grid cards" markdown>

-   :material-rocket-launch: **Getting Started**

    ---

    Install the library, run your first query, and build a family tree.

    [:octicons-arrow-right-24: Getting Started](getting-started.md) ·
    [First Program](guide/first-program.md)

-   :material-book-open-variant: **Guide**

    ---

    Querying, the rule DSL, builtins, arithmetic, DCG grammars, and recipes.

    [:octicons-arrow-right-24: Querying](guide/querying.md) ·
    [Builtin Goals](guide/builtin-goals.md) ·
    [Cookbook](guide/cookbook.md)

-   :material-file-document-outline: **Reference**

    ---

    The exported symbol index, proof semantics, conditions, and parser limits.

    [:octicons-arrow-right-24: API Reference](reference/api.md) ·
    [Semantics](reference/semantics.md) ·
    [Conditions](reference/conditions.md)

-   :material-cog-outline: **Internals & Project**

    ---

    The CPS engine's architecture, plus the test, benchmark, and release
    workflow.

    [:octicons-arrow-right-24: Architecture](reference/architecture.md) ·
    [Call Graph Analysis](reference/callgraph.md) ·
    [Benchmarks](reference/benchmarks.md) ·
    [Development](project/development.md)

</div>

## Feature highlights

- A **macro-first DSL**: `prolog`, `define-rulebase`, `extend-rulebase`,
  `def-rule`, and `:when` guards compiled to closures.
- **Streaming or collecting** query APIs over one CPS core:
  `map-prolog-solutions`, `query-prolog`, `query-prolog-first`,
  `prolog-succeeds-p`.
- A broad builtin set — control and meta-call, collection and sorting, the
  dynamic database, arithmetic, ISO string/atom predicates, `library(lists)`,
  `library(apply)`, formatted output, and finite-domain constraints.
- An **SLG tabling engine** with automatic left-recursion handling.
- **DCG** grammar rules and combinators.
- A **transactional source loader** for Prolog text with configurable
  [parser resource limits](reference/parser-limits.md).
- **Foreign predicates** via `define-foreign-predicate` as the supported
  extension surface.
- **Call graph analysis** (`cl-prolog-kit/callgraph`) — Prolog-backed reachability,
  dead-code and mutual-recursion detection, and FD-constraint graph coloring
  over any caller/callee edge set. See
  [Call Graph Analysis](reference/callgraph.md).

## Install

cl-prolog-kit is not currently distributed by Quicklisp. Clone the repository and
either load its ASDF definition directly or place the checkout in a directory
configured in your
[ASDF source registry](https://asdf.common-lisp.dev/asdf.html#Configuring-ASDF).

```sh
git clone https://github.com/nerima-lisp/cl-prolog-kit.git
cd cl-prolog-kit
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(asdf:load-asd (truename "cl-prolog-kit.asd"))' \
  --eval '(asdf:load-system :cl-prolog-kit)'
```

To run the cl-weave regression suite through the Nix app on `x86_64-linux` or
Apple Silicon (`aarch64-darwin`):

```sh
nix run github:nerima-lisp/cl-prolog-kit
```

See [Getting Started](getting-started.md) for the full matrix of load paths;
use its ASDF instructions on other environments.

## Project policy

Conduct, contribution, security and support policies are org-wide defaults,
published once in the [nerima-lisp/.github](https://github.com/nerima-lisp/.github)
repository rather than copied into each of the 21 packages:

- [Contributing](https://github.com/nerima-lisp/.github/blob/main/CONTRIBUTING.md)
- [Code of Conduct](https://github.com/nerima-lisp/.github/blob/main/CODE_OF_CONDUCT.md)
- [Security policy](https://github.com/nerima-lisp/.github/blob/main/SECURITY.md)
- [Support](https://github.com/nerima-lisp/.github/blob/main/SUPPORT.md)

What is specific to this package lives here: [Development](project/development.md) for
the workflow, [Compatibility](reference/compatibility.md) for what the public surface
promises, and the
[releases](https://github.com/nerima-lisp/cl-prolog-kit/releases)
for the per-entry history.
