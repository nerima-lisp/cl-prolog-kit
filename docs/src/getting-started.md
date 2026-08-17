# Getting Started

`cl-prolog-kit` is a **dependency-free** Common Lisp library: the core engine pulls
in nothing beyond the ANSI standard. It is developed and tested against
[SBCL](https://www.sbcl.org/), and leans on a portable, ANSI-facing core.

!!! info "Not on Quicklisp yet"
    cl-prolog-kit is not currently distributed by Quicklisp. Install it by cloning
    the repository and making the checkout visible to ASDF.

## Requirements

- A Common Lisp implementation — SBCL is the tested target.
- [ASDF](https://asdf.common-lisp.dev/) (bundled with SBCL) to load the system.
- Optionally [Nix](https://nixos.org/) for the reproducible test and
  documentation workflows.

## Load from a local checkout

Clone the repository and load its ASDF definition directly. Run these commands
from the repository root:

```sh
git clone https://github.com/nerima-lisp/cl-prolog-kit.git
cd cl-prolog-kit
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(asdf:load-asd (truename "cl-prolog-kit.asd"))' \
  --eval '(asdf:load-system :cl-prolog-kit)'
```

From inside a running Lisp, the same three forms load the library:

```lisp
(require :asdf)
(asdf:load-asd (truename "cl-prolog-kit.asd")) ; run from the repository root
(asdf:load-system :cl-prolog-kit)
```

The public package is `cl-prolog-kit`. Enter it — or `:use` it — before writing
queries:

```lisp
(in-package #:cl-prolog-kit)
```

## Register with the ASDF source registry

To load `cl-prolog-kit` from anywhere (not only the repository root), place the
checkout in a directory configured in your
[ASDF source registry](https://asdf.common-lisp.dev/asdf.html#Configuring-ASDF).
Once ASDF can find the `.asd`, a single form loads it:

```lisp
(asdf:load-system :cl-prolog-kit)
```

## Run through Nix

The flake exposes a runner that executes the
[cl-weave](https://github.com/nerima-lisp/cl-weave) regression suite:

```sh
nix run github:nerima-lisp/cl-prolog-kit
```

!!! info "Supported systems"
    The flake defines outputs for `x86_64-linux` and `aarch64-darwin` (Apple
    Silicon). On other platforms (e.g. Intel Mac, Windows), use the ASDF
    workflow above instead. See [Development](project/development.md) for
    details.

## Systems provided

The `cl-prolog-kit.asd` file defines several ASDF systems:

| System                 | Purpose                                              |
| ---------------------- | ---------------------------------------------------- |
| `cl-prolog-kit`            | The production library and public package.           |
| `cl-prolog-kit/test`       | The cl-weave regression suite ([Development](project/development.md#testing)). |
| `cl-prolog-kit/weave`      | Public query test helpers built on cl-weave.         |
| `cl-prolog-kit/examples`   | Runnable examples ([Examples](guide/examples.md)).   |

## Define a rulebase and query it

```lisp
(define-rulebase *family*
  ((parent tom bob))
  ((parent bob alice))
  ((ancestor ?x ?y) (parent ?x ?y))
  ((ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y)))

(query-prolog *family* '(ancestor tom ?who))
;; => (((?WHO . BOB)) ((?WHO . ALICE)))
```

That is the whole loop: build a rulebase, then ask it questions.

## Reading the syntax

- **Facts** are one-element clauses: `((parent tom bob))`.
- **Rules** are a head clause followed by body goals:
  `((ancestor ?x ?y) (parent ?x ?y))`.
- **Logic variables** are `?`-prefixed symbols: `?x`, `?who`.
- A **solution** is an alist of query-variable bindings. Two solutions above
  mean the query proved twice, binding `?who` to `bob` and then to `alice`.

!!! tip "Ground success vs. failure"
    A ground proof that binds no variables returns `(nil)` — a one-element list
    whose single solution is the empty alist. Failure returns `()`. See
    [Troubleshooting](guide/troubleshooting.md#query-prolog-returned-nil) if
    that surprises you.

## Four ways to ask

```lisp
(query-prolog *family* '(ancestor tom ?who))          ; all solutions, as a list
(query-prolog *family* '(ancestor tom ?who) :limit 2) ; bounded search
(query-prolog-first *family* '(ancestor ?x bob))      ; first solution or NIL
(prolog-succeeds-p *family* '(ancestor tom eve))      ; boolean, stops at first proof
```

Streaming — the function is called as each solution is proven:

```lisp
(map-prolog-solutions
 (lambda (solution) (format t "~&=> ~S~%" solution))
 *family* '(ancestor tom ?who))
```

See [Querying](guide/querying.md) for the full contract of each entry point.

## A taste of the builtins

```lisp
;; arithmetic
(query-prolog (make-rulebase) '(is ?total (+ 20 (* 2 11))))
;; => (((?TOTAL . 42)))

;; list relations need no rulebase clauses at all
(query-prolog (make-rulebase) '(append ?l ?r (a b c)))

;; collect every solution of a subgoal into a bag
(query-prolog *family* '(findall ?child (parent tom ?child) ?children))
;; => (((?CHILDREN BOB)))
```

The full catalogue lives in [Builtin Goals](guide/builtin-goals.md).

## Where to go next

- [Your First Program](guide/first-program.md) — build the family tree step by
  step.
- [Rule DSL](guide/rule-dsl.md) — `define-rulebase`, `extend-rulebase`, guards.
- [Cookbook](guide/cookbook.md) — task-oriented recipes.
- [Examples](guide/examples.md) — the runnable programs shipped in `examples/`.
