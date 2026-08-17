# Development

This page covers the development environment, the test suite, coverage, and
the day-to-day workflow. Benchmarks have their own page:
[Benchmarks](../reference/benchmarks.md).

## Environment

The flake defines outputs for `x86_64-linux` and `aarch64-darwin` (Apple
Silicon). On either system, enter the reproducible development environment
with:

```sh
nix develop        # sbcl, cl-weave, paredit-cli, treefmt, mkdocs-material
```

!!! info "Other platforms have no flake outputs"
    On platforms outside the two supported systems (e.g. Intel Mac,
    Windows), the flake does not expose a development shell, package, check,
    or app. Load the local checkout with ASDF instead (see
    [Getting Started](../getting-started.md)) and rely on CI for Nix
    verification.

## Running examples

```sh
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(asdf:load-asd (truename "cl-prolog-kit.asd"))' \
  --eval '(asdf:load-system :cl-prolog-kit/examples)'
```

Run this from the repository root. Loading `cl-prolog-kit/examples` loads the
library first and then executes all three example files. The example files are
not standalone scripts, so invoking them directly with `sbcl --script` does not
load the `cl-prolog-kit` package. See [Examples](../guide/examples.md) for a
walkthrough.

## Testing

cl-prolog-kit's regression suites are the `cl-prolog-kit/test` and
`cl-prolog-kit/callgraph/test` ASDF systems. They depend on
[cl-weave](https://github.com/nerima-lisp/cl-weave) and cover isolated table
cases, per-query cases, fixtures, generated relational properties, and the
callgraph analysis API.

The Nix runner is self-contained and is the authoritative path on supported
systems (`x86_64-linux`, `aarch64-darwin`):

```sh
nix run .                     # cl-weave regression suite, via the cl-weave CLI
sbcl --script run-tests.lisp  # requires cl-weave on CL_SOURCE_REGISTRY
nix flake check               # full verification suite
nix fmt                       # format Nix sources (treefmt)
```

Pass any cl-weave CLI options after `--`; for example, to produce a JSON result:

```sh
nix run . -- --reporter json --output cl-prolog-kit-weave-results.json
```

!!! info "Unsupported platforms"
    On platforms outside the two supported systems (e.g. Intel Mac,
    Windows), ensure `cl-weave` is discoverable through ASDF and run the
    suite directly with `sbcl --script run-tests.lisp`, then rely on CI for
    the Nix `nix flake check` path.

## What `nix flake check` runs

- **`checks.default`** — both cl-weave regression suites, run through
  `run-tests.lisp` under a plain SBCL with the compiled-in default dynamic
  space.
- **`checks.paredit-lint`** — a structural parse gate over every tracked
  `.lisp`/`.asd` file, failing if any is not a balanced S-expression document.
- **`checks.examples`** — loads every shipped example through ASDF
  ([Examples](../guide/examples.md)).
- **`checks.docs`** — builds the MkDocs site with `--strict` and fails if it
  does not produce a valid `index.html`.
- **`checks.formatting`** — checks every Nix file against `nixfmt`, via
  treefmt. `nix fmt` fixes what it reports.
- **`checks.package`** — builds `packages.default`, so the package README.md
  advertises (`nix run github:nerima-lisp/cl-prolog-kit`) is actually realised,
  not merely evaluated.
- **`checks.app-test`** — runs `apps.test`, the cl-weave CLI wrapper (a
  distinct code path from `checks.default`: it sets a 4096 MB dynamic space).
- **`checks.coverage`** — builds `packages.coverage` and asserts it produced a
  report; it does not gate on a coverage percentage.

## Coverage

`packages.coverage` runs both regression suites under `sb-cover`, instrumenting
`cl-prolog-kit`, `cl-prolog-kit/weave`, and `cl-prolog-kit/callgraph` (not the `cl-weave`
harness driving them), and writes an HTML report. The report helper generates
its runner from `flake.nix`, so it does not read the source-tree
`run-coverage.lisp`:

```sh
nix build .#coverage
open result/cover-index.html
```

Outside Nix, `run-coverage.lisp` remains the direct SBCL entry point. With
`cl-weave` discoverable through `CL_SOURCE_REGISTRY`:

```sh
sbcl --script run-coverage.lisp coverage/
```

This is a visibility tool, not an enforced gate: `checks.coverage` fails only
if the report fails to build, not if coverage drops.

## Query test helpers

Load the `cl-prolog-kit/weave` ASDF system to use the public query test helpers:

```lisp
(asdf:load-system :cl-prolog-kit/weave)
```

`deftest-queries` creates an independent cl-weave case and a fresh rulebase for
every query. A leading case label is optional; without one, the printed query is
used.

```lisp
(cl-prolog-kit/weave:deftest-queries family-queries ((make-family-rulebase))
  ("keeps proof order" (parent alice ?child) :ordered
   (((?child . bob)) ((?child . carol))))
  ((parent alice ?child) :set
   (((?child . carol)) ((?child . bob))))
  ((parent alice ?child) :first ((?child . bob)))
  ((parent alice bob) :succeeds)
  ((parent bob alice) :fails)
  ((parent alice bob) :signals cl-prolog-kit:invalid-max-depth-error
   :max-depth :invalid))
```

Assertion kinds:

- `:ordered` — compares the full solution sequence, order included.
- `:set` — ignores only the order of complete solutions; it still compares the
  structure within each solution with `equal`.
- `:first` — compares the first solution's bindings.
- `:succeeds` / `:fails` — assert provability without inspecting bindings.
- `:signals` — asserts a condition is raised, optionally of a given type.

Query options (such as `:max-depth`) follow the expected value or assertion
kind.

Use `assert-query` inside an existing cl-weave case when a table is not needed:

```lisp
(cl-weave:it "finds Alice's first child"
  (cl-prolog-kit/weave:assert-query (make-family-rulebase)
    (parent alice ?child) :first ((?child . bob))))
```

## Track new files before trusting `nix flake check`

Git-backed flake input selection drops untracked files *before* this
repository's own source filter runs. A new docs page, example, or test file
that exists only in a dirty worktree is therefore absent from every Nix build,
and `nix flake check` will pass without ever seeing it — or fail with a
confusing "file does not exist" from inside the sandbox.

```sh
git add path/to/new-file.lisp   # then re-run
nix flake check
```

## Structural refactors

`nix develop` puts [paredit](https://github.com/nerima-lisp/paredit-cli) on
`PATH`. Prefer it over hand-editing parentheses for renames, moves, and other
structural changes to Lisp sources:

```sh
paredit inspect check --file src/engine.lisp
paredit refactor rename-function --from old-name --to new-name --output json src/*.lisp
```

Run a plan or preview command without `--write` first, review the JSON, then
re-run with `--write`. `checks.paredit-lint` fails the build if any tracked
`.lisp` or `.asd` file stops being a balanced S-expression document.

## Benchmarks at a glance

```sh
sbcl --script benchmarks/performance.lisp      # in-process micro-benchmarks
ITERATIONS=5000 benchmarks/external-comparison.sh   # cross-engine comparison
```

These are diagnostic tools, not part of `nix flake check`. See
[Benchmarks](../reference/benchmarks.md).

## Documentation

The site is built with [Material for MkDocs](https://squidfunk.github.io/mkdocs-material/).
The config lives in `docs/mkdocs.yml` and content in `docs/src/`.

=== "Nix"

    ```sh
    nix build .#docs   # rendered site in ./result
    ```

=== "MkDocs directly"

    ```sh
    # from the dev shell, or any environment with mkdocs-material installed
    mkdocs serve -f docs/mkdocs.yml          # live-reloading preview
    mkdocs build -f docs/mkdocs.yml --strict # one-shot strict build
    ```

`--strict` promotes broken links and unlisted pages to build failures, matching
the Nix build and the `checks.docs` gate. The published site deploys to
GitHub Pages from `.github/workflows/docs.yml` on every push to `main` that
touches `docs/`, `flake.nix`, `flake.lock`, or the workflow itself.

## Design constraints

- no runtime dependencies, SBCL-tested, ANSI-leaning core
- a single canonical public API surface

## Releasing

Release mechanics — the tag/`:version` guard and writing the GitHub Release
description, which is this project's only canonical release history — follow
the org-wide
[RELEASE_STANDARD.md](https://github.com/nerima-lisp/.github/blob/main/RELEASE_STANDARD.md).

A change is releasable when `nix flake check` is green and, if the public
surface moved, the matching pages were updated in the same change:
[API](../reference/api.md),
[Conditions and Errors](../reference/conditions.md), and
[Compatibility](../reference/compatibility.md).
