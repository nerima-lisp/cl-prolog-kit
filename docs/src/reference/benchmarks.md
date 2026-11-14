# Benchmarks

The `benchmarks/` directory holds diagnostic performance tools. They are **not**
part of `nix flake check` or the regression suite — they exist to measure and
compare, not to gate releases.

## In-process micro-benchmarks

`benchmarks/performance.lisp` runs micro-benchmarks and reports
warm-steady-state timing and allocation per operation:

```sh
sbcl --script benchmarks/performance.lisp
```

It covers:

- alias-chain resolution,
- predicate and first-argument indexing,
- recursive path queries,
- branching path queries,
- table-answer replay and the left-recursion cache,
- knowledge-base workloads, whose names start with `kb/`:
  - `kb/small-query-fixed-cost`: the per-call cost of `prolog-succeeds-p`,
    `query-prolog-first`, and `query-prolog` against a 20-fact rulebase,
  - `kb/bulk-insert-10000`: loading 10000 facts into an empty rulebase by
    repeated `rulebase-insert-clause!`, by `make-rulebase :clauses`, and by
    `rulebase-insert-clauses!` when the loaded source defines it, plus
    `kb/bulk-insert-2000/assertz-query-loop`, a failure-driven
    `between/3` and `assertz/1` loop,
  - `kb/string-first-arg-lookup-5000`, `kb/atom-first-arg-lookup-5000`, and
    `kb/compound-first-arg-lookup-5000`: a bound first argument that matches
    the first, middle, or last fact, or none, with string, symbol, or
    compound keys (the compound keys use 50 distinct functors),
  - `kb/setof-2000`: `setof/3` grouped by a free variable over 20 groups, and
    over all facts with the group variable existentially quantified,
  - `kb/max-inferences-overhead`: the small-query workload with
    `:max-inferences 1000000`, run only when the loaded source accepts that
    option.

Workloads that need an API the loaded source lacks are skipped without
output, so the same script measures the release before and after the API is
added.

Each is measured after warm-up so the numbers reflect steady state rather than
first-call compilation.

## Cross-engine comparison

`benchmarks/external-comparison.sh` cross-checks cl-prolog-kit against
[SWI-Prolog](https://www.swi-prolog.org/),
[Trealla](https://github.com/trealla-prolog/trealla), and
[Scryer Prolog](https://github.com/mthom/scryer-prolog) on a shared workload
(`benchmarks/external-workload.pl` and `benchmarks/external-cl-prolog-kit.lisp`):

```sh
ITERATIONS=5000 benchmarks/external-comparison.sh
```

Before comparing wall-clock time, it verifies that **every engine returns an
identical solution count, checksum, and fingerprint** — a correctness gate that
prevents comparing engines that disagree on the answer. It shells out to
`nix shell nixpkgs#{swi-prolog,trealla,scryer-prolog}`, so it requires Nix and
network access to the binary cache. Each engine run is capped at 60 seconds per
iteration count.

!!! note "Diagnostic, not a gate"
    These scripts are tools for investigating performance regressions and
    cross-engine parity. They are intentionally excluded from the verification
    suite so that CI does not depend on external Prolog binaries or timing
    stability.

## CI

`.github/workflows/benchmarks.yml` runs `benchmarks/performance.lisp` (the
self-contained micro-benchmarks only — the cross-engine comparison needs
external Prolog binaries and stays local-only) on `workflow_dispatch` and a
weekly schedule. It is never triggered by a push or pull request and never
gates a merge, consistent with the diagnostic-not-a-gate design above.

## See also

- [Development](../project/development.md) — the regression suite and query helpers.
- [Architecture](architecture.md) — the proof-state prover the benchmarks
  measure.
