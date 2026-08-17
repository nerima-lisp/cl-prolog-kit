# Call Graph Analysis

`cl-prolog-kit/callgraph` is a generic, program-representation-independent call-graph
analysis library built on top of the core `cl-prolog-kit` engine: reachability,
dead-code and mutual-recursion detection, and FD-constraint graph coloring. It
was split out of [nerima-lisp/cl-cc](https://github.com/nerima-lisp/cl-cc)'s
`packages/prolog-tools`, which paired this same logic with a thin adapter that
walked cl-cc's own AST nodes. Nothing here refers to any host AST — every entry
point takes a plain list of names and `(caller . callee)` conses, so it applies
equally to a compiler's call graph, a build system's dependency graph, or any
other caller/callee relationship you can enumerate as edges.

## Loading

```lisp
(asdf:load-system :cl-prolog-kit/callgraph)
```

This loads `cl-prolog-kit` first. The test suite is a separate secondary system,
scoped to keep the `cl-weave` test dependency out of the production system:

```lisp
(asdf:test-system "cl-prolog-kit/callgraph/test")
```

## Building a call graph

`build-call-graph-from-edges` asserts each edge as a `calls/2` fact in a
private rulebase:

```lisp
(in-package #:cl-prolog-kit/callgraph)

(defvar *cg*
  (build-call-graph-from-edges
   '(main helper leaf orphan)
   '((main . helper) (helper . leaf) (orphan . leaf))
   :entry-points '(main)))
```

`names` should list every defined function, even ones that call nothing, so
dead-code and coloring queries see them. `entry-points` marks the functions a
program can be invoked from.

## Reachability

- `direct-callees` — the functions a name calls directly, via one `findall`
  query
- `reachable-from` — every function reachable from a name via one or more
  calls
- `reachable-p` — whether one name can reach another

```lisp
(direct-callees *cg* 'main)   ;; => (helper)
(reachable-from *cg* 'main)   ;; => (helper leaf)
(reachable-p *cg* 'main 'leaf) ;; => t
(reachable-p *cg* 'leaf 'main) ;; => nil
```

!!! info "Why reachability is BFS in Lisp, not a recursive Prolog rule"
    The obvious Prolog formulation —
    `reachable(X,Y) :- calls(X,Y). reachable(X,Y) :- calls(X,Z), reachable(Z,Y).`
    — searches forever on a cyclic call graph: nothing stops it from
    re-deriving the same targets by going around a cycle again, and mutual or
    self recursion is exactly the case a call-graph tool has to handle.
    `reachable-from` instead does a breadth-first search in Lisp over repeated
    single-hop `direct-callees` queries with an explicit visited set — safe on
    cycles, while still routing every edge lookup through the engine.

## Dead code and mutual recursion

```lisp
(find-dead-code *cg*)                  ;; => (orphan)
(find-mutually-recursive-pairs *cg*)   ;; => ((a . b) ...)
```

`find-dead-code` returns every defined function that is neither an entry point
nor reachable from one. `find-mutually-recursive-pairs` returns `(a . b)`
pairs of distinct functions each reachable from the other.

## Graph coloring

`color-call-graph` treats a direct call between two functions as an
interference and assigns each defined function a color in `1..num-colors`
such that no two functions joined by a call share one — using cl-prolog-kit's
finite-domain constraint solver (`ins` domain restriction, pairwise `#\=`
disequality, `labeling` search) rather than a hand-rolled greedy algorithm:

```lisp
(color-call-graph *cg* 2)
;; => ((main . 1) (helper . 2) (leaf . 1) (orphan . 2))

(valid-coloring-p *cg* (color-call-graph *cg* 2)) ;; => t
```

`color-call-graph` returns `nil` if no coloring exists with the given number
of colors. `valid-coloring-p` checks that a coloring assigns distinct colors to
every pair of functions joined by a direct call edge.

## Edge notation

`edge-dcg.lisp` defines a small textual notation for describing call edges
outside of Lisp source — useful for fixtures and hand-authored graphs — using
a `cl-prolog-kit` DCG grammar as the recognizer:

```lisp
(parse-edge-spec "main -> helper, helper -> leaf")
;; => ((main . helper) (helper . leaf))
```

`tokenize-edge-spec` and `edge-spec-well-formed-p` are exported separately if
you need tokenizing or well-formedness checking without full extraction.
`parse-edge-spec` signals an error on a malformed spec.

## API summary

- `call-graph`, `call-graph-rulebase`, `call-graph-defined`,
  `call-graph-entry-points`
- `build-call-graph-from-edges`
- `reachable-p`, `reachable-from`, `direct-callees`
- `find-dead-code`, `find-mutually-recursive-pairs`
- `tokenize-edge-spec`, `edge-spec-well-formed-p`, `parse-edge-spec`
- `color-call-graph`, `valid-coloring-p`

See [API Reference](api.md) for the core `cl-prolog-kit` package these are built
on.
