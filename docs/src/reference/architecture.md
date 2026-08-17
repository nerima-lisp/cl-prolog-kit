# Architecture

## Design Direction

`cl-prolog-kit` is a relational programming library embedded in Common Lisp. Its
main design choices are:

- macro-first authoring: macros own syntax and produce runtime data
- continuation-passing proof search: solutions stream through continuations
- explicit rulebase and query state rather than a process-global database
- one ordered clause representation for facts and rules
- a narrow exported package surface with internal solver machinery

It implements a focused Prolog runtime rather than attempting to mirror every
facility of a standalone ISO Prolog system.

## ASDF Load Order

`cl-prolog-kit.asd` is serial. The production system loads these components in
this exact dependency order, which groups into layers:

**Foundations** — packages, operator/module/source registries, the clause and
predicate-index data models, the rulebase container with its property and
compaction layers, the tabling data model, and unification:

1. `package.lisp`
2. `atom-name.lisp`
3. `operator-table.lisp`
4. `module-system.lisp`
5. `source-registry.lisp`
6. `logic-variable.lisp`
7. `clause.lisp`
8. `predicate-index.lisp`
9. `data.lisp`
10. `rulebase-properties.lisp`
11. `rulebase-compaction.lisp`
12. `table-variant.lisp`
13. `environment-index.lisp`
14. `unification.lisp`

**Text front end** — the lexer/parser split and the term writer:

15. `lexer.lisp`
16. `lexer-operator-lexemes.lisp`
17. `lexer-tokenizer.lisp`
18. `grammar.lisp`
19. `term-writer.lisp`

**Search core** — conditions and registries, the I/O context, proof state, goal
resolution and left-recursion analysis, the CPS prover, and the tabling layer:

20. `engine.lisp`
21. `io-context.lisp`
22. `proof-state.lisp`
23. `goal-resolution.lisp`
24. `left-recursion-analysis.lisp`
25. `prover.lisp`
26. `tabling.lisp`

**Builtin goal set** — the `define-builtin` machinery and the builtin modules:

27. `builtins/term-sorting.lisp`
28. `builtins/core.lisp`
29. `builtins/unify.lisp`
30. `builtins/control.lisp`
31. `builtins/collection.lisp`
32. `builtins/aggregate.lisp`
33. `builtins/dynamic.lisp`
34. `builtins/arithmetic-functions.lisp`
35. `builtins/arithmetic.lisp`
36. `builtins/list.lisp`
37. `builtins/text-conversion.lisp`
38. `builtins/atom-ops.lisp`
39. `builtins/atom-number-conversion.lisp`
40. `builtins/operator.lisp`
41. `builtins/io.lisp`
42. `builtins/io-streams.lisp`
43. `builtins/io-code.lisp`
44. `fd-store.lisp`
45. `builtins/fd.lisp`
46. `term-inspect.lisp`
47. `term-compare.lisp`
48. `term-construct.lisp`
49. `builtins/list-extra.lisp`
50. `builtins/apply.lisp`
51. `builtins/format.lisp`
52. `builtins/char-type.lisp`
53. `builtins/term-io.lisp`
54. `builtins/string.lisp`
55. `builtins/assoc.lisp`
56. `builtins/pairs.lisp`

**Front ends** — DCG runtime, the public query API, the transactional source
loader, and the authoring macros:

57. `dcg-runtime.lisp`
58. `query.lisp`
59. `source-io.lisp`
60. `source-directives.lisp`
61. `source-rollback.lisp`
62. `source-loader.lisp`
63. `dsl-compiler.lisp`
64. `dsl.lisp`
65. `dcg.lisp`

The important boundaries are:

- `atom-name.lisp` owns the bijection between an atom's text and the Common
  Lisp symbol that represents it, and the text-based equality and ordering the
  rest of the engine decides atom identity with. It sits directly on
  `package.lisp` because the parser, the writer, unification, the standard
  order, and the text-conversion builtins all have to agree on it
- `logic-variable.lisp` owns what a logic variable is and its creation-order
  bookkeeping; `clause.lisp` owns the clause representation and its compiled
  instantiation templates; `predicate-index.lisp` owns the per-predicate
  descriptor index built on top of clauses; `data.lisp` owns the rulebase
  container itself (construction, copy, insert/retract, revisions) built on
  all three; `rulebase-properties.lisp` owns the per-predicate `table` and
  predicate-property declarations stored alongside those clauses, and
  `rulebase-compaction.lisp` owns the physical reclamation of retracted ones;
  `table-variant.lisp` owns the tabling data model;
  `environment-index.lisp` owns the indexed-substitution structure used
  during proof search; `unification.lisp` owns the unification algorithm and
  term substitution/freshening built on `environment-index.lisp`
- `lexer.lisp` tokenizes source text and enforces the parser resource limits,
  `lexer-operator-lexemes.lisp` holds the standard/symbolic operator lexeme
  tables the tokenizer matches against, and `lexer-tokenizer.lisp` is the
  tokenizer itself; `grammar.lisp` runs the precedence-climbing parser on top
  and exposes the public reader API
- `engine.lisp` owns conditions plus the builtin and foreign-predicate
  registries and the CPS `emit` protocol
- `proof-state.lisp` owns the pure `proof-state` representation threaded
  through the search; `goal-resolution.lisp` owns the non-continuation half of
  dispatch — query normalization, predicate indicators, revision-stable clause
  snapshots, and module resolution; `prover.lisp` owns the CPS search built on
  both — clause resolution, cut barriers, and depth accounting;
  `left-recursion-analysis.lisp` owns the static first-user-goal call-graph
  analysis and its per-revision cache, and `tabling.lisp` layers memoized
  resolution on top of `prover.lisp` using it
- `query.lisp` turns the continuation protocol into the public mapping and
  result APIs
- the builtin set is split by concern — unification, control, collection,
  aggregation, term sorting, dynamic database, arithmetic (function tables
  separate from the evaluator and surface goals), lists and the list/apply
  libraries, atom/text conversion, strings, character classification,
  formatted output, operators, stream and term I/O, association maps and
  pairs, finite domains, and term inspection/comparison/construction
- the `source-*.lisp` files, `dsl*.lisp`, and `dcg*.lisp` are separate front
  ends that produce or consume the same clause and query representation

## Unified Clause Store

Facts and rules use the same `clause` structure. A fact is a clause whose body
is empty. The rulebase stores clauses in one definition-ordered sequence and a
predicate index; it does not search a separate fact collection before a rule
collection.

Stored entries carry their module, source identity, and born/died revisions.
A predicate call takes the visible entries for its logical-update snapshot and
tries them in definition order. Each selected clause is freshened before
unification, so variables do not leak between uses.

## Proof-State Prover

The prover streams solutions in continuation-passing style. Its internal
`proof-state` carries:

- the explicit rulebase
- the current persistent binding environment
- remaining user-rule depth
- the active module
- the table session
- the current cut tag

State transitions construct updated proof states rather than replacing the
rulebase with hidden global state. Goal dispatch first recognizes registered
builtin or foreign solvers; otherwise it resolves the goal against the visible
user clauses. Foreign predicates are keyed by exact name and arity and use the
same zero-to-many `emit` continuation contract as internal solvers.

`map-prolog-solutions` exposes this streaming model. Convenience query APIs
fold or stop that stream instead of requiring the prover to accumulate all
answers.

### Cut

Cut uses dynamically scoped `catch`/`throw` tags:

- each user-predicate invocation establishes a fresh cut barrier
- `!` emits the current state and throws to that invocation's tag
- the throw abandons remaining alternatives for the invocation
- opaque meta-calls establish their own barrier
- transparent control constructs deliberately reuse the caller's barrier

This keeps a rule-body cut local to the predicate invocation while preserving
the intended transparency of control constructs.

### Depth and tabling

Depth decreases when proof enters a user rule, not for every builtin or
unification step. `nil` means unbounded search. Declared tabled predicates and
detected left recursion share one table session per rulebase revision, reused
across every query until the next mutation invalidates it; depth-limited or
active finite-domain searches bypass that tabling path where required.

### Guards

`(:when expression)` guards are compiled by the DSL macros into
`(:when function variable...)` goals. At runtime the solver substitutes bound
values and calls the function. Relational rule data therefore does not require
runtime `eval`.

## Explicit Dynamic Mutation

There is no process-global rulebase. `prolog` constructs one, `extend-rulebase`
derives another, and every query receives its rulebase explicitly.

The default authoring style is immutable, but mutation is a supported and
visible operation:

- `asserta/1`, `assert/1`, and `assertz/1` insert into the supplied rulebase
- `retract/1`, `retractall/1`, and `abolish/1` retire matching entries
- `consult-prolog` transactionally replaces the clauses registered for a
  source after validation
- `rulebase-insert-clause!` exposes insertion to Lisp callers

Born/died revisions preserve logical-update behavior: a running predicate
invocation continues over its snapshot even when a dynamic goal changes the
database. Later invocations observe the newer revision. Callers that require
isolation can use `copy-rulebase` before mutation.

Retracted entries are marked dead, not removed, so an in-flight invocation's
snapshot stays valid. Once a rulebase's dead-entry backlog passes an internal
threshold, the next top-level engine call (`map-prolog-solutions`,
`query-prolog`, `query-prolog-first`, `prolog-succeeds-p`) to return while no
other top-level call is active anywhere on the stack compacts them away. Any
new top-level entry point into the engine must join that same call-tracking or
compaction becomes unsafe.

## Transactional Source Loading

Loading Prolog source is all-or-nothing. `consult-prolog` and
`ensure-prolog-loaded` run inside a loading transaction that copies the live
rulebase, applies every clause and directive to the detached copy, runs any
`:- initialization` goals, and only publishes the copy back on success. Any
parse error, failed directive, or resource-limit violation aborts before the
live rulebase is touched.

The pipeline is split across five files:

- `source-registry.lisp` records one entry per canonical source pathname,
  tracking its load state and the effects it applied
- `source-io.lisp` resolves pathnames and streams and translates parser and
  I/O failures into ISO source-loading errors (including the catchable
  `resource_error/1` form of a parser resource-limit breach)
- `source-directives.lisp` evaluates one directive or clause at a time —
  `op`, `dynamic`, `discontiguous`, `multifile`, `table`, `module`,
  `use_module`, `include`, `set_prolog_flag`, `initialization`, `consult`,
  `ensure_loaded`, and `load_files` — recording each operator,
  predicate-property, and table declaration for later rollback
- `source-rollback.lisp` undoes a previously loaded unit on reload: it removes
  the unit's clauses and replays or restores the operator, predicate-property,
  and table effects it recorded
- `source-loader.lisp` orchestrates the transaction and exposes the public
  `consult`, `load_files`, and `ensure_loaded` surface

Load-state tracking breaks reload cycles and honors the `if-loaded` policy that
distinguishes `consult` (always reload) from `ensure_loaded` (load once).

## Macro-First Surface

```lisp
(prolog
  ((parent tom bob))
  ((score bob 42))
  ((rich ?x) (score ?x ?n) (:when (> ?n 10))))
```

The DSL expands into clause construction, including precompiled guard
closures. Parsed Prolog source, Lisp DSL forms, dynamic goals, and DCGs all
converge on the same runtime terms and rulebase.

## Verification Layers

1. `nix run .` — run the cl-weave-backed ASDF regression behavior
2. `nix flake check` — verify packaging and clean-source behavior

When architecture changes, update the narrowest affected verification layer
first.
