# API Reference

The core `cl-prolog-kit` system exposes the public `cl-prolog-kit` package. This page
lists every symbol exported by that package; the separately loaded public
test-helper package `cl-prolog-kit/weave` is documented in
[Development](../project/development.md).
Anything not listed here is internal.

See [Querying](../guide/querying.md), [Builtin goals](../guide/builtin-goals.md),
[Rule DSL](../guide/rule-dsl.md), and [DCG](../guide/dcg.md) for narrative explanations and
examples.

## Data

- Clause representation: `clause`, `clause-p`, `clause-head`, `clause-body`,
  `make-clause`
- Rulebase representation: `rulebase`, `rulebase-p`,
  `rulebase-visible-clauses`, `make-rulebase`, `copy-rulebase`,
  `rulebase-extend`, `rulebase-insert-clause!`

A clause with an empty body is a fact. Rulebases are explicit values; the
library has no global rulebase. Prefer `prolog` and `extend-rulebase` for
declarative construction. `rulebase-insert-clause!` and the dynamic database
goals provide intentional mutation with logical-update semantics.

`copy-rulebase` is useful when dynamic updates must not affect a reusable
base. It copies stored terms and mutable registries while allowing immutable
metadata to be shared.

The exported symbol `clause` names the clause type/accessor API and is also
the Lisp representation of the exported `clause/2` builtin goal.

## Atoms

- `prolog-atom` — `(prolog-atom text)` returns the atom whose printed text is
  the string `text`
- `prolog-atom-text` — the inverse: the string an atom prints as

An atom is a Common Lisp symbol, but its symbol name is not its text — the text
of `CL-PROLOG-KIT::PARENT` is `"parent"`. Bare symbols therefore spell exactly the
lower-case atoms; use `prolog-atom` for any other spelling. See
[Semantics](semantics.md#atoms).

## Unification

- `logic-var-p` — true for `?`-prefixed non-keyword symbols
- `fresh-logic-variable` — return a fresh symbol satisfying `logic-var-p`
- `unify` — `(unify left right &optional environment occurs-check)` returns
  `(values extended-environment t)` on success and `(values nil nil)` on
  failure; the occurs check defaults to enabled
- `logic-substitute` — apply an environment to a term while preserving dotted
  structure

Environments are persistent association lists. `unify` extends rather than
mutates an environment, so older environments remain valid choice points.

## Proof Search and Depth

- `*max-prolog-depth*` — default maximum user-rule depth; `nil` means
  unbounded
- `define-foreign-predicate` — register one exact predicate name/arity pair
  using the solver's zero-to-many `emit` continuation protocol

`define-foreign-predicate` is the supported extension surface. Its form is:

```lisp
(define-foreign-predicate (name argument...)
    (rulebase environment depth emit)
  body...)
```

The argument list has fixed arity. Call `emit` once for each solution
environment and do not collect solutions in the predicate. The builtin
registry and its defining machinery are internal implementation details.

## Queries

- `map-prolog-solutions`
- `query-prolog`
- `query-prolog-first`
- `prolog-succeeds-p`
- `solution-binding`

The query functions accept an explicit rulebase and query. The mapping and
list-returning APIs support `:max-depth`, `:environment`, `:project`, and
`:limit`; `prolog-succeeds-p` supports `:max-depth`. See
[Querying](../guide/querying.md) for contracts and result shapes.

## Parser, Writer, and Loader

- Readers and parser: `read-prolog-term`, `read-prolog-clause`, `parse-prolog`
- Writer: `write-prolog-term`, `prolog-term-string`
- Source loading: `consult-prolog`, `ensure-prolog-loaded`
- Loader goal symbols: `consult`, `ensure_loaded`, `load_files`

`read-prolog-term` and `read-prolog-clause` accept a string or stream and an
optional operator table. `parse-prolog` parses Prolog source into clause data.
`write-prolog-term` writes to its optional stream, while
`prolog-term-string` returns a string.

`consult-prolog` validates a source before replacing its registered clauses.
`ensure-prolog-loaded` loads a pathname only when that source is not already
registered. The goal symbols correspond to `consult/1`, `ensure_loaded/1`,
and `load_files/1` in parsed or Lisp-shaped queries.

## Parser Resource Limits

Every parser entry point — `read-prolog-term`, `read-prolog-clause`,
`parse-prolog`, and the `consult`/`ensure_loaded`/`load_files` source loaders —
enforces a set of finite resource bounds so untrusted Prolog text cannot
exhaust memory or stack. Each bound is a special variable you can rebind; the
defaults are conservative but generous for ordinary source. Binding a variable
to `nil` disables that single limit.

- `*max-prolog-source-characters*` — total source characters consumed
  (default `1048576`)
- `*max-prolog-delimiter-depth*` — maximum nesting of `(`, `[`, `{`
  (default `256`)
- `*max-prolog-parser-depth*` — expression-parser recursion depth
  (default `256`)
- `*max-prolog-tokens*` — total tokens produced, excluding EOF
  (default `65536`)
- `*max-prolog-identifier-length*` — length of one unquoted name lexeme
  (default `1024`)
- `*max-prolog-quoted-lexeme-length*` — content length of one quoted atom or
  string lexeme (default `65536`)
- `*max-prolog-numeric-lexeme-length*` — length of one numeric lexeme
  (default `4096`)
- `*max-prolog-interned-symbols*` — cumulative count of new symbols the parser
  may intern (default `65536`)

`*max-prolog-interned-symbols*` differs from the others: it is a cumulative,
process-wide bound tracked across parse calls rather than a per-parse limit.

When a bound is exceeded the parser signals `prolog-parser-resource-error`
(readers `-resource`, `-limit`, `-observed`, `-position`). For the full
narrative — how each bound behaves, direct-reader vs. in-engine propagation, and
how to raise or disable a limit — see the dedicated
[Parser Resource Limits](parser-limits.md) page.

## Runtime Resource Limits

Separate from the *parser* limits above, one exported special bounds how much a
single builtin call may materialize at **runtime**:

- `*max-prolog-builtin-output-length*` — the maximum number of characters or
  list elements a single builtin call may produce in one step (default
  `1048576`). It caps `format` fill/repeat/newline runs (`~t`, `~|`, `~Nc`,
  `~Nn`), `tab/1`, and `numlist/3` ranges, so an attacker-sized count in a tiny
  query cannot exhaust memory. Exceeding it raises a catchable ISO
  `resource_error/1`; binding it to `nil` disables the bound.

See [Semantics](semantics.md#builtin-output-resource-limit) for the behavioral
notes.

## Rule DSL

- `prolog`
- `define-rulebase`
- `extend-rulebase`
- `def-rule`
- `with-prolog-query`
- `prolog-match`

See [Rule DSL](../guide/rule-dsl.md) for macro forms and examples.

## Builtin Goal Symbols

These exported symbols form the public Lisp package surface for builtin goals.
The same names can be written in parsed Prolog syntax with their supported
arities. See [Builtin goals](../guide/builtin-goals.md) for behavior-oriented guidance.

- Control and meta-call: `!`, `call`, `call_nth`,
  `call_with_depth_limit`, `once`, `setup_call_cleanup`, `call_cleanup`,
  `forall`, `if-then-else`, `soft-if-then-else`, `catch`, `throw`,
  `unify_with_occurs_check`, `repeat`, `true`, `fail`, `false`, `\+`
- Collection and sorting: `findall`, `bagof`, `setof`, `sort`, `msort`,
  `keysort`
- Dynamic database and reflection: `asserta`, `assert`, `assertz`, `retract`,
  `retractall`, `current_predicate`, `predicate_property`, `abolish`, `clause`
- Unification and term construction: `\=`, `=..` (univ). `..` is the exported
  finite-domain range operator (`xfx`, priority 450) used inside `in`/`ins`
  domains — see [Arithmetic and Comparison](../guide/arithmetic.md#domain-assignment) —
  not a callable goal.
- Arithmetic evaluation and comparison: `is`, `=:=`, `=\=`, `<`, `=<`, `>`,
  `>=`
- Finite domains: `in`, `ins`, `#=`, `#\=`, `#<`, `#=<`, `#>`, `#>=`,
  `all_different`, `labeling`, `indomain`
- Type and term tests: `var`, `nonvar`, `atom`, `atomic`, `number`, `integer`,
  `float`, `compound`, `callable`, `ground`, `acyclic_term`, `cyclic_term`
- Standard term order and comparison: `==`, `\==`, `@<`, `@=<`, `@>`, `@>=`,
  `compare`, `unifiable`
- Term inspection and copying: `term_variables`, `functor`, `arg`, `copy_term`,
  `numbervars`

## Prolog-Source and Query Goals

The engine also implements a range of ISO goals callable from parsed Prolog
source (via `consult`/`load_files` or `read-prolog-*`). Several of the goals
listed below are exported as `cl-prolog-kit` symbols, so Lisp code can use those
symbols as Prolog goal names in Lisp-shaped queries. Those exports are **not**
ordinary Common Lisp host-function APIs: invoking the relation still happens
through the Prolog query interface. Goals not called out as exported here are
source/query-goal vocabulary only. This catalogue lists them by predicate
indicator; see
[Builtin Goals](../guide/builtin-goals.md) for behavior-oriented notes.

- **Operators:** `op/3` defines operators in the rulebase operator table;
  `current_op/3` enumerates them.
- **Character conversion:** `char_conversion/2` registers a mapping;
  `current_char_conversion/2` queries it.
- **Prolog flags:** `current_prolog_flag/2` reads flags (enumerating when the
  name is unbound); `set_prolog_flag/2` sets one, raising `domain_error` for an
  unknown flag.
- **Stream lifecycle:** `open/3`, `open/4`, `close/1`, `close/2`,
  `stream_property/2`, `set_stream_position/2`, `current_input/1`,
  `current_output/1`, `set_input/1`, `set_output/1`,
  `at_end_of_stream/0`, `at_end_of_stream/1`, `flush_output/0`,
  `flush_output/1`.
- **Term I/O:** `read/1`, `read/2`, `read_term/2`, `read_term/3`, `write/1`,
  `write/2`, `writeq/1`, `writeq/2`, `write_canonical/1`, `write_canonical/2`,
  `write_term/2`, `write_term/3`, `nl/0`, `nl/1`.
- **Character and byte I/O:** `get_char/1`, `get_char/2`, `peek_char/1`,
  `peek_char/2`, `put_char/1`, `put_char/2`, `get_byte/1`, `get_byte/2`,
  `peek_byte/1`, `peek_byte/2`, `put_byte/1`, `put_byte/2`.
- **Term comparison (standard order):** `=@=/2` (structural variant),
  `\=@=/2` (not a variant), and `subsumes_term/2` (one-way subsumption without
  binding either term) — companions to the exported `==`, `@<`, `compare`, and
  `unifiable`.
- **Relational arithmetic:** `between/3` (enumerate or test an integer range),
  and the exported query goals `succ/2` (the non-negative successor relation,
  usable in either direction) and `plus/3` (`A + B =:= C`, any single
  unknown). See
  [Arithmetic and Comparison](../guide/arithmetic.md#relational-arithmetic).
- **List library:** the exported query goals `sum_list/2` (`sumlist/2`),
  `numlist/3`, `list_to_set/2`, `subtract/3`, `intersection/3`, `union/3`,
  and `permutation/2`; plus `max_list/2` and `min_list/2`.
- **Apply (meta) library:** the exported query goals `maplist/2` and up,
  `foldl/4`, `foldl/5`, `foldl/6`, `include/3`, `exclude/3`, and
  `partition/4`.
- **Sorting and aggregation:** `sort/4`, plus the exported query goals
  `predsort/3` and `aggregate_all/3`.
- **Character classification:** the exported query goals `char_type/2`,
  `code_type/2`, `upcase_atom/2`, and `downcase_atom/2`.
- **Term ↔ text:** the exported query goals `term_to_atom/2` and
  `read_term_from_atom/3`.
- **Strings:** `string/1`, `string_length/2`, `string_concat/3`,
  `atom_string/2`, `string_to_atom/2`, `number_string/2`, `string_chars/2`,
  `string_codes/2`, `term_string/2`, `text_concat/3`, `sub_string/5`,
  `split_string/4`; every listed predicate other than `string/1` is an
  exported query goal.
- **Association maps:** the exported query goals `empty_assoc/1`,
  `put_assoc/4`, `get_assoc/3`, `del_assoc/4`, `list_to_assoc/2`,
  `assoc_to_list/2`, `assoc_to_keys/2`, and `assoc_to_values/2`.
- **Pairs:** the exported query goals `pairs_keys_values/3`, `pairs_keys/2`,
  and `pairs_values/2`.
- **Formatted output:** the exported query goals `format/1`, `format/2`,
  `format/3`, `tab/1`, `tab/2`, `print/1`, and `print/2`.
- **Modules and reflection:** `current_module/1`; and the extra arities
  `findall/4` (with a difference-list tail) and `term_variables/3` (with a
  tail).
- **Process control:** `halt/0`, `halt/1` (raise the `prolog-halt` condition).

!!! note "Exports and the Lisp API"
    cl-prolog-kit keeps a narrow exported package surface. The exported query-goal
    names above are interned as `cl-prolog-kit` symbols, but they denote Prolog
    relations when used in a Lisp-shaped query; they are not Common Lisp
    functions to call directly. The remaining goals in this section are
    available through parsed source and the query-goal vocabulary without being
    exported package symbols. The [Rule DSL](../guide/rule-dsl.md) and
    [Extending the Engine](../guide/extending.md) pages cover the Lisp-facing API.

## DCG

- Grammar definition and execution: `def-dcg-rule`, `phrase`, `phrase-all`
- Combinators: `dcg-alt`, `dcg-opt`, `dcg-star`, `dcg-plus`,
  `dcg-error-recovery`
- Token matchers: `dcg-token-match`, `dcg-token-match-value`

DCG tokens are either a bare token-kind symbol or a `(kind . value)` cons.
`dcg-token-match` matches the kind and returns the remaining input;
`dcg-token-match-value` matches both kind and value. See [DCG](../guide/dcg.md).

## Conditions

The symbols below are the exported condition surface. For when each is
signalled and how to handle it, see
[Conditions and Errors](conditions.md).

- Depth configuration: `invalid-max-depth-error` and reader
  `invalid-max-depth-error-value`
- Depth exhaustion: `prolog-depth-limit-exceeded` and reader
  `prolog-depth-limit-exceeded-goal`
- Structurally invalid goals: `invalid-goal-error` and reader
  `invalid-goal-error-goal`
- Prolog throws: `prolog-exception` and reader `prolog-exception-term`
- Runtime hierarchy: `prolog-runtime-error`, `prolog-instantiation-error`,
  `prolog-type-error`, `prolog-domain-error`, `prolog-permission-error`,
  `prolog-existence-error`, `prolog-evaluation-error`,
  `prolog-resource-error`
- ISO categories: `prolog-representation-error` and `prolog-syntax-error`
  (see [Conditions and Errors](conditions.md))
- Process-level halt: `prolog-halt` and reader `prolog-halt-code`
- Arithmetic diagnostics: `arithmetic-evaluation-error` and readers
  `arithmetic-error-expression`, `arithmetic-error-reason`
- Parser resource limits: `prolog-parser-resource-error` and readers
  `prolog-parser-resource-error-resource`,
  `prolog-parser-resource-error-limit`,
  `prolog-parser-resource-error-observed`,
  `prolog-parser-resource-error-position` (see
  [Parser resource limits](#parser-resource-limits))

`prolog-halt` is not a `prolog-exception`, so `catch/3` does not intercept it.
The embedding application decides how to translate the condition into process
termination.

## Script Entry Points

- `nix run .` — run the cl-weave-backed ASDF regression suite on
  `x86_64-linux` or Apple Silicon (`aarch64-darwin`); use ASDF directly on
  other environments
- `asdf:load-system :cl-prolog-kit/examples` — load runnable examples
