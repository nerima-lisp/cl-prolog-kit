# Semantics

This page states the operational rules the engine follows. For the machinery
behind them, see [Architecture](architecture.md); for the exported symbols, see
the [API Reference](api.md).

## Core rules

- **Clause order** — clauses are tried in definition order. Facts and rules
  share one ordered sequence per predicate; a fact (a clause with an empty
  body) is **not** tried ahead of a rule defined before it.
- **Cut** (`!`) — prunes the running clause's remaining choice points and the
  predicate's remaining rule clauses. It is local to the predicate invocation.
- **Optional depth bound** — rule resolution is unbounded by default. Set
  `:max-depth` to a non-negative integer to bound user-rule resolution;
  exhaustion signals `prolog-depth-limit-exceeded` rather than masquerading as
  logical failure.
- **Occurs check** — on during clause resolution, so the engine never
  introduces a cyclic term behind your back; the `occurs_check` flag (default
  `true`) governs `=/2` and `\=/2` specifically, see
  [below](#occurs_check-and-cyclic-terms). Host-provided cyclic cons structures
  are handled safely either way: unification compares them coinductively,
  substitution preserves cycles and sharing, and variable collection
  terminates.

## Atoms

**An atom is its text.** Two atoms are the same atom exactly when they print
the same, and `=/2`, `==/2`, `compare/3`, and `sort/2` all agree on that.
Quoting is therefore invisible, as ISO 13211-1 6.4.2 requires — `hello ==
'hello'` — while case is significant: `'FooBar'`, `fooBar`, and `foobar` are
three distinct atoms, each of which `writeq/1` renders so that reading the
output back yields it again.

`[]` is an atom (ISO 6.3.5) of text `"[]"`, represented as Common Lisp `NIL`,
so `[] == '[]'` holds and `atom_length([], 2)` succeeds.

### Atoms in Lisp-authored rulebases

An atom is an ordinary Common Lisp symbol, which is what lets a Lisp-authored
rulebase and a Prolog-authored one denote the same predicate: `(parent ?x ?y)`
read by the Common Lisp reader and `parent(X, Y).` read by this engine's parser
both arrive as `CL-PROLOG-KIT::PARENT`. The consequence is that a **symbol's name
is not its atom's text**: the text of `CL-PROLOG-KIT::PARENT` is `"parent"`.

Write a bare symbol for any atom whose text is lower case. For anything else,
call `prolog-atom` rather than writing a symbol literal, and `prolog-atom-text`
to go back:

```lisp
(prolog-atom "FooBar")             ; the atom Prolog source spells 'FooBar'
(prolog-atom-text (prolog-atom "FooBar"))
;; => "FooBar"
```

`cl-prolog-kit::|FooBar|` is *not* that atom — its text is `"foobar"`, because the
bar syntax only escapes the Common Lisp reader, which is a different question
from what the atom is called. Inside quoted data, where `prolog-atom` cannot be
called, `#.(cl-prolog-kit:prolog-atom "FooBar")` reads it at read time.

## Proof-search semantics

- Registered builtins and foreign predicates are **authoritative** for their
  predicate indicator; otherwise the predicate's clauses are resolved in
  definition order, with facts and rules interleaved as written.
- Rule variables (and variables inside facts) are **freshly renamed per use**,
  so bindings never leak between two uses of the same clause.
- An explicit depth bound decrements **per user-rule resolution**, so left
  recursion terminates with the solutions found within the bound.

### What `:max-depth` counts

Depth decreases when proof enters a **user rule**, not for every builtin or
unification step:

- `0` — facts and builtins only; no rule expansion.
- `1` — one level of rule expansion.
- larger values — deeper derived proofs.

`nil` means unbounded. See [Querying](../guide/querying.md#depth-bounds) for the caller's
view and [Conditions and Errors](conditions.md) for the exhaustion condition.

## Cut in detail

Cut uses dynamically scoped `catch`/`throw` tags:

- Each user-predicate invocation establishes a fresh cut barrier.
- `!` emits the current state and throws to that invocation's tag, abandoning
  the invocation's remaining alternatives.
- Opaque meta-calls establish their own barrier; transparent control constructs
  deliberately reuse the caller's barrier.

This keeps a rule-body cut local to the predicate invocation while preserving
the transparency of control constructs. See
[Architecture](architecture.md#cut).

## Modules

Unqualified calls first resolve predicates in the current module and then its
imports. `current_predicate/1` follows the same visible-predicate view,
including imported predicates. In a qualified call `Module:Goal`, `Module` is
resolved through the current bindings and must become an atom naming an existing
module; violations raise catchable ISO instantiation, type, or existence errors.

## Tabling

A predicate declared with a `:- table name/arity` directive resolves through a
memo table instead of plain clause resolution. The engine also detects left
recursion automatically and routes it through the same table, so a
left-recursive definition terminates with the answers reachable within the
query.

- Answers are keyed **up to variance**, so calls that differ only in variable
  naming share memoized results.
- The table is shared by every query against the current rulebase **revision**,
  not just the query that first populated it, and is invalidated the next time
  the rulebase mutates (`assert`/`retract`/etc.).
- Depth-limited searches and active finite-domain constraints **bypass** the
  table where memoization would be unsound.

Tabling has no exported Common Lisp symbols — it is engaged only through the
source directive. See [Architecture](architecture.md#depth-and-tabling).

## Parser resource limits

Reading Prolog *text* is bounded by configurable resource limits (source length,
nesting depth, token count, lexeme sizes, and interned-symbol count). Exceeding
one signals `prolog-parser-resource-error` from the direct reader APIs, or a
catchable ISO `resource_error/1` term when the limit is hit while a
`consult`/`load_files` goal runs. See [Parser Resource Limits](parser-limits.md)
for the full list of specials and their defaults.

## Library-predicate divergences from SWI

The `library(lists)` and `library(apply)` predicates aim for SWI compatibility
but differ deliberately in a few edge modes:

- **`subtract/3`, `intersection/3`, `union/3`** test membership by
  *unifiability* but discard the successful unification's bindings, whereas
  SWI's `memberchk`-based versions propagate them. On ground lists the results
  are identical; the divergence only shows when a list element is an unbound
  variable, where these predicates never bind the caller's variable.
- **`permutation/2`** requires at least one argument to be a proper list and
  raises `instantiation_error` when neither is; SWI's clause-based version can
  instead loop or partially unify. Results and solution order match SWI for the
  common `permutation(+List, -Perm)` mode.
- **`format/2` `~e` and `~g`** are rendered through the host and then
  normalized to the C/Prolog exponent convention (`3.14e+0`); exact digit and
  exponent-width formatting may differ slightly from SWI.
- **`char_type/2` and `code_type/2` are input-only** on the character/code
  argument (it must be instantiated) — they test or extract, but do not
  generate a character from a type. `char_type/2`'s `to_lower`/`to_upper`/
  `upper`/`lower` yield a one-character atom (SWI yields a code even here);
  `code_type/2` yields codes. `graphic` is the Prolog symbol-char class
  (distinct from `graph`), and `space`/`white` and `end_of_line`/`newline` are
  treated as synonyms.
- **`aggregate_all/3`** evaluates the `sum`/`max`/`min` template as an
  arithmetic expression (so `sum(X*2)` works), but does not support the
  `max(Expr, Witness)`/`min(Expr, Witness)` witness forms. `read_term_from_atom/3`
  validates but ignores its options list (`variable_names` etc. are not
  honored). `predsort/3` fails (rather than raising) when its comparison
  predicate has no proof, matching SWI.

## ISO conformance

`t/iso-conformance-test.lisp` states the ISO 13211-1 requirements this engine is
checked against as goals in Prolog source text, each citing its subclause, so a
regression names the requirement it broke. The knowingly divergent cases are
asserted there too, so a change of behavior surfaces as a failure rather than
drifting:

- **A non-empty `{T}` is not `'{}'(T)`** (ISO 6.3.6). It is the engine's internal
  `brace` term, which DCG bodies are built on. A bare `{}` *is* the atom of that
  name.
- **A functor is not required to be followed immediately by `(`** (ISO 6.3.3), so
  a bare `+(1,2)` reads as the prefix operator `+` applied to `(1,2)`. The writer
  compensates by quoting a compound's functor unless it is a plain atom name,
  which keeps `write_canonical/1` output re-readable.
- **`,`/2, `;`/2 and `\+`/1 are represented internally as `and`, `or` and
  `not`**, so `write_canonical((a;b))` emits `or(a,b)`. That reads back as the
  same term here, but is not portable to another system.
- **`occurs_check` defaults to `true`**, so `X = f(X)` fails where ISO would let
  it build a cyclic term — see below.
- **The character set is the host's, so Unicode.** ISO 6.5 leaves it
  implementation-defined; a code past 255 is a character here rather than a
  `representation_error`, which is what lets an atom hold text in any script.
  The 1999 conformance corpus assumes a 256-character set and expects the error
  on two of its cases; keeping Unicode is a deliberate choice, asserted in
  `t/iso-conformance-test.lisp`.

## occurs_check and cyclic terms

Unification during clause resolution always performs the occurs check, so the
engine never builds a cyclic term behind your back. The `occurs_check` flag
(default `true`) controls `=/2` and `\=/2` specifically: `false` lets `X = f(X)`
succeed with a cyclic binding, and `error` raises when a cycle would form.
Substitution, the standard-order comparison, and the term writer all handle
cyclic structures safely — the writer marks each cons on the current path and
prints `...` (compound argument) or `|...]` (cyclic list tail) on revisiting one,
so a cyclic term prints in bounded time.  That `...` output is not re-readable,
so a cyclic term written with `writeq`/`write_canonical` cannot be read back —
building cyclic terms (only possible under `occurs_check=false`) trades away
re-readability.
`unify_with_occurs_check/2` always checks regardless of the flag. This differs
from SWI, where the flag is global and defaults to `false`; here the default is
`true` and the flag is scoped to `=/2`/`\=/2`, keeping the core cycle-free by
default (the writer being cycle-safe means the default could be flipped to
`false` in a future release without introducing a non-terminating print).

## Strings

A string is a distinct atomic term type. The `double_quotes` flag
(`codes` default, `chars`, `atom`, `string`) selects how a `"..."` literal is
read; it is honored by `read_term/2,3`, `consult`/`load_files`,
`read_term_from_atom/3`, `term_to_atom/2`, and `term_string/2`. In the standard
order of terms strings sort between atoms and compound terms; `==`/`compare/3`
compare string content. `atom_string/2` and friends coerce freely between text
kinds. The direct Lisp reader APIs default to `codes` regardless of any
rulebase flag.

## library(assoc) representation

`library(assoc)` associations are keyed by the standard order of terms, but the
representation diverges from SWI: an assoc here is the ordinary, inspectable,
unifiable compound `assoc(SortedPairList)` (the empty assoc is `assoc([])`),
not SWI's opaque AVL `t/…` term. Operations are therefore O(n) rather than
O(log n), and a program can (but should not) inspect or forge the structure —
a malformed `assoc(...)` is rejected with a catchable `type_error`. The
implemented subset is `empty_assoc/1`, `get_assoc/3`, `put_assoc/4`,
`del_assoc/4`, `list_to_assoc/2` (which rejects duplicate keys), `assoc_to_list/2`,
`assoc_to_keys/2`, `assoc_to_values/2`; `get_assoc/3` is a semidet lookup (it
does not enumerate keys on backtracking).

## Builtin output resource limit

Builtins that materialize a character run or list from a small input — format
fill/repeat/newline runs (`~t`, `~|`, `~Nc`, `~Nn`), `tab/1`, and `numlist/3`
ranges — are bounded by `*max-prolog-builtin-output-length*`. Exceeding it
raises a catchable ISO `resource_error/1`, preventing attacker-controlled
allocation from a tiny query.

## See also

- [Architecture](architecture.md) — how cut, guards, and tabling are
  implemented in the CPS engine.
- [Conditions and Errors](conditions.md) — the conditions these rules raise.
