# Builtin Goals

The list below is a representative, task-oriented overview, not an exhaustive
list. The authoritative list of builtin symbols exported to Lisp callers is
the [API Reference](../reference/api.md#builtin-goal-symbols). Parsed Prolog
source also uses implemented goal names that need not be exported as Common
Lisp package symbols.

!!! tip "Related pages"
    Arithmetic has a dedicated page:
    [Arithmetic and Comparison](arithmetic.md). To add your own goals, see
    [Extending the Engine](extending.md). The conditions these goals raise are
    catalogued in [Conditions and Errors](../reference/conditions.md).

- **Unification:** `(= a b)`, `(\= a b)`, and
  `(unifiable a b ?unifier)` unify two terms, require that they cannot unify,
  or return a unifier without binding either input. `=/2` honors the
  `occurs_check` flag (`true` default fails on a cycle, `false` allows a cyclic
  binding, `error` raises); `(unify_with_occurs_check a b)` always checks.
- **Control:** `!`, `(not g)`, `(and g...)`, and `(or g...)` provide cut,
  negation as failure, conjunction, and disjunction.
- **Meta-call:** `(call g)`, `(once g)`, `(ignore g)`, `(not g)`,
  `(forall c a)`, and `(repeat)` invoke goals, control their solutions, or
  generate repeated proofs.
- **Collection:** `(findall t g ?bag)`, `(bagof t g ?bag)`, and
  `(setof t g ?set)` collect templates. `bagof` groups free variables, while
  `setof` also removes duplicates and sorts.
- **Dynamic database:** `(asserta c)`, `(assertz c)`, `(retract c)`,
  `(abolish (p / n))`, and `(clause h b)` inspect or mutate clauses in the
  rulebase passed to the query. A clause may be a fact, the Lisp shape
  `(:- HEAD GOAL...)`, or the `:-`/2 term Prolog source text reads a rule as,
  so `assertz((h :- a, b))` asserts a rule either way.
- **Arithmetic:** `(is ?x expr)`, `(=:= a b)`, `(=\= a b)`, `(< a b)`,
  `(=< a b)`, `(> a b)`, and `(>= a b)` evaluate arithmetic expressions and
  compare their numeric values. See
  [Arithmetic and Comparison](arithmetic.md) for the operator tables.
- **Lists:** `(member ?x list)`, `(memberchk ?x list)`, `(append ?a ?b ?c)`,
  `(reverse ?a ?b)`, `(length ?l ?n)`, `(select ?x ?list ?rest)`,
  `(nth0 ?i ?list ?x)`, `(nth1 ?i ?list ?x)`, `(last ?list ?x)`, and
  `(is_list ?l)` define relations over proper and partially instantiated
  lists. `memberchk/2` commits to the first match. Unlike the list-library
  goals below, none of these is exported as a `cl-prolog-kit` symbol; they are
  source/query-goal vocabulary only (see
  [Prolog-Source and Query Goals](../reference/api.md#prolog-source-and-query-goals)).
- **List library (`library(lists)`):** `(sum_list ?l ?sum)` (alias
  `sumlist`), `(max_list ?l ?m)`, `(min_list ?l ?m)`, `(numlist ?lo ?hi ?l)`,
  `(list_to_set ?l ?set)` (deduplicates under `==`, preserving first
  occurrence), and `(permutation ?l ?p)` (enumerates permutations of the
  instantiated-list argument). `(subtract ?a ?b ?c)`, `(intersection ?a ?b ?c)`,
  and `(union ?a ?b ?c)` test membership by unifiability, discarding any
  bindings (identical to SWI on ground lists; see [Semantics](../reference/semantics.md)).
- **Apply library (`library(apply)`):** `(maplist g ?l1 ...)` applies `g`
  across one or more lists in lockstep (arity 2 and up; the common SWI surface
  is `/2..5`); `(foldl g ?l ?v0 ?v)` through `(foldl g ?l1 ?l2 ?l3 ?v0 ?v)`
  thread an accumulator; and `(include g ?l ?kept)`, `(exclude g ?l ?dropped)`,
  and `(partition g ?l ?in ?out)` filter by whether `g` succeeds on each
  element. Filter goals are tested for provability only, so their bindings do
  not leak into the result. All element goals are cut-opaque, like `call/1`.
- **Formatted output:** `(format fmt)`, `(format fmt args)`, and
  `(format stream fmt args)` render `fmt` — an atom, code list, character
  list, or Lisp string — with the directives `~w ~p ~q ~a ~s ~d ~D ~f ~e ~g
  ~r ~R ~c ~n ~~` and column control `~t ~| ~+`. The fill character for `~t`
  may be set with `` ~`c ``, and `~*` takes the count/column from the next
  argument. Malformed directives and out-of-range values raise catchable ISO
  errors, and attacker-sized repeat/fill counts raise `resource_error`
  (bounded by `*max-prolog-builtin-output-length*`). `(tab ?n)` / `(tab stream
  ?n)` writes `n` spaces; `(print ?t)` / `(print stream ?t)` writes a term
  quoted with `numbervars`.
- **Atoms and text:** `(atom_length ?atom ?len)`,
  `(atom_concat ?a ?b ?whole)`, `(sub_atom ?atom ?b ?l ?a ?sub)`,
  `(atom_chars ?atom ?chars)`, `(atom_codes ?atom ?codes)`,
  `(char_code ?char ?code)`, `(number_chars ?n ?chars)`,
  `(number_codes ?n ?codes)`, and `(atom_number ?atom ?n)` inspect and convert
  atoms, character lists, code lists, and numbers. These are ISO string
  predicates; they raise the corresponding instantiation or type error when
  under-instantiated or given the wrong term kind.
- **Character classification:** `(char_type ?char ?type)` and
  `(code_type ?code ?type)` test a character (one-char atom) or code against a
  type: the simple types `alpha alnum digit space/white upper lower punct graph
  graphic csym csymf ascii end_of_line/newline period`, and the parametric
  types `digit(W)`, `to_lower(X)`, `to_upper(X)`, `upper(Lower)`,
  `lower(Upper)`, `code(C)`. The character/code argument must be instantiated
  (these are not generative). `(upcase_atom ?a ?upper)` and
  `(downcase_atom ?a ?lower)` fold the case of any atomic source (atom or
  number).
- **Term ↔ text:** `(term_to_atom ?term ?atom)` renders a term to a
  re-readable atom, or parses an atom's text into a term; `(read_term_from_atom
  ?atom ?term ?options)` parses text into a term. Parser failures surface as
  catchable ISO `syntax_error/1` (or `resource_error/1`).
- **Strings (SWI):** a string is a distinct atomic type (written `"..."`; the
  `double_quotes` flag selects whether `"..."` reads as codes, chars, an atom,
  or a string). `(string ?x)` tests it; strings are `atomic` but not `atom`.
  `(string_length ?s ?n)`, `(string_concat ?a ?b ?c)` (relational),
  `(atom_string ?atom ?string)`, `(string_to_atom ?string ?atom)`,
  `(number_string ?n ?string)`, `(string_chars ?s ?chars)`,
  `(string_codes ?s ?codes)`, `(term_string ?term ?string)`,
  `(text_concat ?a ?b ?atom)`, `(sub_string ?s ?before ?len ?after ?sub)`, and
  `(split_string ?s ?sep-chars ?pad-chars ?parts)` convert and manipulate text;
  any of atom/string/number/code list/char list is accepted where text is
  expected.
- **Sorting and aggregation:** `(sort ?list ?sorted)` sorts under the standard
  order removing duplicates, `(msort ?list ?sorted)` keeps duplicates, and
  `(keysort ?pairs ?sorted)` stably sorts `Key-Value` pairs by key.
  `(sort ?key ?order ?list ?sorted)` sorts by an argument key (0 = whole term)
  under `@<`, `@=<`, `@>`, or `@>=` (`@<`/`@>` remove key-duplicates);
  `(predsort g ?list ?sorted)` sorts with a
  `call(g, Order, A, B)` comparison, dropping `=` duplicates; and
  `(aggregate_all ?template g ?result)` reduces a goal's solutions via
  `count`, `count(T)`, `sum(E)`, `max(E)`, `min(E)`, `bag(T)`, or `set(T)`.
- **Finite domains:** `(#= a b)`, `(#\= a b)`, `(#< a b)`, `(#=< a b)`,
  `(#> a b)`, `(#>= a b)`, `(in x range)`, and `(indomain x)` constrain
  integers and enumerate finite-domain solutions.
- **Association maps (`library(assoc)`):** `(empty_assoc ?a)`,
  `(put_assoc ?key ?a ?value ?new)`, `(get_assoc ?key ?a ?value)`,
  `(del_assoc ?key ?a ?value ?new)`, `(list_to_assoc ?pairs ?a)`,
  `(assoc_to_list ?a ?pairs)`, `(assoc_to_keys ?a ?keys)`, and
  `(assoc_to_values ?a ?values)` maintain a key→value map ordered by the
  standard order of terms (keys may be any ground term).
- **Pairs (`library(pairs)`):** `(pairs_keys_values ?pairs ?keys ?values)`
  (usable either direction), `(pairs_keys ?pairs ?keys)`, and
  `(pairs_values ?pairs ?values)` relate a `Key-Value` list to its keys/values.
- **Integer relation:** `(plus ?a ?b ?c)` is `A + B =:= C` and solves for any
  single unbound argument.
- **Lisp guard:** `(:when fn ?x...)` calls a Lisp predicate with solved values.

Arithmetic expressions use prefix Lisp-shaped terms — the operator tables and
comparison goals are documented in
[Arithmetic and Comparison](arithmetic.md).

Collection and dynamic-database goals can be used like any other query:

```lisp
(query-prolog *family* '(findall ?child (parent tom ?child) ?children))
;; => (((?CHILDREN BOB)))

(query-prolog *family* '(assertz (parent tom eve)))
(query-prolog *family* '(parent tom ?child))
;; includes EVE

(query-prolog (make-rulebase) '(is ?total (+ 20 (* 2 11))))
;; => (((?TOTAL . 42)))
```

## Relational list length

`length/2` can check a proper list, bind its length, complete a partial list to
a requested non-negative length, or generate lists of increasing length when
both arguments are variables. A non-integer length raises `type_error(integer,
...)`; a negative length raises `domain_error(not_less_than_zero, ...)`.
Cyclic lists fail rather than making the solver loop.

## Meta-call validation

Goals executed by `call/1`, `once/1`, `ignore/1`, negation, `forall/2`,
conditionals, `catch/3`, and cleanup predicates must be callable after applying
the current bindings. Invalid goals raise the corresponding ISO instantiation
or callable type error. Validation happens when that goal is actually reached,
so an unreachable branch is not inspected eagerly.

## Finite-domain equality

`#=/2` binds an unconstrained variable when the other operand is an integer;
the binding is visible to ordinary unification and later goals. Comparing a
variable with itself succeeds for `#=`, `#=<`, and `#>=`, and fails for `#\=`,
`#<`, and `#>`.

## Term and stream I/O

`read_term/3` supports `variable_names/1` and `singletons/1`. The latter
returns `Name=Variable` entries for named variables that occur exactly once;
the anonymous variable `_` is omitted.

For input streams, `stream_property/2` reports `end_of_stream(not)`,
`end_of_stream(at)`, or `end_of_stream(past)`. Peeking at EOF establishes
`at`; an attempted consuming read advances it to `past`. Repositioning a
stream resets the state to `not`.

## Structurally unusable goals

A non-function `:when` guard, or a goal that is not a symbol or symbol-headed
list, signals `invalid-goal-error` (`invalid-goal-error-goal` returns the
offending goal). Calling a known name at an unsupported arity denotes a
different, undefined procedure and signals the ISO `existence_error` instead.
`halt/0` and `halt/1` raise the `prolog-halt` condition (readers:
`prolog-halt-code`); it is deliberately not a `prolog-exception`, so
`catch/3` never intercepts it and the embedding application decides how to
exit.

Prolog numbers are integers and floating-point values. Common Lisp ratios and
complex values are not accepted by numeric or atomic term predicates and do
not receive numeric term ordering.

## Extending the builtin set

`define-foreign-predicate` is the public extension API. It registers one exact
predicate name/arity pair and participates in proof search through the engine's
`emit` continuation protocol:

```lisp
(define-foreign-predicate (twice input output)
    (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (let ((value (logic-substitute input environment)))
    (when (numberp value)
      (multiple-value-bind (extended ok) (unify output (* 2 value) environment)
        (when ok (funcall emit extended))))))
```

The internal builtin-definition machinery is not exported. The full protocol,
dispatch rules, and more examples live on the
[Extending the Engine](extending.md) page.
