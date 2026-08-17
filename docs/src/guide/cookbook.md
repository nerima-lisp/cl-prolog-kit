# Cookbook

Task-oriented recipes that combine the query API, the rule DSL, and the builtin
goals. Each recipe is self-contained. For the reference behind them see
[Querying](querying.md), [Builtin Goals](builtin-goals.md), and
[Rule DSL](rule-dsl.md).

All snippets assume you are in the `cl-prolog-kit` package:

```lisp
(in-package #:cl-prolog-kit)
```

## Collect all children of a parent

Use `findall/3` to gather every solution of a subgoal into a list:

```lisp
(define-rulebase *family*
  ((parent tom bob))
  ((parent tom liz))
  ((parent bob alice)))

(query-prolog *family* '(findall ?child (parent tom ?child) ?children))
;; => (((?CHILDREN BOB LIZ)))
```

`bagof/3` and `setof/3` are the grouping and sorted-deduplicated variants.

## Count solutions

Combine `findall/3` with `length/2` to count without writing a counter:

```lisp
(query-prolog *family*
  '(and (findall ?c (parent tom ?c) ?cs)
        (length ?cs ?n)))
;; ?N is bound to the number of children of tom
```

## Test membership without enumerating

`memberchk/2` commits to the first match, so it answers a yes/no membership
question without leaving choice points:

```lisp
(query-prolog (make-rulebase) '(memberchk b (a b c)))
;; => (nil)   -- succeeds once
```

Use plain `member/2` when you *want* to enumerate every matching element.

## Run `append/3` backwards

Relational predicates work in every mode. Splitting a list into all
prefix/suffix pairs is just `append/3` with the whole list fixed:

```lisp
(query-prolog (make-rulebase) '(append ?prefix ?suffix (a b c)))
;; => every way to split (a b c)
```

## Generate a numeric range

`numlist/3` from `library(lists)` builds an ascending integer list:

```lisp
(query-prolog (make-rulebase) '(numlist 1 5 ?xs))
;; => (((?XS 1 2 3 4 5)))
```

Sum it with `sum_list/2`:

```lisp
(query-prolog (make-rulebase)
  '(and (numlist 1 100 ?xs) (sum_list ?xs ?total)))
;; ?TOTAL is 5050
```

## Map a goal across a list

`maplist/N` from `library(apply)` applies a goal across one to four lists in
lockstep. Pair it with a foreign predicate or a user rule:

```lisp
(define-rulebase *db*
  ((double 1 2))
  ((double 2 4))
  ((double 3 6)))

(query-prolog *db* '(maplist double (1 2 3) ?doubled))
;; => (((?DOUBLED 2 4 6)))
```

`foldl/N` threads an accumulator; `include/3`, `exclude/3`, and `partition/4`
filter by whether the goal succeeds on each element.

## Filter with a guard

A `:when` guard fires a rule only when a Lisp condition over bound values
holds:

```lisp
(define-rulebase *scores*
  ((score bob 42))
  ((score amy 8))
  ((score cal 25))
  ((passing ?who) (score ?who ?n) (:when (>= ?n 20))))

(query-prolog *scores* '(passing ?who))
;; => (((?WHO . BOB)) ((?WHO . CAL)))
```

See [Arithmetic and Comparison](arithmetic.md) for the guard vs. `#=/2` vs.
`is/2` distinctions.

## Extend a rulebase without mutating it

`extend-rulebase` returns a *new* rulebase with additional clauses, leaving the
base untouched:

```lisp
(define-rulebase *base*
  ((color apple red)))

(defparameter *extended*
  (extend-rulebase *base*
    ((color lime green))))

(query-prolog *extended* '(color ?fruit ?color))  ; sees both
(query-prolog *base*     '(color ?fruit ?color))  ; sees only apple
```

## Mutate a rulebase deliberately

When you *do* want mutation, the dynamic-database goals insert and retire
clauses in the rulebase passed to the query:

```lisp
(query-prolog *family* '(assertz (parent tom eve)))
(query-prolog *family* '(parent tom ?child))   ; now includes EVE
```

Born/died revisions give logical-update semantics: a running query proceeds
over its snapshot even as the database changes. Use `copy-rulebase` first when
you need isolation. See [Architecture](../reference/architecture.md#explicit-dynamic-mutation).

## Bind variables from the first solution

`with-prolog-query` destructures the first solution's bindings into Lisp
variables; `prolog-match` dispatches over queries like `cond`:

```lisp
(with-prolog-query (?who) (*family* (ancestor tom ?who))
  (format t "~&first ancestor: ~S~%" ?who))
```

## Bound (streaming) search over a large space

For a potentially large search, stream solutions and stop early rather than
collecting them all:

```lisp
(map-prolog-solutions
 (lambda (solution)
   (format t "~&=> ~S~%" (solution-binding '?who solution)))
 *family* '(ancestor tom ?who))
```

Or cap the count directly with `:limit`:

```lisp
(query-prolog *family* '(ancestor tom ?who) :limit 2)
```

## Bound recursion depth

`:max-depth` bounds user-rule expansion so a runaway recursive rule fails loudly
with `prolog-depth-limit-exceeded` instead of looping:

```lisp
(query-prolog *family* '(ancestor tom ?who) :max-depth 3)
```

See [Semantics](../reference/semantics.md) and [Conditions and Errors](../reference/conditions.md) for
what each depth value means.
