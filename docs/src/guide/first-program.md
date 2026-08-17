# Your First Program

This tutorial builds a small family-tree knowledge base and queries it from
several angles. It mirrors the shipped `examples/family-tree.lisp` program (see
[Examples](examples.md)) but explains each step. By the end you will know how
to state facts, derive new relations with rules, and choose the right query
entry point.

## 1. Load and enter the package

```lisp
(require :asdf)
(asdf:load-asd (truename "cl-prolog-kit.asd")) ; run from the repository root
(asdf:load-system :cl-prolog-kit)
(in-package #:cl-prolog-kit)
```

## 2. State the facts

Facts are one-element clauses. Here `parent/2` records who is a parent of whom:

```lisp
(define-rulebase *family*
  ((parent tom bob))
  ((parent bob alice))
  ((parent alice eve)))
```

`define-rulebase` is `defparameter` plus rulebase construction — it binds the
special variable `*family*` to a rulebase value. There is no global,
process-wide database; every query takes its rulebase explicitly. See the
[Rule DSL](rule-dsl.md) for the full family of authoring forms.

## 3. Derive a new relation with rules

`ancestor/2` is not a fact — it is *derived* from `parent/2`. Read the two
clauses as: "X is an ancestor of Y if X is a parent of Y, **or** if X is a
parent of some Z who is an ancestor of Y."

```lisp
(define-rulebase *family*
  ((parent tom bob))
  ((parent bob alice))
  ((parent alice eve))
  ((ancestor ?x ?y) (parent ?x ?y))                     ; base case
  ((ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y)))   ; recursive case
```

- `?x`, `?y`, `?z` are logic variables.
- The clause head is the relation being defined; the goals after it are its
  body.
- Clauses are tried in definition order, so the base case is attempted first.
  See [Semantics](../reference/semantics.md) for the exact search order.

## 4. Ask for every solution

```lisp
(query-prolog *family* '(ancestor tom ?who))
;; => (((?WHO . BOB)) ((?WHO . ALICE)) ((?WHO . EVE)))
```

`query-prolog` collects **all** solutions into a list. Each solution is an
alist binding the query's variables. Tom is an ancestor of Bob (directly),
and of Alice and Eve (transitively).

## 5. Ask for just the first solution

```lisp
(query-prolog-first *family* '(ancestor tom ?who))
;; => ((?WHO . BOB))
```

`query-prolog-first` stops at the first proof and returns that single solution,
or `nil` if there is none. It is the right tool when you only need one answer.

## 6. Ask a yes/no question

```lisp
(prolog-succeeds-p *family* '(parent tom bob))
;; => T

(prolog-succeeds-p *family* '(parent bob tom))
;; => NIL
```

`prolog-succeeds-p` returns a boolean and stops at the first proof. Use it for
membership-style checks where you do not need the bindings.

## 7. Run goals as they are proven

For large or streaming searches, `map-prolog-solutions` calls your function
once per solution *as it is found*, without collecting a list:

```lisp
(map-prolog-solutions
 (lambda (solution)
   (format t "~&tom is an ancestor of ~S~%"
           (solution-binding '?who solution)))
 *family* '(ancestor tom ?who))
```

`solution-binding` looks up one variable in a solution alist.

## What you learned

- Facts are one-element clauses; rules are a head plus body goals.
- Rules can be recursive and are tried in definition order.
- `query-prolog` collects all solutions, `query-prolog-first` returns one,
  `prolog-succeeds-p` answers yes/no, and `map-prolog-solutions` streams.

## Next steps

- [Querying](querying.md) — every entry point, its keywords, and result shapes.
- [Builtin Goals](builtin-goals.md) — arithmetic, lists, collection, and more.
- [Cookbook](cookbook.md) — recipes that combine these pieces.
