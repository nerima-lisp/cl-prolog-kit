# Rule DSL

cl-prolog-kit is **macro-first**: macros own the surface syntax and produce plain
clause data that the engine walks. This page covers the authoring forms for
building and extending rulebases. For the goals that go *inside* clauses, see
[Builtin Goals](builtin-goals.md); for extension via Lisp, see
[Extending the Engine](extending.md).

## Clause shape

A clause is written as a list. Its first element is the head; the rest are body
goals.

- `((pred args...))` — a **fact** (a one-element clause, empty body).
- `(head goal...)` — a **rule** (a head clause followed by body goals).

```lisp
((parent tom bob))                                ; fact
((ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y)) ; rule
```

Logic variables are `?`-prefixed symbols. Facts and rules share one
definition-ordered sequence per predicate — see
[Semantics](../reference/semantics.md#proof-search-semantics).

## Building rulebases

`define-rulebase` creates a named rulebase (it is `defparameter` plus `prolog`).
`extend-rulebase` returns a **new** rulebase with additional clauses, leaving
the base object unchanged:

```lisp
(define-rulebase *base*
  ((color apple red)))

(defparameter *extended*
  (extend-rulebase *base*
    ((color lime green))))

(query-prolog *extended* '(color ?fruit ?color))  ; sees apple and lime
(query-prolog *base*     '(color ?fruit ?color))  ; sees only apple
```

`prolog` is the underlying builder when you want an anonymous rulebase value:

```lisp
(defparameter *rb*
  (prolog
    ((parent tom bob))
    ((score bob 42))
    ((rich ?x) (score ?x ?n) (:when (> ?n 10)))))
```

## Available forms

| Form                 | Purpose                                                    |
| -------------------- | ---------------------------------------------------------- |
| `prolog`             | Build a rulebase value from clauses.                       |
| `define-rulebase`    | `defparameter` + `prolog` — a named rulebase.              |
| `extend-rulebase`    | Functional extension; new clauses shadow the base.         |
| `def-rule`           | Define a reusable clause-producing form.                   |
| `with-prolog-query`  | Bind variables from the first solution.                    |
| `prolog-match`       | `cond` over queries.                                       |

`with-prolog-query` destructures the first solution's bindings into Lisp
variables, and `prolog-match` chooses a branch by which query proves first:

```lisp
(with-prolog-query (?who) (*base* (color ?who red))
  (format t "~&red thing: ~S~%" ?who))
```

## Guards with `:when`

In the `prolog` / `def-rule` DSL you write guards as **expressions** —
`(:when (> ?n 10))` — and the macro compiles them to closures at definition
time. The engine never calls `eval` on user data at runtime; instead the DSL
lowers the guard to a `(:when function variable...)` goal that the solver
invokes over already-bound values.

```lisp
(define-rulebase *scores*
  ((score bob 42))
  ((score amy 8))
  ((rich ?x) (score ?x ?n) (:when (> ?n 10))))

(query-prolog *scores* '(rich ?who))
;; => (((?WHO . BOB)))
```

A guard is for a **pure Lisp predicate** over bound values. When you need a goal
that unifies outputs or emits several solutions, reach for a foreign predicate
instead. See [Architecture](../reference/architecture.md#guards) for the compilation model.

## Foreign predicates

`define-foreign-predicate` is the supported way to add new goals in Lisp.
Foreign predicates use the engine's CPS `emit` protocol and dispatch by exact
name and arity:

```lisp
(define-foreign-predicate (choose output) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (dolist (value '(left right))
    (multiple-value-bind (extended ok) (unify output value environment)
      (when ok (funcall emit extended)))))
```

The full protocol — `emit`, `unify`'s two-value contract, dispatch rules, and
more examples — is documented in [Extending the Engine](extending.md).

## Dynamic vs. functional construction

The default authoring style is immutable: `prolog` and `extend-rulebase`
produce values without touching a shared database. Mutation is still available
and deliberate — `asserta/1`, `assertz/1`, `retract/1`, and friends operate on
the rulebase passed to a query, with logical-update semantics. See the
[Cookbook](cookbook.md#mutate-a-rulebase-deliberately) and
[Architecture](../reference/architecture.md#explicit-dynamic-mutation).

## Next steps

- [Builtin Goals](builtin-goals.md) — the goal vocabulary for clause bodies.
- [DCG Grammars](dcg.md) — a grammar-rule front end over the same engine.
- [Extending the Engine](extending.md) — foreign predicates in depth.
