# Examples

The repository ships three runnable programs under `examples/`. They are loaded
as the `cl-prolog-kit/examples` ASDF system, which loads the library first and then
executes all three files:

```sh
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --eval '(asdf:load-asd (truename "cl-prolog-kit.asd"))' \
  --eval '(asdf:load-system :cl-prolog-kit/examples)'
```

!!! note "Not standalone scripts"
    The example files are **not** standalone scripts. Invoking one directly
    with `sbcl --script` does not load the `cl-prolog-kit` package. Load them
    through the `cl-prolog-kit/examples` system, which is also exercised by the
    `checks.examples` Nix check ([Development](../project/development.md)).

## `quick-start.lisp` — the smallest useful program

```lisp
(in-package #:cl-prolog-kit)

(define-rulebase *family*
  ((parent tom bob))
  ((parent bob alice))
  ((ancestor ?x ?y) (parent ?x ?y))
  ((ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y)))

(format t "~&quick-start ancestor(tom, ?who) => ~S~%"
        (query-prolog *family* '(ancestor tom ?who)))
```

Two `parent/2` facts and a recursive `ancestor/2` definition. `query-prolog`
returns every solution as a list of binding alists.

## `family-tree.lisp` — three ways to query

```lisp
(in-package #:cl-prolog-kit)

(define-rulebase *family*
  ((parent tom bob))
  ((parent bob alice))
  ((parent alice eve))
  ((ancestor ?x ?y) (parent ?x ?y))
  ((ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y)))

(format t "~&ancestor(tom, ?who) => ~S~%"
        (query-prolog *family* '(ancestor tom ?who)))
(format t "first ancestor(tom, ?who) => ~S~%"
        (query-prolog-first *family* '(ancestor tom ?who)))
(format t "parent(tom, bob) succeeds => ~S~%"
        (prolog-succeeds-p *family* '(parent tom bob)))
```

The same rulebase, queried three ways:

- `query-prolog` — all solutions as a list.
- `query-prolog-first` — the first solution, or `nil`.
- `prolog-succeeds-p` — a boolean yes/no.

This is the program walked through step by step in
[Your First Program](first-program.md).

## `relational-lists.lisp` — list relations without any clauses

```lisp
(in-package #:cl-prolog-kit)

(let ((rulebase (make-rulebase)))
  (format t "~&append(?l ?r (a b c)) => ~S~%"
          (query-prolog rulebase '(append ?l ?r (a b c))))
  (format t "reverse(?xs (c b a)) => ~S~%"
          (query-prolog rulebase '(reverse ?xs (c b a))))
  (format t "length(?xs 3) => ~S~%"
          (query-prolog-first rulebase '(length ?xs 3)))
  (format t "member(?x (a b c)) => ~S~%"
          (query-prolog rulebase '(member ?x (a b c)))))
```

`make-rulebase` builds an **empty** rulebase. The list predicates are builtins,
so they work with no user clauses at all:

- `append/3` run "backwards" enumerates every way to split `(a b c)` into a
  prefix `?l` and suffix `?r`.
- `reverse/2` binds `?xs` to the reverse of `(c b a)`.
- `length/2` with an unbound list and a fixed length generates a list of three
  fresh variables — see the relational `length/2` notes in
  [Builtin Goals](builtin-goals.md#relational-list-length).
- `member/2` enumerates each element of `(a b c)` in turn.

## Where to go next

- [Cookbook](cookbook.md) — more task-oriented recipes.
- [Builtin Goals](builtin-goals.md) — the full builtin catalogue.
- [Development](../project/development.md) — how the query test helpers assert on these shapes.
