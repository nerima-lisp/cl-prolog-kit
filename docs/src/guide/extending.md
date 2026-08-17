# Extending the Engine

cl-prolog-kit exposes exactly one supported extension point: **foreign
predicates**, defined with `define-foreign-predicate`. A foreign predicate is a
Lisp function that participates in proof search using the engine's
continuation-passing `emit` protocol. The internal builtin-definition machinery
is deliberately *not* exported — foreign predicates are the public surface.

## The shape of a foreign predicate

```lisp
(define-foreign-predicate (name argument...)
    (rulebase environment depth emit)
  body...)
```

- `(name argument...)` is the predicate indicator. The argument list has
  **fixed arity**; lambda-list keywords such as `&rest` are not supported. One
  definition registers exactly one name/arity pair.
- `rulebase`, `environment`, and `depth` expose the current solver context.
- `emit` is the continuation. Call it **once per solution**, passing the
  environment that represents that solution. Call it zero times to fail. Never
  collect results into a list — stream them through `emit`. It is a synchronous,
  dynamically scoped multi-shot continuation: call it only before the foreign
  predicate returns. Do not retain it, invoke it from another thread, or invoke
  it after returning.

## A worked example: `twice/2`

`twice/2` succeeds when its second argument is double the first:

```lisp
(define-foreign-predicate (twice input output)
    (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (let ((value (logic-substitute input environment)))
    (when (numberp value)
      (multiple-value-bind (extended ok) (unify output (* 2 value) environment)
        (when ok (funcall emit extended))))))
```

Step by step:

1. `logic-substitute` applies the current environment to `input`, resolving any
   bound variables to their values.
2. The `when (numberp value)` guard means the predicate simply *fails* (emits
   nothing) if `input` is not yet a number.
3. `unify` attempts to bind `output` to `(* 2 value)`. It returns two values:
   the extended environment and a success flag.
4. On success, `emit` is called once with the extended environment — that is
   one solution.

## Emitting many solutions

Because `emit` is a continuation, a predicate can produce several solutions by
calling it more than once. `choose/1` emits two:

```lisp
(define-foreign-predicate (choose output) (rulebase environment depth emit)
  (declare (ignore rulebase depth))
  (dolist (value '(left right))
    (multiple-value-bind (extended ok) (unify output value environment)
      (when ok (funcall emit extended)))))
```

Querying `(choose ?x)` then proves twice, binding `?x` to `left` and `right`.

## The `unify` contract

`unify` is the workhorse for foreign predicates:

```lisp
(unify left right &optional environment)
;; => (values extended-environment t)   on success
;; => (values nil nil)                  on failure
```

The occurs check is always enabled, so unification never introduces cyclic
terms. Environments are **persistent** association lists: `unify` extends rather
than mutates, so older environments remain valid choice points. See the
[API Reference](../reference/api.md#unification) for `unify`, `logic-substitute`,
`logic-var-p`, and `fresh-logic-variable`.

## Dispatch rules

- Foreign predicates dispatch by **exact name and arity**. If your predicate
  does not run, check both parts of the indicator — see
  [Troubleshooting](troubleshooting.md#a-foreign-predicate-did-not-run).
- Registered builtins remain **authoritative** when names overlap: you cannot
  shadow a builtin with a foreign predicate of the same indicator.
- Calling a known name at an unsupported arity denotes a different, undefined
  procedure and signals an ISO `existence_error`.

## Guards are not foreign predicates

The `(:when expression)` guard in the [Rule DSL](rule-dsl.md) is a lighter
mechanism: the DSL macros compile the expression into a precompiled closure
invoked as a `(:when function variable...)` goal. Guards are for pure Lisp
predicates over already-bound values; foreign predicates are for goals that
must unify outputs or emit multiple solutions. See
[Architecture](../reference/architecture.md#guards) for how guards are compiled.
